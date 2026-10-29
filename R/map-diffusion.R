# Tiles a map warped by a diffusion cartogram, so that each group keeps its
# shape and the borders between groups. The (simplified) shape of each group
# is warped so that its area is area^(1 - cartogram) * size^cartogram, where size is
# the area in proportion to its number of regions, so that cartogram moves from
# the real map (0) to a cartogram (1). The hexagons tile the warped
# landmasses, and are split between groups along the warped borders.
#
# Returns the tile of each region, with the warped point that each region was
# allocated from as the attribute "point", and the function that warps
# geometries as the attribute "warp" (both used by repair_groups()).
#' @importFrom sf st_area st_coordinates st_make_valid st_point_on_surface
#' @importFrom sf st_union
diffusion_tiles <- function(geometry, group, land, cellsize, cartogram = 0.75,
                            resolution = 256, proximity = 0.1, passes = 3) {
  id <- as.integer(factor(group, exclude = NULL))
  shape <- do.call(c, lapply(split(geometry, id), st_union))
  shape <- simplify_polygons(shape, cellsize / 10)
  area <- as.numeric(st_area(shape))
  size <- tabulate(id) / length(id) * sum(area)
  target <- area^(1 - cartogram) * size^cartogram
  target <- target / sum(target)

  # Repeat the warp until each group's area is within half a hexagon of its
  # target
  maps <- list()
  warped <- shape
  for (pass in seq_len(if (cartogram > 0) passes else 0)) {
    area <- as.numeric(st_area(warped))
    if (max(abs(area / sum(area) - target)) * length(id) < 0.5) break
    f <- diffusion_map(warped, target * sum(area), resolution)
    warped <- st_make_valid(map_coordinates(warped, f))
    maps <- c(maps, f)
  }
  warp <- function(x) {
    for (f in maps) x <- map_coordinates(x, f)
    x
  }

  # Without warping, the landmasses are tiled as they are
  land_shape <- land$geometry
  if (length(maps)) {
    land_shape <- simplify_polygons(land_shape, cellsize / 10)
    land_shape <- st_make_valid(warp(land_shape))
    cellsize <- hex_cellsize(land_shape, length(geometry))
  }
  point <- warp(st_point_on_surface(geometry))
  n <- tabulate(land$region, nbins = length(land_shape))
  grid <- landmass_grid(land_shape, n, cellsize)
  index <- allocate_tiles(point, grid$tile, group, land$region, grid$landmass,
                          warped[id], proximity)
  tile <- grid$tile[index]
  attr(tile, "point") <- st_coordinates(point)
  attr(tile, "warp") <- warp
  tile
}

# Finds the diffusion cartogram (Gastner & Newman, 2004) that resizes each
# shape to its target area. The density of each shape (target over area) is
# rasterised onto a square grid, with the mean density elsewhere, and spreads
# out by diffusion (solved exactly with the Fourier transform). Points of a
# regular grid of tracers are moved with the flow of density until it is
# uniform, and other points are moved by interpolating between the tracers.
#
# Returns a function that moves a matrix of coordinates.
#' @importFrom sf st_as_sf st_bbox st_crs st_intersects
diffusion_map <- function(shape, target, resolution = 256, padding = 0.75) {
  n <- resolution
  area <- as.numeric(st_area(shape))
  bbox <- st_bbox(shape)
  width <- max(bbox[3] - bbox[1], bbox[4] - bbox[2]) * (1 + 2 * padding)
  x0 <- (bbox[1] + bbox[3]) / 2 - width / 2
  y0 <- (bbox[2] + bbox[4]) / 2 - width / 2
  h <- width / n

  # The density of each grid cell, from the shape containing its centre
  centre <- expand.grid(x = x0 + (seq_len(n) - 0.5) * h,
                        y = y0 + (seq_len(n) - 0.5) * h)
  centre <- st_as_sf(centre, coords = c("x", "y"), crs = st_crs(shape))
  owner <- vapply(st_intersects(centre, shape), function(i) c(i, NA)[1], integer(1))
  density <- ifelse(is.na(owner), sum(target) / sum(area), (target / area)[owner])
  density <- matrix(density / mean(density), n, n)

  # Velocity of the density flow at time t, on the grid
  k <- 2 * pi * c(0:(n / 2), -((n / 2 - 1):1))[seq_len(n)] / n
  kx <- matrix(k, n, n)
  ky <- t(kx)
  k2 <- kx^2 + ky^2
  spectrum <- stats::fft(density)
  # The density and its gradient (packed as x + iy, as both are real)
  velocity <- function(t) {
    s <- spectrum * exp(-k2 * t)
    rho <- Re(stats::fft(s, inverse = TRUE))
    gradient <- stats::fft((1i * kx - ky) * s, inverse = TRUE) / rho
    list(x = -Re(gradient), y = -Im(gradient))
  }
  # Bilinear interpolation of a square grid at (0 based) grid coordinates
  interpolate <- function(m, x, y) {
    n <- nrow(m)
    x <- pmin(pmax(x, 0), n - 1.001)
    y <- pmin(pmax(y, 0), n - 1.001)
    i <- floor(x)
    j <- floor(y)
    fx <- x - i
    fy <- y - j
    k <- i + j * n + 1
    (m[k] * (1 - fx) + m[k + 1] * fx) * (1 - fy) +
      (m[k + n] * (1 - fx) + m[k + n + 1] * fx) * fy
  }

  # Move a tracer at every other grid point with midpoint steps, never more
  # than a fifth of a cell
  m <- n / 2
  gx <- matrix(2 * (seq_len(m) - 1), m, m)
  gy <- t(gx)
  x <- as.vector(gx)
  y <- as.vector(gy)
  t <- 0
  dt <- 0.5
  repeat {
    v <- velocity(t)
    vx <- interpolate(v$x, x, y)
    vy <- interpolate(v$y, x, y)
    speed <- max(sqrt(vx^2 + vy^2))
    if (speed < 1e-4 || t > 1e5) break
    step <- min(dt, 0.5 / speed)
    v <- velocity(t + step / 2)
    mx <- x + vx * step / 2
    my <- y + vy * step / 2
    x <- x + interpolate(v$x, mx, my) * step
    y <- y + interpolate(v$y, mx, my) * step
    t <- t + step
    dt <- dt * 1.1
  }
  dx <- matrix(x, m, m) - gx
  dy <- matrix(y, m, m) - gy

  function(xy) {
    x <- ((xy[, 1] - x0) / h - 0.5) / 2
    y <- ((xy[, 2] - y0) / h - 0.5) / 2
    cbind(xy[, 1] + interpolate(dx, x, y) * h, xy[, 2] + interpolate(dy, x, y) * h)
  }
}

# Moves every vertex of a geometry with a function of a coordinate matrix
#' @importFrom sf st_crs st_sfc
map_coordinates <- function(geometry, f) {
  move <- function(m) {
    m[, 1:2] <- f(m[, 1:2, drop = FALSE])
    m
  }
  st_sfc(lapply(geometry, function(x) {
    if (is.matrix(x)) return(structure(move(x), class = class(x)))
    if (is.numeric(x)) return(structure(move(matrix(x, 1))[1, ], class = class(x)))
    structure(rapply(unclass(x), move, classes = "matrix", how = "replace"),
              class = class(x))
  }), crs = st_crs(geometry))
}

# Simplifies each polygon separately, as GEOS simplifies the polygons of a
# multipolygon in an order that can differ between runs (giving different
# results)
#' @importFrom sf st_cast st_crs st_make_valid st_sfc st_simplify st_union
simplify_polygons <- function(geometry, tolerance) {
  do.call(c, lapply(geometry, function(x) {
    x <- st_cast(st_sfc(x, crs = st_crs(geometry)), "POLYGON")
    x <- st_simplify(x, preserveTopology = TRUE, dTolerance = tolerance)
    st_union(st_make_valid(x))
  }))
}
