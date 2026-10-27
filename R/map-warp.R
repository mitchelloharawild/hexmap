# Finds a continuous deformation of the plane that resizes each group's area
# in proportion to its number of regions, using the rubber sheet cartogram
# algorithm of Dougenik, Chrisman & Niemeyer (1985). Groups keep their shape
# and neighbours as closely as possible while being resized.
#
# The geometry is given as the shape of each group (or its pieces), with the
# number of regions of each group as its desired size.
#
# Returns the forces of each iteration, to be applied with warp_apply().
#' @importFrom sf st_area st_centroid st_coordinates
cartogram_forces <- function(shape, group, size, tolerance = 0.02, max_iter = 50) {
  forces <- list()
  for (iter in seq_len(max_iter)) {
    # Warped shapes may be slightly invalid, so their pieces are not unioned
    piece_area <- as.numeric(st_area(shape))
    piece_centre <- st_coordinates(st_centroid(shape))[, 1:2, drop = FALSE]
    area <- as.numeric(tapply(piece_area, group, sum))
    centre <- rowsum(piece_centre * piece_area, group) / area
    desired <- size / sum(size) * sum(area)
    error <- pmax(area, desired) / pmin(area, desired)
    if (max(error) < 1 + tolerance) break

    radius <- sqrt(area / pi)
    force <- list(
      centre = centre,
      radius = radius,
      mass = sqrt(desired / pi) - radius,
      reduction = 1 / mean(error)
    )
    shape <- warp_apply(shape, list(force))
    forces <- c(forces, list(force))
  }
  forces
}

# Moves every vertex of a geometry by the cartogram forces. The coordinates of
# all geometries are gathered and moved together, then put back in place.
#' @importFrom sf st_crs st_sfc
warp_apply <- function(geometry, forces) {
  each_matrix <- function(f) lapply(geometry, function(x) {
    if (is.matrix(x)) return(structure(f(x), class = class(x)))
    if (is.numeric(x)) return(structure(f(matrix(x, 1))[1, ], class = class(x)))
    structure(rapply(unclass(x), f, classes = "matrix", how = "replace"),
              class = class(x))
  })

  coords <- list()
  each_matrix(function(m) {
    coords[[length(coords) + 1]] <<- m[, 1:2, drop = FALSE]
    m
  })
  xy <- do.call(rbind, coords)
  for (force in forces) xy <- warp_points(xy, force)

  end <- cumsum(vapply(coords, nrow, integer(1)))
  k <- 0L
  st_sfc(each_matrix(function(m) {
    k <<- k + 1L
    m[, 1:2] <- xy[end[k] - nrow(m) + seq_len(nrow(m)), ]
    m
  }), crs = st_crs(geometry))
}

warp_points <- function(xy, force) {
  shift <- xy * 0
  for (k in seq_along(force$radius)) {
    dx <- xy[, 1] - force$centre[k, 1]
    dy <- xy[, 2] - force$centre[k, 2]
    d <- pmax(sqrt(dx^2 + dy^2), 1e-9)
    r <- force$radius[k]
    size <- ifelse(d > r, force$mass[k] * r / d,
                   force$mass[k] * (d / r)^2 * (4 - 3 * d / r))
    shift <- shift + cbind(dx, dy) / d * size
  }
  xy + shift * force$reduction
}
