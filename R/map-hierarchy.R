# Lays out the groups first, then the regions within each group. The groups
# are warped by a diffusion cartogram (to areas in proportion to their numbers
# of regions), and the warp is blended by shape towards a scaled copy of each
# group's own shape, so groups keep their shapes better than the warp alone
# allows. A hexagon lattice is grown over the warped landmasses, and split
# between the pieces of each group (on each landmass) with exactly their
# number of regions, nearest to a mix of the warped piece and its scaled
# copy, which is moved towards the piece's hexagons over a few rounds. Pieces
# with little coast are kept off the edge of the hexagons, and the borders
# between pieces are then adjusted (by simulated annealing) so that pieces
# touch where they share a border on the map. Within each piece, regions are
# placed by the transform that matches the spread of the piece on the map to
# the spread of its hexagons, mixed with the warp.
#
# Returns the tile of each region, with the points that regions were
# allocated from as the attribute "point" and the warp as the attribute
# "warp" (both used by repair_groups()).
#' @importFrom sf st_area st_as_sfc st_bbox st_boundary st_buffer st_centroid
#' @importFrom sf st_coordinates st_crs st_crs<- st_intersection st_is_within_distance
#' @importFrom sf st_length st_make_grid st_make_valid st_point_on_surface st_union
hierarchy_tiles <- function(geometry, group, land, cellsize, shape = 0.4,
                            resolution = 256, iterations = 30 * length(geometry),
                            weight = c(copy = 0.6, coast = 8)) {
  n_region <- length(geometry)
  id <- as.integer(factor(group, exclude = NULL))
  group_shape <- do.call(c, lapply(split(geometry, id), st_union))
  group_shape <- simplify_polygons(group_shape, cellsize / 10)
  warp <- warp_groups(group_shape, tabulate(id), resolution, shape, 1.5 * cellsize)

  # The pieces of each group on each landmass, warped and as scaled copies
  piece <- as.integer(interaction(id, land$region, drop = TRUE))
  n_piece <- max(piece)
  first <- match(seq_len(n_piece), piece)
  piece_land <- land$region[first]
  size <- tabulate(piece, n_piece)
  original <- do.call(c, lapply(split(geometry, piece), st_union))
  simple <- simplify_polygons(original, cellsize / 10)
  coast <- coast_share(simple, land$geometry, cellsize / 20)
  neighbour <- st_is_within_distance(original, original, cellsize / 20, sparse = FALSE) &
    outer(piece_land, piece_land, "==")
  warped <- st_make_valid(warp(simple))
  cellsize <- hex_cellsize(warped, n_region)
  hex_area <- sqrt(3) / 2 * cellsize^2
  from <- st_coordinates(st_centroid(simple))[, 1:2, drop = FALSE]
  at <- st_coordinates(st_centroid(warped))[, 1:2, drop = FALSE]
  scale <- sqrt(size * hex_area / as.numeric(st_area(simple)))
  place <- function(at) {
    scaled <- do.call(c, lapply(seq_len(n_piece), function(p) {
      (simple[p] - from[p, ]) * scale[p] + at[p, ]
    }))
    st_crs(scaled) <- st_crs(warped)
    st_make_valid(scaled)
  }
  scaled <- place(at)

  # Grow each landmass's hexagons on a lattice, nearest to its pieces
  bbox <- st_bbox(c(warped, scaled))
  bbox[] <- bbox + c(-1, -1, 1, 1) * 4 * cellsize
  lattice <- st_make_grid(st_as_sfc(bbox), cellsize = cellsize, square = FALSE)
  centre <- st_centroid(lattice)
  to_warped <- signed_distance(centre, warped) / cellsize
  to_scaled <- signed_distance(centre, scaled) / cellsize
  adjacent <- hex_adjacency(centre, cellsize)
  n_land <- tabulate(land$region)
  owner <- integer(length(lattice))
  for (b in order(n_land, decreasing = TRUE)) {
    if (n_land[b] == 0) next
    k <- piece_land == b
    score <- 0.7 * apply(to_warped[, k, drop = FALSE], 1, min) +
      0.3 * apply(to_scaled[, k, drop = FALSE], 1, min)
    taken <- owner > 0
    blocked <- taken | vapply(adjacent, function(i) any(taken[i]), logical(1))
    tiles <- grow_tiles(n_land[b], order(score), adjacent, blocked)
    owner[fill_tiles(tiles, score, adjacent, blocked)] <- b
  }
  keep <- which(owner > 0)
  tile <- lattice[keep]
  centre <- centre[keep]
  hex_land <- owner[keep]
  xy <- st_coordinates(centre)
  to_warped <- to_warped[keep, , drop = FALSE]
  adjacent <- hex_adjacency(centre, cellsize)

  # Pieces with little coast on the map stay off the edge of the hexagons
  extra <- matrix(0, length(tile), n_piece)
  extra[lengths(adjacent) < 6, coast < 0.15] <- weight[["coast"]]

  # Split each landmass's hexagons between its pieces, moving the scaled
  # copies halfway to the centroid of their hexagons each round
  for (round in 0:4) {
    distance <- (1 - weight[["copy"]]) * to_warped +
      weight[["copy"]] * signed_distance(centre, scaled) / cellsize
    cost <- distance + pmax(distance, 0)^2 + extra
    label <- integer(length(tile))
    for (b in unique(land$region)) {
      i <- which(land$region == b)
      j <- which(hex_land == b)
      split <- cost[j, piece[i], drop = FALSE]
      label[j] <- piece[i][as.integer(clue::solve_LSAP(split - min(split)))]
      label[j] <- contiguous_groups(label[j], hex_adjacency(centre[j], cellsize))
    }
    if (round == 4) break
    at <- at + 0.5 * (rowsum(xy, label)[as.character(seq_len(n_piece)), , drop = FALSE] /
                        size - st_coordinates(st_centroid(scaled))[, 1:2, drop = FALSE])
    scaled <- place(at)
  }
  label <- anneal_pieces(label, cost, neighbour, adjacent, xy / cellsize, iterations)

  # Place the regions of each piece on its hexagons
  point <- st_coordinates(st_point_on_surface(geometry))
  warped_point <- st_coordinates(warp(st_point_on_surface(geometry)))
  index <- integer(n_region)
  allocated <- matrix(NA_real_, n_region, 2)
  for (p in seq_len(n_piece)) {
    i <- which(piece == p)
    j <- which(label == p)
    hex <- xy[j, , drop = FALSE]
    middle <- colMeans(hex)
    if (length(i) >= 3) {
      # The spread of a hexagon is about that of a disc of its area
      spread <- polygon_moments(original[p])
      A <- matrix_sqrt(stats::cov(hex) + diag(0.45 * hex_area / pi, 2)) %*%
        matrix_sqrt(spread$cov, inverse = TRUE)
      to <- sweep(point[i, , drop = FALSE], 2, spread$mean) %*% t(A)
    } else {
      to <- sweep(point[i, , drop = FALSE], 2, from[p, ]) * scale[p]
    }
    to <- sweep(to, 2, middle, "+")
    by_warp <- sweep(warped_point[i, , drop = FALSE], 2,
                     colMeans(warped_point[i, , drop = FALSE]) - middle)
    to <- 0.7 * to + 0.3 * by_warp
    index[i] <- j[solve_assignment(to, hex)]
    allocated[i, ] <- to
  }
  tile <- tile[index]
  attr(tile, "point") <- allocated
  attr(tile, "warp") <- warp
  tile
}

# Finds a diffusion cartogram that resizes each shape in proportion to n, and
# blends it by blend towards scaling each shape about its centroid (keeping
# its warped area and centroid). The blend is weighted by each shape's
# indicator blurred by a Gaussian of width sigma, so the warp stays smooth and
# shapes don't come apart.
#
# Returns a function that warps geometries.
#' @importFrom sf st_area st_as_sf st_bbox st_centroid st_coordinates st_crs
#' @importFrom sf st_intersects st_make_valid
warp_groups <- function(shape, n, resolution, blend, sigma, passes = 6, grid = 200) {
  area <- as.numeric(st_area(shape))
  target <- n / sum(n)
  maps <- list()
  warped <- shape
  for (pass in seq_len(passes)) {
    now <- as.numeric(st_area(warped))
    if (max(abs(now / sum(now) - target)) * sum(n) < 0.25) break
    f <- diffusion_map(warped, target * sum(now), resolution)
    warped <- st_make_valid(map_coordinates(warped, f))
    maps <- c(maps, f)
  }
  diffuse <- function(xy) {
    for (f in maps) xy <- f(xy)
    xy
  }
  if (blend == 0) return(function(x) map_coordinates(x, diffuse))

  from <- st_coordinates(st_centroid(shape))[, 1:2, drop = FALSE]
  to <- st_coordinates(st_centroid(warped))[, 1:2, drop = FALSE]
  scale <- sqrt(as.numeric(st_area(warped)) / area)

  # The weight of each shape on a square grid, blurred with the Fourier
  # transform
  bbox <- st_bbox(shape)
  width <- 1.4 * max(bbox[3] - bbox[1], bbox[4] - bbox[2])
  step <- width / grid
  x0 <- (bbox[1] + bbox[3]) / 2 - width / 2
  y0 <- (bbox[2] + bbox[4]) / 2 - width / 2
  cell <- expand.grid(x = x0 + (seq_len(grid) - 0.5) * step,
                      y = y0 + (seq_len(grid) - 0.5) * step)
  cell <- st_as_sf(cell, coords = c("x", "y"), crs = st_crs(shape))
  inside <- vapply(st_intersects(cell, shape), function(i) c(i, NA)[1], integer(1))
  k <- c(0:(grid / 2), -((grid / 2 - 1):1))
  kernel <- exp(-outer(k^2, k^2, "+") / (2 * (sigma / step)^2))
  kernel <- stats::fft(kernel / sum(kernel))
  weight <- lapply(seq_along(shape), function(g) {
    m <- matrix(as.numeric(inside %in% g), grid, grid)
    pmax(Re(stats::fft(stats::fft(m) * kernel, inverse = TRUE)) / grid^2, 0)
  })
  interpolate <- function(m, x, y) {
    x <- pmin(pmax(x, 0), grid - 1.001)
    y <- pmin(pmax(y, 0), grid - 1.001)
    i <- floor(x)
    j <- floor(y)
    fx <- x - i
    fy <- y - j
    k <- i + j * grid + 1
    (m[k] * (1 - fx) + m[k + 1] * fx) * (1 - fy) +
      (m[k + grid] * (1 - fx) + m[k + grid + 1] * fx) * fy
  }
  blended <- function(xy) {
    moved <- diffuse(xy)
    gx <- (xy[, 1] - x0) / step - 0.5
    gy <- (xy[, 2] - y0) / step - 0.5
    w <- matrix(vapply(weight, interpolate, numeric(nrow(xy)), x = gx, y = gy), nrow(xy))
    w <- w / (rowSums(w) + 1e-12)
    shift <- matrix(0, nrow(xy), 2)
    for (g in seq_along(shape)) {
      similar <- sweep(sweep(xy, 2, from[g, ]) * scale[g], 2, to[g, ], "+")
      shift <- shift + w[, g] * (similar - moved)
    }
    moved + blend * shift
  }
  function(x) map_coordinates(x, blended)
}

# The share of each shape's boundary that lies on the coast of the land. The
# land is first closed by tolerance, to remove slivers between regions that
# were simplified separately.
#' @importFrom sf st_boundary st_buffer st_intersection st_length st_union
coast_share <- function(shape, land, tolerance) {
  land <- st_buffer(st_buffer(st_union(land), tolerance), -tolerance)
  coast <- st_buffer(st_boundary(land), tolerance)
  vapply(seq_along(shape), function(p) {
    edge <- st_boundary(shape[p])
    sum(as.numeric(st_length(st_intersection(edge, coast)))) / as.numeric(st_length(edge))
  }, numeric(1))
}

# Distance from each point to the boundary of each shape, negative inside
#' @importFrom sf st_boundary st_distance st_intersects
signed_distance <- function(point, shape) {
  d <- matrix(as.numeric(st_distance(point, st_boundary(shape))), ncol = length(shape))
  ifelse(st_intersects(point, shape, sparse = FALSE), -d, d)
}

# Swaps hexagons in pairs across the borders between pieces (by simulated
# annealing), keeping the number of hexagons of each piece and its
# contiguity, to minimise the cost of each hexagon's piece plus 20 times the
# contacts that are wrong. Pieces that touch but shouldn't count once, and
# again for each hexagon edge between them, so that contacts can be undone
# gradually; missing contacts count by how far apart the pieces are.
anneal_pieces <- function(label, cost, neighbour, adjacent, xy, iterations) {
  n_piece <- ncol(cost)
  edge <- cbind(rep(seq_along(adjacent), lengths(adjacent)), unlist(adjacent))
  edge <- edge[edge[, 1] < edge[, 2], , drop = FALSE]
  total <- function(label) {
    a <- label[edge[, 1]]
    b <- label[edge[, 2]]
    cut <- a != b
    touch <- matrix(FALSE, n_piece, n_piece)
    touch[cbind(c(a[cut], b[cut]), c(b[cut], a[cut]))] <- TRUE
    wrong <- sum(touch & !neighbour) / 2 + sum(cut & !neighbour[cbind(a, b)]) / 2
    for (p in which(neighbour & !touch & upper.tri(touch))) {
      pq <- arrayInd(p, dim(touch))
      from <- xy[label == pq[1], , drop = FALSE]
      to <- xy[label == pq[2], , drop = FALSE]
      wrong <- wrong + sqrt(min(outer(from[, 1], to[, 1], "-")^2 +
                                  outer(from[, 2], to[, 2], "-")^2))
    }
    sum(cost[cbind(seq_along(label), label)]) + 20 * wrong
  }
  pieces <- function(label, p) max(0L, connected_components(adjacent, which(label == p)))

  # Use a fixed seed without changing the user's random numbers
  if (exists(".Random.seed", globalenv())) {
    seed <- get(".Random.seed", globalenv())
    on.exit(assign(".Random.seed", seed, globalenv()))
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()))
  }
  set.seed(1)

  current <- total(label)
  best <- label
  lowest <- current
  for (iter in seq_len(if (n_piece > 1) iterations else 0)) {
    temperature <- 5 * (1 - iter / iterations) + 0.01
    i <- sample.int(length(label), 1)
    across <- adjacent[[i]][label[adjacent[[i]]] != label[i]]
    if (!length(across)) next
    a <- label[i]
    b <- label[across[sample.int(length(across), 1)]]
    candidate <- which(label == b)
    candidate <- candidate[vapply(adjacent[candidate], function(k) any(label[k] == a),
                                  logical(1))]
    j <- candidate[sample.int(length(candidate), 1)]
    swapped <- label
    swapped[c(i, j)] <- c(b, a)
    proposed <- total(swapped)
    if (proposed <= current || stats::runif(1) < exp((current - proposed) / temperature)) {
      if (pieces(swapped, a) > 1 || pieces(swapped, b) > 1) next
      label <- swapped
      current <- proposed
      if (current < lowest) {
        best <- label
        lowest <- current
      }
    }
  }
  best
}

# Fills the dents and holes of a set of hexagons (those filled by growing and
# then shrinking it by one hexagon), by moving hexagons that stick out (with
# the most empty neighbours, and the highest score) into the empty hexagons
# with the lowest score, keeping the set contiguous.
fill_tiles <- function(tiles, score, adjacent, blocked) {
  chosen <- seq_along(adjacent) %in% tiles
  grow <- function(x) x | vapply(adjacent, function(i) any(x[i]), logical(1))
  shrink <- function(x) x & vapply(adjacent, function(i) length(i) == 6 && all(x[i]), logical(1))
  for (iter in seq_len(2 * length(tiles))) {
    holes <- which(shrink(grow(chosen)) & !chosen & !blocked)
    if (!length(holes)) break
    add <- holes[which.min(score[holes])]
    empty <- vapply(adjacent, function(i) sum(!chosen[i]) + 6 - length(i), numeric(1))
    candidate <- which(chosen & empty >= 3)
    candidate <- candidate[!candidate %in% adjacent[[add]]]
    candidate <- utils::head(candidate[order(-empty[candidate], -score[candidate])], 10)
    moved <- FALSE
    for (r in candidate) {
      new <- chosen
      new[c(r, add)] <- c(FALSE, TRUE)
      if (max(connected_components(adjacent, which(new))) == 1) {
        chosen <- new
        moved <- TRUE
        break
      }
    }
    if (!moved) break
  }
  which(chosen)
}

# The centroid and covariance of the area of a polygon
#' @importFrom sf st_coordinates
polygon_moments <- function(shape) {
  xy <- st_coordinates(shape)
  ring <- xy[, grepl("^L", colnames(xy)), drop = FALSE]
  shift <- colMeans(xy[, 1:2])
  key <- do.call(paste, as.data.frame(ring))
  total <- numeric(6)
  for (r in split(seq_len(nrow(xy)), factor(key, unique(key)))) {
    x <- xy[r, 1] - shift[1]
    y <- xy[r, 2] - shift[2]
    x0 <- x[-length(r)]
    x1 <- x[-1]
    y0 <- y[-length(r)]
    y1 <- y[-1]
    cross <- x0 * y1 - x1 * y0
    m <- c(sum(cross) / 2, sum((x0 + x1) * cross) / 6, sum((y0 + y1) * cross) / 6,
           sum((x0^2 + x0 * x1 + x1^2) * cross) / 12,
           sum((y0^2 + y0 * y1 + y1^2) * cross) / 12,
           sum((x0 * y1 + 2 * x0 * y0 + 2 * x1 * y1 + x1 * y0) * cross) / 24)
    # Holes (rings after the first of each polygon) are subtracted
    total <- total + m * sign(m[1]) * (if (ring[r[1], 1] > 1) -1 else 1)
  }
  mean <- total[2:3] / total[1]
  list(mean = mean + shift,
       cov = matrix(total[c(4, 6, 6, 5)], 2) / total[1] - outer(mean, mean))
}

# The square root (or inverse square root) of a symmetric matrix
matrix_sqrt <- function(m, inverse = FALSE) {
  e <- eigen(m, symmetric = TRUE)
  v <- pmax(e$values, 1e-12 * max(e$values))
  e$vectors %*% diag(if (inverse) 1 / sqrt(v) else sqrt(v), 2) %*% t(e$vectors)
}
