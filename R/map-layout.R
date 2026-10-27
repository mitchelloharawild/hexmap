# Lays out each group as a compact block of hexagons, in the style of hand
# drawn election maps. Each group (on each landmass) is treated as a circle
# with the area of its hexagons. The circles are arranged so that each pair of
# neighbouring groups keeps its direction from the original map while just
# touching (weighted by the length of their shared border), and are then
# packed together without overlapping. The hexagons nearest the circles are used,
# filling the gaps between them, and split between groups by their distance
# to each group's centre, with one hexagon for each region.
#
# `anchor` optionally replaces the centroids of some groups when finding the
# direction between groups, to adjust the layout.
#
# Returns the hexagons and the hexagon of each region.
#' @importFrom sf st_area st_as_sfc st_bbox st_centroid st_coordinates st_crs
#' @importFrom sf st_make_grid st_point_on_surface st_union
compact_tiles <- function(geometry, group, region_land, cellsize, anchor = NULL) {
  hex_area <- sqrt(3) / 2 * cellsize^2
  point <- st_coordinates(st_point_on_surface(geometry))

  # The pieces of each group on each landmass
  piece <- interaction(as.integer(factor(group, exclude = NULL)), region_land,
                       drop = TRUE)
  region_piece <- as.integer(piece)
  first <- match(seq_len(nlevels(piece)), region_piece)
  piece_group <- group[first]
  piece_land <- region_land[first]
  shape <- do.call(c, lapply(split(geometry, piece), st_union))
  size <- tabulate(region_piece)
  radius <- sqrt(size * hex_area / pi)
  centroid <- st_coordinates(st_centroid(shape))[, 1:2, drop = FALSE]
  if (!is.null(anchor)) {
    pinned <- match(piece_group, rownames(anchor))
    centroid[!is.na(pinned), ] <- anchor[pinned[!is.na(pinned)], , drop = FALSE]
  }

  # Arrange the circles, keeping landmasses two hexagons apart
  same_land <- outer(piece_land, piece_land, "==")
  border <- shared_border(shape, cellsize / 10) * same_land
  distance <- outer(radius, radius, "+") + ifelse(same_land, 0, 2 * cellsize)
  centre <- arrange_circles(centroid, distance, border / max(border, 1) + 0.05)
  centre <- spread_circles(centre, distance)

  # Each region's position within its piece, scaled to the piece's circle
  scale <- sqrt(size * hex_area / as.numeric(st_area(shape)))
  layout <- centre[region_piece, , drop = FALSE] +
    (point - centroid[region_piece, , drop = FALSE]) * scale[region_piece]

  # A lattice of hexagons covering the circles
  margin <- max(radius) + 3 * cellsize
  bbox <- st_bbox(c(xmin = min(centre[, 1]) - margin, ymin = min(centre[, 2]) - margin,
                    xmax = max(centre[, 1]) + margin, ymax = max(centre[, 2]) + margin),
                  crs = st_crs(geometry))
  lattice <- st_make_grid(st_as_sfc(bbox), cellsize = cellsize, square = FALSE)
  to <- st_coordinates(st_centroid(lattice))
  adjacent <- hex_adjacency(st_centroid(lattice), cellsize)

  # Fill each landmass in turn, never using hexagons next to another landmass
  index <- integer(length(geometry))
  taken <- rep(FALSE, length(lattice))
  for (land in order(-tabulate(region_land))) {
    i <- which(region_land == land)
    if (!length(i)) next
    blocked <- taken | vapply(adjacent, function(k) any(taken[k]), logical(1))

    # Use the hexagons nearest the landmass's circles, filling gaps between them
    # (a soft minimum of the distance outside each circle)
    p <- unique(region_piece[i])
    outside <- sqrt(outer(to[, 1], centre[p, 1], "-")^2 +
                    outer(to[, 2], centre[p, 2], "-")^2) -
      rep(radius[p], each = nrow(to))
    outside <- -log(rowSums(exp(-outside / cellsize)))
    outside[blocked] <- Inf
    j <- order(outside)[seq_along(i)]

    # Split the hexagons between groups, nearest to each group's centre
    from <- centre[region_piece[i], , drop = FALSE]
    j <- j[solve_assignment(from, to[j, , drop = FALSE])]

    # Make each group's hexagons contiguous, then allocate them to regions
    tile_group <- contiguous_groups(region_piece[i], lapply(adjacent[j], function(k) {
      k <- match(k, j)
      k[!is.na(k)]
    }))
    if (!attr(tile_group, "contiguous")) {
      warning("Could not make the tiles of every group contiguous.", call. = FALSE)
    }
    for (g in p) {
      ig <- i[region_piece[i] == g]
      jg <- j[tile_group == g]
      index[ig] <- jg[solve_assignment(layout[ig, , drop = FALSE], to[jg, , drop = FALSE])]
    }
    taken[j] <- TRUE
  }
  list(tile = lattice, index = index)
}

# The length of the border shared by each pair of shapes, within a tolerance
#' @importFrom sf st_boundary st_buffer st_intersection st_length
shared_border <- function(shape, tolerance) {
  n <- length(shape)
  boundary <- st_boundary(shape)
  near <- st_buffer(shape, tolerance)
  border <- matrix(0, n, n)
  for (a in seq_len(n)) for (b in seq_len(n)) {
    if (a == b) next
    border[a, b] <- sum(as.numeric(st_length(st_intersection(boundary[a], near[b]))))
  }
  (border + t(border)) / 2
}

# Places circles so that the offset between each pair is as close as possible
# (in weighted least squares) to the direction between their original
# positions with the given distance, keeping the mean position unchanged.
arrange_circles <- function(xy, distance, weight) {
  n <- nrow(xy)
  if (n == 1) return(xy)
  dx <- outer(xy[, 1], xy[, 1], "-")
  dy <- outer(xy[, 2], xy[, 2], "-")
  d <- pmax(sqrt(dx^2 + dy^2), 1e-9)
  diag(weight) <- 0
  # Minimise sum(weight * |(p_i - p_j) - target_ij|^2) over positions p
  laplacian <- diag(rowSums(weight)) - weight
  target_x <- rowSums(weight * dx / d * distance)
  target_y <- rowSums(weight * dy / d * distance)
  # The mean position is fixed to remove the free translation
  system <- rbind(cbind(laplacian, 1), c(rep(1, n), 0))
  solve_xy <- function(target, mean) solve(system, c(target, n * mean))[seq_len(n)]
  cbind(solve_xy(target_x, mean(xy[, 1])), solve_xy(target_y, mean(xy[, 2])))
}

# Packs circles together by pulling them towards their (area weighted) centre,
# while moving them apart until each pair is at least the given distance apart
spread_circles <- function(xy, distance, iterations = 200) {
  radius <- diag(distance) / 2
  share <- outer(radius, radius, function(a, b) b / (a + b))
  diag(distance) <- 0
  for (iter in seq_len(iterations)) {
    middle <- colSums(xy * radius^2) / sum(radius^2)
    xy <- xy + (rep(middle, each = nrow(xy)) - xy) * 0.02
    for (pass in 1:5) {
      dx <- outer(xy[, 1], xy[, 1], "-")
      dy <- outer(xy[, 2], xy[, 2], "-")
      d <- pmax(sqrt(dx^2 + dy^2), 1e-9)
      # Smaller circles move more than larger ones
      step <- pmax(distance - d, 0) * share / d
      xy <- xy + cbind(rowSums(step * dx), rowSums(step * dy))
    }
  }
  xy
}
