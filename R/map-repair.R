# Moves the borders between groups so that groups touch where they share a
# border on the map, and don't touch where they don't. The hexagons are
# unchanged, and the pieces of each group (on each landmass) swap hexagons in
# pairs across their borders, which keeps the number of hexagons in each
# piece and never splits a piece apart. Swaps are found by simulated
# annealing, minimising the contacts that are wrong (missing contacts count
# by how far apart the pieces are) while keeping the pieces compact and near
# where they were. Regions of pieces that change are then reallocated to
# their piece's hexagons, nearest to the points the layout allocated them from
# (the attribute "point" of tile, in the coordinates of the tiles), or else to
# the hexagons they had. With a border weight, the regions of every piece are
# reallocated, also keeping their distances to borders (see border_cost()).
#
# Returns the tile of each region.
#' @importFrom sf st_area st_centroid st_coordinates st_union
repair_groups <- function(tile, geometry, group, region_land, iterations = 30 * length(tile),
                          border = 0, weight = c(contact = 20, border = 1, position = 0.3)) {
  piece <- as.integer(interaction(as.integer(factor(group, exclude = NULL)),
                                  region_land, drop = TRUE))
  n_piece <- max(piece)
  if (n_piece < 2 && border == 0) return(tile)
  cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
  centre <- st_centroid(tile)
  xy <- st_coordinates(centre) / cellsize
  adjacent <- hex_adjacency(centre, cellsize)

  # The pieces that share a border on the map
  shape <- do.call(c, lapply(split(geometry, piece), st_union))
  shape <- simplify_polygons(shape, cellsize / 10)
  first <- match(seq_len(n_piece), piece)
  neighbour <- shared_border(shape, cellsize / 10) > 0 &
    outer(region_land[first], region_land[first], "==")

  edge <- cbind(rep(seq_along(adjacent), lengths(adjacent)), unlist(adjacent))
  edge <- edge[edge[, 1] < edge[, 2], , drop = FALSE]
  size <- tabulate(piece, n_piece)
  start <- rowsum(xy, piece) / size
  cost <- function(label) {
    a <- label[edge[, 1]]
    b <- label[edge[, 2]]
    cut <- a != b
    touch <- matrix(FALSE, n_piece, n_piece)
    touch[cbind(c(a[cut], b[cut]), c(b[cut], a[cut]))] <- TRUE
    wrong <- sum(touch & !neighbour) / 2
    for (p in which(neighbour & !touch & upper.tri(touch))) {
      pq <- arrayInd(p, dim(touch))
      from <- xy[label == pq[1], , drop = FALSE]
      to <- xy[label == pq[2], , drop = FALSE]
      wrong <- wrong + sqrt(min(outer(from[, 1], to[, 1], "-")^2 +
                                  outer(from[, 2], to[, 2], "-")^2))
    }
    drift <- sum(size * rowSums((rowsum(xy, label) / size - start)^2))
    sum(weight * c(wrong, sum(cut), drift))
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

  label <- piece
  current <- cost(label)
  best <- label
  lowest <- current
  for (iter in seq_len(if (n_piece > 1) iterations else 0)) {
    temperature <- 5 * (1 - iter / iterations) + 0.01
    # A hexagon on a border, and a hexagon across it that touches its piece
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
    if (pieces(swapped, a) > pieces(label, a) || pieces(swapped, b) > pieces(label, b)) next
    proposed <- cost(swapped)
    if (proposed <= current || stats::runif(1) < exp((current - proposed) / temperature)) {
      label <- swapped
      current <- proposed
      if (current < lowest) {
        best <- label
        lowest <- current
      }
    }
  }

  # Reallocate the regions of the pieces that changed. Reallocating from the
  # hexagons they had would compound how far they moved from their points.
  from <- if (is.null(attr(tile, "point"))) xy else attr(tile, "point") / cellsize
  warp <- attr(tile, "warp")
  attr(tile, "point") <- NULL
  attr(tile, "warp") <- NULL
  changed <- unique(c(piece[best != piece], best[best != piece]))
  if (border > 0) {
    extra <- border_cost(geometry, group, region_land, best, piece, xy, adjacent,
                         cellsize, warp)
    changed <- seq_len(n_piece)
  }
  original <- tile
  for (p in changed) {
    i <- which(piece == p)
    j <- which(best == p)
    cost <- squared_distance(from[i, , drop = FALSE], xy[j, , drop = FALSE])
    if (border > 0) cost <- cost + border * extra(i, j)
    tile[i] <- original[j][as.integer(clue::solve_LSAP(cost))]
  }
  tile
}

# Returns a function giving the cost of placing regions i on hexagons j (the
# indices of the regions holding them, with hex_piece the piece of each), as the squared difference between how
# far each region is from each border and how far its hexagon is. The borders
# are the coast (the edge of the land, or of the hexagons) and each other
# group. Distances are measured from the region's polygon, so large regions
# touch more borders, in hexagon widths on the map warped by warp (if any),
# and are capped at cap widths, so only regions near a border are held to it.
#' @importFrom sf st_boundary st_distance st_union
border_cost <- function(geometry, group, region_land, hex_piece, piece, xy, adjacent,
                        cellsize, warp = NULL, cap = 3) {
  if (is.null(warp)) warp <- identity
  id <- as.integer(factor(group, exclude = NULL))
  union_by <- function(by) {
    shape <- do.call(c, lapply(split(geometry, by), st_union))
    warp(simplify_polygons(shape, cellsize / 10))
  }
  region <- warp(simplify_polygons(geometry, cellsize / 10))
  map <- matrix(as.numeric(st_distance(region, union_by(id))), length(id)) / cellsize
  map[cbind(seq_along(id), id)] <- NA
  coast <- st_boundary(union_by(region_land))
  land <- sort(unique(region_land))
  map <- cbind(map, as.numeric(st_distance(region, coast))[seq_along(id) +
    length(id) * (match(region_land, land) - 1)] / cellsize)

  # The same distances on the hexagons: from each hexagon to the nearest
  # hexagon of each group (0 if adjacent), and to the edge of the hexagons
  hex_group <- id[match(hex_piece, piece)]
  nearest <- function(to) {
    to <- xy[to, , drop = FALSE]
    vapply(seq_len(nrow(xy)), function(k) sqrt(min((to[, 1] - xy[k, 1])^2 + (to[, 2] - xy[k, 2])^2)),
           numeric(1))
  }
  hex <- vapply(seq_len(max(id)), function(g) pmax(nearest(which(hex_group == g)) - 1, 0),
                numeric(nrow(xy)))
  hex <- cbind(hex, nearest(which(lengths(adjacent) < 6)))

  map <- pmin(map, cap)
  hex <- pmin(hex, cap)
  function(i, j) {
    cost <- matrix(0, length(i), length(j))
    for (k in seq_len(ncol(map))) {
      m <- map[i, k]
      if (all(is.na(m)) || (all(m >= cap, na.rm = TRUE) && all(hex[j, k] >= cap))) next
      mismatch <- outer(m, hex[j, k], "-")^2
      mismatch[is.na(mismatch)] <- 0
      cost <- cost + mismatch
    }
    cost
  }
}
