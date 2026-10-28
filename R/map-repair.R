# Moves the borders between groups so that groups touch where they share a
# border on the map, and don't touch where they don't. The hexagons are
# unchanged, and the pieces of each group (on each landmass) swap hexagons in
# pairs across their borders, which keeps the number of hexagons in each
# piece and never splits a piece apart. Swaps are found by simulated
# annealing, minimising the contacts that are wrong (missing contacts count
# by how far apart the pieces are) while keeping the pieces compact and near
# where they were. Regions of pieces that change are then reallocated to
# their piece's hexagons, nearest to the hexagons they had.
#
# Returns the tile of each region.
#' @importFrom sf st_area st_centroid st_coordinates st_simplify st_union
repair_groups <- function(tile, geometry, group, region_land, iterations = 30 * length(tile),
                          weight = c(contact = 20, border = 1, position = 0.3)) {
  piece <- as.integer(interaction(as.integer(factor(group, exclude = NULL)),
                                  region_land, drop = TRUE))
  n_piece <- max(piece)
  if (n_piece < 2) return(tile)
  cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
  centre <- st_centroid(tile)
  xy <- st_coordinates(centre) / cellsize
  adjacent <- hex_adjacency(centre, cellsize)

  # The pieces that share a border on the map
  shape <- do.call(c, lapply(split(geometry, piece), st_union))
  shape <- st_simplify(shape, preserveTopology = TRUE, dTolerance = cellsize / 10)
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
  for (iter in seq_len(iterations)) {
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

  # Reallocate the regions of the pieces that changed
  original <- tile
  for (p in unique(c(piece[best != piece], best[best != piece]))) {
    i <- which(piece == p)
    j <- which(best == p)
    tile[i] <- original[j][solve_assignment(xy[i, , drop = FALSE], xy[j, , drop = FALSE])]
  }
  tile
}
