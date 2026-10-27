#' Allocates tiled geometries to original map geometries
#'
#' Allocates each tile to exactly one of the original geometries, such that
#' the total squared distance between each geometry and its tile is minimised.
#' This optimal assignment is solved with the Hungarian algorithm via
#' [clue::solve_LSAP()].
#'
#' Minimising squared (rather than absolute) distance guarantees that no two
#' geometries swap their relative position: for any two regions, the
#' displacement from one tile to the other never points against the
#' displacement between the original regions.
#'
#' When `group` is given, the tiles of each group are kept contiguous. The
#' tiles are first allocated as above, then tiles that are cut off from the
#' rest of their group are moved to its border by shifting tiles of other
#' groups along the shortest path between them. Finally, the regions of each
#' group are optimally allocated to that group's tiles. This guarantee of
#' contiguity comes at the cost of the no-swap guarantee between groups.
#'
#' @param object An sf object or geometry that was used to be tiled.
#' @param tile A vector of tile geometries with one tile per row of `object`,
#'   such as those produced by [hex_grid()].
#' @param group Optionally, a column of `object` (unquoted) that groups its
#'   rows (e.g. states). The tiles of each group will be contiguous.
#'
#' @return
#' `object` with its geometry replaced by the allocated tiles.
#'
#' @examples
#' library(ozmaps)
#' library(sf)
#' australia <- st_transform(abs_ste, 3577)
#' australia <- australia[australia$NAME != "Other Territories", ]
#' grid <- hex_grid(australia, n_tiles = 8)
#' hexes <- tile_allocate(australia, grid)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = australia) +
#'   geom_sf(data = hexes, aes(fill = NAME), alpha = 0.5)
#'
#' @importFrom sf st_geometry st_geometry<-
#' @export
tile_allocate <- function(object, tile, group = NULL) {
  group <- eval(substitute(group), attribute_data(object), parent.frame())
  tile <- st_geometry(tile)
  if (length(tile) != NROW(object)) {
    stop("`tile` must contain exactly one geometry for each row of `object`.",
         call. = FALSE)
  }
  check_group(group, object)
  check_not_empty(object)

  st_geometry(object) <- tile[allocate_tiles(st_geometry(object), tile, group)]
  object
}

# Returns the index of the tile allocated to each region. Regions may only be
# allocated tiles from the same block (e.g. landmass).
#' @importFrom sf st_area st_centroid st_coordinates st_point_on_surface
allocate_tiles <- function(geometry, tile, group = NULL,
                           region_block = rep(1L, length(geometry)),
                           tile_block = rep(1L, length(tile))) {
  from <- st_coordinates(st_point_on_surface(geometry))
  centre <- st_centroid(tile)
  to <- st_coordinates(centre)
  cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
  if (!is.null(group)) group <- as.integer(factor(group, exclude = NULL))

  index <- integer(length(geometry))
  for (block in unique(region_block)) {
    i <- which(region_block == block)
    j <- which(tile_block == block)
    index[i] <- j[solve_assignment(from[i, , drop = FALSE], to[j, , drop = FALSE])]
    if (is.null(group)) next

    # Make each group's tiles contiguous, then allocate tiles within groups
    tile_group <- group[i][match(j, index[i])]
    tile_group <- contiguous_groups(tile_group, hex_adjacency(centre[j], cellsize))
    for (g in unique(group[i])) {
      ig <- i[group[i] == g]
      jg <- j[tile_group == g]
      index[ig] <- jg[solve_assignment(from[ig, , drop = FALSE], to[jg, , drop = FALSE])]
    }
  }
  index
}

# Minimises the total squared distance between points in from and to
solve_assignment <- function(from, to) {
  cost <- outer(from[, 1], to[, 1], "-")^2 + outer(from[, 2], to[, 2], "-")^2
  as.integer(clue::solve_LSAP(cost))
}

# Relabels tiles so that each group's tiles are contiguous, keeping the number
# of tiles in each group. While a group is split into pieces, a tile of a
# smaller piece is moved next to its largest piece by shifting the labels of
# the tiles along the shortest path between them. A move is only made if it
# reduces the number of tiles that are cut off from their group overall.
contiguous_groups <- function(label, adjacent) {
  groups <- unique(label)
  cut_off <- function(label, g) {
    members <- which(label == g)
    length(members) - max(tabulate(connected_components(adjacent, members)))
  }
  cost <- vapply(groups, cut_off, numeric(1), label = label)

  while (any(cost > 0)) {
    moved <- FALSE
    for (g in groups[cost > 0]) {
      members <- which(label == g)
      piece <- connected_components(adjacent, members)
      main <- members[piece == which.max(tabulate(piece))]
      for (start in setdiff(members, main)) {
        for (path in paths_to_group(start, main, label != g, adjacent)) {
          new <- label
          new[path] <- c(label[path[-1]], g)
          affected <- match(unique(label[path]), groups)
          new_cost <- vapply(groups[affected], cut_off, numeric(1), label = new)
          if (sum(new_cost) < sum(cost[affected])) {
            label <- new
            cost[affected] <- new_cost
            moved <- TRUE
            break
          }
        }
        if (moved) break
      }
      if (moved) break
    }
    if (!moved) {
      warning("Could not make the tiles of every group contiguous.", call. = FALSE)
      break
    }
  }
  label
}

# Breadth first search from start through passable tiles, returning the paths
# to each passable tile next to the target tiles in order of increasing length
paths_to_group <- function(start, target, passable, adjacent) {
  parent <- rep(NA_integer_, length(adjacent))
  parent[start] <- 0L
  queue <- start
  ends <- integer(0)
  while (length(queue)) {
    current <- queue[1]
    queue <- queue[-1]
    if (current != start && any(adjacent[[current]] %in% target)) {
      ends <- c(ends, current)
    }
    neighbours <- adjacent[[current]]
    neighbours <- neighbours[passable[neighbours] & is.na(parent[neighbours])]
    parent[neighbours] <- current
    queue <- c(queue, neighbours)
  }

  lapply(ends, function(end) {
    path <- end
    while (parent[path[1]] != 0L) path <- c(parent[path[1]], path)
    path
  })
}

#' @importFrom sf st_drop_geometry
attribute_data <- function(object) {
  if (inherits(object, "sf")) st_drop_geometry(object) else NULL
}

check_group <- function(group, object) {
  if (!is.null(group) && length(group) != NROW(object)) {
    stop("`group` must have one value for each row of `object`.", call. = FALSE)
  }
}
