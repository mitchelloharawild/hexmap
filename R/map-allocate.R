# Returns the index of the tile allocated to each region, given a point for
# each region. Regions may only be allocated tiles from the same block (e.g.
# landmass).
#
# Tiles are split between groups by which group's shape (group_shape, the
# shape of each region's group) is nearest, so that the borders between
# groups are kept, and with weight proximity, which group's regions are
# nearest. If that can't keep each group's tiles contiguous, they are split by
# the regions' optimal allocation instead.
#' @importFrom sf st_area st_centroid st_coordinates
allocate_tiles <- function(point, tile, group, region_block, tile_block,
                           group_shape, proximity) {
  from <- st_coordinates(point)
  centre <- st_centroid(tile)
  to <- st_coordinates(centre)
  cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
  group <- as.integer(factor(group, exclude = NULL))

  index <- integer(length(point))
  for (block in unique(region_block)) {
    i <- which(region_block == block)
    j <- which(tile_block == block)

    # Make each group's tiles contiguous, starting from the tiles nearest each
    # group's shape if possible, or else from the regions' allocated tiles
    adjacent <- hex_adjacency(centre[j], cellsize)
    tile_group <- nearest_shape_groups(group[i], group_shape[i], centre[j],
                                       from[i, , drop = FALSE], proximity)
    tile_group <- contiguous_groups(tile_group, adjacent)
    if (!attr(tile_group, "contiguous")) {
      index[i] <- j[solve_assignment(from[i, , drop = FALSE], to[j, , drop = FALSE])]
      tile_group <- contiguous_groups(group[i][match(j, index[i])], adjacent)
    }
    if (!attr(tile_group, "contiguous")) {
      warning("Could not make the tiles of every group contiguous.", call. = FALSE)
    }

    # Allocate the regions of each group to its tiles
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
  as.integer(clue::solve_LSAP(squared_distance(from, to)))
}

squared_distance <- function(from, to) {
  outer(from[, 1], to[, 1], "-")^2 + outer(from[, 2], to[, 2], "-")^2
}

# Splits tiles between groups, with one tile for each region, minimising the
# total squared distance between tiles and the shape of their group, plus
# proximity times the squared distance to the nearest point of the group. The
# shapes alone don't say which of a group's tiles to give up when it has more
# area than regions, so it can give up the tiles near its regions and take
# tiles far from them.
#' @importFrom sf st_coordinates st_distance
nearest_shape_groups <- function(group, group_shape, centre, point = NULL, proximity = 0) {
  groups <- unique(group)
  shape <- group_shape[match(groups, group)]
  cost <- matrix(as.numeric(st_distance(centre, shape))^2, ncol = length(groups))
  if (proximity > 0) {
    to <- st_coordinates(centre)
    for (k in seq_along(groups)) {
      near <- squared_distance(to, point[group == groups[k], , drop = FALSE])
      cost[, k] <- cost[, k] + proximity * apply(near, 1, min)
    }
  }
  column <- match(group, groups)
  group[as.integer(clue::solve_LSAP(cost[, column, drop = FALSE]))]
}

# Relabels tiles so that each group's tiles are contiguous, keeping the number
# of tiles in each group. While a group is split into pieces, a tile of a
# smaller piece is moved next to its largest piece by shifting the labels of
# the tiles along the shortest path between them. A move is only made if it
# reduces the number of tiles that are cut off from their group overall.
# The result records whether every group was made contiguous.
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
    if (!moved) break
  }
  structure(label, contiguous = all(cost == 0))
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

# Evaluates the group argument as a column of object or in env, accepting
# a string naming a column, and checks it
eval_group <- function(expr, object, env) {
  data <- attribute_data(object)
  group <- eval(expr, data, env)
  if (is.character(group) && length(group) == 1 && NROW(object) != 1 &&
      group %in% names(data)) {
    group <- data[[group]]
  }
  check_group(group, object)
  group
}

check_group <- function(group, object) {
  if (is.null(group)) return()
  if (length(group) != NROW(object)) {
    stop("`group` must have one value for each region of `object`, such as a ",
         "column of `object` given unquoted (e.g. `group = state`).", call. = FALSE)
  }
  if (anyNA(group)) {
    row <- which(is.na(group))
    stop("`group` must not contain missing values (in rows ",
         paste(row[seq_len(min(5, length(row)))], collapse = ", "),
         if (length(row) > 5) ", ...", ").", call. = FALSE)
  }
}
