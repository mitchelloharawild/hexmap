#' Layouts for groups of hexagons
#'
#' These functions create a layout for the `group_layout` argument of
#' [hexmap()], which arranges the hexagons of each group (e.g. state). Groups
#' usually need a very different area of hexagons than they cover on the map
#' (e.g. densely populated states have many small regions), so the map has to
#' be distorted, and each layout distorts it differently. See
#' `vignette("layouts")` for how they compare.
#'
#' * `layout_hierarchy()` (the default) lays out the groups first, then the
#'   regions within each group. The groups are warped by a diffusion cartogram,
#'   blended towards a scaled copy of each group's own shape (by `shape`), so
#'   that groups keep their shapes while still touching their neighbours, at
#'   the expense of the outline of the whole map. The hexagons are split
#'   between groups nearest to these shapes, keeping groups with little coast
#'   (such as the ACT) off the edge of the hexagons, and the borders between
#'   groups are adjusted (by simulated annealing) so that groups touch as they
#'   do on the map. Regions are then placed within each group by matching the
#'   spread of the group on the map to the spread of its hexagons, so each
#'   group's regions keep their positions within it.
#' * `layout_diffusion()` warps the map with a diffusion cartogram (Gastner &
#'   Newman, 2004), which grows or shrinks each group while keeping its shape
#'   and its borders with other groups. The hexagons tile the warped map, and
#'   are split between groups along the warped borders. `cartogram` sets how
#'   far the map is warped: with `cartogram = 0` the outline of the map is
#'   kept and groups are squeezed into it, while with `cartogram = 1` the area
#'   of each group is proportional to its number of regions and the outline is
#'   distorted to fit.
#' * `layout_compact()` draws each group as a compact block of hexagons, in
#'   the style of hand drawn election maps. Each group is treated as a circle
#'   with the area of its hexagons, and the circles are arranged so that
#'   neighbouring groups keep their direction from each other (weighting
#'   longer shared borders more), then packed together without overlapping.
#' * `layout_slice()` lays out groups as a spatial treemap (Wood & Dykes,
#'   2008), so that the borders between groups are long straight lines. Each
#'   landmass is drawn as a rectangle of hexagons, which is recursively cut in
#'   two between rows or columns of hexagons, giving each side exactly the
#'   hexagons of its groups. Each cut is chosen so that neighbouring groups
#'   keep their relative position (e.g. one above the other for a horizontal
#'   cut) and pieces don't become thin.
#'
#' `layout_repair()` wraps any layout to then adjust the borders between
#' groups, so that groups touch where they share a border on the map, and
#' don't touch where they don't. Pairs of hexagons are swapped across group
#' borders (by simulated annealing), which keeps the hexagons of the layout,
#' and keeps each group's hexagons contiguous and compact. Repairing a slice
#' layout bends its straight borders. With `border`, the regions are then
#' placed on their group's hexagons keeping their distances to borders as well
#' as their positions: regions near the coast or another group stay near it,
#' and regions away from borders stay away from them.
#'
#' @section Arguments shared by layouts:
#' An argument has the same name, meaning and default in every layout that
#' takes it:
#'
#' * `resolution` (`layout_hierarchy()`, `layout_diffusion()`) is the size of
#'   the grid used to find the diffusion cartogram.
#' * `iterations` (`layout_hierarchy()`, `layout_repair()`) is the number of
#'   hexagon swaps tried per region when adjusting the borders between groups.
#' * `anchor` (`layout_compact()`, `layout_slice()`) moves where groups are
#'   placed relative to each other.
#'
#' Arguments that set how far the map is distorted are named after what they
#' change, and range from 0 to 1: `cartogram` is how far the whole map is
#' warped towards a cartogram, and `shape` is how far each group is then
#' restored to its own shape. Arguments that weigh one thing to keep against
#' the position of regions are named after what they keep, range from 0
#' (ignore it) upwards, and keep it more as they increase: `proximity` (keep
#' groups' hexagons near their regions) and `border` (keep regions' distances
#' to borders).
#'
#' @section Custom layouts:
#' A layout is a function with arguments `geometry` (the regions' geometry),
#' `group` (the group of each region), `land` and `cellsize` (the width of
#' the hexagons to use), which returns the hexagon of each region as an `sfc`
#' of polygons. `land` is a list with the separately tiled landmasses as
#' `geometry`, and the landmass of each region as `region`. Each group's
#' hexagons should be contiguous on each landmass.
#'
#' @param cartogram For `layout_diffusion()`, how far to warp the map, from 0
#'   (keep the outline of the map, squeezing groups into it) to 1 (make each
#'   group's area proportional to its number of regions, distorting the
#'   outline). Each group's area is warped to
#'   `area^(1 - cartogram) * size^cartogram`, where `size` is its area in
#'   proportion to its number of regions.
#' @param resolution For `layout_hierarchy()` and `layout_diffusion()`, the
#'   width of the grid used to find the diffusion cartogram, in cells (at
#'   least 16). Larger grids are more accurate for small groups but slower.
#' @param proximity For `layout_diffusion()`, how much to keep each group's
#'   hexagons near its regions, from 0 upwards. Unless `cartogram = 1`, groups
#'   don't cover the area their regions need, so hexagons pass from group to
#'   group. Each hexagon is given to groups by its squared distance to the
#'   group's warped shape plus `proximity` times its squared distance to the
#'   group's nearest region, so that a group gives up the hexagons far from
#'   its regions (e.g. outback rather than near its cities). With 0, only the
#'   shapes are used.
#' @param shape For `layout_hierarchy()`, how far to restore each group's own
#'   shape after warping the map into a cartogram, from 0 (the cartogram) to 1
#'   (a scaled copy of each group's shape). Larger values keep groups more
#'   recognisable, but values above about 0.5 can fold the map.
#' @param layout For `layout_repair()`, the layout to repair, such as
#'   `layout_compact()`.
#' @param iterations For `layout_hierarchy()` and `layout_repair()`, the
#'   number of swaps of hexagons between groups to try, per region, when
#'   adjusting the borders between groups (from 0 upwards). More iterations
#'   are slower, but more likely to make every group touch as on the map.
#' @param border For `layout_repair()`, how much to keep each region's
#'   distance to borders (the coast and each other group) when placing it,
#'   relative to keeping its position, from 0 upwards. Distances are measured
#'   from each region's polygon, in hexagon widths, and only up to 3 widths,
#'   so only regions near a border are held to it. With `border = 1`, a region
#'   one hexagon further from a border than on the map is as bad as a region
#'   one hexagon from its position. Larger values keep borders at the expense
#'   of the order of regions within groups. With 0, only regions of groups
#'   whose hexagons changed are placed again.
#' @param anchor For `layout_compact()` and `layout_slice()`, a data frame
#'   with columns `group`, `x` and `y` giving positions (in the coordinates of
#'   the map, after any transformation by the `crs` argument of [hexmap()]) to
#'   use in place of the centroids of some groups when arranging them. For
#'   example, moving a group's anchor east of its neighbour places its block
#'   on the east side of that neighbour.
#' @param tree For `layout_slice()`, the order in which groups are cut apart,
#'   as a nested list of group names. Each list is cut into its elements,
#'   along whichever axis best separates them, and each character vector of
#'   several groups is cut automatically. For example,
#'   `list("WA", list(list("NT", "SA"), list("QLD", "NSW", "VIC")))` first
#'   separates WA from the other states, then NT and SA from the eastern
#'   states. Every group must appear exactly once.
#'
#' @return
#' A layout function, for the `group_layout` argument of [hexmap()].
#'
#' @references
#' Gastner, M. T., & Newman, M. E. J. (2004). Diffusion-based method for
#' producing density-equalizing maps. *Proceedings of the National Academy of
#' Sciences*, 101(20), 7499--7504.
#'
#' Wood, J., & Dykes, J. (2008). Spatially ordered treemaps. *IEEE
#' Transactions on Visualization and Computer Graphics*, 14(6), 1348--1355.
#'
#' @examples
#' library(sf)
#' # The 2025 Australian federal election, in Australian Albers
#' election <- st_transform(aus_election_2025, 3577)
#'
#' # Each state keeps its shape, at the expense of the outline of Australia
#' election_hex <- hexmap(election, group = state,
#'                        group_layout = layout_hierarchy())
#' # Keep more of the outline of Australia
#' election_hex <- hexmap(election, group = state,
#'                        group_layout = layout_diffusion(cartogram = 0.5))
#' # Compact blocks, with the ACT on the east side of NSW
#' nsw <- st_coordinates(st_centroid(st_union(election[election$state == "NSW", ])))
#' anchor <- data.frame(group = "ACT", x = nsw[1] + 5e5, y = nsw[2])
#' election_hex <- hexmap(election, group = state,
#'                        group_layout = layout_compact(anchor = anchor))
#' # Compact blocks that touch as the states do
#' election_hex <- hexmap(election, group = state,
#'                        group_layout = layout_repair(layout_compact()))
#' @name layouts
NULL

#' @rdname layouts
#' @export
layout_diffusion <- function(cartogram = 0.75, resolution = 256, proximity = 0.1) {
  check_number(cartogram, 0, 1)
  check_number(resolution, 16, Inf)
  check_number(proximity, 0, Inf)
  resolution <- 2 * round(resolution / 2)
  function(geometry, group, land, cellsize) {
    diffusion_tiles(geometry, group, land, cellsize, cartogram, resolution, proximity)
  }
}

#' @rdname layouts
#' @export
layout_compact <- function(anchor = NULL) {
  check_anchor_shape(anchor)
  function(geometry, group, land, cellsize) {
    layout <- compact_tiles(geometry, group, land$region, cellsize,
                            check_anchor(anchor, group))
    layout$tile[layout$index]
  }
}

#' @rdname layouts
#' @export
layout_slice <- function(tree = NULL, anchor = NULL) {
  check_anchor_shape(anchor)
  check_tree_shape(tree)
  function(geometry, group, land, cellsize) {
    layout <- slice_tiles(geometry, group, land$region, cellsize,
                          check_anchor(anchor, group), check_tree(tree, group))
    layout$tile[layout$index]
  }
}

#' @rdname layouts
#' @export
layout_hierarchy <- function(shape = 0.4, resolution = 256, iterations = 30) {
  check_number(shape, 0, 1)
  check_number(resolution, 16, Inf)
  check_number(iterations, 0, Inf)
  resolution <- 2 * round(resolution / 2)
  function(geometry, group, land, cellsize) {
    hierarchy_tiles(geometry, group, land, cellsize, shape, resolution,
                    iterations = round(iterations * length(geometry)))
  }
}

#' @rdname layouts
#' @export
layout_repair <- function(layout, iterations = 30, border = 0) {
  check_layout(layout)
  check_number(iterations, 0, Inf)
  check_number(border, 0, Inf)
  function(geometry, group, land, cellsize) {
    tile <- layout(geometry, group, land, cellsize)
    repair_groups(tile, geometry, group, land$region,
                  iterations = round(iterations * length(tile)), border = border)
  }
}

# Checks that anchor is a data frame of group positions, before the groups
# are known
check_anchor_shape <- function(anchor) {
  if (is.null(anchor)) return(invisible())
  if (!is.data.frame(anchor) || !all(c("group", "x", "y") %in% names(anchor))) {
    stop("`anchor` must be a data frame with columns `group`, `x` and `y`.",
         call. = FALSE)
  }
  if (!is.numeric(anchor$x) || !is.numeric(anchor$y) ||
      anyNA(anchor$x) || anyNA(anchor$y)) {
    stop("`anchor` columns `x` and `y` must be numbers.", call. = FALSE)
  }
}

# Converts group anchors to a matrix of coordinates named by group
check_anchor <- function(anchor, group) {
  if (is.null(anchor)) return(NULL)
  check_anchor_shape(anchor)
  unknown <- setdiff(anchor$group, group)
  if (length(unknown)) {
    stop("`anchor` contains unknown groups: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  matrix(c(anchor$x, anchor$y), ncol = 2,
         dimnames = list(as.character(anchor$group), NULL))
}

# Checks that a slicing tree is a nested list of group names, before the
# groups are known
check_tree_shape <- function(tree) {
  if (is.null(tree)) return(invisible())
  name <- if (is.list(tree) || is.character(tree)) unlist(tree)
  if (!is.character(name)) {
    stop("`tree` must be a nested list of group names.", call. = FALSE)
  }
  repeated <- unique(name[duplicated(name)])
  if (length(repeated)) {
    stop("`tree` contains groups more than once: ",
         paste(repeated, collapse = ", "), call. = FALSE)
  }
}

# Checks that a slicing tree names every group exactly once
check_tree <- function(tree, group) {
  if (is.null(tree)) return(NULL)
  check_tree_shape(tree)
  name <- unlist(tree)
  unknown <- setdiff(name, group)
  if (length(unknown)) {
    stop("`tree` contains unknown groups: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(as.character(group), name)
  if (length(missing)) {
    stop("`tree` is missing groups: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  tree
}

check_number <- function(x, min, max) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x > max) {
    stop("`", deparse(substitute(x)), "` must be a number between ", min,
         " and ", max, ".", call. = FALSE)
  }
}

# Checks that layout is a layout function, which is called with the arguments
# geometry, group, land and cellsize. A layout_*() function passed without
# being called (e.g. `layout_compact` rather than `layout_compact()`) is
# detected by name.
check_layout <- function(layout) {
  arg <- deparse(substitute(layout))
  if (!is.function(layout)) {
    stop("`", arg, "` must be a layout function, such as ",
         "`layout_hierarchy()`.", call. = FALSE)
  }
  factories <- c("layout_hierarchy", "layout_diffusion", "layout_compact",
                 "layout_slice", "layout_repair")
  factory <- factories[vapply(factories, function(f) {
    identical(layout, get(f, envir = asNamespace("hexmap")))
  }, logical(1))]
  if (length(factory)) {
    stop("`", arg, "` must be a layout function, not a function that creates ",
         "one. Did you mean `", factory, "()`?", call. = FALSE)
  }
  args <- names(formals(layout))
  if (!"..." %in% args && length(args) < 4) {
    stop("`", arg, "` must be a layout function with arguments `geometry`, ",
         "`group`, `land` and `cellsize`.", call. = FALSE)
  }
}
