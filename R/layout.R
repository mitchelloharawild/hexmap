#' Layouts for groups of hexagons
#'
#' These functions create a layout for the `group_layout` argument of
#' [hexmap()], which arranges the hexagons of each group (e.g. state). Groups
#' usually need a very different area of hexagons than they cover on the map
#' (e.g. densely populated states have many small regions), so the map has to
#' be distorted, and each layout distorts it differently.
#'
#' * `layout_diffusion()` (the default) warps the map with a diffusion
#'   cartogram (Gastner & Newman, 2004), which grows or shrinks each group
#'   while keeping its shape and its borders with other groups. The hexagons
#'   tile the warped map, and are split between groups along the warped
#'   borders. `alpha` sets how far the map is warped: with `alpha = 0` the
#'   outline of the map is kept and groups are squeezed into it, while with
#'   `alpha = 1` the area of each group is proportional to its number of
#'   regions and the outline is distorted to fit.
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
#' and keeps each group's hexagons contiguous and compact. The default layout
#' is `layout_repair(layout_diffusion())`. Repairing a slice layout bends its
#' straight borders.
#'
#' @section Custom layouts:
#' A layout is a function with arguments `geometry` (the regions' geometry),
#' `group` (the group of each region), `land` and `cellsize` (the width of
#' the hexagons to use), which returns the hexagon of each region as an `sfc`
#' of polygons. `land` is a list with the separately tiled landmasses as
#' `geometry`, and the landmass of each region as `region`. Each group's
#' hexagons should be contiguous on each landmass.
#'
#' @param alpha How far to warp the map, from 0 (keep the outline of the map)
#'   to 1 (make each group's area proportional to its number of regions). The
#'   area of each group is warped to `area^(1 - alpha) * size^alpha`, where
#'   `size` is its area in proportion to its number of regions.
#' @param resolution The width of the grid used to find the diffusion
#'   cartogram, in cells. Larger grids are more accurate for small groups but
#'   slower.
#' @param layout A layout function to repair, such as `layout_compact()`.
#' @param iterations The number of swaps to try, per hexagon.
#' @param anchor A data frame with columns `group`, `x` and `y` giving
#'   positions (in the coordinates of the map) to use in place of the
#'   centroids of some groups when arranging them. For example, moving a
#'   group's anchor east of its neighbour places its block on the east side of
#'   that neighbour.
#' @param tree The order in which groups are cut apart, as a nested list of
#'   group names. Each list is cut into its elements, along whichever axis
#'   best separates them, and each character vector of several groups is cut
#'   automatically. For example,
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
#' aec <- st_transform(aec2025, 3577)
#' # Keep more of the outline of Australia
#' aec_hex <- hexmap(aec, group = state,
#'                   group_layout = layout_repair(layout_diffusion(alpha = 0.5)))
#' # Compact blocks, with the ACT on the east side of NSW
#' nsw <- st_coordinates(st_centroid(st_union(aec[aec$state == "NSW", ])))
#' anchor <- data.frame(group = "ACT", x = nsw[1] + 5e5, y = nsw[2])
#' aec_hex <- hexmap(aec, group = state, group_layout = layout_compact(anchor = anchor))
#' # Compact blocks that touch as the states do
#' aec_hex <- hexmap(aec, group = state, group_layout = layout_repair(layout_compact()))
#' @name layouts
NULL

#' @rdname layouts
#' @export
layout_diffusion <- function(alpha = 0.75, resolution = 256) {
  check_number(alpha, 0, 1)
  check_number(resolution, 16, Inf)
  resolution <- 2 * round(resolution / 2)
  function(geometry, group, land, cellsize) {
    diffusion_tiles(geometry, group, land, cellsize, alpha, resolution)
  }
}

#' @rdname layouts
#' @export
layout_compact <- function(anchor = NULL) {
  function(geometry, group, land, cellsize) {
    layout <- compact_tiles(geometry, group, land$region, cellsize,
                            check_anchor(anchor, group))
    layout$tile[layout$index]
  }
}

#' @rdname layouts
#' @export
layout_slice <- function(tree = NULL, anchor = NULL) {
  function(geometry, group, land, cellsize) {
    layout <- slice_tiles(geometry, group, land$region, cellsize,
                          check_anchor(anchor, group), check_tree(tree, group))
    layout$tile[layout$index]
  }
}

#' @rdname layouts
#' @export
layout_repair <- function(layout, iterations = 30) {
  check_layout(layout)
  check_number(iterations, 0, Inf)
  function(geometry, group, land, cellsize) {
    tile <- layout(geometry, group, land, cellsize)
    repair_groups(tile, geometry, group, land$region,
                  iterations = round(iterations * length(tile)))
  }
}

# Converts group anchors to a matrix of coordinates named by group
check_anchor <- function(anchor, group) {
  if (is.null(anchor)) return(NULL)
  if (!is.data.frame(anchor) || !all(c("group", "x", "y") %in% names(anchor))) {
    stop("`anchor` must be a data frame with columns `group`, `x` and `y`.",
         call. = FALSE)
  }
  unknown <- setdiff(anchor$group, group)
  if (length(unknown)) {
    stop("`anchor` contains unknown groups: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  matrix(c(anchor$x, anchor$y), ncol = 2,
         dimnames = list(as.character(anchor$group), NULL))
}

# Checks that a slicing tree names every group exactly once
check_tree <- function(tree, group) {
  if (is.null(tree)) return(NULL)
  name <- if (is.list(tree) || is.character(tree)) unlist(tree)
  if (!is.character(name)) {
    stop("`tree` must be a nested list of group names.", call. = FALSE)
  }
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
  repeated <- unique(name[duplicated(name)])
  if (length(repeated)) {
    stop("`tree` contains groups more than once: ",
         paste(repeated, collapse = ", "), call. = FALSE)
  }
  tree
}

check_number <- function(x, min, max) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x > max) {
    stop("`", deparse(substitute(x)), "` must be a number between ", min,
         " and ", max, ".", call. = FALSE)
  }
}

check_layout <- function(layout) {
  if (!is.function(layout)) {
    stop("`", deparse(substitute(layout)), "` must be a layout function, such as ",
         "`layout_diffusion()`.", call. = FALSE)
  }
}
