#' Convert a map into a hexagon tile map
#'
#' Replaces each region of a map with a hexagon of equal size. The hexagons
#' tile the outline of the map, and each region is allocated the hexagon that
#' minimises the total squared displacement of all regions.
#'
#' Landmasses that are further apart than `island_gap` hexagon widths are
#' tiled separately, with one hexagon for each region on that landmass. Their
#' hexagons are separated by at least one empty hexagon, so islands such as
#' Tasmania remain distinct from the mainland.
#'
#' When `group` is given, the hexagons of each group (e.g. state) are kept
#' contiguous and a gap of `group_gap` hexagon widths is cut along the borders
#' between groups, so that each group is visually distinct. Groups usually
#' need a very different area of hexagons than they cover on the map (e.g.
#' densely populated states have many small regions), so the outline of the
#' map can't be kept. Instead, `group_layout` chooses how groups are arranged:
#'
#' * `"compact"` draws each group as a compact block of hexagons, in the
#'   style of hand drawn election maps. Each group is treated as a circle with
#'   the area of its hexagons, and the circles are arranged so that
#'   neighbouring groups keep their direction from each other (weighting
#'   longer shared borders more), then packed together without overlapping.
#'   The positions used for some groups can be changed with `group_anchor`
#'   to adjust the layout.
#' * `"cartogram"` keeps the shape of each group by warping the map into a
#'   continuous cartogram (Dougenik, Chrisman & Niemeyer, 1985) where each
#'   group's area is proportional to its number of regions. The hexagons tile
#'   this warped map, and are split between groups along the warped borders.
#'   Groups that shrink a lot can become thin.
#'
#' Grouping works best for groups that are spatially contiguous, such as
#' states.
#'
#' @param object An sf object in a projected coordinate reference system.
#' @param group Optionally, a column of `object` (unquoted) that groups its
#'   regions (e.g. states).
#' @param group_layout How groups are arranged, either `"compact"` blocks or
#'   a `"cartogram"` that keeps the shape of each group. See details.
#' @param island_gap The distance, in hexagon widths, beyond which landmasses
#'   are tiled as separate islands. Use `Inf` to tile all land together.
#' @param group_gap The width of the gap between groups, as a fraction of the
#'   hexagon width. The hexagons along group borders are trimmed to make this
#'   gap, so they are slightly smaller than other hexagons.
#' @param group_anchor For the `"compact"` layout, a data frame with columns
#'   `group`, `x` and `y` giving positions (in the coordinates of `object`) to
#'   use in place of the centroids of some groups when arranging them. For
#'   example, moving a group's anchor east of its neighbour places its block
#'   on the east side of that neighbour.
#'
#' @return
#' `object` with its geometry replaced by hexagons.
#'
#' @references
#' Dougenik, J. A., Chrisman, N. R., & Niemeyer, D. R. (1985). An algorithm to
#' construct continuous area cartograms. *The Professional Geographer*,
#' 37(1), 75--81.
#'
#' @seealso [hex_grid()] to create the hexagons and [tile_allocate()] to
#'   allocate them to regions.
#'
#' @examples
#' library(sf)
#' aec_hex <- hexmap(st_transform(aec2025, 3577), group = state)
#'
#' library(ggplot2)
#' ggplot(aec_hex) +
#'   geom_sf(aes(fill = winner))
#'
#' @importFrom sf st_geometry st_geometry<- st_point_on_surface
#' @export
hexmap <- function(object, group = NULL, group_layout = c("compact", "cartogram"),
                   island_gap = 0.5, group_gap = 0.1, group_anchor = NULL) {
  check_projected(object)
  check_not_empty(object)
  group <- eval(substitute(group), attribute_data(object), parent.frame())
  check_group(group, object)
  group_layout <- match.arg(group_layout)

  geometry <- st_geometry(object)
  cellsize <- hex_cellsize(geometry, NROW(object))
  land <- landmasses(geometry, cellsize, island_gap)

  if (is.null(group)) {
    n <- tabulate(land$region, nbins = length(land$geometry))
    grid <- landmass_grid(land$geometry, n, cellsize)
    point <- st_point_on_surface(geometry)
    tile <- grid$tile[allocate_tiles(point, grid$tile, NULL, land$region,
                                     grid$landmass)]
  } else if (group_layout == "compact") {
    anchor <- check_anchor(group_anchor, group)
    layout <- compact_tiles(geometry, group, land$region, cellsize, anchor)
    tile <- layout$tile[layout$index]
  } else {
    layout <- cartogram_tiles(geometry, group, land, cellsize)
    tile <- layout$tile
    cellsize <- layout$cellsize
  }

  if (!is.null(group) && group_gap > 0) {
    tile <- inset_groups(tile, group, group_gap * cellsize)
  }
  st_geometry(object) <- tile
  object
}

# Converts group anchors to a matrix of coordinates named by group
check_anchor <- function(anchor, group) {
  if (is.null(anchor)) return(NULL)
  if (!is.data.frame(anchor) || !all(c("group", "x", "y") %in% names(anchor))) {
    stop("`group_anchor` must be a data frame with columns `group`, `x` and `y`.",
         call. = FALSE)
  }
  unknown <- setdiff(anchor$group, group)
  if (length(unknown)) {
    stop("`group_anchor` contains unknown groups: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  matrix(c(anchor$x, anchor$y), ncol = 2,
         dimnames = list(as.character(anchor$group), NULL))
}

# Trims gap / 2 from each tile along the borders it shares with other groups
#' @importFrom sf st_buffer st_crs st_difference st_sfc st_union
inset_groups <- function(tile, group, gap) {
  group <- as.integer(factor(group, exclude = NULL))
  inset <- tile
  for (g in unique(group)) {
    others <- tile[group != g]
    if (!length(others)) next
    border <- st_buffer(st_union(others), gap / 2,
                        joinStyle = "MITRE", mitreLimit = 2)[[1]]
    i <- which(group == g)
    inset[i] <- st_sfc(lapply(tile[i], st_difference, border), crs = st_crs(tile))
  }
  inset
}
