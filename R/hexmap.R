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
#' between groups, so that each group is visually distinct.
#'
#' @param object An sf object in a projected coordinate reference system.
#' @param group Optionally, a column of `object` (unquoted) that groups its
#'   regions (e.g. states).
#' @param island_gap The distance, in hexagon widths, beyond which landmasses
#'   are tiled as separate islands. Use `Inf` to tile all land together.
#' @param group_gap The width of the gap between groups, as a fraction of the
#'   hexagon width. The hexagons along group borders are trimmed to make this
#'   gap, so they are slightly smaller than other hexagons.
#'
#' @return
#' `object` with its geometry replaced by hexagons.
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
#' @importFrom sf st_geometry st_geometry<-
#' @export
hexmap <- function(object, group = NULL, island_gap = 0.5, group_gap = 0.1) {
  check_projected(object)
  check_not_empty(object)
  group <- eval(substitute(group), attribute_data(object), parent.frame())
  check_group(group, object)

  geometry <- st_geometry(object)
  cellsize <- hex_cellsize(geometry, NROW(object))
  land <- landmasses(geometry, cellsize, island_gap)
  n <- tabulate(land$region, nbins = length(land$geometry))
  grid <- landmass_grid(land$geometry, n, cellsize)

  tile <- grid$tile[allocate_tiles(geometry, grid$tile, group,
                                   land$region, grid$landmass)]
  if (!is.null(group) && group_gap > 0) {
    tile <- inset_groups(tile, group, group_gap * cellsize)
  }
  st_geometry(object) <- tile
  object
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
