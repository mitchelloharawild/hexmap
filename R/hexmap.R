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
#' between groups, so that each group is visually distinct. To keep the shape
#' and position of each group, the map is first warped into a continuous
#' cartogram (Dougenik, Chrisman & Niemeyer, 1985) where each group's area is
#' proportional to its number of regions. The hexagons tile this warped map,
#' and are split between groups along the warped borders. As a result, the
#' outline of the hexagons follows the warped map rather than the original
#' outline: groups with many small regions grow, while groups with few large
#' regions shrink. Grouping works best for groups that are spatially
#' contiguous, such as states.
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
#' @importFrom sf st_geometry st_geometry<- st_make_valid st_point_on_surface
#' @importFrom sf st_simplify st_union
#' @export
hexmap <- function(object, group = NULL, island_gap = 0.5, group_gap = 0.1) {
  check_projected(object)
  check_not_empty(object)
  group <- eval(substitute(group), attribute_data(object), parent.frame())
  check_group(group, object)

  geometry <- st_geometry(object)
  cellsize <- hex_cellsize(geometry, NROW(object))
  land <- landmasses(geometry, cellsize, island_gap)
  land_shape <- land$geometry
  point <- st_point_on_surface(geometry)
  group_shape <- NULL

  if (!is.null(group)) {
    # Warp the map so that each group's area is proportional to its regions,
    # using the (simplified) shape of each group on each landmass
    tolerance <- cellsize / 10
    id <- as.integer(factor(group, exclude = NULL))
    piece <- interaction(id, land$region, drop = TRUE)
    shape <- do.call(c, lapply(split(geometry, piece), st_union))
    shape <- st_simplify(shape, preserveTopology = TRUE, dTolerance = tolerance)
    land_shape <- st_simplify(land_shape, preserveTopology = TRUE,
                              dTolerance = tolerance)
    forces <- cartogram_forces(shape, id[match(levels(piece), piece)], tabulate(id))
    group_shape <- st_make_valid(warp_apply(shape, forces))[as.integer(piece)]
    land_shape <- st_make_valid(warp_apply(land_shape, forces))
    point <- warp_apply(point, forces)
    cellsize <- hex_cellsize(land_shape, NROW(object))
  }

  n <- tabulate(land$region, nbins = length(land_shape))
  grid <- landmass_grid(land_shape, n, cellsize)
  tile <- grid$tile[allocate_tiles(point, grid$tile, group, land$region,
                                   grid$landmass, group_shape)]
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
