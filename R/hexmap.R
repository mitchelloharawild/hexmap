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
#' need a very different area of hexagons than they cover on the map, so the
#' map is distorted to fit them, as chosen by `group_layout`. By default, the
#' groups are laid out first, keeping each group's shape and touching the
#' groups it borders on the map, and the regions are then placed within each
#' group. See [layouts] for the available layouts, and
#' `vignette("layouts")` for how they compare. Grouping works best for groups
#' that are spatially contiguous, such as states.
#'
#' The hexagons are regular and equally sized in the coordinate reference
#' system (CRS) they are made in, which is the CRS of `object` unless `crs` is
#' given, or a Lambert azimuthal equal-area CRS centred on the data for
#' longitude/latitude input. The hexagons keep their location when plotted in
#' another projection, but are stretched by it, so make the hexagons in the
#' CRS you will plot in. Use an equal-area CRS for hexagons that also cover
#' equal areas on the ground. See `vignette("projections")` for details.
#'
#' @param object An sf object, or a geometry column (sfc), of regions in a
#'   projected or longitude/latitude coordinate reference system.
#' @param group Optionally, the group of each region (e.g. states), as a column
#'   of `object` given unquoted (`group = state`) or as a string
#'   (`group = "state"`), or as a vector with one value for each region.
#' @param group_layout How groups are arranged, as a layout function such as
#'   [layout_hierarchy()] (the default), [layout_diffusion()],
#'   [layout_compact()] or [layout_slice()], optionally wrapped in
#'   [layout_repair()] to fix which groups touch. Only used with `group`.
#' @param island_gap The distance, in hexagon widths, beyond which landmasses
#'   are tiled as separate islands. Use `Inf` to tile all land together.
#' @param group_gap The width of the gap between groups, as a fraction of the
#'   hexagon width. The hexagons along group borders are trimmed to make this
#'   gap, so they are slightly smaller than other hexagons. Only used with
#'   `group`.
#' @param crs The projected coordinate reference system to make the hexagons
#'   in, such as the one the map will be plotted in. `object` is transformed
#'   to `crs` with [sf::st_transform()]. Defaults to the CRS of `object`, or
#'   for longitude/latitude input, to a Lambert azimuthal equal-area CRS
#'   centred on the data (`"+proj=laea +lat_0=... +lon_0=..."`).
#'
#' @return
#' For an sf object, `object` with its geometry replaced by hexagons, keeping
#' all of its other columns and the order of its rows, so the hexagons can be
#' joined to or compared with the original regions. For a geometry column, a
#' geometry column of hexagons in the same order, so `hexmap()` can be used
#' within `dplyr::mutate()` to add the hexagons as another geometry column.
#' The hexagons are in the coordinate reference system they were made in.
#'
#' @seealso [hex_grid()] and [hex_allocate()], which make an ungrouped hexagon
#'   map in two steps: `hexmap(x)` is the same as
#'   `hex_allocate(x, hex_grid(x))`.
#'
#' @examples
#' library(sf)
#' # The 2025 Australian federal election, in the Australian Albers projection
#' election <- st_transform(aus_election_2025, 3577)
#' # A hexagon for each electoral division
#' election_hex <- hexmap(election)
#'
#' # Every column of the map is kept, in the same order
#' head(election_hex)
#'
#' \donttest{
#' # Keep the divisions of each state together (takes a few seconds)
#' election_hex <- hexmap(election, group = state)
#' }
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot(election_hex) +
#'   geom_sf(aes(fill = party))
#'
#' \donttest{
#' # Hexagons made for a globe centred on Australia
#' globe <- "+proj=ortho +lat_0=-25 +lon_0=134"
#' ggplot(hexmap(election, group = state, crs = globe)) +
#'   geom_sf(aes(fill = state)) +
#'   coord_sf(crs = globe)
#' }
#'
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' \donttest{
#' # Add the hexagons as a second geometry column, alongside the regions
#' election <- dplyr::mutate(election, hex = hexmap(geometry, group = state))
#' }
#'
#' @importFrom sf st_area st_geometry st_point_on_surface
#' @export
hexmap <- function(object, group = NULL,
                   group_layout = layout_hierarchy(),
                   island_gap = 0.5, group_gap = 0.1, crs = NULL) {
  check_object(object)
  group <- eval_group(substitute(group), object, parent.frame())
  check_number(island_gap, 0, Inf)
  check_number(group_gap, 0, 1)
  check_layout(group_layout)
  if (is.null(group)) {
    ignored <- c("group_layout", "group_gap")[c(!missing(group_layout), !missing(group_gap))]
    if (length(ignored)) {
      warning(paste0("`", ignored, "`", collapse = " and "),
              if (length(ignored) == 1) " is" else " are",
              " ignored without `group`.", call. = FALSE)
    }
  }
  object <- prepare_crs(object, crs)
  check_not_empty(object)
  geometry <- st_geometry(object)

  if (is.null(group)) {
    grid <- make_grid(geometry, length(geometry), island_gap)
    return(set_geometry(object, grid[allocate_regions(geometry, grid, NULL)]))
  }

  cellsize <- hex_cellsize(geometry, length(geometry))
  land <- landmasses(geometry, cellsize, island_gap)
  tile <- group_layout(geometry, group, land, cellsize)
  attr(tile, "point") <- NULL
  attr(tile, "warp") <- NULL
  # Layouts may change the size of the hexagons
  cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
  if (group_gap > 0) tile <- inset_groups(tile, group, group_gap * cellsize)
  set_geometry(object, tile)
}

# Projects longitude/latitude objects to an equal-area CRS centred on the data
# (unless crs is given), or transforms them to crs, and checks the result is
# projected
#' @importFrom sf st_is_longlat st_transform
prepare_crs <- function(object, crs = NULL) {
  if (is.null(crs) && isTRUE(st_is_longlat(object))) {
    crs <- equal_area_crs(object)
    message("Making hexagons in the equal-area CRS \"", crs, "\". ",
            "Use it with `coord_sf(crs = ...)`, or choose another with `crs`.")
  }
  if (!is.null(crs)) {
    check_has_crs(object)
    object <- st_transform(object, crs)
  }
  check_projected(object)
  object
}

# Replaces the geometry of an sf object, or the geometry column itself
#' @importFrom sf st_geometry<-
set_geometry <- function(object, geometry) {
  if (!inherits(object, "sf")) return(geometry)
  st_geometry(object) <- geometry
  object
}

# A Lambert azimuthal equal-area CRS centred on the bounding box of longlat
# data, or on the centroid of the data if it spans more than 180 degrees of
# longitude (such as data crossing the antimeridian)
#' @importFrom sf sf_use_s2 st_bbox st_centroid st_coordinates
equal_area_crs <- function(object) {
  box <- st_bbox(object)
  centre <- c((box[["xmin"]] + box[["xmax"]]) / 2, (box[["ymin"]] + box[["ymax"]]) / 2)
  if (box[["xmax"]] - box[["xmin"]] > 180) {
    s2 <- suppressMessages(sf_use_s2(TRUE))
    on.exit(suppressMessages(sf_use_s2(s2)))
    centre <- st_coordinates(st_centroid(st_union(st_geometry(object))))[1, 1:2]
  }
  centre <- round(centre, 2)
  paste0("+proj=laea +lat_0=", centre[2], " +lon_0=", centre[1])
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
