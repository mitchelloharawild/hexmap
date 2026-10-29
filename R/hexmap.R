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
#' map is warped with a diffusion cartogram so that each group keeps its shape,
#' and the borders between groups are then repaired so that groups touch as
#' they do on the map. See [layouts] for the available layouts. Grouping works
#' best for groups that are spatially contiguous, such as states.
#'
#' The hexagons are regular and equally sized in the coordinate reference
#' system (CRS) they are made in, which is the CRS of `object` unless `crs` is
#' given, or a Lambert azimuthal equal-area CRS centred on the data for
#' longitude/latitude input. The hexagons keep their location when plotted in
#' another projection, but are stretched by it, so make the hexagons in the
#' CRS you will plot in. Use an equal-area CRS for hexagons that also cover
#' equal areas on the ground. See `vignette("projections")` for details.
#'
#' @param object An sf object in a projected or longitude/latitude
#'   coordinate reference system.
#' @param group Optionally, a column of `object` (unquoted) that groups its
#'   regions (e.g. states).
#' @param group_layout How groups are arranged, as a layout function such as
#'   [layout_diffusion()], [layout_compact()] or [layout_slice()], optionally
#'   wrapped in [layout_repair()] to fix which groups touch.
#' @param island_gap The distance, in hexagon widths, beyond which landmasses
#'   are tiled as separate islands. Use `Inf` to tile all land together.
#' @param group_gap The width of the gap between groups, as a fraction of the
#'   hexagon width. The hexagons along group borders are trimmed to make this
#'   gap, so they are slightly smaller than other hexagons.
#' @param crs The projected coordinate reference system to make the hexagons
#'   in, such as the one the map will be plotted in. `object` is transformed
#'   to `crs` with [sf::st_transform()]. Defaults to the CRS of `object`, or
#'   for longitude/latitude input, to a Lambert azimuthal equal-area CRS
#'   centred on the data (`"+proj=laea +lat_0=... +lon_0=..."`).
#'
#' @return
#' `object` with its geometry replaced by hexagons, in the coordinate reference
#' system they were made in.
#'
#' @seealso [hex_grid()] to create the hexagons and [tile_allocate()] to
#'   allocate them to regions.
#'
#' @examplesIf requireNamespace("ozmaps", quietly = TRUE)
#' library(sf)
#' # Australian electoral divisions without offshore territories, and their state
#' ced <- st_crop(ozmaps::abs_ced, xmin = 112, ymin = -44, xmax = 154, ymax = -9)
#' ced <- st_simplify(st_transform(ced, 3577), dTolerance = 1000)
#' states <- st_transform(ozmaps::abs_ste, 3577)
#' names(states)[1] <- "state"
#' ced <- st_join(ced, states, largest = TRUE)
#'
#' ced_hex <- hexmap(ced, group = state)
#'
#' library(ggplot2)
#' ggplot(ced_hex) +
#'   geom_sf(aes(fill = state))
#'
#' # Hexagons made for a globe centred on Australia
#' globe <- "+proj=ortho +lat_0=-25 +lon_0=134"
#' ggplot(hexmap(ced, crs = globe)) +
#'   geom_sf(aes(fill = state)) +
#'   coord_sf(crs = globe)
#'
#' @importFrom sf st_area st_geometry st_geometry<- st_point_on_surface
#' @importFrom sf st_is_longlat st_transform
#' @export
hexmap <- function(object, group = NULL,
                   group_layout = layout_repair(layout_diffusion()),
                   island_gap = 0.5, group_gap = 0.1, crs = NULL) {
  if (is.null(crs) && isTRUE(st_is_longlat(object))) {
    crs <- equal_area_crs(object)
    message("Making hexagons in the equal-area CRS \"", crs, "\". ",
            "Use it with `coord_sf(crs = ...)`, or choose another with `crs`.")
  }
  if (!is.null(crs)) object <- st_transform(object, crs)
  check_projected(object)
  check_not_empty(object)
  group <- eval(substitute(group), attribute_data(object), parent.frame())
  check_group(group, object)
  check_layout(group_layout)

  geometry <- st_geometry(object)
  cellsize <- hex_cellsize(geometry, NROW(object))
  land <- landmasses(geometry, cellsize, island_gap)

  if (is.null(group)) {
    n <- tabulate(land$region, nbins = length(land$geometry))
    grid <- landmass_grid(land$geometry, n, cellsize)
    point <- st_point_on_surface(geometry)
    tile <- grid$tile[allocate_tiles(point, grid$tile, NULL, land$region,
                                     grid$landmass)]
  } else {
    tile <- group_layout(geometry, group, land, cellsize)
    attr(tile, "point") <- NULL
    attr(tile, "warp") <- NULL
    # Layouts may change the size of the hexagons
    cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
  }

  if (!is.null(group) && group_gap > 0) {
    tile <- inset_groups(tile, group, group_gap * cellsize)
  }
  st_geometry(object) <- tile
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
