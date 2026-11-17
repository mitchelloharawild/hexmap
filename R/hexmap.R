#' Convert a map into a hexagon tile map
#'
#' Replaces each region of a map with a hexagon of equal size. The hexagons
#' tile the outline of the map, and regions are allocated hexagons by
#' minimising the total squared displacement of all regions (from where the
#' layout moves them), so regions keep their relative positions.
#'
#' Landmasses that are further apart than `island_gap` hexagon widths are
#' tiled separately, with one hexagon for each region on that landmass. Their
#' hexagons are separated by at least one empty hexagon, so islands such as
#' Tasmania remain distinct from the mainland.
#'
#' The hexagons are arranged by `layout`, which also sets the groups of
#' regions (e.g. states) to keep together, such as
#' `layout_hierarchy(state)`. The hexagons of each group are kept contiguous,
#' and a gap is cut along the borders between groups, so that each group is
#' visually distinct. Groups can be nested, such as `layout_hierarchy(state /
#' region)`, to keep regions together within each state. Groups usually need
#' a very different area of hexagons than they cover on the map, so the map
#' is distorted to fit them, and each layout distorts it differently. Without
#' groups, all regions form a single group, so the layout only sets the shape
#' of the hexagons and where regions are placed within them. See [layouts]
#' for the available layouts, and `vignette("layouts")` for how they compare.
#' Grouping works best for groups that are spatially contiguous, such as
#' states.
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
#' @param layout How the hexagons, and the groups of regions, are arranged,
#'   as a layout function such as [layout_hierarchy()] (the default),
#'   [layout_diffusion()], [layout_compact()] or [layout_slice()], optionally
#'   wrapped in [layout_repair()] to fix which groups touch. The groups are
#'   given to the layout, as in `layout_hierarchy(state)`.
#' @param island_gap The distance, in hexagon widths, beyond which landmasses
#'   are tiled as separate islands. Use `Inf` to tile all land together.
#' @param crs The projected coordinate reference system to make the hexagons
#'   in, such as the one the map will be plotted in. `object` is transformed
#'   to `crs` with [sf::st_transform()]. Defaults to the CRS of `object`, or
#'   for longitude/latitude input, to a Lambert azimuthal equal-area CRS
#'   centred on the data (`"+proj=laea +lat_0=... +lon_0=..."`).
#' @param ... Not used. The groups of regions, and the gap between them, are
#'   now arguments of the layout, such as `layout_hierarchy(state, gap = 0.1)`.
#'
#' @return
#' For an sf object, `object` with its geometry replaced by hexagons, keeping
#' all of its other columns and the order of its rows, so the hexagons can be
#' joined to or compared with the original regions. For a geometry column, a
#' geometry column of hexagons in the same order, so `hexmap()` can be used
#' within `dplyr::mutate()` to add the hexagons as another geometry column.
#' The hexagons are in the coordinate reference system they were made in.
#'
#' @seealso [layouts] for how the hexagons are arranged.
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
#' election_hex <- hexmap(election, layout_hierarchy(state))
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
#' ggplot(hexmap(election, layout_hierarchy(state), crs = globe)) +
#'   geom_sf(aes(fill = state)) +
#'   coord_sf(crs = globe)
#' }
#'
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' \donttest{
#' # Add the hexagons as a second geometry column, alongside the regions
#' election <- dplyr::mutate(election, hex = hexmap(geometry, layout_hierarchy(state)))
#' }
#'
#' @importFrom sf st_area st_geometry st_point_on_surface
#' @export
hexmap <- function(object, layout = layout_hierarchy(), island_gap = 0.5,
                   crs = NULL, ...) {
  check_object(object)
  check_dots(...)
  check_layout(layout)
  check_number(island_gap, 0, Inf)
  object <- prepare_crs(object, crs)
  check_not_empty(object)
  geometry <- st_geometry(object)

  cellsize <- hex_cellsize(geometry, length(geometry))
  regions <- list(
    geometry = geometry,
    data = attribute_data(object),
    land = landmasses(geometry, cellsize, island_gap),
    cellsize = cellsize
  )
  result <- as_layout_result(layout(regions), regions)
  tile <- result$tile
  if (any(result$gap > 0)) {
    # Layouts may change the size of the hexagons
    cellsize <- result$cellsize
    if (is.null(cellsize)) cellsize <- sqrt(2 * stats::median(as.numeric(st_area(tile))) / sqrt(3))
    tile <- inset_groups(tile, result$groups, result$gap * cellsize, cellsize)
  }
  set_geometry(object, tile)
}

# Arguments that are now given to the layout
check_dots <- function(...) {
  if (!...length()) return(invisible())
  args <- names(substitute(list(...)))[-1]
  if (any(c("group", "group_gap") %in% args)) {
    stop("The groups (and the gap between them) are now given to the layout, ",
         "such as `layout = layout_hierarchy(state, gap = 0.1)`.", call. = FALSE)
  }
  stop("Unknown arguments to `hexmap()`",
       if (length(args)) paste0(": ", paste0("`", args[nzchar(args)], "`", collapse = ", ")),
       ".", call. = FALSE)
}

# Checks and completes the result of a layout: the hexagon of each region as
# an sfc, or a list with the hexagons as tile, and optionally the groups (a
# data frame of levels) and gap (for each level) to cut between them,
# cellsize (the width of the hexagons), point (where each region was
# allocated from) and warp (a function that warps geometries like the map).
as_layout_result <- function(result, regions) {
  if (inherits(result, "sfc")) result <- list(tile = result)
  if (!is.list(result) || !inherits(result$tile, "sfc")) {
    stop("A layout must return the hexagon of each region as an `sfc`, or a ",
         "list with the hexagons as `tile`.", call. = FALSE)
  }
  n <- length(regions$geometry)
  if (length(result$tile) != n) {
    stop("A layout must return one hexagon for each region (", n, "), not ",
         length(result$tile), ".", call. = FALSE)
  }
  groups <- result$groups
  if (!is.null(groups) && (!is.data.frame(groups) || nrow(groups) != n || !ncol(groups))) {
    stop("The `groups` of a layout must be a data frame with one row for each ",
         "region, and a column for each level of groups.", call. = FALSE)
  }
  result$gap <- level_gap(if (is.null(result$gap)) 0.1 else result$gap, groups)
  result
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

# Trims gap[k] / 2 from each tile along the borders it shares with other
# groups of level k, so a border between outer groups takes the largest gap
# of any level it separates
#' @importFrom sf st_buffer st_centroid st_crs st_difference st_is_within_distance
#' @importFrom sf st_sfc st_union
inset_groups <- function(tile, groups, gap, cellsize) {
  near <- st_is_within_distance(st_centroid(tile), dist = 1.5 * cellsize)
  inset <- tile
  for (k in seq_along(gap)) {
    if (gap[k] <= 0) next
    key <- nest_key(groups, k)
    for (g in unique(key)) {
      i <- which(key == g)
      # Only neighbouring hexagons are close enough to trim
      others <- unique(unlist(near[i]))
      others <- others[key[others] != g]
      if (!length(others)) next
      border <- st_buffer(st_union(tile[others]), gap[k] / 2,
                          joinStyle = "MITRE", mitreLimit = 2)[[1]]
      inset[i] <- st_sfc(lapply(inset[i], st_difference, border), crs = st_crs(tile))
    }
  }
  inset
}
