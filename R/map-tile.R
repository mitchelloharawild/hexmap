#' Create a hexagon grid that tiles a geometry
#'
#' Tiles a geometry with exactly `n_tiles` equally sized hexagons. The hexagon
#' size is chosen so that `n_tiles` hexagons have the same total area as the
#' geometry, and the hexagons that overlap the geometry the most are kept.
#'
#' @param object An sf object or geometry to tile, in a projected coordinate
#'   reference system. It is recommended to first simplify this geometry to
#'   speed up the hexagon tiling.
#' @param n_tiles The number of tiles to use for representing the geometry.
#'
#' @return
#' A vector of `n_tiles` hexagon geometries that tile the input geometry.
#'
#' @examples
#' library(ozmaps)
#' library(sf)
#' australia <- st_transform(abs_ste, 3577)
#' grid <- hex_grid(australia, n_tiles = 75)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = australia) +
#'   geom_sf(data = grid, fill = "steelblue", alpha = 0.2)
#'
#' @importFrom sf st_area st_geometry st_intersection st_intersects st_make_grid st_union
#' @export
hex_grid <- function(object, n_tiles = 100) {
  check_projected(object)
  land <- st_union(st_geometry(object))

  # Spacing between hexagon centres such that n_tiles hexagons cover the land
  hex_area <- as.numeric(st_area(land)) / n_tiles
  cellsize <- sqrt(2 * hex_area / sqrt(3))

  repeat {
    grid <- st_make_grid(land, cellsize = cellsize, square = FALSE)
    grid <- grid[lengths(st_intersects(grid, land)) > 0]
    if (length(grid) >= n_tiles) break
    # Too few hexagons touch the land, so shrink them and try again
    cellsize <- cellsize * sqrt(length(grid) / n_tiles) * 0.99
  }

  # Keep the n_tiles hexagons that overlap the land the most
  overlap <- as.numeric(st_area(st_intersection(grid, land)))
  grid[order(overlap, decreasing = TRUE)[seq_len(n_tiles)]]
}

#' @importFrom sf st_crs st_is_longlat
check_projected <- function(object) {
  if (isTRUE(st_is_longlat(object)) || is.na(st_crs(object))) {
    stop(
      "`object` must use a projected coordinate reference system. ",
      "Use `sf::st_transform()` first, e.g. EPSG:3577 for Australia.",
      call. = FALSE
    )
  }
}
