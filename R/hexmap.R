#' Convert a map into a hexagon tile map
#'
#' Replaces each region of a map with a hexagon of equal size. The hexagons
#' tile the outline of the map, and each region is allocated the hexagon that
#' minimises the total squared displacement of all regions.
#'
#' @param object An sf object in a projected coordinate reference system.
#'
#' @return
#' `object` with its geometry replaced by hexagons.
#'
#' @seealso [hex_grid()] to create the hexagons and [tile_allocate()] to
#'   allocate them to regions.
#'
#' @examples
#' library(sf)
#' aec_hex <- hexmap(st_transform(aec2025, 3577))
#'
#' library(ggplot2)
#' ggplot(aec_hex) +
#'   geom_sf(aes(fill = winner))
#'
#' @export
hexmap <- function(object) {
  tile_allocate(object, hex_grid(object, n_tiles = NROW(object)))
}
