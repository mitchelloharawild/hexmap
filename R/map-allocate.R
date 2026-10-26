#' Allocates tiled geometries to original map geometries
#'
#' Allocates each tile to exactly one of the original geometries, such that
#' the total squared distance between each geometry and its tile is minimised.
#' This optimal assignment is solved with the Hungarian algorithm via
#' [clue::solve_LSAP()].
#'
#' Minimising squared (rather than absolute) distance guarantees that no two
#' geometries swap their relative position: for any two regions, the
#' displacement from one tile to the other never points against the
#' displacement between the original regions.
#'
#' @param object An sf object or geometry that was used to be tiled.
#' @param tile A vector of tile geometries with one tile per row of `object`,
#'   such as those produced by [hex_grid()].
#'
#' @return
#' `object` with its geometry replaced by the allocated tiles.
#'
#' @examples
#' library(ozmaps)
#' library(sf)
#' australia <- st_transform(abs_ste, 3577)
#' australia <- australia[australia$NAME != "Other Territories", ]
#' grid <- hex_grid(australia, n_tiles = 8)
#' hexes <- tile_allocate(australia, grid)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = australia) +
#'   geom_sf(data = hexes, aes(fill = NAME), alpha = 0.5)
#'
#' @importFrom sf st_centroid st_coordinates st_geometry st_geometry<- st_point_on_surface
#' @export
tile_allocate <- function(object, tile) {
  tile <- st_geometry(tile)
  if (length(tile) != NROW(object)) {
    stop("`tile` must contain exactly one geometry for each row of `object`.",
         call. = FALSE)
  }

  from <- st_coordinates(st_point_on_surface(st_geometry(object)))
  to <- st_coordinates(st_centroid(tile))
  cost <- outer(from[, 1], to[, 1], "-")^2 + outer(from[, 2], to[, 2], "-")^2

  st_geometry(object) <- tile[as.integer(clue::solve_LSAP(cost))]
  object
}
