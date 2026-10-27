#' Create a hexagon grid that tiles a geometry
#'
#' Tiles a geometry with exactly `n_tiles` equally sized hexagons. The hexagon
#' size is chosen so that `n_tiles` hexagons have the same total area as the
#' geometry, and the hexagons that overlap the geometry the most are kept.
#'
#' Landmasses that are further apart than `island_gap` hexagon widths are
#' tiled separately, with their share of the `n_tiles` hexagons proportional
#' to their area. The hexagons of different landmasses are always separated
#' by at least one empty hexagon, so islands such as Tasmania remain visibly
#' distinct from the mainland.
#'
#' @param object An sf object or geometry to tile, in a projected coordinate
#'   reference system. It is recommended to first simplify this geometry to
#'   speed up the hexagon tiling.
#' @param n_tiles The number of tiles to use for representing the geometry.
#' @param island_gap The distance, in hexagon widths, beyond which landmasses
#'   are tiled as separate islands. Closer landmasses are tiled together. Use
#'   `Inf` to tile all land together.
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
#' @importFrom sf st_area st_geometry
#' @export
hex_grid <- function(object, n_tiles = 100, island_gap = 0.5) {
  check_projected(object)
  geometry <- st_geometry(object)
  cellsize <- hex_cellsize(geometry, n_tiles)
  land <- landmasses(geometry, cellsize, island_gap)

  # Share the tiles between landmasses in proportion to their area
  area <- as.numeric(st_area(land$geometry))
  n <- round_preserving_sum(n_tiles * area / sum(area))
  landmass_grid(land$geometry, n, cellsize)$tile
}

# Spacing between hexagon centres such that n_tiles hexagons cover the land
#' @importFrom sf st_area st_union
hex_cellsize <- function(geometry, n_tiles) {
  hex_area <- as.numeric(st_area(st_union(geometry))) / n_tiles
  sqrt(2 * hex_area / sqrt(3))
}

# Groups the land into landmasses that are separated by more than island_gap
# hexagon widths. Only polygons that contain a region (or are large enough to
# hold a hexagon) form landmasses, so small islands don't act as stepping
# stones that join landmasses together.
#
# Returns the landmass geometries and the landmass of each region.
#' @importFrom sf st_area st_cast st_intersects st_is_within_distance
#' @importFrom sf st_nearest_feature st_point_on_surface st_union
landmasses <- function(geometry, cellsize, island_gap) {
  polygons <- st_cast(st_union(geometry), "POLYGON")
  points <- st_point_on_surface(geometry)
  where <- vapply(st_intersects(points, polygons),
                  function(i) if (length(i)) i[1] else NA_integer_, integer(1))
  where[is.na(where)] <- st_nearest_feature(points[is.na(where)], polygons)

  hex_area <- sqrt(3) / 2 * cellsize^2
  seed <- seq_along(polygons) %in% where |
    as.numeric(st_area(polygons)) >= hex_area
  near <- st_is_within_distance(polygons[seed], dist = island_gap * cellsize)
  id <- integer(length(polygons))
  id[seed] <- connected_components(near)

  geometry <- lapply(split(polygons[seed], id[seed]), st_union)
  list(geometry = do.call(c, geometry), region = id[where])
}

# Tiles each landmass with n[k] hexagons from a common hexagon lattice. Each
# landmass grows a contiguous set of hexagons, starting from the hexagon that
# overlaps it the most and repeatedly adding the neighbouring hexagon that
# overlaps it the most (or failing that, is nearest to it). Hexagons next to
# another landmass's hexagons are never used, which keeps islands apart.
#
# Returns the hexagons and the landmass that each belongs to.
#' @importFrom sf st_area st_as_sfc st_bbox st_centroid st_distance
#' @importFrom sf st_intersection st_make_grid
landmass_grid <- function(land, n, cellsize) {
  # Leave room around the land for landmasses with more tiles than area
  hex_area <- sqrt(3) / 2 * cellsize^2
  shortfall <- pmax(n - as.numeric(st_area(land)) / hex_area, 0)
  margin <- cellsize * (2 + sqrt(max(shortfall)))

  repeat {
    bbox <- st_bbox(land)
    bbox[] <- bbox + c(-1, -1, 1, 1) * margin
    lattice <- st_make_grid(st_as_sfc(bbox), cellsize = cellsize, square = FALSE)
    centre <- st_centroid(lattice)
    adjacent <- hex_adjacency(centre, cellsize)

    owner <- integer(length(lattice))
    for (k in order(n, decreasing = TRUE)) {
      if (n[k] == 0) next
      overlap <- numeric(length(lattice))
      inside <- st_intersection(lattice, land[k])
      overlap[attr(inside, "idx")[, 1]] <- as.numeric(st_area(inside))
      distance <- as.numeric(st_distance(centre, land[k]))

      taken <- owner > 0
      blocked <- taken | vapply(adjacent, function(i) any(taken[i]), logical(1))
      tiles <- grow_tiles(n[k], order(-overlap, distance), adjacent, blocked)
      if (is.null(tiles)) break
      owner[tiles] <- k
    }
    if (sum(owner > 0) == sum(n)) break
    # The lattice was too small to fit every landmass, so enlarge it
    margin <- margin * 2
  }

  keep <- which(owner > 0)
  list(tile = lattice[keep], landmass = owner[keep])
}

# Greedily grows a contiguous set of n hexagons in order of priority
grow_tiles <- function(n, priority, adjacent, blocked) {
  rank <- integer(length(priority))
  rank[priority] <- seq_along(priority)
  rank[blocked] <- NA

  frontier <- rep(FALSE, length(rank))
  chosen <- integer(0)
  repeat {
    candidates <- if (length(chosen)) which(frontier) else seq_along(rank)
    if (all(is.na(rank[candidates]))) return(NULL)
    best <- candidates[which.min(rank[candidates])]
    chosen <- c(chosen, best)
    if (length(chosen) == n) return(chosen)
    rank[best] <- NA
    frontier[adjacent[[best]]] <- TRUE
  }
}

# Neighbouring hexagons have centres one cellsize apart
#' @importFrom sf st_is_within_distance
hex_adjacency <- function(centre, cellsize) {
  near <- st_is_within_distance(centre, dist = 1.2 * cellsize)
  lapply(seq_along(near), function(i) setdiff(near[[i]], i))
}

# Labels the connected components of a graph given as an adjacency list,
# optionally restricted to a subset of its nodes
connected_components <- function(adjacent, nodes = seq_along(adjacent)) {
  id <- integer(length(adjacent))
  id[-nodes] <- NA
  n <- 0L
  for (start in nodes) {
    if (id[start] != 0L) next
    n <- n + 1L
    id[start] <- n
    queue <- start
    while (length(queue)) {
      neighbours <- adjacent[[queue[1]]]
      neighbours <- neighbours[id[neighbours] %in% 0L]
      id[neighbours] <- n
      queue <- c(queue[-1], neighbours)
    }
  }
  id[nodes]
}

# Rounds non-negative numbers to integers while preserving their total
round_preserving_sum <- function(x) {
  n <- floor(x)
  extra <- round(sum(x)) - sum(n)
  top <- order(x - n, decreasing = TRUE)[seq_len(extra)]
  n[top] <- n[top] + 1
  n
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

#' @importFrom sf st_is_empty
check_not_empty <- function(object) {
  if (any(st_is_empty(object))) {
    stop("`object` must not contain empty geometries.", call. = FALSE)
  }
}
