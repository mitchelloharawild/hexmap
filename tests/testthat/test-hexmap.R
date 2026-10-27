aec <- sf::st_transform(aec2025, 3577)
cellsize <- hex_cellsize(sf::st_geometry(aec), nrow(aec))

# Number of separate pieces formed by the tiles of each group
group_pieces <- function(hex, group) {
  vapply(split(sf::st_geometry(hex), group), function(x) {
    length(sf::st_cast(sf::st_union(sf::st_buffer(x, 1)), "POLYGON"))
  }, integer(1))
}

test_that("hex_grid() returns exactly n_tiles distinct hexagons", {
  grid <- hex_grid(aec, n_tiles = 150)
  expect_length(grid, 150)
  expect_true(all(sf::st_geometry_type(grid) == "POLYGON"))
  expect_length(unique(grid), 150)
})

test_that("hex_grid() tiles distant islands separately", {
  islands <- sf::st_sfc(
    sf::st_buffer(sf::st_point(c(0, 0)), 1),
    sf::st_buffer(sf::st_point(c(100, 0)), 1),
    crs = 3577
  )
  grid <- hex_grid(islands, n_tiles = 2)
  expect_length(grid, 2)
  expect_true(all(lengths(sf::st_intersects(grid, islands)) == 1))
})

test_that("tile_allocate() assigns each region a unique tile", {
  hex <- hexmap(aec)
  expect_s3_class(hex, "sf")
  expect_equal(nrow(hex), nrow(aec))
  expect_equal(hex$division, aec$division)
  expect_length(unique(sf::st_geometry(hex)), nrow(aec))
})

test_that("tile_allocate() never swaps the relative position of two regions", {
  hex <- tile_allocate(aec, hex_grid(aec, n_tiles = nrow(aec)))
  from <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(aec)))
  to <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(hex)))
  inner <- outer(from[, 1], from[, 1], "-") * outer(to[, 1], to[, 1], "-") +
    outer(from[, 2], from[, 2], "-") * outer(to[, 2], to[, 2], "-")
  expect_gte(min(inner), -1e-6)
})

test_that("hexmap() keeps islands apart from the mainland", {
  hex <- hexmap(aec)
  tas <- hex$state == "TAS"
  gap <- sf::st_distance(sf::st_union(hex[tas, ]), sf::st_union(hex[!tas, ]))
  expect_gt(as.numeric(gap), cellsize / 2)
  expect_equal(unname(group_pieces(hex[tas, ], "TAS")), 1L)

  # With no island gap Tasmania is tiled together with the mainland
  hex <- hexmap(aec, island_gap = Inf)
  gap <- sf::st_distance(sf::st_union(hex[tas, ]), sf::st_union(hex[!tas, ]))
  expect_lt(as.numeric(gap), cellsize / 2)
})

test_that("hexmap() keeps the tiles of each group contiguous", {
  for (layout in c("compact", "cartogram")) {
    hex <- hexmap(aec, group = state, group_layout = layout, group_gap = 0)
    expect_true(all(group_pieces(hex, hex$state) == 1))

    # Each piece of a group that spans islands is contiguous
    expect_warning(
      hex <- hexmap(aec, group = winner, group_layout = layout, group_gap = 0),
      NA
    )
    pieces <- group_pieces(hex, hex$winner)
    expect_true(all(pieces <= 2))
    tas <- hex$state == "TAS"
    expect_true(all(group_pieces(hex[!tas, ], hex$winner[!tas]) == 1))
  }
})

centre_of <- function(x) {
  xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(x)))
  apply(xy, 2, function(v) tapply(v, x$state, mean))
}

test_that("hexmap() keeps each group in its place", {
  for (layout in c("compact", "cartogram")) {
    xy <- centre_of(hexmap(aec, group = state, group_layout = layout))
    west_of <- function(a, b) all(xy[a, "X"] < xy[b, "X"])
    north_of <- function(a, b) all(xy[a, "Y"] > xy[b, "Y"])
    expect_true(west_of("WA", c("SA", "NT", "QLD", "NSW", "VIC")))
    expect_true(west_of(c("SA", "NT"), c("QLD", "NSW")))
    expect_true(north_of(c("NT", "QLD"), c("SA", "NSW", "VIC", "TAS")))
    expect_true(north_of("NSW", c("VIC", "TAS")))
    expect_true(north_of("VIC", "TAS"))
  }
})

test_that("hexmap() lays out groups as compact blocks", {
  hex <- hexmap(aec, group = state, group_gap = 0)
  tas <- hex$state == "TAS"
  # The mainland has no holes
  mainland <- sf::st_union(sf::st_buffer(sf::st_geometry(hex[!tas, ]), 1))
  expect_length(sf::st_cast(mainland, "POLYGON"), 1)
  expect_length(mainland[[1]], 1)
  # Tasmania is at least one hexagon away from the mainland
  size <- sqrt(2 * max(as.numeric(sf::st_area(hex))) / sqrt(3))
  gap <- sf::st_distance(sf::st_union(hex[tas, ]), sf::st_union(hex[!tas, ]))
  expect_gt(as.numeric(gap), size / 2)
})

test_that("group_anchor moves groups", {
  nsw <- sf::st_coordinates(sf::st_centroid(sf::st_union(aec[aec$state == "NSW", ])))
  anchor <- data.frame(group = "ACT", x = nsw[1] + 5e5, y = nsw[2])
  xy <- centre_of(hexmap(aec, group = state, group_anchor = anchor))
  expect_gt(xy["ACT", "X"], xy["NSW", "X"])
})

test_that("tile_allocate() keeps the tiles of each group contiguous", {
  hex <- tile_allocate(aec, hex_grid(aec, n_tiles = nrow(aec), island_gap = Inf),
                       group = state)
  expect_true(all(group_pieces(hex, hex$state) == 1))
})

test_that("hexmap() separates groups with a gap", {
  gap <- 0.1
  hex <- hexmap(aec, group = state, group_gap = gap)
  states <- split(sf::st_geometry(hex), hex$state)
  between <- sf::st_distance(sf::st_union(states$ACT), sf::st_union(states$NSW))
  size <- sqrt(2 * max(as.numeric(sf::st_area(hex))) / sqrt(3))
  expect_equal(as.numeric(between), gap * size, tolerance = 1e-6)

  # Hexagons only shrink along group borders
  ungrouped <- hexmap(aec, group = state, group_gap = 0)
  area <- as.numeric(sf::st_area(hex)) / as.numeric(sf::st_area(ungrouped))
  expect_true(all(area <= 1 + 1e-9))
  expect_true(any(area == 1))
  expect_true(any(area < 1))
})

test_that("inputs are validated", {
  expect_error(hexmap(aec2025), "projected")
  expect_error(tile_allocate(aec, hex_grid(aec, n_tiles = 10)), "exactly one")
  expect_error(hexmap(aec, group = state[1:3]), "one value for each row")
  empty <- aec[1:3, ]
  sf::st_geometry(empty)[2] <- sf::st_polygon()
  expect_error(hexmap(empty), "empty")
  expect_error(hexmap(aec, group = state, group_anchor = data.frame(group = "ACT")),
               "columns")
  expect_error(
    hexmap(aec, group = state, group_anchor = data.frame(group = "X", x = 0, y = 0)),
    "unknown groups"
  )
})
