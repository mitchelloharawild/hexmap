aec <- sf::st_transform(aec2025, 3577)

test_that("hex_grid() returns exactly n_tiles distinct hexagons", {
  grid <- hex_grid(aec, n_tiles = 150)
  expect_length(grid, 150)
  expect_true(all(sf::st_geometry_type(grid) == "POLYGON"))
  expect_length(unique(grid), 150)
})

test_that("hex_grid() shrinks hexagons when too few overlap the map", {
  islands <- sf::st_sfc(
    sf::st_buffer(sf::st_point(c(0, 0)), 1),
    sf::st_buffer(sf::st_point(c(100, 0)), 1),
    crs = 3577
  )
  expect_length(hex_grid(islands, n_tiles = 2), 2)
})

test_that("tile_allocate() assigns each region a unique tile", {
  hex <- hexmap(aec)
  expect_s3_class(hex, "sf")
  expect_equal(nrow(hex), nrow(aec))
  expect_equal(hex$division, aec$division)
  expect_length(unique(sf::st_geometry(hex)), nrow(aec))
})

test_that("tile_allocate() never swaps the relative position of two regions", {
  hex <- hexmap(aec)
  from <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(aec)))
  to <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(hex)))
  inner <- outer(from[, 1], from[, 1], "-") * outer(to[, 1], to[, 1], "-") +
    outer(from[, 2], from[, 2], "-") * outer(to[, 2], to[, 2], "-")
  expect_gte(min(inner), -1e-6)
})

test_that("inputs are validated", {
  expect_error(hexmap(aec2025), "projected")
  expect_error(tile_allocate(aec, hex_grid(aec, n_tiles = 10)), "exactly one")
})
