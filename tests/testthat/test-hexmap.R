skip_if_not_installed("ozmaps")

cellsize <- hex_cellsize(sf::st_geometry(ced), nrow(ced))

# Number of separate pieces formed by the tiles of each group
group_pieces <- function(hex, group) {
  vapply(split(sf::st_geometry(hex), group), function(x) {
    length(sf::st_cast(sf::st_union(sf::st_buffer(x, 1)), "POLYGON"))
  }, integer(1))
}

test_that("every layout gives each region a distinct hexagon without groups", {
  layouts <- list(layout_hierarchy(), layout_diffusion(), layout_compact(),
                  layout_slice(), layout_repair(layout_compact()))
  for (layout in layouts) {
    hex <- hexmap(sf::st_geometry(ced), layout = layout)
    expect_length(hex, nrow(ced))
    expect_true(all(sf::st_geometry_type(hex) == "POLYGON"))
    expect_length(unique(hex), nrow(ced))
  }
})

test_that("hexmap() tiles distant islands separately", {
  islands <- sf::st_sfc(
    sf::st_buffer(sf::st_point(c(0, 0)), 1),
    sf::st_buffer(sf::st_point(c(100, 0)), 1),
    crs = 3577
  )
  grid <- hexmap(islands)
  expect_length(grid, 2)
  expect_true(all(lengths(sf::st_intersects(grid, islands)) == 1))
})

test_that("hexmap() assigns each region a unique tile", {
  hex <- hexmap(ced)
  expect_s3_class(hex, "sf")
  expect_equal(nrow(hex), nrow(ced))
  expect_equal(hex$division, ced$division)
  expect_length(unique(sf::st_geometry(hex)), nrow(ced))
})

test_that("layout_diffusion() never swaps the relative position of two regions", {
  hex <- hexmap(ced, layout = layout_diffusion(), island_gap = Inf)
  from <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(ced)))
  to <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(hex)))
  inner <- outer(from[, 1], from[, 1], "-") * outer(to[, 1], to[, 1], "-") +
    outer(from[, 2], from[, 2], "-") * outer(to[, 2], to[, 2], "-")
  expect_gte(min(inner), -1e-6)
})

test_that("hexmap() keeps islands apart from the mainland", {
  hex <- hexmap(ced)
  tas <- hex$state == "TAS"
  gap <- sf::st_distance(sf::st_union(hex[tas, ]), sf::st_union(hex[!tas, ]))
  expect_gt(as.numeric(gap), cellsize / 2)
  expect_equal(unname(group_pieces(hex[tas, ], "TAS")), 1L)

  # With no island gap Tasmania is tiled together with the mainland
  hex <- hexmap(ced, island_gap = Inf)
  gap <- sf::st_distance(sf::st_union(hex[tas, ]), sf::st_union(hex[!tas, ]))
  expect_lt(as.numeric(gap), cellsize / 2)
})

centre_of <- function(x) {
  xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(x)))
  apply(xy, 2, function(v) tapply(v, x$state, mean))
}

layouts <- list(layout_hierarchy(), layout_repair(layout_diffusion()), layout_diffusion(cartogram = 0),
                layout_diffusion(cartogram = 1), layout_compact(),
                layout_repair(layout_compact()), layout_slice())

# The pairs of states whose tiles touch
touching <- function(hex) {
  size <- sqrt(2 * max(as.numeric(sf::st_area(hex))) / sqrt(3))
  near <- sf::st_is_within_distance(sf::st_centroid(sf::st_geometry(hex)), dist = 1.2 * size)
  pair <- cbind(hex$state[rep(seq_along(near), lengths(near))], hex$state[unlist(near)])
  pair <- pair[pair[, 1] < pair[, 2], , drop = FALSE]
  sort(unique(paste(pair[, 1], pair[, 2], sep = "-")))
}
borders <- c("ACT-NSW", "NSW-QLD", "NSW-SA", "NSW-VIC", "NT-QLD", "NT-SA", "NT-WA",
             "QLD-SA", "SA-VIC", "SA-WA")

test_that("the diffusion layout keeps the borders between states", {
  hex <- hexmap(ced, group = state, layout = layout_repair(layout_diffusion()),
                group_gap = 0)
  expect_equal(touching(hex), borders)
  # Without repair, the borders are close but not exact
  hex <- hexmap(ced, group = state, layout = layout_diffusion(), group_gap = 0)
  expect_gte(length(intersect(touching(hex), borders)), length(borders) - 1)
})

test_that("layout_repair() fixes the borders of other layouts", {
  hex <- hexmap(ced, group = state, group_gap = 0,
                layout = layout_repair(layout_compact()))
  expect_true(all(borders %in% touching(hex)))
  # Repair keeps the hexagons of the layout
  compact <- hexmap(ced, group = state, layout = layout_compact(), group_gap = 0)
  expect_setequal(sf::st_geometry(hex), sf::st_geometry(compact))
})

test_that("the default hierarchy layout keeps borders, and the ACT inside NSW", {
  hex <- hexmap(ced, group = state, group_gap = 0)
  expect_equal(touching(hex), borders)
  # Every hexagon of the ACT is surrounded by hexagons
  size <- sqrt(2 * max(as.numeric(sf::st_area(hex))) / sqrt(3))
  centre <- sf::st_centroid(sf::st_geometry(hex))
  near <- sf::st_is_within_distance(centre, dist = 1.2 * size)
  expect_true(all(lengths(near[hex$state == "ACT"]) == 7))
  # SA is west of VIC, and QLD north of NSW
  xy <- centre_of(hex)
  expect_lt(xy["SA", "X"], xy["VIC", "X"])
  expect_gt(xy["QLD", "Y"], xy["NSW", "Y"])
})

# Share of regions that touch another state on the map whose hexagon touches
# that state's hexagons
border_kept <- function(hex) {
  state <- unique(ced$state)
  shape <- do.call(c, lapply(state, function(s) sf::st_union(ced[ced$state == s, ])))
  on_map <- sf::st_is_within_distance(ced, shape, 1000, sparse = FALSE)
  size <- sqrt(2 * max(as.numeric(sf::st_area(hex))) / sqrt(3))
  centre <- sf::st_centroid(sf::st_geometry(hex))
  near <- sf::st_is_within_distance(centre, dist = 1.2 * size)
  on_hex <- t(vapply(near, function(k) state %in% hex$state[k], logical(length(state))))
  other <- outer(ced$state, state, "!=")
  sum(on_map & on_hex & other) / sum(on_map & other)
}

test_that("border keeps regions on the borders between states", {
  hex <- hexmap(ced, group = state, layout = layout_repair(layout_diffusion()),
                group_gap = 0)
  kept <- hexmap(ced, group = state, group_gap = 0,
                 layout = layout_repair(layout_diffusion(), border = 2))
  # Only the placement within each state changes
  for (s in unique(ced$state)) {
    expect_setequal(sf::st_geometry(kept)[kept$state == s], sf::st_geometry(hex)[hex$state == s])
  }
  expect_gt(border_kept(kept), border_kept(hex))
})

test_that("the diffusion layout keeps the outline of the map without warping", {
  hex <- hexmap(ced, group = state, layout = layout_diffusion(cartogram = 0),
                group_gap = 0)
  outline <- sf::st_geometry(hexmap(ced, layout = layout_diffusion()))
  expect_setequal(sf::st_geometry(hex), outline)
  # Warping moves the outline
  hex <- hexmap(ced, group = state, layout = layout_diffusion(), group_gap = 0)
  expect_false(setequal(sf::st_geometry(hex), outline))
})

test_that("hexmap() keeps the tiles of each group contiguous", {
  for (layout in layouts) {
    hex <- hexmap(ced, group = state, layout = layout, group_gap = 0)
    expect_true(all(group_pieces(hex, hex$state) == 1))

    # Each piece of a group that spans islands is contiguous
    expect_warning(
      hex <- hexmap(ced, group = band, layout = layout, group_gap = 0),
      NA
    )
    pieces <- group_pieces(hex, hex$band)
    expect_true(all(pieces <= 2))
    tas <- hex$state == "TAS"
    expect_true(all(group_pieces(hex[!tas, ], hex$band[!tas]) == 1))
  }
})

test_that("hexmap() keeps each group in its place", {
  for (layout in layouts) {
    xy <- centre_of(hexmap(ced, group = state, layout = layout))
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
  hex <- hexmap(ced, group = state, layout = layout_compact(), group_gap = 0)
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

test_that("anchor moves compact groups", {
  nsw <- sf::st_coordinates(sf::st_centroid(sf::st_union(ced[ced$state == "NSW", ])))
  anchor <- data.frame(group = "ACT", x = nsw[1] + 5e5, y = nsw[2])
  xy <- centre_of(hexmap(ced, group = state,
                          layout = layout_compact(anchor = anchor)))
  expect_gt(xy["ACT", "X"], xy["NSW", "X"])
})

test_that("the slice layout cuts groups apart with straight borders", {
  hex <- hexmap(ced, group = state, layout = layout_slice(), group_gap = 0)
  expect_length(unique(sf::st_geometry(hex)), nrow(ced))
  expect_true(all(group_pieces(hex, hex$state) == 1))

  # The mainland is a block with no holes, and Tasmania is kept apart
  tas <- hex$state == "TAS"
  mainland <- sf::st_union(sf::st_buffer(sf::st_geometry(hex[!tas, ]), 1))
  expect_length(sf::st_cast(mainland, "POLYGON"), 1)
  expect_length(mainland[[1]], 1)
  size <- sqrt(2 * max(as.numeric(sf::st_area(hex))) / sqrt(3))
  gap <- sf::st_distance(sf::st_union(hex[tas, ]), sf::st_union(hex[!tas, ]))
  expect_gt(as.numeric(gap), size)

  xy <- centre_of(hex)
  expect_gt(xy["NT", "Y"], xy["SA", "Y"])
  expect_lt(xy["WA", "X"], xy["SA", "X"])
  expect_lt(xy["VIC", "Y"], xy["NSW", "Y"])
})

test_that("tree sets the cuts of the slice layout", {
  tree <- list(c("WA", "NT", "SA", "TAS"), c("QLD", "NSW", "ACT", "VIC"))
  hex <- hexmap(ced, group = state, layout = layout_slice(tree = tree),
                group_gap = 0)
  expect_true(all(group_pieces(hex, hex$state) == 1))
  # The western states are all west of the eastern states
  x <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(hex)))[, 1]
  west <- hex$state %in% c("WA", "NT", "SA")
  east <- hex$state %in% c("QLD", "NSW", "ACT", "VIC")
  expect_lte(max(x[west]), min(x[east]))
})

test_that("hexmap() separates groups with a gap", {
  gap <- 0.1
  hex <- hexmap(ced, group = state, group_gap = gap)
  states <- split(sf::st_geometry(hex), hex$state)
  between <- sf::st_distance(sf::st_union(states$ACT), sf::st_union(states$NSW))
  size <- sqrt(2 * max(as.numeric(sf::st_area(hex))) / sqrt(3))
  expect_equal(as.numeric(between), gap * size, tolerance = 1e-6)

  # Hexagons only shrink along group borders
  ungrouped <- hexmap(ced, group = state, group_gap = 0)
  area <- as.numeric(sf::st_area(hex)) / as.numeric(sf::st_area(ungrouped))
  expect_true(all(area <= 1 + 1e-9))
  expect_true(any(area == 1))
  expect_true(any(area < 1))
})

test_that("inputs are validated", {
  no_crs <- sf::st_set_crs(ced, NA)
  expect_error(hexmap(no_crs), "projected")
  expect_error(hexmap(ced, group = state[1:3]), "one value for each region")
  empty <- ced[1:3, ]
  sf::st_geometry(empty)[2] <- sf::st_polygon()
  expect_error(hexmap(empty), "empty")
  compact <- function(anchor) {
    hexmap(ced, group = state, layout = layout_compact(anchor = anchor))
  }
  expect_error(compact(data.frame(group = "ACT")), "columns")
  expect_error(compact(data.frame(group = "X", x = 0, y = 0)), "unknown groups")
  slice <- function(tree) {
    hexmap(ced, group = state, layout = layout_slice(tree = tree))
  }
  expect_error(slice(list(1, 2)), "nested list of group names")
  expect_error(slice(list(unique(ced$state), "X")), "unknown groups: X")
  expect_error(slice(list("WA", "NT")), "missing groups")
  expect_error(slice(list(unique(ced$state), "WA")), "more than once: WA")
  expect_error(layout_diffusion(cartogram = 2), "between 0 and 1")
  expect_error(layout_hierarchy(shape = 2), "between 0 and 1")
  expect_error(layout_repair(layout_compact(), border = -1), "`border`")
  expect_error(layout_diffusion(proximity = -1), "`proximity`")
  expect_error(layout_repair("compact"), "`layout` must be a layout function")
  expect_error(hexmap(ced, group = state, layout = "compact"), "layout function")
})

# Ratio of the shortest to the longest edge of each hexagon
edge_ratio <- function(hex) {
  vapply(sf::st_geometry(hex), function(p) {
    xy <- p[[1]]
    edge <- sqrt(rowSums(diff(xy)^2))
    min(edge) / max(edge)
  }, numeric(1))
}

test_that("crs sets the coordinate reference system of the hexagons", {
  expect_equal(hexmap(ced_ll, crs = 3577), hexmap(ced))

  globe <- "+proj=ortho +lat_0=-25 +lon_0=134"
  hex <- hexmap(ced_ll, crs = globe)
  expect_equal(sf::st_crs(hex), sf::st_crs(globe))
  expect_gt(min(edge_ratio(hex)), 1 - 1e-6)
  # Hexagons made in another CRS are stretched by the globe
  expect_lt(min(edge_ratio(sf::st_transform(hexmap(ced), globe))), 0.99)
})

test_that("longlat input defaults to an equal-area CRS centred on the data", {
  expect_message(hex <- hexmap(ced_ll), "+proj=laea", fixed = TRUE)
  crs <- sf::st_crs(hex)
  expect_match(crs$proj4string, "+proj=laea", fixed = TRUE)
  expect_false(sf::st_is_longlat(hex))
  # The hexagons cover equal areas on the ground
  area <- as.numeric(sf::st_area(sf::st_transform(hex, "OGC:CRS84")))
  expect_lt(max(area) / min(area), 1.02)

  # Projected input keeps its CRS
  expect_no_message(hex <- hexmap(ced))
  expect_equal(sf::st_crs(hex), sf::st_crs(ced))

  # Data crossing the antimeridian is centred near it
  square <- function(x) sf::st_polygon(list(cbind(x + c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
  fiji <- sf::st_sf(id = 1:4, geometry = sf::st_sfc(
    square(178), square(179), square(-180), square(-179), crs = 4326
  ))
  expect_message(hex <- hexmap(fiji), "+lon_0=", fixed = TRUE)
  lon_0 <- as.numeric(sub(".*\\+lon_0=(\\S+).*", "\\1", sf::st_crs(hex)$proj4string))
  expect_gt(abs(lon_0), 179)
})

test_that("hexmap() shares hexagons between landmasses by their regions", {
  hex <- hexmap(sf::st_geometry(ced))
  # Tasmania and the mainland get one hexagon for each of their regions
  island <- connected_components(hex_adjacency(sf::st_centroid(hex), cellsize))
  expect_equal(sort(tabulate(island)), c(sum(ced$state == "TAS"), sum(ced$state != "TAS")))
})

test_that("hexmap() handles coordinate reference systems", {
  expect_message(hex <- hexmap(ced_ll), "+proj=laea", fixed = TRUE)
  expect_false(sf::st_is_longlat(hex))
  expect_equal(sf::st_crs(hexmap(ced_ll, crs = 3577)), sf::st_crs(ced))
  expect_error(hexmap(sf::st_set_crs(ced, NA), crs = 3577),
               "must have a coordinate reference system")
})

test_that("hexmap() keeps the columns and order of rows", {
  shuffled <- ced[rev(seq_len(nrow(ced))), ]
  hex <- hexmap(shuffled, group = state, layout = layout_compact())
  expect_named(hex, names(shuffled))
  expect_equal(sf::st_drop_geometry(hex), sf::st_drop_geometry(shuffled))
})

test_that("hexmap() accepts a geometry column", {
  geometry <- sf::st_geometry(ced)
  hex <- hexmap(geometry)
  expect_s3_class(hex, "sfc")
  expect_equal(hex, sf::st_geometry(hexmap(ced)))

  group <- ced$state
  grouped <- hexmap(geometry, group = group, layout = layout_compact())
  expect_equal(grouped, sf::st_geometry(
    hexmap(ced, group = state, layout = layout_compact())
  ))

  skip_if_not_installed("dplyr")
  with_hex <- dplyr::mutate(ced, hex = hexmap(geometry, group = state,
                                              layout = layout_compact()))
  expect_equal(with_hex$hex, grouped)
  expect_equal(with_hex$geometry, geometry)
})

test_that("group can be a column name", {
  expect_equal(
    hexmap(ced, group = "state", layout = layout_compact()),
    hexmap(ced, group = state, layout = layout_compact())
  )
  # Strings that aren't columns, or with a geometry column, hint at the column
  expect_error(hexmap(ced, group = "states"), "unquoted")
  expect_error(hexmap(sf::st_geometry(ced), group = "state"), "unquoted")
})

test_that("more inputs are validated", {
  missing_group <- ced
  missing_group$state[c(2, 5)] <- NA
  expect_error(hexmap(missing_group, group = state), "missing values \\(in rows 2, 5\\)")

  expect_error(hexmap(ced[0, ]), "at least one region")
  expect_error(hexmap(data.frame(x = 1)), "sf object or a geometry column")
  expect_error(hexmap(ced, island_gap = "a"), "`island_gap` must be a number")
  expect_error(hexmap(ced, island_gap = -1), "`island_gap` must be a number")
  expect_error(hexmap(ced, group = state, group_gap = -0.1), "`group_gap` must be a number")

  # Without groups there are no borders, so group_gap has no effect
  expect_equal(hexmap(ced, group_gap = 0.2), hexmap(ced, group_gap = 0))
})
