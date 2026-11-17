election <- sf::st_transform(aus_election_2025, 3577)

test_that("layout functions passed without calling them are detected", {
  expect_error(layout_repair(layout_compact), "Did you mean `layout_compact\\(\\)`")
  expect_error(check_layout(layout_hierarchy), "Did you mean `layout_hierarchy\\(\\)`")
  expect_error(check_layout(hexmap::layout_slice), "Did you mean `layout_slice\\(\\)`")
  layout <- layout_diffusion
  expect_error(check_layout(layout), "`layout` must be a layout function")
  expect_error(check_layout(function() 1), "one argument, `regions`")
  expect_error(check_layout(function(geometry, group, land, cellsize) NULL),
               "one argument, `regions`")
})

test_that("layout functions and custom layouts pass check_layout()", {
  for (layout in list(layout_hierarchy(), layout_diffusion(), layout_compact(),
                      layout_slice(), layout_repair(layout_compact()))) {
    expect_silent(check_layout(layout))
  }
  expect_silent(check_layout(function(regions) NULL))
  expect_silent(check_layout(function(regions, extra = 1) NULL))
  expect_silent(check_layout(function(...) NULL))
})

test_that("layout arguments use the shared names", {
  expect_named(formals(layout_diffusion),
               c("group", "gap", "cartogram", "resolution", "proximity"))
  expect_named(formals(layout_hierarchy),
               c("group", "gap", "shape", "resolution", "iterations"))
  expect_named(formals(layout_repair), c("layout", "iterations", "border"))
  expect_named(formals(layout_compact), c("group", "gap", "anchor"))
  expect_named(formals(layout_slice), c("group", "gap", "tree", "anchor"))
  expect_error(layout_diffusion(alpha = 0.5), "unused argument")
  expect_error(layout_hierarchy(blend = 0.5), "unused argument")
})

test_that("layout arguments are validated when the layout is created", {
  expect_error(layout_diffusion(cartogram = -0.1), "`cartogram` must be a number between 0 and 1")
  expect_error(layout_hierarchy(shape = 1.5), "`shape` must be a number between 0 and 1")
  expect_error(layout_hierarchy(resolution = 8), "`resolution`")
  expect_error(layout_repair(layout_compact(), iterations = -1), "`iterations`")
  expect_error(layout_compact(anchor = c(x = 1, y = 2)), "`anchor` must be a data frame")
  expect_error(layout_slice(anchor = data.frame(group = "ACT", x = "a", y = 1)),
               "`x` and `y` must be numbers")
  expect_error(layout_slice(tree = 1:3), "`tree` must be a nested list of group names")
  expect_error(layout_slice(tree = list("WA", list("WA", "NT"))), "more than once: WA")
})

test_that("anchor and tree groups are checked against the map's groups", {
  anchor <- data.frame(group = "Atlantis", x = 0, y = 0)
  expect_error(hexmap(election, layout_compact(state, anchor = anchor)),
               "unknown groups: Atlantis")
  expect_error(hexmap(election, layout_slice(state, tree = list("WA", "NT"))),
               "missing groups")
})

test_that("layouts with renamed arguments run on the bundled data", {
  hex <- hexmap(election, layout_diffusion(state, cartogram = 0.5, resolution = 64))
  expect_equal(nrow(hex), nrow(election))
  expect_length(unique(sf::st_geometry(hex)), nrow(election))

  nsw <- sf::st_coordinates(sf::st_centroid(sf::st_union(election[election$state == "NSW", ])))
  anchor <- data.frame(group = "ACT", x = nsw[1] + 5e5, y = nsw[2])
  hex <- hexmap(election, layout_compact(state, gap = 0, anchor = anchor))
  centre <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(hex)))[, 1]
  expect_gt(mean(centre[hex$state == "ACT"]), mean(centre[hex$state == "NSW"]))
})

test_that("groups are evaluated when the layout is used", {
  # Created before the data exists, and reused across maps
  layout <- layout_compact(state)
  expect_equal(nrow(hexmap(election, layout)), nrow(election))
  expect_equal(nrow(hexmap(election[election$state != "WA", ], layout)),
               sum(election$state != "WA"))
  # A variable where the layout was created
  by_state <- election$state
  expect_equal(hexmap(election, layout_compact(by_state)), hexmap(election, layout))
})

test_that("the tree of a slice layout names outer groups, which are cut into inner groups", {
  election$area <- ifelse(election$division %in% c("Sydney", "Melbourne"), "city", "country")
  tree <- list("WA", list(list("NT", "SA"), list("QLD", "NSW", "ACT", "VIC")), "TAS")
  hex <- hexmap(election, layout_slice(state / area, tree = tree))
  expect_equal(nrow(hex), nrow(election))
  expect_error(hexmap(election, layout_slice(state / area, tree = list("city", "country"))),
               "unknown groups")
})

test_that("groups that evaluate to NULL are no groups", {
  group <- NULL
  expect_equal(hexmap(election, layout_compact(group)), hexmap(election, layout_compact()))
})
