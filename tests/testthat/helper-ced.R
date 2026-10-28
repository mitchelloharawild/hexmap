# Australian federal electoral divisions (2018) of the mainland and Tasmania,
# with their state, in the Australian Albers projection (`ced`) and in
# longitude and latitude (`ced_ll`)
if (requireNamespace("ozmaps", quietly = TRUE)) {
  ced <- suppressWarnings(sf::st_crop(ozmaps::abs_ced, xmin = 112, ymin = -44,
                                      xmax = 154, ymax = -9))
  ced <- sf::st_simplify(sf::st_transform(ced, 3577), dTolerance = 1000)
  names(ced)[1] <- "division"

  states <- sf::st_transform(ozmaps::abs_ste, 3577)
  states$state <- c(
    "New South Wales" = "NSW", "Victoria" = "VIC", "Queensland" = "QLD",
    "South Australia" = "SA", "Western Australia" = "WA", "Tasmania" = "TAS",
    "Northern Territory" = "NT", "Australian Capital Territory" = "ACT",
    "Other Territories" = "OT"
  )[states$NAME]
  ced <- suppressWarnings(sf::st_join(ced, states["state"], largest = TRUE))

  # Bands of latitude, the southern one spanning the mainland and Tasmania
  y <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(ced)))[, 2]
  ced$band <- cut(y, stats::quantile(y, 0:3 / 3), include.lowest = TRUE,
                  labels = c("south", "central", "north"))

  ced_ll <- sf::st_transform(ced, 4326)
}
