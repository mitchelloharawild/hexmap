
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hexmap

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

hexmap converts a map of regions into a map of equally sized hexagons,
one for each region. Every region gets the same amount of ink, so small
but important regions (such as inner-city electorates) are as visible as
large, sparsely populated ones, while each region stays close to where
it is on the map.

## Installation

You can install the development version of hexmap like so:

``` r
install.packages("remotes")
remotes::install_github("numbats/hexmap")
```

## Usage

`hexmap()` takes an `sf` object and returns it with each region’s
geometry replaced by a hexagon:

``` r
library(hexmap)
hexmap(electorates)
hexmap(electorates, group = state)
```

- **Optimal placement.** Each region gets the hexagon that minimises the
  total squared distance between regions and their hexagons, so the
  relative positions of regions are kept.
- **Groups.** With `group`, the hexagons of each group (such as a state)
  are kept together, separated by a small gap (`group_gap`), and touch
  the same neighbouring groups as on the map.
- **Group layouts.** `group_layout` sets how groups are arranged: warped
  into a cartogram that keeps each group’s shape (`layout_diffusion()`,
  the default), compact blocks (`layout_compact()`), or a spatial
  treemap (`layout_slice()`). `layout_repair()` fixes the borders
  between groups of any layout.
- **Islands.** Distant landmasses, such as Tasmania, are tiled
  separately and kept apart from the mainland (`island_gap`).
- **Projections.** Hexagons are regular in the projection they are made
  in (`crs`), which defaults to an equal-area projection for maps in
  longitude and latitude. See `vignette("projections")`.

The two steps of `hexmap()` are also available on their own:
`hex_grid()` tiles a map with hexagons, and `tile_allocate()` allocates
them to regions.

## Example: the 2025 Australian federal election

Labor won 94 of the 150 seats at the 2025 Australian federal election.
On a geographic map the result looks like a Coalition win, as the large
rural divisions hide Labor’s many small city seats. A cartogram with
equal areas for each division shows the number of seats, but distorts
the map beyond recognition. A hexagon grid made with
[geogrid](https://github.com/jbaileyh/geogrid) gives each division the
same space, but scatters the divisions of each state, so Tasmania’s five
seats end up across the mainland. hexmap keeps each state together, in
its place and with its neighbours.

``` r
election_hexmap <- hexmap(election, group = state)
```

<img src="man/figures/README-comparison-1.png" alt="" width="100%" />

The `aus_election2025` data has the simplified boundaries of each
division from the [Australian Bureau of
Statistics](https://www.abs.gov.au/), and the party that won it from the
[Australian Electoral Commission](https://www.aec.gov.au/), licensed
under CC BY 4.0. See [`README.Rmd`](README.Rmd) for the code.

## Related

- [geogrid](https://github.com/jbaileyh/geogrid) allocates regions to a
  regular grid of hexagons or squares.
- [cartogram](https://github.com/sjewo/cartogram) distorts regions so
  their areas are proportional to a variable.
