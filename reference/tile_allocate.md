# Allocates tiled geometries to original map geometries

This function will allocate tiles to their original geometries. This
currently requires a 1:1 mapping of geometries to their tiles.

## Usage

``` r
tile_allocate(object, tile)
```

## Arguments

- object:

  An sf object or geometry that was used to be tiled.

- tile:

  An sf object produced with the
  [`hex_grid()`](https://numbats.github.io/hexmap/reference/hex_grid.md)
  function.

## Value

An sf dataset of hexagon geometries with their original data.

## Examples

``` r
library(ozmaps)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
australia <- abs_ste %>% filter(NAME != "Other Territories")
grid <- hex_grid(australia, n_tiles = 8)
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
grid <- tile_allocate(australia, grid)

library(ggplot2)
ggplot() +
  geom_sf(data = australia) +
  geom_sf(data = grid, aes(fill = NAME), alpha = 0.2)

```
