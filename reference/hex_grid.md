# Create a hexagon grid that tiles a geometry

This function will tile a geometry using a specified number of hexagons.

## Usage

``` r
hex_grid(object, n_tiles = 100)
```

## Arguments

- object:

  An sf object or geometry to tile. It is recommended to first simplify
  this geometry to speed up the hexagon tiling.

- n_tiles:

  The number of tiles to use for representing the geometry.

## Value

A vector of hexagon geometries that tile the input geometry.

## Examples

``` r
library(ozmaps)
grid <- hex_grid(abs_ste, n_tiles = 75)
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries

library(ggplot2)
ggplot() +
  geom_sf(data = abs_ste) +
  geom_sf(data = grid, fill = "steelblue", alpha = 0.2)

```
