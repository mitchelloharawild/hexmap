# Match geospatial polygons

Given two geospatial polygons `x` and `y`, assign the regions from `y`
to the regions in `x`.

## Usage

``` r
map_match(x, y)
```

## Arguments

- x, y:

  An `sf` object of the same size.

## Value

An sf object with the geometry from `x` with rows matched from `y`.
