# Map transition for animation

This function transforms the data in the long form so that the
transition from the first map to the second map can be seen in the
animation. This function is like `tidyr::pivot_longer` but works for sf
objects (error is given `tidyr::pivot_longer` when combining `sf`
objects).

## Usage

``` r
transition_map_data(x, map_from, map_to, name = "geometry")
```

## Arguments

- x:

  An sf map object with two maps.

- map_from, map_to:

  Unquoted name of the column with the map to transition from/to.

- name:

  The name of the column with the map object.
