# Layouts of hexagons

These functions create a layout for the `layout` argument of
[`hexmap()`](https://numbats.github.io/hexmap/reference/hexmap.md),
which arranges the hexagons, and the hexagons of each group (e.g.
state). Groups usually need a very different area of hexagons than they
cover on the map (e.g. densely populated states have many small
regions), so the map has to be distorted, and each layout distorts it
differently. See
[`vignette("layouts")`](https://numbats.github.io/hexmap/articles/layouts.md)
for how they compare.

## Usage

``` r
layout_diffusion(cartogram = 0.75, resolution = 256, proximity = 0.1)

layout_compact(anchor = NULL)

layout_slice(tree = NULL, anchor = NULL)

layout_hierarchy(shape = 0.4, resolution = 256, iterations = 30)

layout_repair(layout, iterations = 30, border = 0)
```

## Arguments

- cartogram:

  For `layout_diffusion()`, how far to warp the map, from 0 (keep the
  outline of the map, squeezing groups into it) to 1 (make each group's
  area proportional to its number of regions, distorting the outline).
  Each group's area is warped to
  `area^(1 - cartogram) * size^cartogram`, where `size` is its area in
  proportion to its number of regions.

- resolution:

  For `layout_hierarchy()` and `layout_diffusion()`, the width of the
  grid used to find the diffusion cartogram, in cells (at least 16).
  Larger grids are more accurate for small groups but slower.

- proximity:

  For `layout_diffusion()`, how much to keep each group's hexagons near
  its regions, from 0 upwards. Unless `cartogram = 1`, groups don't
  cover the area their regions need, so hexagons pass from group to
  group. Each hexagon is given to groups by its squared distance to the
  group's warped shape plus `proximity` times its squared distance to
  the group's nearest region, so that a group gives up the hexagons far
  from its regions (e.g. outback rather than near its cities). With 0,
  only the shapes are used.

- anchor:

  For `layout_compact()` and `layout_slice()`, a data frame with columns
  `group`, `x` and `y` giving positions (in the coordinates of the map,
  after any transformation by the `crs` argument of
  [`hexmap()`](https://numbats.github.io/hexmap/reference/hexmap.md)) to
  use in place of the centroids of some groups when arranging them. For
  example, moving a group's anchor east of its neighbour places its
  block on the east side of that neighbour.

- tree:

  For `layout_slice()`, the order in which groups are cut apart, as a
  nested list of group names. Each list is cut into its elements, along
  whichever axis best separates them, and each character vector of
  several groups is cut automatically. For example,
  `list("WA", list(list("NT", "SA"), list("QLD", "NSW", "VIC")))` first
  separates WA from the other states, then NT and SA from the eastern
  states. Every group must appear exactly once.

- shape:

  For `layout_hierarchy()`, how far to restore each group's own shape
  after warping the map into a cartogram, from 0 (the cartogram) to 1 (a
  scaled copy of each group's shape). Larger values keep groups more
  recognisable, but values above about 0.5 can fold the map.

- iterations:

  For `layout_hierarchy()` and `layout_repair()`, the number of swaps of
  hexagons between groups to try, per region, when adjusting the borders
  between groups (from 0 upwards). More iterations are slower, but more
  likely to make every group touch as on the map.

- layout:

  For `layout_repair()`, the layout to repair, such as
  `layout_compact()`.

- border:

  For `layout_repair()`, how much to keep each region's distance to
  borders (the coast and each other group) when placing it, relative to
  keeping its position, from 0 upwards. Distances are measured from each
  region's polygon, in hexagon widths, and only up to 3 widths, so only
  regions near a border are held to it. With `border = 1`, a region one
  hexagon further from a border than on the map is as bad as a region
  one hexagon from its position. Larger values keep borders at the
  expense of the order of regions within groups. With 0, only regions of
  groups whose hexagons changed are placed again.

## Value

A layout function, for the `layout` argument of
[`hexmap()`](https://numbats.github.io/hexmap/reference/hexmap.md).

## Details

Without groups, all regions form a single group, which every layout
keeps close to the outline of the map, except `layout_compact()`, which
draws it as one compact block, and `layout_slice()`, which draws each
landmass as a rectangle of hexagons. `layout_diffusion()` then places
each region on the hexagon that minimises the total squared displacement
of all regions, so no two regions swap their relative position.

- `layout_hierarchy()` (the default) lays out the groups first, then the
  regions within each group. The groups are warped by a diffusion
  cartogram, blended towards a scaled copy of each group's own shape (by
  `shape`), so that groups keep their shapes while still touching their
  neighbours, at the expense of the outline of the whole map. The
  hexagons are split between groups nearest to these shapes, keeping
  groups with little coast (such as the ACT) off the edge of the
  hexagons, and the borders between groups are adjusted (by simulated
  annealing) so that groups touch as they do on the map. Regions are
  then placed within each group by matching the spread of the group on
  the map to the spread of its hexagons, so each group's regions keep
  their positions within it.

- `layout_diffusion()` warps the map with a diffusion cartogram (Gastner
  & Newman, 2004), which grows or shrinks each group while keeping its
  shape and its borders with other groups. The hexagons tile the warped
  map, and are split between groups along the warped borders.
  `cartogram` sets how far the map is warped: with `cartogram = 0` the
  outline of the map is kept and groups are squeezed into it, while with
  `cartogram = 1` the area of each group is proportional to its number
  of regions and the outline is distorted to fit.

- `layout_compact()` draws each group as a compact block of hexagons, in
  the style of hand drawn election maps. Each group is treated as a
  circle with the area of its hexagons, and the circles are arranged so
  that neighbouring groups keep their direction from each other
  (weighting longer shared borders more), then packed together without
  overlapping.

- `layout_slice()` lays out groups as a spatial treemap (Wood & Dykes,
  2008), so that the borders between groups are long straight lines.
  Each landmass is drawn as a rectangle of hexagons, which is
  recursively cut in two between rows or columns of hexagons, giving
  each side exactly the hexagons of its groups. Each cut is chosen so
  that neighbouring groups keep their relative position (e.g. one above
  the other for a horizontal cut) and pieces don't become thin.

`layout_repair()` wraps any layout to then adjust the borders between
groups, so that groups touch where they share a border on the map, and
don't touch where they don't. Pairs of hexagons are swapped across group
borders (by simulated annealing), which keeps the hexagons of the
layout, and keeps each group's hexagons contiguous and compact.
Repairing a slice layout bends its straight borders. With `border`, the
regions are then placed on their group's hexagons keeping their
distances to borders as well as their positions: regions near the coast
or another group stay near it, and regions away from borders stay away
from them.

## Arguments shared by layouts

An argument has the same name, meaning and default in every layout that
takes it:

- `resolution` (`layout_hierarchy()`, `layout_diffusion()`) is the size
  of the grid used to find the diffusion cartogram.

- `iterations` (`layout_hierarchy()`, `layout_repair()`) is the number
  of hexagon swaps tried per region when adjusting the borders between
  groups.

- `anchor` (`layout_compact()`, `layout_slice()`) moves where groups are
  placed relative to each other.

Arguments that set how far the map is distorted are named after what
they change, and range from 0 to 1: `cartogram` is how far the whole map
is warped towards a cartogram, and `shape` is how far each group is then
restored to its own shape. Arguments that weigh one thing to keep
against the position of regions are named after what they keep, range
from 0 (ignore it) upwards, and keep it more as they increase:
`proximity` (keep groups' hexagons near their regions) and `border`
(keep regions' distances to borders).

## Custom layouts

A layout is a function with arguments `geometry` (the regions'
geometry), `group` (the group of each region, all equal without groups),
`land` and `cellsize` (the width of the hexagons to use), which returns
the hexagon of each region as an `sfc` of polygons. `land` is a list
with the separately tiled landmasses as `geometry`, and the landmass of
each region as `region`. Each group's hexagons should be contiguous on
each landmass.

## References

Gastner, M. T., & Newman, M. E. J. (2004). Diffusion-based method for
producing density-equalizing maps. *Proceedings of the National Academy
of Sciences*, 101(20), 7499–7504.

Wood, J., & Dykes, J. (2008). Spatially ordered treemaps. *IEEE
Transactions on Visualization and Computer Graphics*, 14(6), 1348–1355.

## Examples

``` r
library(sf)
# The 2025 Australian federal election, in Australian Albers
election <- st_transform(aus_election_2025, 3577)

# Compact blocks, with the ACT on the east side of NSW
nsw <- st_coordinates(st_centroid(st_union(election[election$state == "NSW", ])))
anchor <- data.frame(group = "ACT", x = nsw[1] + 5e5, y = nsw[2])
election_hex <- hexmap(election, group = state,
                       layout = layout_compact(anchor = anchor))

# \donttest{
# The other layouts take a few seconds each
# Each state keeps its shape, at the expense of the outline of Australia
election_hex <- hexmap(election, group = state,
                       layout = layout_hierarchy())
# Keep more of the outline of Australia
election_hex <- hexmap(election, group = state,
                       layout = layout_diffusion(cartogram = 0.5))
# Compact blocks that touch as the states do
election_hex <- hexmap(election, group = state,
                       layout = layout_repair(layout_compact()))
# }
```
