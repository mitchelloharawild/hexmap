# hexmap: Convert a Map into a Hexagon Tile Map

Converts a map of regions, as an 'sf' object, into a map of equally
sized hexagons with one hexagon for each region. Regions are allocated
hexagons by minimising the total squared displacement of all regions, so
regions keep their relative positions, while distant islands are tiled
separately. Groups of regions (such as states) can be kept together
using one of several layouts, including a diffusion cartogram (Gastner
and Newman, 2004)
[doi:10.1073/pnas.0400280101](https://doi.org/10.1073/pnas.0400280101)
and spatially ordered treemaps (Wood and Dykes, 2008)
[doi:10.1109/TVCG.2008.165](https://doi.org/10.1109/TVCG.2008.165) .
This makes small but important regions as visible as large, sparsely
populated ones.

## See also

Useful links:

- <https://numbats.github.io/hexmap/>

- <https://github.com/numbats/hexmap>

- Report bugs at <https://github.com/numbats/hexmap/issues>

## Author

**Maintainer**: Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
([ORCID](https://orcid.org/0000-0001-6729-7695))

Authors:

- Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
  ([ORCID](https://orcid.org/0000-0001-6729-7695))

- Emi Tanaka <dr.emi.tanaka@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-1455-259X))
