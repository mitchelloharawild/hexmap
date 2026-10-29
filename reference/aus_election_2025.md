# Australian federal election 2025

The boundaries of the 150 electoral divisions used at the 3 May 2025
Australian federal election, and the party that won each division. The
boundaries are simplified, and exclude the offshore territories
(Christmas, Cocos and Norfolk Islands).

## Usage

``` r
aus_election_2025
```

## Format

An sf object in longitude and latitude (GDA2020), with one row per
electoral division and columns:

- division:

  Name of the electoral division.

- state:

  State or territory abbreviation.

- party:

  Abbreviation of the winning candidate's party, such as `"ALP"`
  (Labor), `"LP"` (Liberal), `"LNP"` (Liberal National), `"NP"`
  (National), `"GRN"` (Greens) and `"IND"` (independent).

- geometry:

  Boundary of the division.

## Source

Election results from the Australian Electoral Commission
<https://results.aec.gov.au/> and boundaries from the Australian Bureau
of Statistics <https://www.abs.gov.au/> (ASGS Edition 3), licensed under
CC BY 4.0.
