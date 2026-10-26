## Code to prepare the `aec2025` dataset
library(sf)
library(dplyr)
library(rmapshaper)

# Commonwealth Electoral Divisions used at the 2025 federal election
# (ABS ASGS Edition 3, CED 2025)
tmp_dir <- tempfile()
tmp_zip <- file.path(tmp_dir, "ced.zip")
dir.create(tmp_dir)
download.file(
  "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs/edition-3-july-2021-june-2026/access-and-downloads/digital-boundary-files/CED_2025_AUST_GDA2020.zip",
  tmp_zip, mode = "wb"
)
unzip(tmp_zip, exdir = tmp_dir)

# Clip offshore territories (Christmas, Cocos and Norfolk Islands) so that the
# map extent covers the mainland and Tasmania.
mainland <- st_bbox(c(xmin = 112, ymin = -44, xmax = 154, ymax = -9),
                    crs = st_crs(7844))

elec_map_2025 <- read_sf(file.path(tmp_dir, "CED_2025_AUST_GDA2020.shp")) |>
  filter(!st_is_empty(geometry)) |>
  select(division = CED_NAME25, area = AREASQKM) |>
  st_crop(mainland) |>
  ms_simplify(keep = 0.002, keep_shapes = TRUE) |>
  st_make_valid()

winners <- read.csv(
  "https://results.aec.gov.au/31496/Website/Downloads/HouseMembersElectedDownload-31496.csv",
  skip = 1
) |>
  select(state = StateAb, division = DivisionNm, party = PartyAb)

aec2025 <- elec_map_2025 |>
  inner_join(winners, by = "division") |>
  mutate(winner = case_when(party %in% "ALP" ~ "ALP",
                            party %in% c("GRN", "GVIC") ~ "GRN",
                            party %in% c("LNP", "NP", "LP") ~ "LNP",
                            TRUE ~ "Other")) |>
  select(division, area, state, party, winner)

stopifnot(nrow(aec2025) == 150)

usethis::use_data(aec2025, overwrite = TRUE)
