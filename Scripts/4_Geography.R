library(dplyr)
library(sf)
library(rnaturalearth)
library(countrycode)
library(here)
library(arrow)

here::i_am("Scripts/4_Geography.R")

# -----------------------------------------------------------------------------
# All FAOSTAT area names appearing in the trade data (reporter or partner),
# used to drive the geography crosswalk.
# -----------------------------------------------------------------------------
TM_staples <- read_parquet(here("Data", "processed", "TM_staples.parquet"))

all_areas <- bind_rows(
  TM_staples |> distinct(Area = Reporter.Countries),
  TM_staples |> distinct(Area = Partner.Countries)
) |> distinct(Area) |> arrange(Area)

# -----------------------------------------------------------------------------
# Areas dropped entirely: negligible trade, no meaningful polygon
# (confirmed in earlier exploration: ~$1,300 total trade each).
# -----------------------------------------------------------------------------
DROPPED_AREAS <- c("Johnston Island", "Midway Island")

# -----------------------------------------------------------------------------
# Defunct/aggregate entities that need a manually reconstructed polygon
# (union of successor/constituent states' current rnaturalearth polygons)
# rather than a 1:1 countrycode match. Year ranges below are the actual
# min/max Year this entity appears as Reporter or Partner in TM_staples
# (not assumed from real-world dissolution dates).
#   USSR                  1986-1991 -> 15 Soviet republics
#   Yugoslav SFR          1986-1991 -> Serbia, Croatia, Slovenia, Bosnia and
#                                      Herzegovina, North Macedonia, Montenegro
#   Czechoslovakia        1986-1992 -> Czechia, Slovakia
#   Serbia and Montenegro 1992-2005 -> Serbia, Montenegro
#   Ethiopia PDR          1986-1992 -> Ethiopia, Eritrea (pre-1993 secession)
#   Sudan (former)        1986-2011 -> Sudan, South Sudan (pre-2011 secession)
#   Belgium-Luxembourg    1986-1999 -> Belgium, Luxembourg
# China, Taiwan Province of is NOT defunct -- it's a real, current place, just
# non-UN -- so it gets its actual rnaturalearth polygon, not a union.
# -----------------------------------------------------------------------------
# NOTE: successor_iso3 cells must be bare character vectors, not
# list(c(...)) -- tribble auto-promotes a ragged column to a list-column on
# its own; wrapping in list() here double-nests it (each cell becomes a
# length-1 list containing the vector, not the vector itself), which makes
# `iso3 %in% codes` silently match nothing downstream.
defunct_entities <- tibble::tribble(
  ~faostat_area,            ~year_start, ~year_end, ~successor_iso3,
  "USSR",                          1986,       1991, c("ARM","AZE","BLR","EST","GEO","KAZ","KGZ","LVA","LTU","MDA","RUS","TJK","TKM","UKR","UZB"),
  "Yugoslav SFR",                  1986,       1991, c("SRB","HRV","SVN","BIH","MKD","MNE"),
  "Czechoslovakia",                1986,       1992, c("CZE","SVK"),
  "Serbia and Montenegro",         1992,       2005, c("SRB","MNE"),
  "Ethiopia PDR",                  1986,       1992, c("ETH","ERI"),
  "Sudan (former)",                1986,       2011, c("SDN","SSD"),
  "Belgium-Luxembourg",            1986,       1999, c("BEL","LUX")
)

# -----------------------------------------------------------------------------
# Standard countries: FAOSTAT name -> ISO3 via countrycode. Any FAOSTAT area
# not in DROPPED_AREAS or defunct_entities$faostat_area that countrycode
# can't resolve needs a manual override added here.
# -----------------------------------------------------------------------------
standard_areas <- all_areas |>
  filter(!Area %in% DROPPED_AREAS, !Area %in% defunct_entities$faostat_area) |>
  mutate(iso3 = countrycode(Area, origin = "country.name", destination = "iso3c", warn = FALSE))

unmatched_standard <- standard_areas |> filter(is.na(iso3))
if (nrow(unmatched_standard) > 0) {
  cat("WARNING: unmatched standard areas needing a manual override:\n")
  print(unmatched_standard)
}

# -----------------------------------------------------------------------------
# Base polygons (present-day) from rnaturalearth.
# Use type = "map_units" (not the default "countries") -- this separates out
# overseas territories/dependencies (French Guiana, Guadeloupe, Martinique,
# Réunion, Tokelau, etc.) as their own polygons instead of folding them into
# their sovereign's shape, with no extra package needed.
# Use iso_a3_eh ("enhanced"), not iso_a3 -- Natural Earth sets iso_a3 to the
# sentinel "-99" for a handful of countries with complex overseas-territory
# relationships (confirmed here for France and Norway); iso_a3_eh has the
# real code in those cases.
# -----------------------------------------------------------------------------
world_raw <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf") |>
  select(iso3 = iso_a3_eh, name, geometry)

# A handful of iso3 codes appear on more than one map_unit row -- e.g.
# Norway's remote dependency Jan Mayen is split out as its own map unit but
# still tagged with Norway's ISO3 code. These aren't separate FAOSTAT
# reporters, so union them into a single polygon per iso3 rather than
# carrying duplicate rows forward.
dup_iso3 <- world_raw |> st_drop_geometry() |> count(iso3) |> filter(n > 1, !is.na(iso3))
if (nrow(dup_iso3) > 0) {
  cat("NOTE: unioning duplicate map_unit rows sharing one ISO3 code:\n")
  print(world_raw |> st_drop_geometry() |> semi_join(dup_iso3, by = "iso3") |> select(name, iso3))
}

world <- world_raw |>
  group_by(iso3) |>
  summarise(name = first(name), geometry = st_union(geometry), .groups = "drop")

# Split any polygon that implicitly crosses the antimeridian (only Russia,
# here) into proper multi-part pieces on either side of +/-180. Without
# this, unprojected lon/lat rendering (ggplot coord_sf, leaflet, etc.) draws
# a spurious sliver connecting Russia's westernmost and easternmost edges
# straight across the map -- and that sliver also propagates into any
# st_union() built from Russia, like the USSR reconstruction below. This is
# a no-op for polygons that don't cross the dateline.
world <- world |>
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=10"), quiet = TRUE)

# -----------------------------------------------------------------------------
# Standard-country geo table: real polygon, valid for all years (year_start/
# year_end = NA signals "no time restriction" downstream).
# -----------------------------------------------------------------------------
successor_years <- defunct_entities |>
  tidyr::unnest(successor_iso3) |>
  transmute(iso3 = successor_iso3, inherited_year_start = year_end + 1L)


geo_standard <- standard_areas |>
  filter(!is.na(iso3)) |>
  left_join(world, by = "iso3") |>
  filter(!is.na(name)) |>
  left_join(successor_years, by = "iso3") |>
  transmute(
    faostat_area = Area,
    year_start = inherited_year_start,   # NA for non-successors, inherited value for successors
    year_end = NA_integer_,
    geometry
  )

missing_polygon <- standard_areas |> filter(!is.na(iso3)) |>
  anti_join(world, by = "iso3")
if (nrow(missing_polygon) > 0) {
  cat("\nWARNING: matched an ISO3 code but rnaturalearth has no polygon for:\n")
  print(missing_polygon)
}

# -----------------------------------------------------------------------------
# Defunct-entity geo table: st_union() of successor states' polygons
# -----------------------------------------------------------------------------
geo_defunct <- defunct_entities |>
  mutate(
    geometry = purrr::map(successor_iso3, function(codes) {
      polys <- world |> filter(iso3 %in% codes)
      st_union(st_geometry(polys))
    })
  ) |>
  mutate(geometry = st_sfc(unlist(geometry, recursive = FALSE), crs = st_crs(world))) |>
  select(faostat_area, year_start, year_end, geometry) |>
  st_as_sf()

# -----------------------------------------------------------------------------
# Combine, compute centroids from whichever polygon is active (real for
# standard/Taiwan, reconstructed union for defunct), using
# st_point_on_surface() so the point always falls within the polygon (unlike
# st_centroid(), which can land outside for irregular/multi-part unions).
# -----------------------------------------------------------------------------
geo_lookup <- bind_rows(geo_standard, geo_defunct) |>
  st_as_sf() |>
  mutate(
    centroid = st_point_on_surface(geometry),
    centroid_lon = st_coordinates(centroid)[, 1],
    centroid_lat = st_coordinates(centroid)[, 2]
  ) |>
  select(-centroid)

cat("\ngeo_lookup rows:", nrow(geo_lookup), "\n")
cat("Standard countries:", nrow(geo_standard), " | Defunct entities:", nrow(geo_defunct), "\n")
cat("Dropped areas (no polygon at all):", paste(DROPPED_AREAS, collapse = ", "), "\n")

saveRDS(geo_lookup, here("Data", "processed", "geo_lookup.rds"))
cat("\nWrote Data/processed/geo_lookup.rds\n")
