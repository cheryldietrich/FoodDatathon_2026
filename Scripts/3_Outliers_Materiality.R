library(dplyr)
library(tidyr)
library(arrow)
library(here)
library(stringr)

here::i_am("Scripts/3_Outliers_Materiality.R")

# -----------------------------------------------------------------------------
# Load staples data
# -----------------------------------------------------------------------------
TM_staples      <- read_parquet(here("Data", "processed", "TM_staples.parquet"))
FBS_staples     <- read_parquet(here("Data", "processed", "FBS_staples.parquet"))
FBShist_staples <- read_parquet(here("Data", "processed", "FBShist_staples.parquet"))
QCL_staples     <- read_parquet(here("Data", "processed", "QCL_staples.parquet"))

# -----------------------------------------------------------------------------
# Item-name crosswalk
#
# TM/QCL item names ("Wheat", "Rice", "Maize (corn)") differ from FBS item
# names ("Wheat and products", "Rice and products"/"Rice (Milled Equivalent)",
# "Maize and products"). FBS also changed rice's item code between the
# historic (2805, "Rice (Milled Equivalent)") and current (2807, "Rice and
# products") releases -- see Scripts/2_Process_FBS.R for the fix.
# -----------------------------------------------------------------------------
item_xwalk <- tibble::tribble(
  ~fbs_item,                   ~canonical_item,
  "Wheat and products",        "Wheat",
  "Rice and products",         "Rice",
  "Rice (Milled Equivalent)",  "Rice",
  "Maize and products",        "Maize (corn)"
)

# RICE BASIS MISMATCH:
# TM/QCL "Rice" (Item.Code 27, CPC 0113) is paddy/unhusked rice.
# FBS "Rice (Milled Equivalent)"/"Rice and products" is milled-rice-equivalent.
# Confirmed via UN CPC classification (0113 -> "Rice paddy, other (not
# husked)"). Applying FAO's standard paddy->milled conversion factor (~0.67)
# to TM rice quantities before comparing against FBS domestic supply. Wheat
# (CPC 0111) and maize (CPC 0112) have no equivalent husking/milling
# sub-classification, so no conversion is applied to them.
RICE_PADDY_TO_MILLED <- 0.67

# -----------------------------------------------------------------------------
# Countries with zero/no recorded QCL production for an item (e.g. Singapore
# for all three staples -- a city-state with essentially no arable land).
# For these, FAOSTAT's FBS domain often has no domestic-supply figure at all
# (production-less countries frequently aren't compiled into FBS), even
# though they have real recorded trade. Rather than leave the materiality
# ratio undefined for them, we substitute a trade-derived proxy below:
#   dom_supply_proxy ~= total imports - total exports (that item/year)
# This is only applied where cumulative QCL production is zero/NA -- i.e.
# genuinely production-less economies. Countries with real (even if unlinked)
# production, like Burundi, are NOT covered by this proxy and remain
# unmatched (dom_supply_tonnes = NA), since we have no basis for assuming
# their true domestic supply.
# -----------------------------------------------------------------------------
# NA (no QCL row at all for that Area x Item) counts the same as zero --
# both mean "never recorded producing this crop".
production_totals <- QCL_staples |>
  filter(Element == "Production") |>
  group_by(Area, Item) |>
  summarise(total_production = sum(Value, na.rm = TRUE), .groups = "drop")

zero_production_areas <- TM_staples |>
  distinct(Area = Reporter.Countries, Item) |>
  left_join(production_totals, by = c("Area", "Item")) |>
  filter(is.na(total_production) | total_production == 0) |>
  select(Area, Item)

PROXY_EPSILON_TONNES <- 1  # floor to avoid zero/negative denominators

# -----------------------------------------------------------------------------
# Domestic supply table (denominator), tonnes, one row per Area x item x year
#
# FBS_staples covers 2010+, FBShist_staples covers up to 2013 -- current
# release supersedes historic for the overlap years (2010-2013), so historic
# is truncated to Year < 2010 rather than deduplicated post hoc.
# FBS quantity elements are in "1000 t"; TM quantities are in "t" -- multiply
# by 1000 to align units.
# -----------------------------------------------------------------------------
dom_supply <- bind_rows(
  FBS_staples     |> filter(Year >= 2010, Element == "Domestic supply quantity"),
  FBShist_staples |> filter(Year <  2010, Element == "Domestic supply quantity")
) |>
  inner_join(item_xwalk, by = c("Item" = "fbs_item")) |>
  transmute(Area, canonical_item, Year, dom_supply_tonnes = Value * 1000)

# -----------------------------------------------------------------------------
# Trade-derived domestic-supply proxy for zero-production countries
# (dom_supply_proxy ~= total imports - total exports, milled-equivalent for
# rice), used as a fallback only where `zero_production_areas` applies and
# no FBS figure exists.
# -----------------------------------------------------------------------------
trade_dom_supply_proxy <- TM_staples |>
  filter(Element %in% c("Import quantity", "Export quantity")) |>
  semi_join(zero_production_areas, by = c("Reporter.Countries" = "Area", "Item" = "Item")) |>
  mutate(Value_milled = if_else(Item == "Rice", Value * RICE_PADDY_TO_MILLED, Value)) |>
  group_by(Area = Reporter.Countries, Item, Year, Element) |>
  summarise(total = sum(Value_milled, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Element, values_from = total, values_fill = 0) |>
  transmute(
    Area, canonical_item = Item, Year,
    dom_supply_proxy_tonnes = pmax(`Import quantity` - `Export quantity`, PROXY_EPSILON_TONNES)
  )

cat("\nTrade-derived domestic-supply proxy built for", n_distinct(trade_dom_supply_proxy$Area),
    "zero-production area(s):", paste(unique(trade_dom_supply_proxy$Area), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# Item-year median implied unit price ($/tonne), for the approximate
# value-based materiality denominator (FBS has no domestic-supply *value*
# element, so this proxies one from TM's own reported prices).
# -----------------------------------------------------------------------------
unit_price_by_item_year <- TM_staples |>
  filter(Element %in% c("Export quantity", "Export value", "Import quantity", "Import value")) |>
  mutate(flow_type = if_else(str_detect(Element, "Export"), "Export", "Import"),
         measure   = if_else(str_detect(Element, "quantity"), "quantity", "value")) |>
  select(Reporter.Countries, Partner.Countries, Item, Year, flow_type, measure, Value) |>
  pivot_wider(names_from = measure, values_from = Value) |>
  filter(quantity > 0, value > 0) |>
  mutate(usd_per_tonne = (value * 1000) / quantity) |>
  group_by(Item, Year) |>
  summarise(median_usd_per_tonne = median(usd_per_tonne, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# Outlier flags, computed per direction (Export / Import) at the
# Reporter x Partner x Item x Year grain.
#   flag_zero_mismatch : qty>0 & value==0 (or vice versa) above a fixed
#                        materiality cutoff (>100 t / >$1,000)
#   flag_price_outlier : implied $/tonne outside [1st, 99th] pctile for
#                        that Item x Year (both qty & value > 0)
# -----------------------------------------------------------------------------
make_flow_table <- function(tm, direction) {
  qty_el <- paste(direction, "quantity")
  val_el <- paste(direction, "value")

  wide <- tm |>
    filter(Element %in% c(qty_el, val_el)) |>
    select(Reporter.Countries, Partner.Countries, Item, Year, Element, Value) |>
    pivot_wider(names_from = Element, values_from = Value) |>
    rename(quantity = !!qty_el, value = !!val_el)

  price_bounds <- wide |>
    filter(quantity > 0, value > 0) |>
    mutate(usd_per_tonne = (value * 1000) / quantity) |>
    group_by(Item, Year) |>
    summarise(
      p01 = quantile(usd_per_tonne, 0.01, na.rm = TRUE),
      p99 = quantile(usd_per_tonne, 0.99, na.rm = TRUE),
      .groups = "drop"
    )

  wide |>
    left_join(price_bounds, by = c("Item", "Year")) |>
    mutate(
      direction = direction,
      usd_per_tonne = if_else(quantity > 0 & value > 0, (value * 1000) / quantity, NA_real_),
      flag_zero_mismatch = (quantity > 100  & value == 0) |
                           (value    > 1    & quantity == 0),  # value col is in 1000 USD -> >1 means >$1,000
      flag_price_outlier = !is.na(usd_per_tonne) &
                           (usd_per_tonne < p01 | usd_per_tonne > p99)
    ) |>
    select(-p01, -p99)
}

export_flows <- make_flow_table(TM_staples, "Export")
import_flows <- make_flow_table(TM_staples, "Import")

cat("Export flows:", nrow(export_flows),
    " | zero-mismatch:", sum(export_flows$flag_zero_mismatch, na.rm = TRUE),
    " | price outliers:", sum(export_flows$flag_price_outlier, na.rm = TRUE), "\n")
cat("Import flows:", nrow(import_flows),
    " | zero-mismatch:", sum(import_flows$flag_zero_mismatch, na.rm = TRUE),
    " | price outliers:", sum(import_flows$flag_price_outlier, na.rm = TRUE), "\n")

# -----------------------------------------------------------------------------
# Materiality filter (importer's perspective, first pass):
#   materiality_ratio_qty = Import quantity (A -> B) / B's domestic supply qty
#   meaningful_partner    = materiality_ratio_qty >= 1%
#
# Rice's import quantity is converted paddy -> milled equivalent before
# ratio-ing against FBS's milled-equivalent domestic supply.
# Export-side normalization (exporter's or importer's supply as denominator)
# is deferred to a later pass.
# -----------------------------------------------------------------------------
import_flows <- import_flows |>
  mutate(
    quantity_milled_equiv = if_else(Item == "Rice", quantity * RICE_PADDY_TO_MILLED, quantity)
  ) |>
  left_join(
    dom_supply,
    by = c("Reporter.Countries" = "Area", "Item" = "canonical_item", "Year" = "Year")
  ) |>
  left_join(
    trade_dom_supply_proxy,
    by = c("Reporter.Countries" = "Area", "Item" = "canonical_item", "Year" = "Year")
  ) |>
  mutate(
    dom_supply_source = case_when(
      !is.na(dom_supply_tonnes)       ~ "FBS",
      !is.na(dom_supply_proxy_tonnes) ~ "trade_proxy_zero_production",
      TRUE                             ~ NA_character_
    ),
    dom_supply_tonnes = coalesce(dom_supply_tonnes, dom_supply_proxy_tonnes)
  ) |>
  select(-dom_supply_proxy_tonnes) |>
  left_join(unit_price_by_item_year, by = c("Item" = "Item", "Year" = "Year")) |>
  mutate(
    dom_supply_value_approx_usd = dom_supply_tonnes * median_usd_per_tonne,
    materiality_ratio_qty       = quantity_milled_equiv / dom_supply_tonnes,
    materiality_ratio_val_approx = (value * 1000) / dom_supply_value_approx_usd,
    meaningful_partner = materiality_ratio_qty >= 0.01
  )

cat("\nImport flows with a domestic-supply denominator:",
    sum(!is.na(import_flows$dom_supply_tonnes)), "of", nrow(import_flows), "\n")
print(import_flows |> count(dom_supply_source))
cat("Import flows flagged as meaningful_partner (>=1% of importer's domestic supply):",
    sum(import_flows$meaningful_partner, na.rm = TRUE),
    " (", round(100 * mean(import_flows$meaningful_partner, na.rm = TRUE), 1), "% of matched rows)\n", sep = "")

# -----------------------------------------------------------------------------
# Tiering (style only): quartile bins on quantity & value, computed only
# among flows retained by the materiality filter, kept separate per Item so
# tiers reflect within-commodity scale.
# -----------------------------------------------------------------------------
tier_labels <- c("Tier 4 (smallest)", "Tier 3", "Tier 2", "Tier 1 (largest)")

add_tiers <- function(df, value_col, tier_col) {
  df |>
    group_by(Item) |>
    mutate(
      !!tier_col := if (sum(meaningful_partner, na.rm = TRUE) >= 4) {
        cut(
          .data[[value_col]],
          breaks = quantile(.data[[value_col]][meaningful_partner], probs = seq(0, 1, 0.25), na.rm = TRUE),
          labels = tier_labels,
          include.lowest = TRUE
        )
      } else {
        NA_character_
      }
    ) |>
    ungroup()
}

import_flows <- import_flows |>
  add_tiers("quantity_milled_equiv", "tier_qty") |>
  add_tiers("value", "tier_val")

cat("\nTier breakdown (quantity), meaningful partners only:\n")
print(import_flows |> filter(meaningful_partner) |> count(Item, tier_qty))

# -----------------------------------------------------------------------------
# Combine export + import into one long flow table. Materiality/tier columns
# are NA for export rows (export-side normalization deferred).
# -----------------------------------------------------------------------------
TM_flows_tiered <- bind_rows(
  export_flows |>
    mutate(quantity_milled_equiv = NA_real_, dom_supply_tonnes = NA_real_,
           dom_supply_source = NA_character_,
           materiality_ratio_qty = NA_real_, materiality_ratio_val_approx = NA_real_,
           meaningful_partner = NA, tier_qty = NA_character_, tier_val = NA_character_),
  import_flows |>
    select(-median_usd_per_tonne, -dom_supply_value_approx_usd)
)

write_parquet(TM_flows_tiered, sink = here("Data", "processed", "TM_flows_tiered.parquet"))

cat("\nWrote Data/processed/TM_flows_tiered.parquet:", nrow(TM_flows_tiered), "rows,",
    ncol(TM_flows_tiered), "columns\n")
