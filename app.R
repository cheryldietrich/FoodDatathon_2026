library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(arrow)
library(geosphere)
library(here)

here::i_am("app.R")

# -----------------------------------------------------------------------------
# Data loaded once at app startup (not reactive -- static inputs to the app).
# -----------------------------------------------------------------------------
flows_all <- arrow::read_parquet(here("Data", "processed", "TM_flows_tiered.parquet")) |>
  filter(meaningful_partner) |>
  # A handful of rows report a country trading with itself (data artifact,
  # not a real cross-border flow) -- these have zero great-circle distance
  # and draw as a degenerate point, not an arc, so drop them here.
  filter(Reporter.Countries != Partner.Countries)

geo_lookup <- readRDS(here("Data", "processed", "geo_lookup.rds"))

STAPLES <- sort(unique(flows_all$Item))
YEAR_RANGE <- range(flows_all$Year)

# Tiers are QUARTILE bins of quantity_milled_equiv (tier_qty) / value
# (tier_val), computed separately within each Item x Year in
# Scripts/4_Outliers_Materiality.R -- i.e. "Tier 1" means "top 25% of this
# year's meaningful wheat flows by tonnage", not a fixed absolute threshold.
# The actual tonnage/dollar range a tier covers therefore varies by
# year/staple, so the legend below recomputes real breakpoints from the
# currently-displayed data rather than hardcoding numbers.
TIER_LEVELS <- c("Tier 1 (largest)", "Tier 2", "Tier 3", "Tier 4 (smallest)")
TIER_PAL <- leaflet::colorFactor("YlOrRd", domain = TIER_LEVELS)
# Tier 1 (largest) should draw thickest; TIER_LEVELS[1] -> weight 5, [4] -> weight 2.
TIER_WEIGHT <- setNames(c(5, 4, 3, 2), TIER_LEVELS)

fmt_num <- function(x, digits = 0) format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x) ifelse(is.finite(x), paste0(fmt_num(x * 100, 1), "%"), "n/a")

# Pure helpers (no reactive context) shared between the initial renderLeaflet()
# paint and the later leafletProxy() updates, so the two never drift apart.
add_flow_layer <- function(map, arcs) {
  if (is.null(arcs) || nrow(arcs) == 0) return(map)
  tooltip <- sprintf(
    "<b>%s \u2192 %s</b><br>Quantity: %s t<br>Value: $%s M<br>%s of %s's domestic supply",
    arcs$Reporter.Countries, arcs$Partner.Countries,
    fmt_num(arcs$quantity), fmt_num(arcs$value / 1000, 1),
    fmt_pct(arcs$materiality_ratio_qty), arcs$Reporter.Countries
  )
  map |> addPolylines(
    data = arcs, group = "flows",
    color = ~TIER_PAL(tier), weight = ~unname(TIER_WEIGHT[as.character(tier)]),
    opacity = 0.7, label = lapply(tooltip, htmltools::HTML)
  )
}

add_legend_layer <- function(map, lc, staple, year) {
  map |> addLegend(
    position = "bottomright", colors = lc$colors, labels = lc$labels,
    title = paste0("Tier (", staple, ", ", year, ")")
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Historical Staple Food Flows",
  sidebar = sidebar(
    sliderInput(
      "year", "Year",
      min = YEAR_RANGE[1], max = YEAR_RANGE[2],
      value = YEAR_RANGE[2], step = 1, sep = "", animate = TRUE
    ),
    radioButtons("staple", "Staple", choices = STAPLES, selected = STAPLES[1]),
    radioButtons(
      "metric", "Tier by",
      choices = c("Quantity" = "tier_qty", "Value" = "tier_val"),
      selected = "tier_qty"
    )
  ),
  card(
    full_screen = TRUE,
    leafletOutput("map", height = "100%")
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- geo_active: which polygon set is valid for input$year -----------------
  geo_active <- reactive({
    geo_lookup |>
      filter(
        is.na(year_start) |
          (input$year >= year_start & input$year <= year_end)
      )
  })

  # Change-detection gate: only true when the *set* of active areas differs
  # from the previous tick (i.e. we crossed one of the 5 known boundary years:
  # 1991/92, 1992/93, 1999/2000, 2005/06, 2011/12). Ordinary year-to-year
  # slides where the entity set is unchanged should NOT trigger a polygon
  # redraw -- only the flow-line layer updates on those ticks.
  prev_areas <- reactiveVal(character(0))
  geo_changed <- reactiveVal(TRUE) # force an initial draw

  observeEvent(geo_active(), {
    current_areas <- sort(geo_active()$faostat_area)
    if (!identical(current_areas, prev_areas())) {
      prev_areas(current_areas)
      geo_changed(TRUE)
    }
  })

  # ---- flows_filtered: cheap per-tick filter (every year, always) -----------
  flows_filtered <- reactive({
    flows_all |>
      filter(Year == input$year, Item == input$staple)
  })

  # ---- arc_endpoints: join flows to active centroids, build great-circle ---
  # lines with geosphere::gcIntermediate (breakAtDateLine handles the same
  # antimeridian issue we fixed for the USSR polygon, but for line geometry).
  # Attributes needed for the hover tooltip are carried through per feature.
  
arc_endpoints <- reactive({
  geo_pts <- geo_active() |>
    st_drop_geometry() |>
    select(faostat_area, centroid_lon, centroid_lat)

  flows_in <- flows_filtered()

  df <- flows_in |>
    inner_join(geo_pts, by = c("Reporter.Countries" = "faostat_area")) |>
    rename(lon_reporter = centroid_lon, lat_reporter = centroid_lat) |>
    inner_join(geo_pts, by = c("Partner.Countries" = "faostat_area")) |>
    rename(lon_partner = centroid_lon, lat_partner = centroid_lat)

  dropped <- nrow(flows_in) - nrow(df)
  if (dropped > 0) {
    message(sprintf(
      "arc_endpoints: dropped %d/%d flow rows for Year=%s Item=%s",
      dropped, nrow(flows_in), input$year, input$staple
    ))
  }

  if (nrow(df) == 0) return(NULL)

  tier_col <- input$metric

  lines <- purrr::map(seq_len(nrow(df)), function(i) {
    gc <- gcIntermediate(
      c(df$lon_reporter[i], df$lat_reporter[i]),
      c(df$lon_partner[i], df$lat_partner[i]),
      n = 50, addStartEnd = TRUE, sp = TRUE, breakAtDateLine = TRUE
    )

    st_sf(
      Reporter.Countries = df$Reporter.Countries[i],
      Partner.Countries = df$Partner.Countries[i],
      quantity = df$quantity_milled_equiv[i],
      value = df$value[i],
      materiality_ratio_qty = df$materiality_ratio_qty[i],
      tier = df[[tier_col]][i],
      geometry = st_geometry(st_as_sf(gc))
    )
  })

  bind_rows(lines) |>
    st_set_crs(4326) |>
    mutate(tier = factor(tier, levels = TIER_LEVELS))
})

  # ---- Legend breakpoints: real tonnage/$ range per tier, recomputed from --
  # the currently-displayed flows (tier definitions are relative/quartile,
  # not fixed absolute cutoffs -- see comment above TIER_LEVELS).
  legend_content <- reactive({
    df <- flows_filtered()
    tier_col <- input$metric
    value_col <- if (tier_col == "tier_qty") "quantity_milled_equiv" else "value"
    unit_fmt <- if (tier_col == "tier_qty") {
      function(x) paste0(fmt_num(x), " t")
    } else {
      function(x) paste0("$", fmt_num(x / 1000, 1), "M")
    }

    ranges <- df |>
      group_by(tier = .data[[tier_col]]) |>
      summarise(lo = min(.data[[value_col]]), hi = max(.data[[value_col]]), .groups = "drop")

    labels <- vapply(TIER_LEVELS, function(lv) {
      row <- ranges |> filter(tier == lv)
      if (nrow(row) == 0) return(paste0(lv, ": (none)"))
      paste0(lv, ": ", unit_fmt(row$lo), " \u2013 ", unit_fmt(row$hi))
    }, character(1))

    list(labels = unname(labels), colors = TIER_PAL(TIER_LEVELS))
  })

  # ---- Initial map draw (base tiles only; layers added via observers) ------
  # Esri's WorldGrayCanvas requires no API key/account, unlike CartoDB's
  # newer hosted basemap tiles which now gate anonymous use behind a key.
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldPhysical) |>
      setView(lng = 15, lat = 30, zoom = 2)
  })

  # ---- Polygon layer: redraw ONLY when the active entity set changes -------
  observeEvent(geo_changed(), {
    req(geo_changed())
    leafletProxy("map") |>
      clearGroup("polygons") |>
      addPolygons(
        data = geo_active(), group = "polygons",
        weight = 2, color = "#888888", fillOpacity = 0.05
      )
    geo_changed(FALSE)
  })



  # ---- Flow-line layer + legend: redraw on EVERY year/staple/metric tick ---
  observe({
    arcs <- arc_endpoints()
    lc <- legend_content()

    proxy <- leafletProxy("map") |>
      clearGroup("flows") |>
      clearControls()

    if (!is.null(arcs) && nrow(arcs) > 0) {
      # Absolute numbers in the tooltip (tonnes, $ millions), plus the one
      # genuinely relative figure we have on hand: this flow's share of the
      # importing country's domestic supply (materiality_ratio_qty).
      tooltip <- sprintf(
        "<b>%s \u2192 %s</b><br>Quantity: %s t<br>Value: $%s M<br>%s of %s's domestic supply",
        arcs$Reporter.Countries, arcs$Partner.Countries,
        fmt_num(arcs$quantity), fmt_num(arcs$value / 1000, 1),
        fmt_pct(arcs$materiality_ratio_qty), arcs$Reporter.Countries
      )

      proxy <- proxy |>
        addPolylines(
          data = arcs, group = "flows",
          color = ~TIER_PAL(tier), weight = ~unname(TIER_WEIGHT[as.character(tier)]),
          opacity = 0.7, label = lapply(tooltip, htmltools::HTML)
        )
    }

    proxy |> addLegend(
      position = "bottomright", colors = lc$colors, labels = lc$labels,
      title = paste0("Tier (", input$staple, ", ", input$year, ")")
    )
  })
}

shinyApp(ui, server)
