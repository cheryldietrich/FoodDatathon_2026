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

    # Diagnostic, not user-facing validation: inner_join silently drops any
    # flow whose reporter/partner isn't in the currently active geography
    # (e.g. a name mismatch, or a defunct entity trading outside its known
    # year range). This should be ~0 given today's data (checked manually),
    # but flags future data drift -- e.g. after rerunning the FAOSTAT bulk
    # download scripts -- instead of silently dropping flows off the map.
    dropped <- nrow(flows_in) - nrow(df)
    if (dropped > 0) {
      message(sprintf(
        "arc_endpoints: dropped %d/%d flow rows for Year=%s Item=%s (reporter/partner missing from geo_active)",
        dropped, nrow(flows_in), input$year, input$staple
      ))
    }

    if (nrow(df) == 0) return(st_sf(geometry = st_sfc(), crs = 4326))

    tier_col <- input$metric
    lines <- purrr::map(seq_len(nrow(df)), function(i) {
      gc <- gcIntermediate(
        c(df$lon_reporter[i], df$lat_reporter[i]),
        c(df$lon_partner[i], df$lat_partner[i]),
        n = 50, addStartEnd = TRUE, sp = TRUE, breakAtDateLine = TRUE
      )
      st_as_sf(gc)
    })

    bind_rows(lines) |>
      st_set_crs(4326) |>
      mutate(tier = factor(df[[tier_col]]))
  })