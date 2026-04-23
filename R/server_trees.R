# ==============================================================================
# SERVER TREES - E4: Detección de árboles y cobertura de copas
# ==============================================================================

calcular_cobertura <- function(chm, roi, umbral_altura = 3.0) {
  if (is.null(chm) || is.null(roi)) stop("chm o roi NULL")

  mask <- chm > umbral_altura
  df_mascara <- as.data.frame(mask, xy = TRUE)
  names(df_mascara)[3] <- "valor"

  copas <- as.polygons(mask, dissolve = TRUE)
  copas <- copas[copas[[1]] == 1, ]

  if (is.null(copas) || nrow(copas) == 0) {
    return(list(
      porc_cobertura  = 0,
      area_copa_total = 0,
      n_poligonos     = 0,
      area_total      = as.numeric(expanse(roi, unit = "m")),
      copas_vect      = NULL,
      mascara_df      = df_mascara,
      umbral_altura   = umbral_altura
    ))
  }

  area_total      <- as.numeric(expanse(roi, unit = "m"))
  areas           <- expanse(copas, unit = "m")
  area_copa_total <- sum(areas, na.rm = TRUE)
  porc_cobertura  <- min(area_copa_total / area_total * 100, 100)

  list(
    porc_cobertura  = porc_cobertura,
    area_copa_total = area_copa_total,
    n_poligonos     = nrow(copas),
    area_total      = area_total,
    copas_vect      = copas,
    mascara_df      = df_mascara,
    umbral_altura   = umbral_altura
  )
}

register_trees <- function(input, output, session, rv, ag, lang) {

  observeEvent(input$btn_arboles, {
    req(rv$datos_norm, rv$chm, rv$roi)
    l <- lang()
    rv$log_arboles <- character(0)

    withProgress(message = tr("progress.trees.message", l), value = 0, {
      tryCatch({
        setProgress(0.3)
        rv$arboles <- detectar_arboles(
          rv$datos_norm,
          ws     = rv$lmf_ws_sel,
          hmin   = rv$lmf_hmin_sel,
          log_fn = function(m) ag("log_arboles", m),
          lang   = l
        )
        ag("log_arboles", tr("trees.log.detected", l, nrow(rv$arboles)))

        setProgress(0.7)
        rv$cobertura_copas <- calcular_cobertura(
          rv$chm,
          rv$roi,
          rv$canopy_cutoff_sel
        )
        ag("log_arboles", tr("trees.log.coverage", l, rv$cobertura_copas$porc_cobertura))

        setProgress(1)
        session$sendCustomMessage("tabEnable", "tab_exportar")
      }, error = function(e) {
        ag("log_arboles", paste(tr("notification.error.prefix", l), conditionMessage(e)))
      })
    })
  })

  output$log_arboles  <- renderText(paste(rv$log_arboles, collapse = "\n"))
  output$met_n_arb    <- renderText(if (is.null(rv$arboles)) "—" else as.character(nrow(rv$arboles)))
  output$met_dens_arb <- renderText({
    if (is.null(rv$arboles) || is.null(rv$area_ha) || is.na(rv$area_ha) || rv$area_ha <= 0) return("—")
    sprintf("%.1f", nrow(rv$arboles) / rv$area_ha)
  })
  output$met_alt_med <- renderText(if (is.null(rv$arboles)) "—" else sprintf("%.1f", mean(rv$arboles$Z, na.rm = TRUE)))
  output$met_alt_max <- renderText(if (is.null(rv$arboles)) "—" else sprintf("%.1f", max(rv$arboles$Z,  na.rm = TRUE)))

  output$met_canopy_pct <- renderText({
    req(rv$cobertura_copas)
    sprintf("%.1f%%", rv$cobertura_copas$porc_cobertura)
  })
  output$met_canopy_area <- renderText({
    req(rv$cobertura_copas)
    sprintf("%.1f ha", rv$cobertura_copas$area_copa_total / 10000)
  })

  output$plot_arb_chm <- renderPlotly({
    req(rv$chm, rv$arboles)
    l      <- lang()
    df_chm <- raster_to_df(rv$chm)
    co_arb <- as.data.frame(st_coordinates(rv$arboles))
    co_arb$altura <- rv$arboles$Z

    plotly_raster(df_chm, tr("plot.title.trees_on_chm", l), PAL_CHM, tr("plot.colorbar.height_short", l), lang = l) |>
      add_trace(
        data = co_arb, x = ~X, y = ~Y,
        type = "scatter", mode = "markers",
        inherit = FALSE,
        marker = list(symbol = "cross", size = 9, color = MARKER_TREE_COLOR,
                      line = list(color = MARKER_TREE_LINE, width = 1.2)),
        text          = ~sprintf(tr("plot.hover.apex", l), round(altura, 1)),
        hovertemplate = "%{text}<extra></extra>",
        showlegend    = FALSE
      )
  })

  output$plot_hist_arb <- renderPlot({
    req(rv$arboles)
    l   <- lang()
    df  <- data.frame(h = rv$arboles$Z)
    n   <- nrow(df)
    rng <- range(df$h)
    bin_w    <- pretty(rng, n = 30) |> diff() |> min()
    h_breaks <- seq(floor(rng[1] / bin_w) * bin_w,
                    ceiling(rng[2] / bin_w) * bin_w,
                    by = bin_w)

    ggplot(df, aes(x = h)) +
      geom_histogram(breaks = h_breaks, fill = GREEN, color = BG_CARD, alpha = 0.9) +
      geom_text(stat = "bin", breaks = h_breaks,
                aes(y = after_stat(count),
                    label = ifelse(after_stat(count) > 0,
                                   paste0(round(after_stat(count) / n * 100, 1), "%"),
                                   "")),
                vjust = -0.4, size = 4, color = TEXT_PRIMARY) +
      scale_x_continuous(breaks = h_breaks) +
      scale_y_continuous(
        name     = tr("plot.axis.frequency",     l),
        sec.axis = sec_axis(~ . / n * 100, name = tr("plot.axis.rel_frequency", l))
      ) +
      labs(title = tr("plot.title.tree_heights", l),
           x     = tr("plot.axis.apex_height",   l)) +
      theme_minimal() +
      theme(plot.background  = element_rect(fill = BG_CARD, colour = NA),
            panel.background = element_rect(fill = BG_CARD, colour = NA),
            panel.grid       = element_line(color = BORDER_COLOR),
            text             = element_text(color = TEXT_PRIMARY),
            axis.text        = element_text(color = TEXT_SECONDARY),
            axis.text.x      = element_text(angle = 90, vjust = 0.5, size = 9),
            plot.title       = element_text(color = ACCENT_PRIMARY, face = "bold"))
  })

  output$plot_canopy_coverage <- renderPlotly({
    req(rv$cobertura_copas)
    l      <- lang()
    df_cov <- rv$cobertura_copas$mascara_df
    names(df_cov) <- tolower(names(df_cov))
    col_val <- setdiff(names(df_cov), c("x", "y"))[1]
    df_cov$valor <- df_cov[[col_val]]
    df_cov$valor <- ifelse(df_cov$valor > 0, 1, 0)

    plotly_raster(
      df_cov,
      tr("plot.title.canopy_coverage", l, rv$cobertura_copas$porc_cobertura),
      PAL_TERRENO_VEGETACION,
      tr("plot.colorbar.coverage", l),
      lang = l
    ) |>
      layout(
        coloraxis = list(
          colorbar = list(
            tickmode = "array",
            tickvals = c(0, 1),
            ticktext = c(tr("plot.canopy.no_canopy", l), tr("plot.canopy.canopy", l))
          )
        ),
        showlegend = FALSE
      )
  })
}
