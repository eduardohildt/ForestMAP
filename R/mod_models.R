# ==============================================================================
# SERVER MODELS - E3: Generación de modelos digitales (DEM, CHM, curvas, hillshade)
# ==============================================================================

register_models <- function(input, output, session, rv, ag, lang) {

  observeEvent(input$btn_modelos, {
    req(rv$las_clasf, rv$roi)
    l <- lang()
    rv$log_modelos <- character(0)
    withProgress(message = tr("progress.models.message", l), value = 0, {
      tryCatch({
        setProgress(0.10, detail = tr("progress.models.dem", l))
        rd <- generar_dem(rv$las_clasf,
                          res_dem  = rv$dem_res_sel,
                          win_min  = rv$dem_win_min_sel,
                          win_mean = rv$dem_win_mean_sel,
                          log_fn   = function(m) ag("log_modelos", m),
                          lang     = l)
        rv$dem      <- rd$dem
        rv$dem_suav <- rd$dem_suav
        rv$dem_min  <- rd$dem_min
        rv$dem_max  <- rd$dem_max
        rv$dem_mean <- rd$dem_mean

        setProgress(0.35, detail = tr("progress.models.contours", l))
        rv$curvas <- generar_curvas_nivel(rv$dem_suav, rv$roi,
                       equidistancia = rv$cn_equidist_sel,
                       log_fn        = function(m) ag("log_modelos", m),
                       lang          = l)

        setProgress(0.50, detail = tr("progress.models.hillshade", l))
        rv$hillshade <- generar_hillshade(rv$dem_suav,
                         angulo_sol = 45, azimut = 315,
                         log_fn     = function(m) ag("log_modelos", m),
                         lang       = l)

        setProgress(0.70, detail = tr("progress.models.chm", l))
        rn <- normalizar_y_chm(rv$las_clasf, rv$dem_suav, rv$roi,
                res_chm   = rv$chm_res_sel,
                subcircle = rv$chm_subcirc_sel,
                log_fn    = function(m) ag("log_modelos", m),
                lang      = l)
        rv$datos_norm <- rn$datos_norm
        rv$chm        <- rn$chm
        rv$chm_min    <- rn$chm_min
        rv$chm_max    <- rn$chm_max
        rv$chm_mean   <- rn$chm_mean

        setProgress(0.90, detail = tr("progress.models.cropping", l))
        ag("log_modelos", tr("models.log.cropping", l))
        rv$dem_suav  <- terra::crop(terra::mask(rv$dem_suav,  rv$roi), rv$roi)
        rv$hillshade <- terra::crop(terra::mask(rv$hillshade, rv$roi), rv$roi)
        rv$chm       <- terra::crop(terra::mask(rv$chm,       rv$roi), rv$roi)
        ag("log_modelos", tr("models.log.crop_done", l))

        setProgress(1)
        session$sendCustomMessage("tabEnable", "tab_arboles")
        showNotification(tr("notification.success.models_done", l), type = "message")
      }, error = function(e) {
        ag("log_modelos", paste(tr("notification.error.prefix", l), conditionMessage(e)))
        showNotification(tr("notification.error.generic", l, conditionMessage(e)), type = "error")
      })
    })
  })

  output$log_modelos <- renderText(paste(rv$log_modelos, collapse = "\n"))

  # Mapas sobre fondo cartográfico; respaldo: heatmap plotly si el raster no tiene CRS.
  registrar_raster <- function(id, valor, titulo_key, pal_mapa, pal_plotly, leyenda_key,
                               opacity = 0.85) {
    registrar_mapa(output, id, height = "640px",
      mapa = function() {
        r <- req(valor())
        l <- lang()
        mapa_raster(r, pal_mapa, tr(titulo_key, l), tr(leyenda_key, l),
                    roi = rv$roi, opacity = opacity, lang = l)
      },
      respaldo = function() {
        r <- req(valor())
        l <- lang()
        plotly_raster(raster_to_df(r), tr(titulo_key, l), pal_plotly, tr(leyenda_key, l), lang = l)
      })
  }

  registrar_raster("plot_dem_bruto", reactive(rv$dem),       "plot.title.dem_raw",
                   PAL_TERRAIN_PNG, PAL_TERRAIN, "plot.colorbar.elev_short")
  registrar_raster("plot_dem_suav",  reactive(rv$dem_suav),  "plot.title.dem_smooth",
                   PAL_TERRAIN_PNG, PAL_TERRAIN, "plot.colorbar.elev_short")
  registrar_raster("plot_hillshade", reactive(rv$hillshade), "plot.title.hillshade",
                   PAL_HILLSHADE_PNG, PAL_HILLSHADE, "plot.colorbar.light", opacity = 0.6)
  registrar_raster("plot_chm",       reactive(rv$chm),       "plot.title.chm",
                   PAL_CHM_PNG, PAL_CHM, "plot.colorbar.height_short")

  registrar_mapa(output, "plot_curvas", height = "640px",
    mapa = function() {
      req(rv$dem_suav, rv$curvas)
      l <- lang()
      mapa_curvas(rv$dem_suav, rv$curvas, tr("plot.title.dem_contours", l),
                  tr("plot.colorbar.elev_short", l), roi = rv$roi, lang = l)
    },
    respaldo = function() {
      req(rv$dem_suav, rv$curvas)
      l <- lang()
      par(mar = c(4, 4, 3, 5))
      plot(rv$dem_suav,
           main = tr("plot.title.dem_contours", l),
           col  = terrain.colors(50),
           mar  = c(3, 1, 3, 5))
      plot(rv$curvas, add = TRUE, col = "black", lwd = 0.8)
    },
    tipo_respaldo = "plot")

  output$plot_hist_z <- renderPlot({
    req(rv$datos_norm)
    l  <- lang()
    z  <- rv$datos_norm@data$Z
    if (length(z) > 100000) z <- sample(z, 100000)
    rng      <- range(z)
    h_breaks <- seq(floor(rng[1]), ceiling(rng[2]), by = 1)
    ggplot(data.frame(Z = z), aes(x = Z)) +
      geom_histogram(breaks = h_breaks, fill = GREEN, color = BG_CARD, alpha = 0.85) +
      scale_x_continuous(breaks = h_breaks) +
      labs(title = tr("plot.title.vertical_profile", l),
           x     = tr("plot.axis.height_m",          l),
           y     = tr("plot.axis.frequency",          l)) +
      theme_minimal() +
      theme(plot.background  = element_rect(fill = BG_CARD, colour = NA),
            panel.background = element_rect(fill = BG_CARD, colour = NA),
            panel.grid       = element_line(color = BORDER_COLOR),
            text             = element_text(color = TEXT_PRIMARY),
            axis.text        = element_text(color = TEXT_SECONDARY),
            axis.text.x      = element_text(angle = 90, vjust = 0.5, size = 9),
            plot.title       = element_text(color = ACCENT_PRIMARY, face = "bold"))
  })
}
