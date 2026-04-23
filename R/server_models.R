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

  output$plot_dem_bruto <- renderPlotly({
    req(rv$dem)
    l <- lang()
    plotly_raster(raster_to_df(rv$dem), tr("plot.title.dem_raw", l), PAL_TERRAIN, tr("plot.colorbar.elev_short", l), lang = l)
  })
  output$plot_dem_suav <- renderPlotly({
    req(rv$dem_suav)
    l <- lang()
    plotly_raster(raster_to_df(rv$dem_suav), tr("plot.title.dem_smooth", l), PAL_TERRAIN, tr("plot.colorbar.elev_short", l), lang = l)
  })
  output$plot_hillshade <- renderPlotly({
    req(rv$hillshade)
    l <- lang()
    plotly_raster(raster_to_df(rv$hillshade), tr("plot.title.hillshade", l), PAL_HILLSHADE, tr("plot.colorbar.light", l), lang = l)
  })
  output$plot_chm <- renderPlotly({
    req(rv$chm)
    l <- lang()
    plotly_raster(raster_to_df(rv$chm), tr("plot.title.chm", l), PAL_CHM, tr("plot.colorbar.height_short", l), lang = l)
  })

  output$plot_curvas <- renderPlot({
    req(rv$dem_suav, rv$curvas)
    l <- lang()
    par(mar = c(4, 4, 3, 5))
    plot(rv$dem_suav,
         main = tr("plot.title.dem_contours", l),
         col  = terrain.colors(50),
         mar  = c(3, 1, 3, 5))
    plot(rv$curvas, add = TRUE, col = "black", lwd = 0.8)
  }, height = 530)

  output$plot_hist_z <- renderPlot({
    req(rv$datos_norm)
    l  <- lang()
    z  <- rv$datos_norm@data$Z
    if (length(z) > 100000) z <- sample(z, 100000)
    ggplot(data.frame(Z = z), aes(x = Z)) +
      geom_histogram(bins = 60, fill = GREEN, color = BG_CARD, alpha = 0.85) +
      labs(title = tr("plot.title.vertical_profile", l),
           x     = tr("plot.axis.height_m",          l),
           y     = tr("plot.axis.frequency",          l)) +
      theme_minimal() +
      theme(plot.background  = element_rect(fill = BG_CARD, colour = NA),
            panel.background = element_rect(fill = BG_CARD, colour = NA),
            panel.grid       = element_line(color = BORDER_COLOR),
            text             = element_text(color = TEXT_PRIMARY),
            axis.text        = element_text(color = TEXT_SECONDARY),
            plot.title       = element_text(color = ACCENT_PRIMARY, face = "bold"))
  })
}
