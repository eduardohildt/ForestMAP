# ==============================================================================
# SERVER EXPORT - E5: Exportación de productos e informe HTML
# ==============================================================================

register_export <- function(input, output, session, rv, ag, lang) {

  # Botón único: exporta productos y genera el informe en un solo paso, usando
  # siempre el mismo idioma capturado una vez (evita mezclar idiomas entre
  # carpetas/archivos exportados y el informe generado)
  observeEvent(input$btn_exportar, {
    req(rv$ruta_proyecto, rv$dem_suav, rv$chm, rv$curvas, rv$hillshade, rv$arboles, rv$roi)
    l <- lang()
    rv$log_export <- character(0)
    withProgress(message = tr("progress.export.message", l), value = 0.1, {
      tryCatch({
        copas_vect <- if (!is.null(rv$cobertura_copas)) rv$cobertura_copas$copas_vect else NULL
        exportar_todos(rv$ruta_proyecto, rv$dem_suav, rv$chm, rv$curvas, rv$hillshade,
                       rv$arboles, rv$roi,
                       copas_vect        = copas_vect,
                       formato_vectorial = input$sel_formato_vectorial %||% "shp",
                       lang              = l,
                       log_fn            = function(m) ag("log_export", m))
        setProgress(0.6)
        ag("log_export", tr("export.log.generating_HTML", l))
        generar_informe_descriptivo(rv, input, lang = l, log_fn = function(m) ag("log_export", m))
        setProgress(1)
        showNotification(tr("notification.success.export_done", l), type = "message")
      }, error = function(e) {
        ag("log_export", paste(tr("notification.error.prefix", l), conditionMessage(e)))
        showNotification(tr("notification.error.generic", l, conditionMessage(e)), type = "error")
      })
    })
  })

  output$ui_export_raster_list <- renderUI({
    l <- lang()
    tags$ul(style=paste0("color:",TEXT_SECONDARY,";font-size:13px;list-style:none;padding:0;"),
      tags$li(style="padding:4px 0;", paste0("✓ ", tr("files.dem", l), ".tif")),
      tags$li(style="padding:4px 0;", paste0("✓ ", tr("files.chm", l), ".tif")),
      tags$li(style="padding:4px 0;", paste0("✓ ", tr("files.hillshade", l), ".tif"))
    )
  })

  output$ui_export_vector_list <- renderUI({
    l   <- lang()
    ext <- if (identical(input$sel_formato_vectorial, "geojson")) ".geojson" else ".shp"
    tags$ul(style=paste0("color:",TEXT_SECONDARY,";font-size:13px;list-style:none;padding:0;"),
      tags$li(style="padding:4px 0;", paste0("✓ ", tr("files.contour_lines", l), ext)),
      tags$li(style="padding:4px 0;", paste0("✓ ", tr("files.trees", l), ext)),
      tags$li(style="padding:4px 0;", paste0("✓ ", tr("files.canopy_cover", l), ext)),
      tags$li(style="padding:4px 0;", paste0("✓ ", tr("files.roi", l), ext))
    )
  })

  output$log_exportar <- renderText(paste(rv$log_export, collapse = "\n"))

  observeEvent(input$btn_abrir_carpeta, {
    req(rv$ruta_proyecto)
    l <- lang()
    carpeta <- rv$ruta_proyecto
    if (!dir.exists(carpeta)) {
      showNotification(tr("notification.error.folder_not_exists", l), type = "error")
      return()
    }
    sysname <- Sys.info()[["sysname"]]
    tryCatch({
      if (sysname == "Windows") {
        shell.exec(carpeta)
      } else if (sysname == "Darwin") {
        system(paste("open", shQuote(carpeta)))
      } else {
        system(paste("xdg-open", shQuote(carpeta)))
      }
      showNotification(tr("notification.success.folder_opened", l), type = "message")
    }, error = function(e) {
      showNotification(tr("notification.error.folder_open_failed", l, carpeta), type = "warning")
    })
  })
}
