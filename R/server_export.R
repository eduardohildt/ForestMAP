# ==============================================================================
# SERVER EXPORT - E5: Exportación de productos e informe PDF
# ==============================================================================

register_export <- function(input, output, session, rv, ag, lang) {

  observeEvent(input$btn_exportar, {
    req(rv$ruta_dir, rv$las_clasf, rv$datos_norm, rv$dem_suav,
        rv$chm, rv$curvas, rv$hillshade, rv$arboles, rv$roi)
    l <- lang()
    rv$log_export <- character(0)
    withProgress(message = tr("progress.export.message", l), value = 0.1, {
      tryCatch({
        copas_vect <- if (!is.null(rv$cobertura_copas)) rv$cobertura_copas$copas_vect else NULL
        exportar_todos(rv$ruta_dir, rv$las_clasf, rv$datos_norm, rv$dem_suav,
                       rv$chm, rv$curvas, rv$hillshade, rv$arboles, rv$roi,
                       copas_vect = copas_vect,
                       log_fn = function(m) ag("log_export", m))
        setProgress(1)
        showNotification(tr("notification.success.export_done", l), type = "message")
      }, error = function(e) {
        ag("log_export", paste(tr("notification.error.prefix", l), conditionMessage(e)))
        showNotification(tr("notification.error.generic", l, conditionMessage(e)), type = "error")
      })
    })
  })

  observeEvent(input$btn_informe, {
    req(rv$ruta_dir)
    l <- lang()
    ag("log_export", tr("export.log.generating_pdf", l))
    tryCatch({
      generar_informe_descriptivo(rv, input, lang = l, log_fn = function(m) ag("log_export", m))
      showNotification(tr("notification.success.pdf_done", l), type = "message")
    }, error = function(e) {
      ag("log_export", paste(tr("notification.error.report_prefix", l), conditionMessage(e)))
      showNotification(tr("notification.error.report_generic", l, conditionMessage(e)), type = "error")
    })
  })

  output$log_exportar <- renderText(paste(rv$log_export, collapse = "\n"))

  observeEvent(input$btn_abrir_carpeta, {
    req(rv$ruta_dir)
    l <- lang()
    carpeta <- rv$ruta_dir
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
