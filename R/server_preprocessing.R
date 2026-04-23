# ==============================================================================
# SERVER PREPROCESSING - E2: Pre-procesamiento LiDAR
# Registra: ejecutar_preproceso(), btn_preprocesar, modales CRS, outputs métricas
# ==============================================================================

register_preprocessing <- function(input, output, session, rv, ag, lang) {

  # ── Lógica interna de preproceso (reutilizada desde btn y modales CRS) ─────
  ejecutar_preproceso <- function(crs_override = NULL, shp_crs_override = NULL,
                                  usar_bbox = FALSE) {
    l <- lang()
    rv$log_prepro <- character(0)
    withProgress(message = tr("progress.preprocessing.message", l), value = 0, {
      tryCatch({
        setProgress(0.10, detail = tr("progress.preprocessing.loading", l))
        res <- cargar_recortar_las(rv$ruta_las, rv$ruta_shp,
                                    buffer_m         = input$buffer_m,
                                    crs_override     = crs_override,
                                    shp_crs_override = shp_crs_override,
                                    usar_bbox        = usar_bbox,
                                    log_fn           = function(m) ag("log_prepro", m),
                                    lang             = l)
        rv$las_raw   <- res$las
        rv$roi       <- res$roi
        rv$dens_orig <- res$dens_orig
        rv$area_ha   <- res$area_ha

        setProgress(0.40, detail = tr("progress.preprocessing.filtering", l))
        rv$las_filtrado <- filtrar_nube(rv$las_raw, densidad = rv$densidad_val,
                                         tipo   = input$decimate_tipo %||% "aleatorio",
                                         log_fn = function(m) ag("log_prepro", m),
                                         lang   = l)

        setProgress(0.70, detail = tr("progress.preprocessing.soil_classification", l))
        rv$las_clasf <- clasificar_suelo(rv$las_filtrado,
                          rigidness        = as.integer(rv$csf_rigidness),
                          class_threshold  = rv$csf_threshold,
                          cloth_resolution = rv$csf_cloth_res,
                          sloop_smooth     = rv$csf_sloop_smooth,
                          log_fn           = function(m) ag("log_prepro", m),
                          lang             = l)

        setProgress(1)
        rv$crs_pendiente <- NULL
        session$sendCustomMessage("tabEnable", "tab_modelos")
        showNotification(tr("notification.success.preprocessing_done", l), type = "message")
      }, error = function(e) {
        ag("log_prepro", paste(tr("notification.error.prefix", l), conditionMessage(e)))
        showNotification(tr("notification.error.generic", l, conditionMessage(e)), type = "error")
      })
    })
  }

  # ── Botón principal de preproceso ─────────────────────────────────────────
  observeEvent(input$btn_preprocesar, {
    req(rv$ruta_las, rv$ruta_dir)
    if (!isTRUE(rv$usar_extension_completa)) req(rv$ruta_shp)
    l <- lang()

    if (isTRUE(rv$usar_extension_completa)) {
      crs_info <- tryCatch(
        verificar_crs_las_solo(rv$ruta_las),
        error = function(e) list(estado = "ok")
      )
      if (crs_info$estado == "sin_crs") {
        rv$crs_pendiente <- crs_info
        showModal(modalDialog(
          title = tr("modal.crs.las_title", l),
          tags$p(tr("modal.crs.las_body_1", l)),
          numericInput("modal_epsg_las",
                       label = tr("modal.crs.epsg_label", l),
                       value = 22185L,
                       min   = 1,
                       max   = 99999,
                       step  = 1),
          tags$p(style = "font-size:12px; color:#666; margin-top:8px;",
                 tr("modal.crs.epsg_reference", l),
                 tags$a("epsg.io", href = "https://epsg.io/", target = "_blank"),
                 tr("modal.crs.epsg_reference_suffix", l)),
          footer = tagList(
            modalButton(tr("common.btn.cancel", l)),
            actionButton("btn_modal_confirmar_crs", tr("common.btn.confirm_continue", l),
                         class = "btn btn-success")
          ),
          easyClose = FALSE
        ))
      } else {
        ejecutar_preproceso(usar_bbox = TRUE)
      }
      return()
    }

    val_shp <- tryCatch(
      validar_shp_un_poligono(rv$ruta_shp),
      error = function(e) list(ok = FALSE, n_poligonos = NA_integer_)
    )
    if (!val_shp$ok) {
      msg <- if (is.na(val_shp$n_poligonos)) {
        tr("notification.error.shp_cannot_read", l)
      } else {
        tr("notification.error.shp_multiple_polygons", l, val_shp$n_poligonos)
      }
      rv$log_prepro <- character(0)
      ag("log_prepro", msg)
      showNotification(msg, type = "error", duration = 10)
      return()
    }

    crs_info <- tryCatch(
      verificar_crs_las(rv$ruta_las, rv$ruta_shp),
      error = function(e) {
        ag("log_prepro", tr("notification.warning.crs_check", l, conditionMessage(e)))
        list(estado = "ok")
      }
    )

    if (crs_info$estado == "sin_crs") {
      rv$crs_pendiente <- crs_info
      epsg_shp_str <- if (!is.na(crs_info$epsg_shp)) as.character(crs_info$epsg_shp) else "desconocido"
      epsg_default  <- if (!is.na(crs_info$epsg_shp)) crs_info$epsg_shp else 22185L
      showModal(modalDialog(
        title = tr("modal.crs.las_title", l),
        tags$p(tr("modal.crs.las_body_1", l)),
        tags$p(tr("modal.crs.las_body_shp_epsg", l, paste0("EPSG:", epsg_shp_str))),
        numericInput("modal_epsg_las",
                     label = tr("modal.crs.epsg_label", l),
                     value = epsg_default,
                     min   = 1,
                     max   = 99999,
                     step  = 1),
        tags$p(style = "font-size:12px; color:#666; margin-top:8px;",
               tr("modal.crs.epsg_reference", l),
               tags$a("epsg.io", href = "https://epsg.io/", target = "_blank"),
               tr("modal.crs.epsg_reference_suffix", l)),
        footer = tagList(
          modalButton(tr("common.btn.cancel", l)),
          actionButton("btn_modal_confirmar_crs", tr("common.btn.confirm_continue", l),
                       class = "btn btn-success")
        ),
        easyClose = FALSE
      ))

    } else if (crs_info$estado == "shp_sin_crs") {
      rv$crs_pendiente <- crs_info
      epsg_las_str <- if (!is.na(crs_info$epsg_las)) as.character(crs_info$epsg_las) else "desconocido"
      epsg_default  <- if (!is.na(crs_info$epsg_las)) crs_info$epsg_las else 22185L
      showModal(modalDialog(
        title = tr("modal.crs.shp_title", l),
        tags$p(tr("modal.crs.shp_body_1", l)),
        tags$p(tr("modal.crs.shp_body_las_epsg", l, paste0("EPSG:", epsg_las_str))),
        numericInput("modal_epsg_shp",
                     label = tr("modal.crs.shp_epsg_label", l),
                     value = epsg_default,
                     min   = 1,
                     max   = 99999,
                     step  = 1),
        tags$p(style = "font-size:12px; color:#666; margin-top:8px;",
               tr("modal.crs.epsg_reference", l),
               tags$a("epsg.io", href = "https://epsg.io/", target = "_blank"),
               tr("modal.crs.epsg_reference_suffix", l)),
        footer = tagList(
          modalButton(tr("common.btn.cancel", l)),
          actionButton("btn_modal_confirmar_shp_crs", tr("common.btn.confirm_continue", l),
                       class = "btn btn-success")
        ),
        easyClose = FALSE
      ))

    } else if (crs_info$estado == "mismatch") {
      rv$crs_pendiente <- crs_info
      epsg_las_str <- if (!is.na(crs_info$epsg_las)) as.character(crs_info$epsg_las) else "?"
      epsg_shp_str <- if (!is.na(crs_info$epsg_shp)) as.character(crs_info$epsg_shp) else "?"
      showModal(modalDialog(
        title = tr("modal.crs.mismatch_title", l),
        tags$p(tr("modal.crs.mismatch_body_1", l)),
        tags$ul(
          tags$li(tags$strong(tr("modal.crs.mismatch_las", l)), paste0("EPSG:", epsg_las_str)),
          tags$li(tags$strong(tr("modal.crs.mismatch_shp", l)), paste0("EPSG:", epsg_shp_str))
        ),
        tags$p(tr("modal.crs.mismatch_body_2", l, paste0("EPSG:", epsg_las_str))),
        footer = tagList(
          modalButton(tr("common.btn.cancel", l)),
          actionButton("btn_modal_confirmar_mismatch", tr("common.btn.understood_continue", l),
                       class = "btn btn-warning")
        ),
        easyClose = FALSE
      ))

    } else {
      ejecutar_preproceso()
    }
  })

  # ── Confirmación de modales CRS ───────────────────────────────────────────
  observeEvent(input$btn_modal_confirmar_crs, {
    req(rv$crs_pendiente)
    removeModal()
    epsg_elegido <- as.integer(input$modal_epsg_las)
    ag("log_prepro", tr("preprocessing.log.crs_las_assigned", lang(), epsg_elegido))
    ejecutar_preproceso(crs_override = epsg_elegido,
                        usar_bbox    = isTRUE(rv$usar_extension_completa))
  })

  observeEvent(input$btn_modal_confirmar_shp_crs, {
    req(rv$crs_pendiente)
    removeModal()
    epsg_elegido <- as.integer(input$modal_epsg_shp)
    ag("log_prepro", tr("preprocessing.log.crs_shp_assigned", lang(), epsg_elegido))
    ejecutar_preproceso(shp_crs_override = epsg_elegido)
  })

  observeEvent(input$btn_modal_confirmar_mismatch, {
    req(rv$crs_pendiente)
    removeModal()
    ag("log_prepro",
       tr("preprocessing.log.reprojection", lang(),
          if (!is.na(rv$crs_pendiente$epsg_shp)) rv$crs_pendiente$epsg_shp else "?",
          if (!is.na(rv$crs_pendiente$epsg_las)) rv$crs_pendiente$epsg_las else "?"))
    ejecutar_preproceso()
  })

  # ── Outputs de métricas y visualizaciones 3D ─────────────────────────────
  output$log_prepro    <- renderText(paste(rv$log_prepro, collapse="\n"))
  output$met_pts_orig  <- renderText(if(is.null(rv$las_raw))      "—" else format(nrow(rv$las_raw@data),     big.mark="."))
  output$met_pts_filt  <- renderText(if(is.null(rv$las_filtrado)) "—" else format(nrow(rv$las_filtrado@data),big.mark="."))
  output$met_pts_suelo <- renderText(if(is.null(rv$las_clasf))    "—" else format(sum(rv$las_clasf@data$Classification==2L,na.rm=TRUE),big.mark="."))
  output$met_pts_veg   <- renderText(if(is.null(rv$las_clasf))    "—" else format(sum(rv$las_clasf@data$Classification!=2L,na.rm=TRUE),big.mark="."))

  output$plot3d_orig  <- renderPlotly({
    req(rv$las_raw)
    plotly_nube_3d(rv$las_raw, color_var="Z",
                   titulo = tr("plot.title.cloud_original", lang()), lang = lang())
  })
  output$plot3d_clasf <- renderPlotly({
    req(rv$las_clasf)
    plotly_nube_3d(rv$las_clasf, color_var="Classification",
                   titulo = tr("plot.title.cloud_classified", lang()), lang = lang())
  })
}
