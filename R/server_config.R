# ==============================================================================
# SERVER CONFIG - E0: Configuración del proyecto y selección de archivos
# Registra: modal de proyecto, selección LAS/SHP/dir, botón Continuar, outputs
# ==============================================================================

register_config <- function(input, output, session, rv, ag, lang) {

  # ── Modal: Configuración del Proyecto ────────────────────────────────────────
  observeEvent(input$btn_abrir_cfg, {
    l <- lang()
    showModal(modalDialog(
      title = tr("config.modal.title", l),
      tags$p(style="font-size:12px;color:#666;margin-bottom:16px;",
             tr("config.modal.instructions", l)),
      fluidRow(
        column(6,
          textInput("modal_cfg_autor", tr("config.modal.author_label", l),
                    value       = rv$cfg_autor,
                    placeholder = tr("config.modal.author_placeholder", l))
        ),
        column(6,
          textInput("modal_cfg_institucion", tr("config.modal.institution_label", l),
                    value       = rv$cfg_institucion,
                    placeholder = tr("config.modal.institution_placeholder", l))
        )
      ),
      fluidRow(
        column(6,
          textInput("modal_cfg_email", tr("config.modal.email_label", l),
                    value       = rv$cfg_email,
                    placeholder = tr("config.modal.email_placeholder", l))
        ),
        column(6,
          textInput("modal_cfg_destinatario", tr("config.modal.recipient_label", l),
                    value       = rv$cfg_destinatario,
                    placeholder = tr("config.modal.recipient_placeholder", l))
        )
      ),
      footer = tagList(
        modalButton(tr("common.btn.cancel", l)),
        actionButton("btn_modal_guardar_cfg", tr("common.btn.save", l),
                     class = "btn btn-success")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$btn_modal_guardar_cfg, {
    autor <- trimws(input$modal_cfg_autor)
    inst  <- trimws(input$modal_cfg_institucion)
    email <- trimws(input$modal_cfg_email)
    dest  <- trimws(input$modal_cfg_destinatario)
    rv$cfg_autor        <- if (nchar(autor) == 0) "-" else autor
    rv$cfg_institucion  <- if (nchar(inst)  == 0) "-" else inst
    rv$cfg_email        <- if (nchar(email) == 0) "-" else email
    rv$cfg_destinatario <- if (nchar(dest)  == 0) "-" else dest
    removeModal()
    showNotification(tr("notification.success.project_saved", lang()), type = "message")
  })

  output$cfg_resumen_autor       <- renderText(rv$cfg_autor)
  output$cfg_resumen_institucion <- renderText(rv$cfg_institucion)

  # ── LAS ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_las, {
    session$sendCustomMessage("btnDisable", "btn_las")
    on.exit(session$sendCustomMessage("btnEnable", "btn_las"))
    ruta <- elegir_archivo(
      caption          = tr("config.file.dialog_las_caption", lang()),
      filtros_rstudio  = tr("config.file.dialog_las_filter",  lang()),
      dir_inicio       = rv$ultima_carpeta
    )
    if (length(ruta) > 0 && !is.na(ruta) && nchar(ruta) > 0) {
      rv$ruta_las       <- ruta
      rv$ultima_carpeta <- dirname(ruta)
      tryCatch({
        hdr       <- lidR::readLASheader(ruta)
        tam_bytes <- file.info(ruta)$size
        tam_str   <- if (tam_bytes >= 1e9) sprintf("%.2f GB", tam_bytes / 1e9)
                     else sprintf("%.1f MB", tam_bytes / 1e6)
        epsg_val  <- tryCatch(as.integer(sf::st_crs(hdr)$epsg), error = function(e) NA_integer_)
        epsg_str  <- if (!is.na(epsg_val) && epsg_val > 0)
                       tr("common.meta.epsg_detected", lang(), epsg_val)
                     else tr("common.meta.no_crs", lang())
        rv$las_meta <- list(
          nombre     = basename(ruta),
          tamano_str = tam_str,
          epsg_str   = epsg_str,
          fecha_str  = format(file.info(ruta)$mtime, "%d/%m/%Y"),
          epsg_ok    = (!is.na(epsg_val) && epsg_val > 0)
        )
      }, error = function(e) {
        rv$las_meta <- list(nombre = basename(ruta), tamano_str = "?",
          epsg_str = tr("common.meta.cannot_read_header", lang()), fecha_str = "-", epsg_ok = FALSE)
      })
    }
  })

  # ── SHP ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_shp, {
    session$sendCustomMessage("btnDisable", "btn_shp")
    on.exit(session$sendCustomMessage("btnEnable", "btn_shp"))
    ruta <- elegir_archivo(
      caption          = tr("config.file.dialog_shp_caption", lang()),
      filtros_rstudio  = tr("config.file.dialog_shp_filter",  lang()),
      dir_inicio       = rv$ultima_carpeta
    )
    if (length(ruta) > 0 && !is.na(ruta) && nchar(ruta) > 0) {
      rv$ruta_shp       <- ruta
      rv$ultima_carpeta <- dirname(ruta)
      tryCatch({
        shp_sf   <- sf::st_read(ruta, quiet = TRUE)
        n_polig  <- nrow(shp_sf)
        epsg_val <- tryCatch(as.integer(sf::st_crs(shp_sf)$epsg), error = function(e) NA_integer_)
        epsg_str <- if (!is.na(epsg_val) && epsg_val > 0)
                      paste0("EPSG: ", epsg_val)
                    else tr("common.meta.no_crs_shp", lang())
        area_ha  <- tryCatch({
          if (!sf::st_is_longlat(shp_sf))
            round(sum(as.numeric(sf::st_area(shp_sf))) / 10000, 2)
          else NA_real_
        }, error = function(e) NA_real_)
        rv$shp_meta <- list(
          nombre      = basename(ruta),
          n_poligonos = n_polig,
          area_ha     = area_ha,
          epsg_str    = epsg_str,
          advertencia = (n_polig > 1)
        )
      }, error = function(e) {
        rv$shp_meta <- list(nombre = basename(ruta), n_poligonos = NA,
          area_ha = NA_real_, epsg_str = tr("common.meta.cannot_read_header", lang()), advertencia = FALSE)
      })
    }
  })

  # ── DIR salida ────────────────────────────────────────────────────────────
  observeEvent(input$btn_dir, {
    session$sendCustomMessage("btnDisable", "btn_dir")
    on.exit(session$sendCustomMessage("btnEnable", "btn_dir"))
    ruta <- elegir_carpeta(
      caption    = tr("config.file.dialog_dir_caption", lang()),
      dir_inicio = rv$ultima_carpeta
    )
    if (!is.na(ruta) && nchar(ruta) > 0)
      rv$ruta_dir <- ruta
  })

  output$txt_ruta_las <- renderText(rv$ruta_las %||% tr("common.status.not_selected", lang()))
  output$txt_ruta_shp <- renderText(rv$ruta_shp %||% tr("common.status.not_selected", lang()))
  output$txt_ruta_dir <- renderText(rv$ruta_dir %||% tr("common.status.not_selected", lang()))
  output$met_las <- renderText(if(!is.null(rv$ruta_las))"✅"else"❌")
  output$met_shp <- renderText(if(!is.null(rv$ruta_shp))"✅"else"❌")
  output$met_dir <- renderText(if(!is.null(rv$ruta_dir))"✅"else"❌")

  # ── Bloque 0: Proyecto ───────────────────────────────────────────────────
  output$cfg_proyecto_html <- renderUI({
    l <- lang()
    tags$table(class = "proyecto-data-table",
      tags$tr(tags$td(tr("config.table.author",      l)), tags$td(rv$cfg_autor)),
      tags$tr(tags$td(tr("config.table.institution", l)), tags$td(rv$cfg_institucion)),
      tags$tr(tags$td(tr("config.table.email",       l)), tags$td(rv$cfg_email)),
      tags$tr(tags$td(tr("config.table.recipient",   l)), tags$td(rv$cfg_destinatario))
    )
  })

  # ── Bloque 1: LAS — botón o tarjeta de metadata ──────────────────────────
  output$cfg_las_ui <- renderUI({
    l <- lang()
    if (is.null(rv$ruta_las)) {
      actionButton("btn_las", tr("config.file.select_las_btn", l),
        class = "btn btn-success w-100",
        style = "font-weight:600;padding:10px;")
    } else {
      m <- rv$las_meta
      tagList(
        div(class = "file-meta-card",
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_filename", l)),
            tags$span(class = "file-meta-value",
              if (!is.null(m)) m$nombre else basename(rv$ruta_las))),
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_size", l)),
            tags$span(class = "file-meta-value",
              if (!is.null(m)) m$tamano_str else "?")),
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_projection", l)),
            tags$span(
              class = paste("file-meta-value", if (!is.null(m) && !isTRUE(m$epsg_ok)) "warn" else ""),
              if (!is.null(m)) m$epsg_str else "?")),
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_modified", l)),
            tags$span(class = "file-meta-value",
              if (!is.null(m)) m$fecha_str else "?"))
        ),
        actionButton("btn_las", tr("config.file.change_file_btn", l), class = "btn btn-cambiar")
      )
    }
  })

  output$cfg_las_badge <- renderUI({
    l <- lang()
    if (!is.null(rv$ruta_las))
      tags$span(class = "step-status-badge status-complete", tr("common.status.loaded", l))
    else
      tags$span(class = "step-status-badge status-pending",  tr("common.status.pending", l))
  })

  # ── Bloque 2: SHP — botón o tarjeta de metadata ──────────────────────────
  output$cfg_shp_ui <- renderUI({
    l <- lang()
    if (is.null(rv$ruta_shp)) {
      actionButton("btn_shp", tr("config.file.select_shp_btn", l),
        class = "btn btn-info w-100",
        style = "font-weight:600;padding:10px;color:#fff;")
    } else {
      m <- rv$shp_meta
      area_txt <- if (!is.null(m) && !is.na(m$area_ha))
                    paste0(format(m$area_ha, big.mark = ".", nsmall = 2), " ha")
                  else tr("common.meta.not_calculable", l)
      polig_txt <- if (!is.null(m) && !is.na(m$n_poligonos)) {
                     if (isTRUE(m$advertencia))
                       tr("common.meta.multiple_polygons_warning", l, m$n_poligonos)
                     else as.character(m$n_poligonos)
                   } else "?"
      tagList(
        div(class = "file-meta-card",
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_filename", l)),
            tags$span(class = "file-meta-value",
              if (!is.null(m)) m$nombre else basename(rv$ruta_shp))),
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_polygons", l)),
            tags$span(
              class = paste("file-meta-value", if (!is.null(m) && isTRUE(m$advertencia)) "warn" else ""),
              polig_txt)),
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_area", l)),
            tags$span(class = "file-meta-value", area_txt)),
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_projection", l)),
            tags$span(class = "file-meta-value",
              if (!is.null(m)) m$epsg_str else "?"))
        ),
        actionButton("btn_shp", tr("config.file.change_file_btn", l), class = "btn btn-cambiar")
      )
    }
  })

  output$cfg_shp_badge <- renderUI({
    l <- lang()
    if (!is.null(rv$ruta_shp))
      tags$span(class = "step-status-badge status-complete", tr("common.status.loaded", l))
    else
      tags$span(class = "step-status-badge status-pending",  tr("common.status.pending", l))
  })

  # ── Bloque 3: Carpeta de salida ───────────────────────────────────────────
  output$cfg_dir_ui <- renderUI({
    l <- lang()
    if (is.null(rv$ruta_dir)) {
      actionButton("btn_dir", tr("config.file.select_dir_btn", l),
        class = "btn btn-outline-secondary w-100",
        style = "font-weight:600;padding:10px;")
    } else {
      tagList(
        div(class = "file-meta-card",
          div(class = "file-meta-row",
            tags$span(class = "file-meta-label", tr("config.file.meta_path", l)),
            tags$span(class = "file-meta-value",
              style = "font-size:11.5px;",
              rv$ruta_dir))
        ),
        actionButton("btn_dir", tr("config.file.change_dir_btn", l), class = "btn btn-cambiar")
      )
    }
  })

  output$cfg_dir_badge <- renderUI({
    l <- lang()
    if (!is.null(rv$ruta_dir))
      tags$span(class = "step-status-badge status-complete", tr("common.status.selected", l))
    else
      tags$span(class = "step-status-badge status-pending",  tr("common.status.pending", l))
  })

  # ── Botón Continuar: habilitar solo cuando todo está completo ─────────────
  observe({
    todo_listo <- !is.null(rv$ruta_las) && !is.null(rv$ruta_shp) && !is.null(rv$ruta_dir)
    if (todo_listo) session$sendCustomMessage("btnEnable",  "btn_continuar")
    else            session$sendCustomMessage("btnDisable", "btn_continuar")
  })

  observeEvent(input$btn_continuar, {
    req(rv$ruta_las, rv$ruta_shp, rv$ruta_dir)
    l <- lang()
    crear_estructura_carpetas(rv$ruta_dir)
    rv$configurado <- TRUE
    ag("log_config", tr("config.log.las", l, basename(rv$ruta_las)))
    ag("log_config", tr("config.log.roi", l, basename(rv$ruta_shp)))
    ag("log_config", tr("config.log.dir", l, rv$ruta_dir))
    ag("log_config", tr("config.log.folders_ready", l))
    showNotification(tr("notification.success.config_saved", l), type = "message")
    session$sendCustomMessage("tabEnable", "tab_prepro")
    updateNavbarPage(session, inputId = "main_nav", selected = "tab_prepro")
  })

  output$log_config <- renderText(paste(rv$log_config, collapse="\n"))
}
