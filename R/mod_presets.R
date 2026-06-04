# ==============================================================================
# SERVER PRESETS - E1: Selección de parámetros via tarjetas preset
# Registra: CSF escenario/avanzado, densidad, DEM/CHM/árbol presets y modales
# ==============================================================================

register_presets <- function(input, output, session, rv, ag, lang) {

  # ── SVGs temáticos reutilizados en todas las tarjetas ──────────────────────

  .svg_grid <- function(cols, rows, fill = "rgba(45,122,61,0.12)", stroke = "#2d7a3d") {
    w <- 56; h <- 36
    cw <- w / cols; rh <- h / rows
    lines_v <- paste(vapply(seq(0, w, by = cw), function(x)
      sprintf('<line x1="%.1f" y1="0" x2="%.1f" y2="%d" stroke="%s" stroke-width="0.8"/>', x, x, h, stroke), character(1)), collapse = "")
    lines_h <- paste(vapply(seq(0, h, by = rh), function(y)
      sprintf('<line x1="0" y1="%.1f" x2="%d" y2="%.1f" stroke="%s" stroke-width="0.8"/>', y, w, y, stroke), character(1)), collapse = "")
    sprintf('<svg viewBox="0 0 %d %d" width="44" height="28" xmlns="http://www.w3.org/2000/svg"><rect width="%d" height="%d" fill="%s" rx="2"/><g opacity="0.7">%s%s</g></svg>',
            w, h, w, h, fill, lines_v, lines_h)
  }
  svg_grid_fine   <- .svg_grid(8, 5)
  svg_grid_med    <- .svg_grid(5, 3)
  svg_grid_coarse <- .svg_grid(3, 2)

  .svg_contour <- function(n_lines) {
    w <- 56; h <- 36
    ys <- seq(7, h - 7, length.out = n_lines)
    offsets <- list(c(-3, 2, -1), c(2, -3, 1), c(-2, 3, -2), c(3, -1, 2), c(-1, 2, -3))
    paths <- paste(vapply(seq_along(ys), function(i) {
      y0  <- ys[i]
      off <- offsets[[((i - 1L) %% length(offsets)) + 1L]]
      sprintf(
        '<path d="M 2,%.1f C 14,%.1f 22,%.1f 29,%.1f C 36,%.1f 44,%.1f 54,%.1f" fill="none" stroke="#2d7a3d" stroke-width="1.5" stroke-linecap="round"/>',
        y0, y0 + off[1], y0 + off[2], y0, y0 + off[3], y0 + off[1], y0
      )
    }, character(1)), collapse = "")
    sprintf('<svg viewBox="0 0 %d %d" width="44" height="28" xmlns="http://www.w3.org/2000/svg"><rect width="%d" height="%d" fill="rgba(45,122,61,0.08)" rx="2"/>%s</svg>',
            w, h, w, h, paths)
  }
  svg_contour_dense  <- .svg_contour(5)
  svg_contour_med    <- .svg_contour(3)
  svg_contour_sparse <- .svg_contour(2)

  .svg_minfilter <- function(spikes) {
    w <- 56; h <- 36
    base <- 28
    peak_h <- c(8, 10, 6, 12, 9, 7, 11, 8)
    n <- min(spikes, length(peak_h))
    xs <- seq(6, 50, length.out = n)
    triangles <- paste(vapply(seq_len(n), function(i) {
      x <- xs[i]; ph <- peak_h[i]
      sprintf('<polygon points="%.0f,%d %.0f,%.0f %.0f,%d" fill="rgba(45,122,61,0.25)" stroke="#2d7a3d" stroke-width="1"/>',
              x - 3, base, x, base - ph, x + 3, base)
    }, character(1)), collapse = "")
    ground <- sprintf('<path d="M 2,%d C 12,%d 20,%d 28,%d C 36,%d 46,%d 54,%d" fill="none" stroke="#2d7a3d" stroke-width="2" stroke-linecap="round"/>',
                      base, base - 6, base + 9, base - 3, base + 6, base - 6, base)
    sprintf('<svg viewBox="0 0 %d %d" width="44" height="28" xmlns="http://www.w3.org/2000/svg"><rect width="%d" height="%d" fill="rgba(45,122,61,0.08)" rx="2"/>%s%s</svg>',
            w, h, w, h, triangles, ground)
  }
  svg_minfilter_cons <- .svg_minfilter(8)
  svg_minfilter_med  <- .svg_minfilter(4)
  svg_minfilter_agr  <- .svg_minfilter(2)

  .svg_smooth <- function(passes) {
    w <- 56; h <- 36
    ys <- c(10, 19, 28)
    mk_line <- function(y, type) {
      if (type == "dentada") {
        xs  <- seq(2, 54, length.out = 10L)
        off <- rep_len(c(0, -3.5, 3.5), 10L)
        pts <- paste(sprintf("%.1f,%.1f", xs, y + off), collapse = " ")
        sprintf('<polyline points="%s" fill="none" stroke="#2d7a3d" stroke-width="1.5" stroke-linejoin="miter" stroke-linecap="butt"/>', pts)
      } else if (type == "lobulada") {
        sprintf('<path d="M 2,%.0f C 14,%.0f 22,%.0f 28,%.0f C 34,%.0f 44,%.0f 54,%.0f" fill="none" stroke="#2d7a3d" stroke-width="1.5" stroke-linecap="round"/>',
                y, y - 5, y + 5, y, y - 5, y + 5, y)
      } else {
        sprintf('<path d="M 2,%.0f C 15,%.0f 41,%.0f 54,%.0f" fill="none" stroke="#2d7a3d" stroke-width="1.5" stroke-linecap="round"/>',
                y, y - 3, y - 3, y)
      }
    }
    type  <- switch(as.character(passes), "1" = "dentada", "2" = "lobulada", "3" = "eliptica")
    paths <- paste(vapply(ys, mk_line, character(1L), type = type), collapse = "")
    sprintf('<svg viewBox="0 0 %d %d" width="44" height="28" xmlns="http://www.w3.org/2000/svg"><rect width="%d" height="%d" fill="rgba(45,122,61,0.08)" rx="2"/>%s</svg>',
            w, h, w, h, paths)
  }
  svg_smooth_cons <- .svg_smooth(1)
  svg_smooth_med  <- .svg_smooth(2)
  svg_smooth_agr  <- .svg_smooth(3)

  .svg_gapfill <- function(gaps) {
    w <- 56; h <- 36
    cx <- 28; cy <- 18
    n_pts   <- 60L
    ang_seq <- seq(0, 2 * pi, length.out = n_pts + 1L)[-(n_pts + 1L)]
    r_seq   <- 10.5 + 2.5 * cos(6 * ang_seq)
    pts_crown <- paste(sprintf("%.1f,%.1f",
                               cx + r_seq * sin(ang_seq),
                               cy - r_seq * cos(ang_seq)), collapse = " ")
    crown <- sprintf('<polygon points="%s" fill="rgba(45,122,61,0.45)" stroke="#2d7a3d" stroke-width="1.5"/>', pts_crown)
    r_gap <- gaps * 1.7
    gap <- if (r_gap > 0.1)
      sprintf('<circle cx="%d" cy="%d" r="%.1f" fill="rgba(245,250,245,0.92)" stroke="#2d7a3d" stroke-width="0.8" stroke-dasharray="2,1.5"/>',
              cx, cy, r_gap)
    else ""
    sprintf('<svg viewBox="0 0 %d %d" width="44" height="28" xmlns="http://www.w3.org/2000/svg"><rect width="%d" height="%d" fill="rgba(45,122,61,0.06)" rx="2"/>%s%s</svg>',
            w, h, w, h, crown, gap)
  }
  svg_gap_cons <- .svg_gapfill(4)
  svg_gap_med  <- .svg_gapfill(2)
  svg_gap_agr  <- .svg_gapfill(0)

  # ── SVGs para cards de Árboles ────────────────────────────────────────────

  .svg_ws <- function(r_outer) {
    sprintf(
      '<svg viewBox="0 0 80 60" width="56" height="42" xmlns="http://www.w3.org/2000/svg"><rect width="80" height="60" fill="rgba(45,122,61,0.06)" rx="2"/><circle cx="40" cy="30" r="%d" fill="none" stroke="#555" stroke-width="1.5" stroke-dasharray="3,2"/><circle cx="40" cy="30" r="10" fill="rgba(45,122,61,0.65)" stroke="#2d7a3d" stroke-width="1"/><circle cx="40" cy="30" r="2.5" fill="#c0392b"/></svg>',
      r_outer
    )
  }
  svg_ws_chica   <- .svg_ws(16)
  svg_ws_mediana <- .svg_ws(22)
  svg_ws_grande  <- .svg_ws(28)

  .svg_pine <- function(apex_y, base_crown_y, trunk_h, half_w) {
    mid_x <- 40
    trunk_x <- mid_x - 3
    sprintf(
      '<svg viewBox="0 0 80 60" width="56" height="42" xmlns="http://www.w3.org/2000/svg"><rect width="80" height="60" fill="rgba(45,122,61,0.06)" rx="2"/><line x1="5" y1="56" x2="75" y2="56" stroke="#aaa" stroke-width="1"/><rect x="%d" y="%d" width="6" height="%d" fill="#8B5E3C"/><polygon points="%d,%d %d,%d %d,%d" fill="rgba(45,122,61,0.75)" stroke="#2d7a3d" stroke-width="1"/></svg>',
      trunk_x, base_crown_y, trunk_h,
      mid_x, apex_y, mid_x - half_w, base_crown_y, mid_x + half_w, base_crown_y
    )
  }
  svg_hmin_baja  <- .svg_pine(43, 51, 5, 10)
  svg_hmin_media <- .svg_pine(28, 48, 8, 15)
  svg_hmin_alta  <- .svg_pine(11, 45, 11, 20)

  .svg_cutoff <- function(line_y) {
    mid_x <- 40
    apex_y <- 12; base_crown_y <- 47; trunk_h <- 9; half_w <- 18
    trunk_x <- mid_x - 3
    sprintf(
      '<svg viewBox="0 0 80 60" width="56" height="42" xmlns="http://www.w3.org/2000/svg"><rect width="80" height="60" fill="rgba(45,122,61,0.06)" rx="2"/><line x1="5" y1="56" x2="75" y2="56" stroke="#aaa" stroke-width="1"/><rect x="%d" y="%d" width="6" height="%d" fill="#8B5E3C"/><polygon points="%d,%d %d,%d %d,%d" fill="rgba(45,122,61,0.75)" stroke="#2d7a3d" stroke-width="1"/><line x1="8" y1="%d" x2="72" y2="%d" stroke="#e74c3c" stroke-width="2.5" stroke-linecap="round"/></svg>',
      trunk_x, base_crown_y, trunk_h,
      mid_x, apex_y, mid_x - half_w, base_crown_y, mid_x + half_w, base_crown_y,
      line_y, line_y
    )
  }
  svg_cutoff_baja  <- .svg_cutoff(50)
  svg_cutoff_media <- .svg_cutoff(43)
  svg_cutoff_alta  <- .svg_cutoff(31)

  # ── Helper genérico de tarjetas preset ────────────────────────────────────
  .render_preset_cards <- function(sel, custom_flag, presets, click_id, unit = "", l = "es") {
    mk <- function(p) {
      active <- isTRUE(abs(sel - p$val) < 1e-9)
      cls <- paste("csf-scenario-card", if (active) "csf-selected" else "")
      div(class = "col-4",
        div(class = cls,
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", click_id, p$id),
          HTML(p$svg),
          div(class = "csf-scenario-title", p$label),
          div(class = "csf-scenario-desc",  p$desc)
        )
      )
    }
    tagList(
      div(class = "row g-0 mb-0", lapply(presets, mk)),
      if (isTRUE(custom_flag))
        div(class = "text-center mb-0",
          tags$span(class = "csf-custom-badge",
            paste0("⚙ ", tr("presets.csf.custom_badge_modal", l), ": ", sel, if (nchar(unit)) paste0(" ", unit) else "")))
    )
  }

  # ── CSF: Selector de escenarios ──────────────────────────────────────────
  output$csf_scenario_ui <- renderUI({
    l  <- lang()
    sc <- rv$csf_scenario

    svg_llano <- '<svg viewBox="0 0 80 38" width="52" height="22" xmlns="http://www.w3.org/2000/svg"><polygon points="0,24 80,24 80,38 0,38" fill="rgba(45,122,61,0.12)"/><polyline points="0,24 80,24" stroke="#2d7a3d" stroke-width="2.5" fill="none" stroke-linecap="round"/></svg>'
    svg_ondulado <- '<svg viewBox="0 0 80 38" width="52" height="22" xmlns="http://www.w3.org/2000/svg"><path d="M0,28 C14,28 14,12 28,20 C42,28 42,10 58,16 C68,20 74,16 80,18 L80,38 L0,38 Z" fill="rgba(45,122,61,0.12)"/><path d="M0,28 C14,28 14,12 28,20 C42,28 42,10 58,16 C68,20 74,16 80,18" stroke="#2d7a3d" stroke-width="2.5" fill="none" stroke-linecap="round"/></svg>'
    svg_quebrado <- '<svg viewBox="0 0 80 38" width="52" height="22" xmlns="http://www.w3.org/2000/svg">
  <!-- Relleno -->
  <polygon points="
    0,28 
    10,24 
    16,30 
    24,14 
    32,28 
    40,18 
    48,30 
    56,12 
    64,28 
    72,16 
    80,28 
    80,38 
    0,38"
    fill="rgba(45,122,61,0.12)"/>

  <!-- Línea -->
  <polyline points="
    0,28 
    10,24 
    16,30 
    24,14 
    32,28 
    40,18 
    48,30 
    56,12 
    64,28 
    72,16 
    80,28"
    stroke="#2d7a3d"
    stroke-width="2.5"
    fill="none"
    stroke-linecap="butt"
    stroke-linejoin="miter"/>
</svg>'


    mk <- function(id, svg, title, desc) {
      cls <- paste("csf-scenario-card", if (sc == id) "csf-selected" else "")
      div(class = "col-4",
        div(class = cls,
          onclick = sprintf("Shiny.setInputValue('csf_scenario_click','%s',{priority:'event'})", id),
          HTML(svg),
          div(class = "csf-scenario-title", title),
          div(class = "csf-scenario-desc",  desc)
        )
      )
    }

    div(class = "row g-2 mb-1",
      mk("llano",    svg_llano,    tr("presets.csf.flat_title",   l), tr("presets.csf.flat_desc",   l)),
      mk("ondulado", svg_ondulado, tr("presets.csf.wavy_title",   l), tr("presets.csf.wavy_desc",   l)),
      mk("quebrado", svg_quebrado, tr("presets.csf.rugged_title", l), tr("presets.csf.rugged_desc", l))
    )
  })

  output$csf_custom_badge_ui <- renderUI({
    l <- lang()
    if (isTRUE(rv$csf_custom))
      div(class = "text-center mb-1",
        tags$span(class = "csf-custom-badge", tr("presets.csf.custom_badge", l)))
    else
      NULL
  })

  observeEvent(input$csf_scenario_click, {
    sc <- input$csf_scenario_click
    rv$csf_scenario  <- sc
    rv$csf_custom    <- FALSE
    p <- csf_presets[[sc]]
    rv$csf_rigidness    <- as.integer(p$rigidness)
    rv$csf_threshold    <- p$threshold
    rv$csf_cloth_res    <- p$cloth_res
    rv$csf_sloop_smooth <- p$sloop_smooth
  })

  # ── CSF: Modal de configuración avanzada ─────────────────────────────────
  observeEvent(input$btn_csf_avanzado, {
    l       <- lang()
    p       <- csf_presets[[rv$csf_scenario %||% "ondulado"]]
    cur_res <- rv$csf_cloth_res    %||% p$cloth_res
    cur_thr <- rv$csf_threshold    %||% p$threshold
    cur_slp <- rv$csf_sloop_smooth %||% p$sloop_smooth

    showModal(modalDialog(
      title = tagList(
        tr("presets.csf.modal_title", l),
        if (isTRUE(rv$csf_custom))
          tags$span(class = "csf-custom-badge ms-2", tr("presets.csf.custom_badge_modal", l))
      ),
      tags$p(style = "font-size:12px;color:#666;margin-bottom:18px;",
        tr("presets.csf.modal_desc", l)),
      div(class = "mb-3",
        tip_label(tr("presets.csf.cloth_res_label", l), tr("presets.csf.cloth_res_tip", l)),
        sliderInput("modal_csf_cloth_res", "", 0.5, 4, cur_res, step = 0.5, width = "100%")
      ),
      div(class = "mb-3",
        tip_label(tr("presets.csf.threshold_label", l), tr("presets.csf.threshold_tip", l)),
        sliderInput("modal_csf_threshold", "", 0.1, 2, cur_thr, step = 0.1, width = "100%")
      ),
      div(class = "mb-2",
        tip_label(tr("presets.csf.slope_label", l), tr("presets.csf.slope_tip", l)),
        checkboxInput("modal_csf_sloop_smooth", tr("presets.csf.slope_checkbox", l), value = cur_slp)
      ),
      footer = tagList(
        actionButton("btn_csf_restablecer", tr("common.btn.reset", l),
          class = "btn btn-outline-secondary btn-sm me-auto"),
        modalButton(tr("common.btn.cancel", l)),
        actionButton("btn_csf_aplicar", tr("common.btn.apply_config", l),
          class = "btn btn-success")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })

  observeEvent(input$btn_csf_aplicar, {
    rv$csf_cloth_res    <- input$modal_csf_cloth_res
    rv$csf_threshold    <- input$modal_csf_threshold
    rv$csf_sloop_smooth <- input$modal_csf_sloop_smooth
    p <- csf_presets[[rv$csf_scenario %||% "ondulado"]]
    rv$csf_custom <- !(
      input$modal_csf_cloth_res == p$cloth_res &&
      input$modal_csf_threshold == p$threshold &&
      isTRUE(input$modal_csf_sloop_smooth) == isTRUE(p$sloop_smooth)
    )
    removeModal()
  })

  observeEvent(input$btn_csf_restablecer, {
    p <- csf_presets[[rv$csf_scenario %||% "ondulado"]]
    updateSliderInput( session, "modal_csf_cloth_res",    value = p$cloth_res)
    updateSliderInput( session, "modal_csf_threshold",    value = p$threshold)
    updateCheckboxInput(session,"modal_csf_sloop_smooth", value = p$sloop_smooth)
  })

  # ── Densidad: presets ─────────────────────────────────────────────────────
  densidad_presets <- list(bajo = 10, medio = 25, alto = 50)

  observeEvent(input$densidad_nivel_click, {
    nivel <- input$densidad_nivel_click
    if (!nivel %in% names(densidad_presets)) return()
    rv$densidad_nivel  <- nivel
    rv$densidad_custom <- FALSE
    rv$densidad_val    <- densidad_presets[[nivel]]
  })

  observeEvent(input$btn_densidad_avanzado, {
    l      <- lang()
    actual <- isolate(rv$densidad_val) %||% densidad_presets[[isolate(rv$densidad_nivel) %||% "medio"]]
    showModal(modalDialog(
      title = tr("presets.density.custom_modal_title", l),
      easyClose = TRUE,
      footer = tagList(
        actionButton("btn_densidad_aplicar",     tr("common.btn.apply", l),   class = "btn-primary"),
        actionButton("btn_densidad_restablecer", tr("common.btn.restore", l), class = "btn-secondary"),
        modalButton(tr("common.btn.cancel", l))
      ),
      numericInput("modal_densidad_custom", tr("presets.density.custom_input_label", l),
                   value = actual, min = 1, max = 500, step = 1)
    ))
  })

  observeEvent(input$btn_densidad_aplicar, {
    val <- input$modal_densidad_custom
    req(!is.null(val), !is.na(val), val >= 1)
    rv$densidad_val    <- val
    rv$densidad_custom <- TRUE
    removeModal()
  })

  observeEvent(input$btn_densidad_restablecer, {
    preset_val <- densidad_presets[[rv$densidad_nivel %||% "medio"]]
    updateNumericInput(session, "modal_densidad_custom", value = preset_val)
  })

  output$densidad_ui <- renderUI({
    l           <- lang()
    nivel_actual <- rv$densidad_nivel %||% "medio"

    mk_d <- function(id, titulo, desc, dots_svg) {
      sel <- identical(nivel_actual, id)
      div(class = paste("density-card", if(sel) "density-selected"),
        onclick = sprintf("Shiny.setInputValue('densidad_nivel_click','%s',{priority:'event'})", id),
        div(class = "density-dots-area", HTML(dots_svg)),
        div(class = "density-card-title", titulo),
        div(class = "density-card-desc",  desc)
      )
    }

    dot <- function(cx, cy, r = 3)
      sprintf('<circle cx="%s" cy="%s" r="%s" fill="currentColor"/>', cx, cy, r)

    svg_wrap <- function(...) {
      sprintf('<svg viewBox="0 0 56 36" width="44" height="28" style="display:block;margin:0 auto;">%s</svg>',
              paste0(...))
    }

    dots_bajo <- svg_wrap(
      dot(8,8), dot(28,8), dot(48,8),
      dot(18,20), dot(38,20),
      dot(8,32), dot(28,32), dot(48,32)
    )
    dots_medio <- svg_wrap(
      dot(5,5,2.5), dot(16,5,2.5), dot(28,5,2.5), dot(40,5,2.5), dot(51,5,2.5),
      dot(10,14,2.5), dot(22,14,2.5), dot(34,14,2.5), dot(46,14,2.5),
      dot(5,23,2.5), dot(16,23,2.5), dot(28,23,2.5), dot(40,23,2.5), dot(51,23,2.5),
      dot(10,32,2.5), dot(22,32,2.5), dot(34,32,2.5), dot(46,32,2.5)
    )
    dots_alto <- svg_wrap(
      dot(4,4,2), dot(12,4,2), dot(20,4,2), dot(28,4,2), dot(36,4,2), dot(44,4,2), dot(52,4,2),
      dot(8,11,2), dot(16,11,2), dot(24,11,2), dot(32,11,2), dot(40,11,2), dot(48,11,2),
      dot(4,18,2), dot(12,18,2), dot(20,18,2), dot(28,18,2), dot(36,18,2), dot(44,18,2), dot(52,18,2),
      dot(8,25,2), dot(16,25,2), dot(24,25,2), dot(32,25,2), dot(40,25,2), dot(48,25,2),
      dot(4,32,2), dot(12,32,2), dot(20,32,2), dot(28,32,2), dot(36,32,2), dot(44,32,2), dot(52,32,2)
    )

    tagList(
      div(class = "row g-2 mb-2",
        div(class="col-4", mk_d("bajo",  tr("presets.density.low_label",    l), tr("presets.density.low_desc",    l), dots_bajo)),
        div(class="col-4", mk_d("medio", tr("presets.density.medium_label", l), tr("presets.density.medium_desc", l), dots_medio)),
        div(class="col-4", mk_d("alto",  tr("presets.density.high_label",   l), tr("presets.density.high_desc",   l), dots_alto))
      ),
      if (isTRUE(rv$densidad_custom))
        div(class = "text-center mb-1",
          tags$span(class = "csf-custom-badge", tr("presets.density.custom_badge", l)))
    )
  })

  # ── Cards DEM ─────────────────────────────────────────────────────────────
  output$ui_dem_res_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$dem_res_sel, rv$dem_res_custom, list(
      list(id="alta",  val=0.25, label=tr("presets.dem.res_high_label",   l), desc=tr("presets.dem.res_high_desc",   l), svg=svg_grid_fine),
      list(id="media", val=0.5,  label=tr("presets.dem.res_medium_label", l), desc=tr("presets.dem.res_medium_desc", l), svg=svg_grid_med),
      list(id="baja",  val=1,    label=tr("presets.dem.res_low_label",    l), desc=tr("presets.dem.res_low_desc",    l), svg=svg_grid_coarse)
    ), "click_dem_res", "m", l)
  })

  output$ui_cn_equidist_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$cn_equidist_sel, rv$cn_equidist_custom, list(
      list(id="c05", val=0.5, label=tr("presets.dem.contour_1_label", l), desc=tr("presets.dem.contour_1_desc", l), svg=svg_contour_dense),
      list(id="c1",  val=1,   label=tr("presets.dem.contour_2_label", l), desc=tr("presets.dem.contour_2_desc", l), svg=svg_contour_med),
      list(id="c5",  val=5,   label=tr("presets.dem.contour_3_label", l), desc=tr("presets.dem.contour_3_desc", l), svg=svg_contour_sparse)
    ), "click_cn_equidist", "m", l)
  })

  output$ui_dem_win_min_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$dem_win_min_sel, rv$dem_win_min_custom, list(
      list(id="cons", val=3, label=tr("presets.dem.filter_conservative_label", l), desc=tr("presets.dem.filter_conservative_desc", l), svg=svg_minfilter_cons),
      list(id="med",  val=5, label=tr("presets.dem.filter_medium_label",       l), desc=tr("presets.dem.filter_medium_desc",       l), svg=svg_minfilter_med),
      list(id="agr",  val=9, label=tr("presets.dem.filter_aggressive_label",   l), desc=tr("presets.dem.filter_aggressive_desc",   l), svg=svg_minfilter_agr)
    ), "click_dem_win_min", "px", l)
  })

  output$ui_dem_win_mean_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$dem_win_mean_sel, rv$dem_win_mean_custom, list(
      list(id="cons", val=5,  label=tr("presets.dem.smooth_conservative_label", l), desc=tr("presets.dem.smooth_conservative_desc", l), svg=svg_smooth_cons),
      list(id="med",  val=15, label=tr("presets.dem.smooth_medium_label",       l), desc=tr("presets.dem.smooth_medium_desc",       l), svg=svg_smooth_med),
      list(id="agr",  val=27, label=tr("presets.dem.smooth_aggressive_label",   l), desc=tr("presets.dem.smooth_aggressive_desc",   l), svg=svg_smooth_agr)
    ), "click_dem_win_mean", "px", l)
  })

  output$ui_chm_res_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$chm_res_sel, rv$chm_res_custom, list(
      list(id="alta",  val=0.25, label=tr("presets.chm.res_high_label",   l), desc=tr("presets.chm.res_high_desc",   l), svg=svg_grid_fine),
      list(id="media", val=0.5,  label=tr("presets.chm.res_medium_label", l), desc=tr("presets.chm.res_medium_desc", l), svg=svg_grid_med),
      list(id="baja",  val=1,    label=tr("presets.chm.res_low_label",    l), desc=tr("presets.chm.res_low_desc",    l), svg=svg_grid_coarse)
    ), "click_chm_res", "m", l)
  })

  output$ui_chm_subcirc_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$chm_subcirc_sel, rv$chm_subcirc_custom, list(
      list(id="cons", val=0.025, label=tr("presets.chm.gap_conservative_label", l), desc=tr("presets.chm.gap_conservative_desc", l), svg=svg_gap_cons),
      list(id="med",  val=0.25,  label=tr("presets.chm.gap_medium_label",       l), desc=tr("presets.chm.gap_medium_desc",       l), svg=svg_gap_med),
      list(id="agr",  val=0.5,   label=tr("presets.chm.gap_aggressive_label",   l), desc=tr("presets.chm.gap_aggressive_desc",   l), svg=svg_gap_agr)
    ), "click_chm_subcirc", "m", l)
  })

  # Click handlers DEM
  observeEvent(input$click_dem_res, {
    v <- c(alta=0.25, media=0.5, baja=1)[input$click_dem_res]
    rv$dem_res_sel <- v; rv$dem_res_custom <- FALSE
  })
  observeEvent(input$click_cn_equidist, {
    v <- c(c05=0.5, c1=1, c5=5)[input$click_cn_equidist]
    rv$cn_equidist_sel <- v; rv$cn_equidist_custom <- FALSE
  })
  observeEvent(input$click_dem_win_min, {
    v <- c(cons=3, med=5, agr=9)[input$click_dem_win_min]
    rv$dem_win_min_sel <- v; rv$dem_win_min_custom <- FALSE
  })
  observeEvent(input$click_dem_win_mean, {
    v <- c(cons=5, med=15, agr=27)[input$click_dem_win_mean]
    rv$dem_win_mean_sel <- v; rv$dem_win_mean_custom <- FALSE
  })
  observeEvent(input$click_chm_res, {
    v <- c(alta=0.25, media=0.5, baja=1)[input$click_chm_res]
    rv$chm_res_sel <- v; rv$chm_res_custom <- FALSE
  })
  observeEvent(input$click_chm_subcirc, {
    v <- c(cons=0.025, med=0.25, agr=0.5)[input$click_chm_subcirc]
    rv$chm_subcirc_sel <- v; rv$chm_subcirc_custom <- FALSE
  })

  # ── Modal DEM avanzado ────────────────────────────────────────────────────
  observeEvent(input$btn_dem_avanzado, {
    l <- lang()
    showModal(modalDialog(
      title = tr("presets.dem.modal_title", l),
      div(class = "mb-3",
        tip_label(tr("presets.dem.res_label",        l), tr("presets.dem.res_tip",        l)),
        sliderInput("modal_dem_res", NULL, min=0.25, max=5, value=rv$dem_res_sel, step=0.25, ticks=TRUE, width="100%")
      ),
      div(class = "mb-3",
        tip_label(tr("presets.dem.contour_label",    l), tr("presets.dem.contour_tip",    l)),
        sliderInput("modal_cn_equidist", NULL, min=0.25, max=10, value=rv$cn_equidist_sel, step=0.25, ticks=TRUE, width="100%")
      ),
      div(class = "mb-3",
        tip_label(tr("presets.dem.min_filter_label", l), tr("presets.dem.min_filter_tip", l)),
        sliderInput("modal_dem_win_min", NULL, min=3, max=15, value=rv$dem_win_min_sel, step=2, ticks=TRUE, width="100%")
      ),
      div(class = "mb-2",
        tip_label(tr("presets.dem.mean_filter_label",l), tr("presets.dem.mean_filter_tip",l)),
        sliderInput("modal_dem_win_mean", NULL, min=3, max=81, value=rv$dem_win_mean_sel, step=2, ticks=TRUE, width="100%")
      ),
      footer = tagList(
        actionButton("btn_dem_config_reset", tr("common.btn.reset", l),
          class = "btn btn-outline-secondary btn-sm me-auto"),
        modalButton(tr("common.btn.cancel", l)),
        actionButton("btn_dem_config_aplicar", tr("common.btn.apply_config", l),
          class = "btn btn-success")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })

  observeEvent(input$btn_dem_config_aplicar, {
    apply_rv <- function(v, rv_sel, rv_custom, presets) {
      rv[[rv_sel]] <- v
      rv[[rv_custom]] <- !any(abs(v - presets) < 1e-9)
    }
    apply_rv(input$modal_dem_res,      "dem_res_sel",      "dem_res_custom",      c(0.25, 0.5, 1))
    apply_rv(input$modal_cn_equidist,  "cn_equidist_sel",  "cn_equidist_custom",  c(0.5, 1, 5))
    apply_rv(input$modal_dem_win_min,  "dem_win_min_sel",  "dem_win_min_custom",  c(3, 5, 9))
    apply_rv(input$modal_dem_win_mean, "dem_win_mean_sel", "dem_win_mean_custom", c(5, 15, 27))
    removeModal()
  })

  observeEvent(input$btn_dem_config_reset, {
    updateSliderInput(session, "modal_dem_res",      value = 1)
    updateSliderInput(session, "modal_cn_equidist",  value = 1)
    updateSliderInput(session, "modal_dem_win_min",  value = 3)
    updateSliderInput(session, "modal_dem_win_mean", value = 9)
  })

  # ── Cards Árboles ─────────────────────────────────────────────────────────
  output$ui_lmf_ws_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$lmf_ws_sel, rv$lmf_ws_custom, list(
      list(id="chica",   val=4, label=tr("presets.tree.ws_small_label",  l), desc=tr("presets.tree.ws_small_desc",  l), svg=svg_ws_chica),
      list(id="mediana", val=6, label=tr("presets.tree.ws_medium_label", l), desc=tr("presets.tree.ws_medium_desc", l), svg=svg_ws_mediana),
      list(id="grande",  val=8, label=tr("presets.tree.ws_large_label",  l), desc=tr("presets.tree.ws_large_desc",  l), svg=svg_ws_grande)
    ), "click_lmf_ws", "m", l)
  })

  output$ui_lmf_hmin_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$lmf_hmin_sel, rv$lmf_hmin_custom, list(
      list(id="baja",  val=5,  label=tr("presets.tree.hmin_low_label",    l), desc=tr("presets.tree.hmin_low_desc",    l), svg=svg_hmin_baja),
      list(id="media", val=10, label=tr("presets.tree.hmin_medium_label", l), desc=tr("presets.tree.hmin_medium_desc", l), svg=svg_hmin_media),
      list(id="alta",  val=15, label=tr("presets.tree.hmin_high_label",   l), desc=tr("presets.tree.hmin_high_desc",   l), svg=svg_hmin_alta)
    ), "click_lmf_hmin", "m", l)
  })

  output$ui_canopy_cutoff_cards <- renderUI({
    l <- lang()
    .render_preset_cards(rv$canopy_cutoff_sel, rv$canopy_cutoff_custom, list(
      list(id="baja",  val=2, label=tr("presets.tree.cutoff_low_label",    l), desc=tr("presets.tree.cutoff_low_desc",    l), svg=svg_cutoff_baja),
      list(id="media", val=4, label=tr("presets.tree.cutoff_medium_label", l), desc=tr("presets.tree.cutoff_medium_desc", l), svg=svg_cutoff_media),
      list(id="alta",  val=8, label=tr("presets.tree.cutoff_high_label",   l), desc=tr("presets.tree.cutoff_high_desc",   l), svg=svg_cutoff_alta)
    ), "click_canopy_cutoff", "m", l)
  })

  # Click handlers Árboles
  observeEvent(input$click_lmf_ws, {
    v <- as.numeric(c(chica=4, mediana=6, grande=8)[input$click_lmf_ws])
    rv$lmf_ws_sel <- v; rv$lmf_ws_custom <- FALSE
  })
  observeEvent(input$click_lmf_hmin, {
    v <- as.numeric(c(baja=5, media=10, alta=15)[input$click_lmf_hmin])
    rv$lmf_hmin_sel <- v; rv$lmf_hmin_custom <- FALSE
  })
  observeEvent(input$click_canopy_cutoff, {
    v <- as.numeric(c(baja=2, media=4, alta=8)[input$click_canopy_cutoff])
    rv$canopy_cutoff_sel <- v; rv$canopy_cutoff_custom <- FALSE
  })

  # Botón Aplicar del modal Árboles
  observeEvent(input$btn_arb_config_aplicar, {
    apply_rv_arb <- function(v, rv_sel, rv_custom, presets) {
      rv[[rv_sel]] <- v
      rv[[rv_custom]] <- !any(abs(v - presets) < 1e-9)
    }
    apply_rv_arb(input$modal_lmf_ws,        "lmf_ws_sel",        "lmf_ws_custom",        c(4, 6, 8))
    apply_rv_arb(input$modal_lmf_hmin,      "lmf_hmin_sel",      "lmf_hmin_custom",      c(5, 10, 15))
    apply_rv_arb(input$modal_canopy_cutoff, "canopy_cutoff_sel", "canopy_cutoff_custom", c(2, 4, 8))
  })

  # ── Modal CHM avanzado ────────────────────────────────────────────────────
  observeEvent(input$btn_chm_avanzado, {
    l <- lang()
    showModal(modalDialog(
      title = tr("presets.chm.modal_title", l),
      div(class = "mb-3",
        tip_label(tr("presets.chm.res_label", l), tr("presets.chm.res_tip", l)),
        sliderInput("modal_chm_res", NULL, min=0.1, max=2, value=rv$chm_res_sel, step=0.05, ticks=FALSE, width="100%")
      ),
      div(class = "mb-2",
        tip_label(tr("presets.chm.gap_label", l), tr("presets.chm.gap_tip", l)),
        sliderInput("modal_chm_subcirc", NULL, min=0, max=0.5, value=rv$chm_subcirc_sel, step=0.005, ticks=FALSE, width="100%")
      ),
      footer = tagList(
        actionButton("btn_chm_config_reset", tr("common.btn.reset", l),
          class = "btn btn-outline-secondary btn-sm me-auto"),
        modalButton(tr("common.btn.cancel", l)),
        actionButton("btn_chm_config_aplicar", tr("common.btn.apply_config", l),
          class = "btn btn-success")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })

  observeEvent(input$btn_chm_config_aplicar, {
    apply_rv_chm <- function(v, rv_sel, rv_custom, presets) {
      rv[[rv_sel]] <- v
      rv[[rv_custom]] <- !any(abs(v - presets) < 1e-9)
    }
    apply_rv_chm(input$modal_chm_res,     "chm_res_sel",     "chm_res_custom",     c(0.25, 0.5, 1))
    apply_rv_chm(input$modal_chm_subcirc, "chm_subcirc_sel", "chm_subcirc_custom", c(0.025, 0.25, 0.5))
    removeModal()
  })

  observeEvent(input$btn_chm_config_reset, {
    updateSliderInput(session, "modal_chm_res",     value = 0.5)
    updateSliderInput(session, "modal_chm_subcirc", value = 0.025)
  })

  # Sincronizar sliders del modal cuando cambia rv$ (p.ej. al seleccionar un card)
  observe({
    updateSliderInput(session, "modal_dem_res",        value = rv$dem_res_sel)
    updateSliderInput(session, "modal_cn_equidist",    value = rv$cn_equidist_sel)
    updateSliderInput(session, "modal_dem_win_min",    value = rv$dem_win_min_sel)
    updateSliderInput(session, "modal_dem_win_mean",   value = rv$dem_win_mean_sel)
    updateSliderInput(session, "modal_chm_res",        value = rv$chm_res_sel)
    updateSliderInput(session, "modal_chm_subcirc",    value = rv$chm_subcirc_sel)
    updateSliderInput(session, "modal_lmf_ws",         value = rv$lmf_ws_sel)
    updateSliderInput(session, "modal_lmf_hmin",       value = rv$lmf_hmin_sel)
    updateSliderInput(session, "modal_canopy_cutoff",  value = rv$canopy_cutoff_sel)
  })
}
