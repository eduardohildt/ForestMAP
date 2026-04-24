# =============================================================================
# GENERADOR DE REPORTE EN HTML - ForestMap INTA
# =============================================================================
# Autor: Dr. Eduardo Hildt - INTA EEA Montecarlo
# Versión: 2026.5
# =============================================================================

#' Generar informe descriptivo HTML de análisis LiDAR.
#'
#' @param rv ReactiveValues object containing inputs, outputs, statistics, and data.
#' @param input Shiny input list with user-set parameters.
#' @param lang Language code: "es", "en", or "pt" (default: "es").
#' @param log_fn Function to log status messages (default: message).
#' @return Path to generated HTML.
#' @export
generar_informe_descriptivo <- function(rv, input, lang = "es", log_fn = message) {
  if (is.null(rv$ruta_dir) || !nzchar(rv$ruta_dir)) {
    stop(tr("report.error.invalid_dir", lang))
  }

  log_fn(tr("report.log.generating", lang))

  # ============================================================================
  # VARIABLES BÁSICAS
  # ============================================================================
  nombre_las  <- basename(rv$ruta_las %||% "N/A")
  nombre_roi  <- if (isTRUE(rv$usar_extension_completa))
                   tr("config.log.roi_bbox", lang)
                 else
                   basename(rv$ruta_shp %||% "N/A")
  area_ha     <- if (!is.na(rv$area_ha)) sprintf("%.2f", rv$area_ha) else "N/D"
  dens_orig_v <- if (!is.na(rv$dens_orig)) sprintf("%.1f", rv$dens_orig) else "N/D"
  dens_sub    <- as.character(rv$densidad_val)
  rig_label   <- switch(as.character(rv$csf_rigidness),
                        "1" = tr("processing.csf.quebrado", lang),
                        "2" = tr("processing.csf.ondulado", lang),
                        "3" = tr("processing.csf.llano",    lang))
  n_pts_orig  <- if (!is.null(rv$las_raw))      format(nrow(rv$las_raw@data),     big.mark=".") else "N/D"
  n_pts_filt  <- if (!is.null(rv$las_filtrado)) format(nrow(rv$las_filtrado@data), big.mark=".") else "N/D"
  n_suelo     <- if (!is.null(rv$las_clasf))    format(sum(rv$las_clasf@data$Classification==2L, na.rm=TRUE), big.mark=".") else "N/D"
  dem_min_v   <- if (!is.na(rv$dem_min))  sprintf("%.1f", rv$dem_min)  else "N/D"
  dem_max_v   <- if (!is.na(rv$dem_max))  sprintf("%.1f", rv$dem_max)  else "N/D"
  dem_mean_v  <- if (!is.na(rv$dem_mean)) sprintf("%.1f", rv$dem_mean) else "N/D"
  dem_rango_v <- if (!is.na(rv$dem_min) && !is.na(rv$dem_max)) sprintf("%.1f", rv$dem_max - rv$dem_min) else "N/D"
  cn_equi_v   <- sprintf("%.2f", rv$cn_equidist_sel)
  chm_min_v   <- if (!is.na(rv$chm_min))  sprintf("%.1f", rv$chm_min)  else "N/D"
  chm_max_v   <- if (!is.na(rv$chm_max))  sprintf("%.1f", rv$chm_max)  else "N/D"
  chm_mean_v  <- if (!is.na(rv$chm_mean)) sprintf("%.1f", rv$chm_mean) else "N/D"
  n_arb       <- if (!is.null(rv$arboles)) as.character(nrow(rv$arboles)) else "N/D"
  alt_med_v   <- if (!is.null(rv$arboles)) sprintf("%.1f", mean(rv$arboles$Z, na.rm=TRUE)) else "N/D"
  alt_max_v   <- if (!is.null(rv$arboles)) sprintf("%.1f", max(rv$arboles$Z, na.rm=TRUE))  else "N/D"
  alt_min_v   <- if (!is.null(rv$arboles)) sprintf("%.1f", min(rv$arboles$Z, na.rm=TRUE))  else "N/D"

  # ============================================================================
  # MÉTRICAS DE GESTIÓN FORESTAL
  # ============================================================================

  dens_arb_ha <- if (!is.null(rv$arboles) && !is.na(rv$area_ha) && rv$area_ha > 0) {
    sprintf("%.1f", nrow(rv$arboles) / rv$area_ha)
  } else "N/D"

  espac_medio <- if (!is.null(rv$arboles) && !is.na(rv$area_ha) && rv$area_ha > 0 && nrow(rv$arboles) > 0) {
    sprintf("%.1f", sqrt(10000 / (nrow(rv$arboles) / rv$area_ha)))
  } else "N/D"

  cv_altura <- if (!is.null(rv$arboles) && nrow(rv$arboles) > 1) {
    cv_val <- sd(rv$arboles$Z, na.rm=TRUE) / mean(rv$arboles$Z, na.rm=TRUE) * 100
    sprintf("%.1f", cv_val)
  } else "N/D"

  cv_eval <- if (cv_altura != "N/D") {
    cv_num <- as.numeric(cv_altura)
    if      (cv_num < 10) tr("report.homogeneity.very_homogeneous",         lang)
    else if (cv_num < 20) tr("report.homogeneity.moderately_homogeneous",   lang)
    else if (cv_num < 30) tr("report.homogeneity.moderately_heterogeneous", lang)
    else                  tr("report.homogeneity.highly_heterogeneous",     lang)
  } else "N/D"

  cob_pct <- if (!is.null(rv$cobertura_copas)) {
    sprintf("%.1f", rv$cobertura_copas$porc_cobertura)
  } else "N/D"

  cob_area_ha <- if (!is.null(rv$cobertura_copas)) {
    sprintf("%.2f", rv$cobertura_copas$area_copa_total / 10000)
  } else "N/D"

  cob_n_polig <- if (!is.null(rv$cobertura_copas)) {
    as.character(rv$cobertura_copas$n_poligonos)
  } else "N/D"

  cob_umbral <- if (!is.null(rv$cobertura_copas)) {
    sprintf("%.1f", rv$cobertura_copas$umbral_altura)
  } else "N/D"

  slope_mean <- "N/D"
  slope_max <- "N/D"
  area_0_15 <- 0
  area_15_30 <- 0
  area_30plus <- 0
  pct_0_15 <- "N/D"
  pct_15_30 <- "N/D"
  pct_30plus <- "N/D"

  if (!is.null(rv$dem_suav) && !is.na(rv$area_ha) && rv$area_ha > 0) {
    tryCatch({
      slope_rast <- terrain(rv$dem_suav, v = "slope", unit = "degrees")
      slope_mean <- sprintf("%.1f", global(slope_rast, "mean", na.rm=TRUE)[[1]])
      slope_max  <- sprintf("%.1f", global(slope_rast, "max",  na.rm=TRUE)[[1]])
      slope_vals <- values(slope_rast, mat=FALSE)
      pixel_area_ha <- res(slope_rast)[1]^2 / 10000
      area_0_15   <- sum(slope_vals < 15,  na.rm=TRUE) * pixel_area_ha
      area_15_30  <- sum(slope_vals >= 15 & slope_vals < 30, na.rm=TRUE) * pixel_area_ha
      area_30plus <- sum(slope_vals >= 30, na.rm=TRUE) * pixel_area_ha
      pct_0_15    <- sprintf("%.1f", area_0_15 / rv$area_ha * 100)
      pct_15_30   <- sprintf("%.1f", area_15_30 / rv$area_ha * 100)
      pct_30plus  <- sprintf("%.1f", area_30plus / rv$area_ha * 100)
      area_0_15   <- sprintf("%.2f", area_0_15)
      area_15_30  <- sprintf("%.2f", area_15_30)
      area_30plus <- sprintf("%.2f", area_30plus)
    }, error = function(e) {
      log_fn(tr("report.log.slope_warning", lang, conditionMessage(e)))
    })
  }

  # ============================================================================
  # GENERACIÓN DE FIGURAS
  # ============================================================================
  dir_inf <- file.path(rv$ruta_dir, "Salidas_INFORME")
  if (!dir.exists(dir_inf)) dir.create(dir_inf, recursive = TRUE)
  # Unified ggplot2 theme and save helper
  gg_theme_cart <- theme_minimal() + theme(
    axis.title = element_blank(),
    axis.text = element_text(size=8),
    plot.title = element_text(face="bold", size=10),
    legend.title = element_text(size=8)
  )

  save_plot <- function(plot_obj, path, w_px=1600, h_px=1600, dpi=150) {
    tryCatch({
      ggsave(filename=path, plot=plot_obj,
             width = w_px / dpi, height = h_px / dpi, units = "in", dpi = dpi, device = "png")
    }, error = function(e) {
      if (file.exists(path)) tryCatch(file.remove(path), error = function(e2) NULL)
      log_fn(tr("report.log.figure_save_error", lang, conditionMessage(e)))
    })
  }

  if (!is.null(rv$dem_suav)) {
    df_dem <- raster_to_df(rv$dem_suav)
    p_dem <- ggplot(df_dem, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours = terrain.colors(100), na.value = "transparent") +
      coord_equal() + gg_theme_cart +
      labs(title = tr("report.fig.dem", lang))
    if (!is.null(rv$curvas)) {
      curvas_sf <- tryCatch(st_as_sf(rv$curvas), error = function(e) NULL)
      if (!is.null(curvas_sf)) p_dem <- p_dem + geom_sf(data=curvas_sf, inherit.aes=FALSE, color="black", size=0.2)
    }
    save_plot(p_dem, file.path(dir_inf, "fig_dem.png"))
  }

  if (!is.null(rv$hillshade)) {
    df_hs <- raster_to_df(rv$hillshade)
    p_hs <- ggplot(df_hs, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours = grey(seq(0,1,.01)), guide = "none", na.value = "transparent") +
      coord_equal() + gg_theme_cart +
      labs(title = tr("report.fig.hillshade", lang))
    save_plot(p_hs, file.path(dir_inf, "fig_hillshade.png"))
  }

  if (!is.null(rv$chm)) {
    df_c <- raster_to_df(rv$chm)
    p_chm <- ggplot(df_c, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours = PAL_CHM_PNG, name = "Altura\n(m)", na.value = "white") +
      coord_equal() + gg_theme_cart +
      labs(title = tr("report.fig.chm", lang))
    save_plot(p_chm, file.path(dir_inf, "fig_chm.png"))
  }

  if (!is.null(rv$chm) && !is.null(rv$arboles)) {
    df_c <- raster_to_df(rv$chm)
    co_arb <- as.data.frame(st_coordinates(rv$arboles))
    co_arb$alt <- rv$arboles$Z
    p_arb <- ggplot(df_c, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours = PAL_CHM_PNG, name = "Altura\n(m)", na.value = "white") +
      geom_point(data=co_arb, aes(x=X, y=Y), inherit.aes=FALSE, shape=3, color="black", size=1, stroke=0.6) +
      coord_equal() + gg_theme_cart +
      labs(title = tr("report.fig.trees", lang, nrow(rv$arboles)))
    save_plot(p_arb, file.path(dir_inf, "fig_arboles.png"))
  }

  if (!is.null(rv$arboles)) {
    p_hist <- ggplot(data.frame(h=rv$arboles$Z), aes(x=h)) +
      geom_histogram(bins=25, fill=GREEN, color="white", alpha=0.85) +
      labs(title = tr("report.fig.tree_heights", lang),
           x = tr("plot.axis.apex_height", lang),
           y = tr("plot.axis.frequency", lang)) +
      gg_theme_cart
    save_plot(p_hist, file.path(dir_inf, "fig_hist_arb.png"), w_px=1200, h_px=1200)
  }

  if (!is.null(rv$cobertura_copas) && !is.null(rv$cobertura_copas$copas_vect)) {
    fig_path <- file.path(dir_inf, "fig_cobertura.png")
    tryCatch({
      copas_sf <- st_as_sf(rv$cobertura_copas$copas_vect)
      p_cob <- ggplot() +
        geom_sf(data=copas_sf, fill="green1", color="black", size=0.2) +
        coord_sf(expand=FALSE) + gg_theme_cart +
        labs(title = tr("report.fig.canopy", lang,
                        rv$cobertura_copas$porc_cobertura,
                        rv$cobertura_copas$umbral_altura))
      save_plot(p_cob, fig_path)
      log_fn(tr("report.log.figure_coverage", lang))
    }, error = function(e) {
      if (file.exists(fig_path)) tryCatch(file.remove(fig_path), error = function(e2) NULL)
      log_fn(tr("report.log.figure_coverage_error", lang, conditionMessage(e)))
    })
  }

  # ============================================================================
  # GENERACIÓN DEL DOCUMENTO RMD
  # ============================================================================
  rmd_p <- file.path(rv$ruta_dir, "informe.Rmd")
  out_h <- file.path(rv$ruta_dir, "Salidas_INFORME", "Informe_Analisis.html")
  out_p <- file.path(rv$ruta_dir, "Salidas_INFORME", "Informe_Analisis.pdf")

  writeLines(con = rmd_p, c(
    "---",
    paste0("title: '", tr("report.title", lang), "'"),
    paste0("subtitle: '", rv$cfg_destinatario, "'"),
    "date: '`r format(Sys.Date(), \"%d de %B de %Y\")`'",
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_depth: 3",
    "    number_sections: true",
    "    fig_caption: true",
    "    self_contained: false",
    "    df_print: paged",
    "    theme: yeti",
    "    highlight: tango",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE,",
    "                      fig.align='center', out.width='100%')",
    "```",
    "",
    "\\newpage",
    paste0("# ", tr("report.section.survey_data", lang)),
    "",
    paste0("| ", tr("report.table.field", lang), " | ", tr("report.table.detail", lang), " |"),
    "|---|---|",
    paste0("| ", tr("report.table.recipient", lang), " | ", rv$cfg_destinatario, " |"),
    paste0("| ", tr("report.table.analysis_date", lang), " | `r format(Sys.time(), '%Y-%m-%d %H:%M')` |"),
    paste0("| ", tr("report.table.pointcloud_file", lang), " | ", nombre_las, " |"),
    paste0("| ", tr("report.table.roi", lang), " | ", nombre_roi, " |"),
    paste0("| ", tr("report.table.analyst", lang), " | ", rv$cfg_autor, " |"),
    paste0("| ", tr("report.table.institution", lang), " | ", rv$cfg_institucion, " |"),
    paste0("| ", tr("report.table.email", lang), " | ", rv$cfg_email, " |"),
    "",
    paste0("# ", tr("report.section.area", lang)),
    "",
    tr("report.content.area", lang, area_ha),
    tr("report.content.lidr_note", lang),
    "",
    paste0("# ", tr("report.section.pointcloud", lang)),
    "",
    paste0("## ", tr("report.subsection.survey_characteristics", lang)),
    "",
    tr("report.content.pointcloud_density", lang, dens_orig_v, n_pts_orig),
    tr("report.content.pointcloud_subsample", lang, dens_sub, n_pts_filt),
    tr("report.content.pointcloud_noise", lang),
    "",
    paste0("## ", tr("report.section.soil", lang)),
    "",
    tr("report.content.soil_csf", lang, rig_label, rv$csf_threshold, rv$csf_cloth_res),
    tr("report.content.soil_result", lang, n_suelo),
    "",
    paste0("# ", tr("report.section.models", lang)),
    "",
    paste0("## ", tr("report.subsection.dem", lang)),
    "",
    tr("report.content.dem_generation", lang, rv$dem_res_sel),
    tr("report.content.dem_smoothing", lang, rv$dem_win_min_sel, rv$dem_win_min_sel, rv$dem_win_mean_sel, rv$dem_win_mean_sel),
    tr("report.content.dem_stats", lang, dem_min_v, dem_max_v, dem_mean_v, dem_rango_v),
    "",
    paste0("```{r fig-dem, fig.cap='", tr("report.fig.dem", lang), "'}"),
    paste0("if(file.exists('", file.path(dir_inf,"fig_dem.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_dem.png"), "')"),
    "```",
    "",
    paste0("```{r fig-hs, fig.cap='", tr("report.fig.hillshade", lang), "'}"),
    paste0("if(file.exists('", file.path(dir_inf,"fig_hillshade.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_hillshade.png"), "')"),
    "```",
    "",
    paste0("## ", tr("report.subsection.contours", lang)),
    "",
    tr("report.content.contours", lang, cn_equi_v),
    "",
    paste0("# ", tr("report.section.vertical", lang)),
    "",
    paste0("## ", tr("report.subsection.chm", lang)),
    "",
    tr("report.content.chm_normalization", lang),
    tr("report.content.chm_generation", lang, rv$chm_res_sel),
    tr("report.content.chm_stats", lang, chm_min_v, chm_max_v, chm_mean_v),
    "",
    paste0("```{r fig-chm, fig.cap='", tr("report.fig.chm", lang), "'}"),
    paste0("if(file.exists('", file.path(dir_inf,"fig_chm.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_chm.png"), "')"),
    "```",
    "",
    paste0("# ", tr("report.section.trees", lang)),
    "",
    tr("report.content.trees_lmf", lang),
    tr("report.content.trees_params", lang, rv$lmf_ws_sel, rv$lmf_hmin_sel),
    tr("report.content.trees_count", lang, n_arb),
    tr("report.content.trees_heights", lang, alt_min_v, alt_max_v, alt_med_v),
    "",
    paste0("```{r fig-arb, fig.cap='", tr("report.fig.trees", lang, n_arb), "'}"),
    paste0("if(file.exists('", file.path(dir_inf,"fig_arboles.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_arboles.png"), "')"),
    "```",
    "",
    paste0("```{r fig-hist, fig.cap='", tr("report.fig.tree_heights", lang), "', out.width='70%'}"),
    paste0("if(file.exists('", file.path(dir_inf,"fig_hist_arb.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_hist_arb.png"), "')"),
    "```",
    "",
    paste0("# ", tr("report.section.management", lang)),
    "",
    paste0("## ", tr("report.subsection.density", lang)),
    "",
    tr("report.content.density_sentence", lang, dens_arb_ha, espac_medio),
    tr("report.content.cv_sentence", lang, cv_altura, cv_eval),
    tr("report.content.cv_note", lang),
    "",
    paste0("## ", tr("report.subsection.canopy", lang)),
    "",
    tr("report.content.canopy_analysis", lang, cob_umbral, cob_pct, cob_area_ha),
    tr("report.content.canopy_importance", lang),
    "",
    if (file.exists(file.path(dir_inf, "fig_cobertura.png"))) {
      c(
        paste0("```{r fig-cobertura, fig.cap='", tr("report.fig.canopy", lang, as.numeric(cob_pct), as.numeric(cob_umbral)), "'}"),
        paste0("knitr::include_graphics('", file.path(dir_inf,"fig_cobertura.png"), "')"),
        "```",
        ""
      )
    } else {
      c("")
    },
    "",
    paste0("# ", tr("report.section.products", lang)),
    "",
    tr("report.content.products_intro", lang),
    "",
    tr("report.products.clouds_header", lang),
    "",
    tr("report.products.classified", lang),
    tr("report.products.normalized", lang),
    "",
    tr("report.products.raster_header", lang),
    "",
    tr("report.products.dem", lang),
    tr("report.products.chm", lang),
    tr("report.products.hillshade", lang),
    "",
    tr("report.products.vector_header", lang),
    "",
    tr("report.products.contours_shp", lang),
    tr("report.products.trees_shp", lang),
    tr("report.products.roi_shp", lang),
    tr("report.products.canopy_shp", lang),
    "",
    paste0("# ", tr("report.section.references", lang)),
    "",
    "Avery, T. E., & Burkhart, H. E. (2002). Forest Measurements (5th ed.). McGraw-Hill. Stand variability can be quantified using standard deviation or coefficient of variation of tree height and diameter.",
    "",
    "Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2021). *lidR: An R package for analysis of Airborne LiDAR Data.* Remote Sensing of Environment, 251, 112061. https://doi.org/10.1016/j.rse.2020.112061",
    "",
    "Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications.* R package version 4.x. https://cran.r-project.org/package=lidR",
    "",
    "Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). An easy-to-use airborne LiDAR data filtering method based on cloth simulation. *Remote Sensing*, 8(6), 501. https://doi.org/10.3390/rs8060501",
    "",
    "---",
    "",
    tr("report.content.footer", lang)
  ))

  rmarkdown::render(rmd_p, output_file=out_h,
                    envir=new.env(parent=globalenv()), quiet=TRUE)
  unlink(rmd_p)

  log_fn(paste("✅ HTML guardado en:", out_h))
  invisible(out_h)

  Sys.sleep(1)
  pagedown::chrome_print(input = out_h, output = out_p)
}
