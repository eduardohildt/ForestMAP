# =============================================================================
# GENERADOR DE REPORTE EN PDF - ForestMap INTA
# =============================================================================
# Autor: Dr. Eduardo Hildt - INTA EEA Montecarlo
# Versión: 2026.5
# =============================================================================

#' Generar informe descriptivo PDF de análisis LiDAR.
#'
#' @param rv ReactiveValues object containing inputs, outputs, statistics, and data.
#' @param input Shiny input list with user-set parameters.
#' @param lang Language code: "es", "en", or "pt" (default: "es").
#' @param log_fn Function to log status messages (default: message).
#' @return Path to generated PDF.
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
  nombre_roi  <- basename(rv$ruta_shp %||% "N/A")
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

  if (!is.null(rv$dem_suav)) {
    png(file.path(dir_inf, "fig_dem.png"), width=1600, height=1600, res=150)
    par(mar = c(4, 4, 3, 5))
    plot(rv$dem_suav,
         main = tr("report.fig.dem", lang),
         col  = terrain.colors(100),
         mar  = c(3, 3, 3, 5))
    if (!is.null(rv$curvas)) plot(rv$curvas, add=TRUE, col="black", lwd=0.8)
    dev.off()
  }

  if (!is.null(rv$hillshade)) {
    png(file.path(dir_inf, "fig_hillshade.png"), width=1600, height=1600, res=150)
    df_hs <- raster_to_df(rv$hillshade)
    p_hs <- ggplot(df_hs, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours=grey(seq(0,1,.01)), guide="none") +
      coord_equal() + theme_void() +
      labs(title = tr("report.fig.hillshade", lang)) +
      theme(plot.title=element_text(face="bold", size=10))
    print(p_hs); dev.off()
  }

  if (!is.null(rv$chm)) {
    png(file.path(dir_inf, "fig_chm.png"), width=1600, height=1600, res=150)
    df_c <- raster_to_df(rv$chm)
    p_chm <- ggplot(df_c, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours=PAL_CHM_PNG, name="Altura\n(m)", na.value="white") +
      coord_equal() + theme_void() +
      labs(title = tr("report.fig.chm", lang)) +
      theme(plot.title=element_text(face="bold", size=10), legend.title=element_text(size=8))
    print(p_chm); dev.off()
  }

  if (!is.null(rv$chm) && !is.null(rv$arboles)) {
    png(file.path(dir_inf, "fig_arboles.png"), width=1600, height=1600, res=150)
    df_c <- raster_to_df(rv$chm)
    co_arb <- as.data.frame(st_coordinates(rv$arboles))
    co_arb$alt <- rv$arboles$Z
    p_arb <- ggplot(df_c, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours=PAL_CHM_PNG, name="Altura\n(m)", na.value="white") +
      geom_point(data=co_arb, aes(x=X, y=Y), inherit.aes=FALSE, shape=3, color="black", size=1.2, stroke=0.6) +
      coord_equal() + theme_void() +
      labs(title = tr("report.fig.trees", lang, nrow(rv$arboles))) +
      theme(plot.title=element_text(face="bold", size=10), legend.title=element_text(size=8))
    print(p_arb); dev.off()
  }

  if (!is.null(rv$arboles)) {
    png(file.path(dir_inf, "fig_hist_arb.png"), width=1200, height=1200, res=150)
    p_hist <- ggplot(data.frame(h=rv$arboles$Z), aes(x=h)) +
      geom_histogram(bins=25, fill=GREEN, color="white", alpha=0.85) +
      labs(title = tr("report.fig.tree_heights", lang),
           x     = tr("plot.axis.apex_height",   lang),
           y     = tr("plot.axis.frequency",      lang)) +
      theme_bw()
    print(p_hist); dev.off()
  }

  if (!is.null(rv$cobertura_copas) && !is.null(rv$cobertura_copas$copas_vect)) {
    fig_path <- file.path(dir_inf, "fig_cobertura.png")
    tryCatch({
      png(fig_path, width=1600, height=1600, res=150)
      copas_sf <- st_as_sf(rv$cobertura_copas$copas_vect)
      p_cob <- ggplot(copas_sf) +
        geom_sf(fill="green1", color="black", linewidth=0.5) +
        coord_sf(expand=FALSE) + theme_void() +
        labs(title = tr("report.fig.canopy", lang,
                        rv$cobertura_copas$porc_cobertura,
                        rv$cobertura_copas$umbral_altura)) +
        theme(plot.title=element_text(face="bold", size=10))
      print(p_cob)
      dev.off()
      log_fn(tr("report.log.figure_coverage", lang))
    }, error = function(e) {
      if (dev.cur() != 1) dev.off()
      if (file.exists(fig_path)) tryCatch(file.remove(fig_path), error = function(e2) NULL)
      log_fn(tr("report.log.figure_coverage_error", lang, conditionMessage(e)))
    })
  }

  # ============================================================================
  # GENERACIÓN DEL DOCUMENTO RMD
  # ============================================================================
  rmd_p <- file.path(rv$ruta_dir, "informe.Rmd")
  pdf_p <- file.path(rv$ruta_dir, "Salidas_INFORME", "Informe_Analisis.pdf")

  writeLines(con = rmd_p, c(
    "---",
    paste0("title: '", tr("report.title", lang), "'"),
    paste0("subtitle: '", rv$cfg_destinatario, "'"),
    "date: '`r format(Sys.Date(), \"%d de %B de %Y\")`'",
    "output:",
    "  pdf_document:",
    "    toc: true",
    "    toc_depth: 3",
    "    number_sections: true",
    "    latex_engine: xelatex",
    "    fig_caption: true",
    "    keep_tex: false",
    "lang: es-ES",
    "fontsize: 11pt",
    "geometry: margin=2cm",
    "header-includes:",
    "  - \\setlength{\\headheight}{14pt}",
    "  - \\addtolength{\\topmargin}{-2pt}",
    "  - \\usepackage{fancyhdr}",
    "  - \\usepackage{graphicx}",
    "  - \\usepackage{xcolor}",
    "  - \\definecolor{forestgreen}{RGB}{34,139,34}",
    "  - \\pagestyle{fancy}",
    "  - \\fancyhf{}",
    "  - \\fancyhead[L]{\\textcolor{forestgreen}{\\textbf{ForestMAP INTA}}}",
    "  - \\fancyhead[R]{\\thepage}",
    "  - \\renewcommand{\\headrulewidth}{0.5pt}",
    "  - \\renewcommand{\\footrulewidth}{0.5pt}",
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

  rmarkdown::render(rmd_p, output_file=pdf_p,
                    envir=new.env(parent=globalenv()), quiet=TRUE)
  unlink(rmd_p)

  log_fn(paste("✅ PDF guardado en:", pdf_p))
  invisible(pdf_p)
}
