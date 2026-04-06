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
#' @param log_fn Function to log status messages (default: message).
#' @return Path to generated PDF.
#' @export
generar_informe_descriptivo <- function(rv, input, log_fn = message) {
  if (is.null(rv$ruta_dir) || !nzchar(rv$ruta_dir)) {
    stop("ruta_dir inválida para generar informe")
  }

  log_fn("📄 Generando informe descriptivo PDF ...")

  # ============================================================================
  # VARIABLES BÁSICAS (originales)
  # ============================================================================
  nombre_las  <- basename(rv$ruta_las %||% "N/A")
  nombre_roi  <- basename(rv$ruta_shp %||% "N/A")
  area_ha     <- if (!is.na(rv$area_ha)) sprintf("%.2f", rv$area_ha) else "N/D"
  dens_orig_v <- if (!is.na(rv$dens_orig)) sprintf("%.1f", rv$dens_orig) else "N/D"
  dens_sub    <- as.character(input$densidad)
  rig_label   <- switch(as.character(input$csf_rigidness),
                        "1" = "1 — Quebrado", "2" = "2 — Ondulado / Medio", "3" = "3 — Llano")
  n_pts_orig  <- if (!is.null(rv$las_raw))      format(nrow(rv$las_raw@data),     big.mark=".") else "N/D"
  n_pts_filt  <- if (!is.null(rv$las_filtrado)) format(nrow(rv$las_filtrado@data), big.mark=".") else "N/D"
  n_suelo     <- if (!is.null(rv$las_clasf))    format(sum(rv$las_clasf@data$Classification==2L, na.rm=TRUE), big.mark=".") else "N/D"
  dem_min_v   <- if (!is.na(rv$dem_min))  sprintf("%.1f", rv$dem_min)  else "N/D"
  dem_max_v   <- if (!is.na(rv$dem_max))  sprintf("%.1f", rv$dem_max)  else "N/D"
  dem_mean_v  <- if (!is.na(rv$dem_mean)) sprintf("%.1f", rv$dem_mean) else "N/D"
  dem_rango_v <- if (!is.na(rv$dem_min) && !is.na(rv$dem_max)) sprintf("%.1f", rv$dem_max - rv$dem_min) else "N/D"
  cn_equi_v   <- sprintf("%.2f", input$cn_equidist)
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
  
  # Densidad de árboles por hectárea
  dens_arb_ha <- if (!is.null(rv$arboles) && !is.na(rv$area_ha) && rv$area_ha > 0) {
    sprintf("%.1f", nrow(rv$arboles) / rv$area_ha)
  } else "N/D"
  
  # Espaciamiento medio (asumiendo distribución regular)
  espac_medio <- if (!is.null(rv$arboles) && !is.na(rv$area_ha) && rv$area_ha > 0 && nrow(rv$arboles) > 0) {
    sprintf("%.1f", sqrt(10000 / (nrow(rv$arboles) / rv$area_ha)))
  } else "N/D"
  
  # Coeficiente de variación de alturas (indicador de homogeneidad)
  cv_altura <- if (!is.null(rv$arboles) && nrow(rv$arboles) > 1) {
    cv_val <- sd(rv$arboles$Z, na.rm=TRUE) / mean(rv$arboles$Z, na.rm=TRUE) * 100
    sprintf("%.1f", cv_val)
  } else "N/D"
  
  # Evaluación de homogeneidad
  cv_eval <- if (cv_altura != "N/D") {
    cv_num <- as.numeric(cv_altura)
    if (cv_num < 10) "muy homogéneo"
    else if (cv_num < 20) "moderadamente homogéneo"
    else if (cv_num < 30) "moderadamente heterogéneo"
    else "altamente heterogéneo"
  } else "N/D"
  
  # Cobertura de copas
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
  
  # Métricas topográficas para operaciones forestales
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
      
      # Áreas por clase de pendiente
      slope_vals <- values(slope_rast, mat=FALSE)
      pixel_area_ha <- res(slope_rast)[1]^2 / 10000  # área de pixel en ha
      
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
      log_fn(paste("  ⚠ No se pudieron calcular pendientes:", conditionMessage(e)))
    })
  }

  # ============================================================================
  # GENERACIÓN DE FIGURAS
  # ============================================================================
  dir_inf <- file.path(rv$ruta_dir, "Salidas_INFORME")
  if (!dir.exists(dir_inf)) dir.create(dir_inf, recursive = TRUE)

# Figura 1: DEM con curvas de nivel
if (!is.null(rv$dem_suav)) {
  png(file.path(dir_inf, "fig_dem.png"), width=1600, height=1600, res=150)
  
  # Configurar márgenes
  par(mar = c(4, 4, 3, 5))
  
  # Usar terra::plot con terrain.colors (escala continua)
  plot(rv$dem_suav, 
       main = "Modelo Digital de Elevación (DEM) con curvas de nivel",
       col = terrain.colors(100),  # 100 colores para transición suave
       mar = c(3, 3, 3, 5))
  
  # Superponer curvas si existen
  if (!is.null(rv$curvas)) {
    plot(rv$curvas, 
         add = TRUE, 
         col = "black", 
         lwd = 0.8)
  }
  
  dev.off()
}

  # Figura 2: Hillshade
  if (!is.null(rv$hillshade)) {
    png(file.path(dir_inf, "fig_hillshade.png"), width=1600, height=1600, res=150)
    df_hs <- raster_to_df(rv$hillshade)
    p_hs <- ggplot(df_hs, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours=grey(seq(0,1,.01)), guide="none") +
      coord_equal() + theme_void() +
      labs(title="Hillshade — Sombreado del Relieve") +
      theme(plot.title=element_text(face="bold", size=10))
    print(p_hs); dev.off()
  }

  # Figura 3: CHM
  if (!is.null(rv$chm)) {
    png(file.path(dir_inf, "fig_chm.png"), width=1600, height=1600, res=150)
    df_c <- raster_to_df(rv$chm)
    p_chm <- ggplot(df_c, aes(x=x, y=y, fill=valor)) +
      geom_raster() +
      scale_fill_gradientn(colours=PAL_CHM_PNG, name="Altura\n(m)", na.value="white") +
      coord_equal() + theme_void() +
      labs(title="Modelo de Altura de Copas (CHM)") +
      theme(plot.title=element_text(face="bold", size=10), legend.title=element_text(size=8))
    print(p_chm); dev.off()
  }

  # Figura 4: Árboles detectados
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
      labs(title=paste0("Árboles detectados (N=", nrow(rv$arboles), ")")) +
      theme(plot.title=element_text(face="bold", size=10), legend.title=element_text(size=8))
    print(p_arb); dev.off()
  }

  # Figura 5: Histograma de alturas
  if (!is.null(rv$arboles)) {
    png(file.path(dir_inf, "fig_hist_arb.png"), width=1200, height=1200, res=150)
    p_hist <- ggplot(data.frame(h=rv$arboles$Z), aes(x=h)) +
      geom_histogram(bins=25, fill=GREEN, color="white", alpha=0.85) +
      labs(title="Distribución de Alturas de Árboles", x="Altura del ápice (m)", y="Frecuencia") +
      theme_bw()
    print(p_hist); dev.off()
  }
  
  # Figura 6: Cobertura de copas
  if (!is.null(rv$cobertura_copas) && !is.null(rv$cobertura_copas$copas_vect)) {
  fig_path <- file.path(dir_inf, "fig_cobertura.png")
  tryCatch({
    png(fig_path, width=1600, height=1600, res=150)
    
    # Graficar solo las copas vectoriales
    copas_sf <- st_as_sf(rv$cobertura_copas$copas_vect)
    p_cob <- ggplot(copas_sf) +
      geom_sf(fill="green1", color="black", linewidth=0.5) +
      coord_sf(expand=FALSE) + 
      theme_void() +
      labs(title=sprintf("Cobertura de copas: %.1f%% del área (umbral %.1f m)",
                         rv$cobertura_copas$porc_cobertura,
                         rv$cobertura_copas$umbral_altura)) +
      theme(plot.title=element_text(face="bold", size=10))
    
    print(p_cob)
    dev.off()
    
    log_fn("  ✓ Figura de cobertura de copas generada")
  }, error = function(e) {
    if (dev.cur() != 1) dev.off()
    if (file.exists(fig_path)) tryCatch(file.remove(fig_path), error = function(e2) NULL)
    log_fn(paste("  ⚠ No se pudo generar figura de cobertura:", conditionMessage(e)))
  })
}

  # ============================================================================
  # GENERACIÓN DEL DOCUMENTO RMD
  # ============================================================================
  rmd_p <- file.path(rv$ruta_dir, "informe.Rmd")
  pdf_p <- file.path(rv$ruta_dir, "Salidas_INFORME", "Informe_Analisis.pdf")

  writeLines(con = rmd_p, c(
    "---",
    "title: 'Informe de análisis de relevamiento aéreo forestal'",
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
    "  - \\usepackage{fancyhdr}",
    "  - \\usepackage{graphicx}",
    "  - \\usepackage{xcolor}",
    "  - \\definecolor{forestgreen}{RGB}{34,139,34}",
    "  - \\pagestyle{fancy}",
    "  - \\fancyhf{}",
    "  - \\fancyhead[L]{\\textcolor{forestgreen}{\\textbf{ForestMAP INTA}}}",
    "  - \\fancyhead[R]{\\thepage}",
    "  - \\fancyfoot[C]{\\small Generado con lidR + terra}",
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
    "# Datos del Relevamiento",
    "",
    "| Campo | Detalle |",
    "|---|---|",
    paste0("| Destinatario | ", rv$cfg_destinatario, " |"),
    paste0("| Fecha de análisis | `r format(Sys.time(), '%Y-%m-%d %H:%M')` |"),
    paste0("| Archivo de nube de puntos | ", nombre_las, " |"),
    paste0("| Área de interés | ", nombre_roi, " |"),
    paste0("| Analista | ", rv$cfg_autor, " |"),
    paste0("| Institución | ", rv$cfg_institucion, " |"),
    paste0("| Email | ", rv$cfg_email, " |"),
    "",
    "# Descripción del Área Analizada",
    "",
    paste0("El área analizada cubre aproximadamente **", area_ha, " hectáreas**, delimitadas por el shapefile de área de interés proporcionado. "),
    "El relevamiento aéreo fue procesado íntegramente con la suite de herramientas del paquete **lidR** para R [@lidR2021; @lidR2023], desarrollado específicamente para el análisis de datos LiDAR aerotransportado en aplicaciones forestales y aplicable a nubes de puntos obtenidas por medio de fotogrametría.",
    "",
    "# Nube de Puntos",
    "",
    "## Características del Relevamiento",
    "",
    paste0("La nube de puntos original presentó una densidad estimada de aproximadamente **", dens_orig_v, " puntos/m²**, con un total de **", n_pts_orig, " puntos** en el área de recorte con buffer. "),
    paste0("Para optimizar el tiempo de procesamiento, la nube fue submuestreada a una densidad de trabajo de **", dens_sub, " puntos/m²**, resultando en **", n_pts_filt, " puntos** tras el filtrado."),
    "Adicionalmente, se eliminaron puntos duplicados y se identificaron y removieron puntos de ruido mediante el algoritmo **SOR** (Statistical Outlier Removal).",
    "",
    "## Clasificación del Terreno",
    "",
    paste0("La clasificación de los puntos de suelo se realizó empleando el algoritmo **CSF** (Cloth Simulation Filter, Zhang et al. 2016), configurado para un nivel de rigidez **", rig_label, "**, un umbral de clasificación de **", input$csf_threshold, " m** y una resolución de tela de **", input$csf_cloth_res, " m**. "),
    paste0("Como resultado, se identificaron **", n_suelo, " puntos** pertenecientes al suelo, los cuales fueron utilizados para la construcción del modelo digital de elevación."),
    "",
    "# Modelos Digitales del Terreno",
    "",
    "## Modelo Digital de Elevación (DEM)",
    "",
    paste0("A partir de los puntos clasificados como suelo, se generó un Modelo Digital de Elevación (DEM) con una resolución espacial de **", input$dem_res, " m**, empleando interpolación triangular (TIN). "),
    paste0("El modelo fue suavizado en dos etapas consecutivas: primero con una ventana de mínimo de ", input$dem_win_min, "x", input$dem_win_min, " píxeles para reducir la influencia de vegetación residual, y luego con una ventana de media de ", input$dem_win_mean, "x", input$dem_win_mean, " píxeles para obtener una superficie continua. "),
    paste0("El terreno del área analizada presentó una elevación mínima de **", dem_min_v, " m**, máxima de **", dem_max_v, " m** y media de **", dem_mean_v, " m**, con un rango altitudinal de **", dem_rango_v, " m**."),
    "",
    "```{r fig-dem, fig.cap='Modelo Digital de Elevación (DEM) suavizado. Los colores representan la elevación absoluta (verde oscuro = cotas bajas, amarillo/dorado = cotas altas). Se observan también las curvas de nivel.'}",
    paste0("if(file.exists('", file.path(dir_inf,"fig_dem.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_dem.png"), "')"),
    "```",
    "",
    "```{r fig-hs, fig.cap='Sombreado del relieve (Hillshade). Permite visualizar la micro-topografía del terreno y la dirección de las pendientes.'}",
    paste0("if(file.exists('", file.path(dir_inf,"fig_hillshade.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_hillshade.png"), "')"),
    "```",
    "",
    "## Curvas de Nivel",
    "",
    paste0("Se generaron curvas de nivel a partir del DEM suavizado, con una equidistancia de **", cn_equi_v, " m**. Las curvas fueron recortadas a los límites del área de interés y exportadas en formato shapefile para su uso en sistemas de información geográfica."),
    "",
    "# Estructura Vertical de la Forestación",
    "",
    "## Normalización y Modelo de Altura de Copas (CHM)",
    "",
    paste0("La nube de puntos fue normalizada al nivel del terreno restando a cada punto la elevación del DEM suavizado. Este proceso permite expresar las alturas en términos relativos al suelo (alturas sobre el terreno), independientemente de la topografía subyacente. "),
    paste0("A partir de la nube normalizada se generó el Modelo de Altura de Copas (CHM) con una resolución de **", input$chm_res, " m**. "),
    paste0("La altura de la vegetación en el área analizada varió entre **", chm_min_v, " m** y **", chm_max_v, " m**, con una altura media de **", chm_mean_v, " m**."),
    "",
    "```{r fig-chm, fig.cap='Modelo de Altura de Copas (CHM). Paleta: gris = suelo / sin vegetación, azul = baja vegetación, verde = copas medias, amarillo = copas altas, rojo = árboles de mayor porte.'}",
    paste0("if(file.exists('", file.path(dir_inf,"fig_chm.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_chm.png"), "')"),
    "```",
    "",
    "# Detección de Árboles Individuales",
    "",
    paste0("La detección de árboles individuales se realizó mediante el algoritmo **LMF** (Local Maximum Filter), que identifica los máximos locales del CHM como ápices de árboles. "),
    paste0("Se utilizó una ventana de búsqueda de **", input$lmf_ws, " m** de diámetro y una altura mínima de **", input$lmf_hmin, " m** para excluir arbustos y regeneración. "),
    paste0("Se detectaron un total de **", n_arb, " árboles** en el área analizada. "),
    paste0("Las alturas individuales estimadas variaron entre **", alt_min_v, " m** y **", alt_max_v, " m**, con una altura media de **", alt_med_v, " m**."),
    "",
    "```{r fig-arb, fig.cap='Árboles detectados (cruces negras) superpuestos sobre el CHM.'}",
    paste0("if(file.exists('", file.path(dir_inf,"fig_arboles.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_arboles.png"), "')"),
    "```",
    "",
    "```{r fig-hist, fig.cap='Distribución de frecuencia de las alturas de árboles individuales detectados.', out.width='70%'}",
    paste0("if(file.exists('", file.path(dir_inf,"fig_hist_arb.png"), "')) knitr::include_graphics('", file.path(dir_inf,"fig_hist_arb.png"), "')"),
    "```",
    "",
    "# Métricas de Gestión Forestal",
    "",
    "## Densidad y Estructura del Rodal",
    "",
    paste0("La densidad de individuos detectados es de **", dens_arb_ha, " árboles/ha**, "),
    paste0("con un espaciamiento promedio estimado de **", espac_medio, " m** entre árboles. "),
    paste0("El coeficiente de variación de las alturas es de **", cv_altura, "%**, lo que indica un rodal "),
    paste0("**", cv_eval, "** en cuanto a estructura vertical (Avery y Burkhart, 2002). "),
    "Este índice es útil para evaluar la homogeneidad del crecimiento y planificar intervenciones silvícolas diferenciadas.",
    "",
    "## Cobertura de Copas",
    "",
    paste0("El análisis de cobertura de copas, calculado mediante vectorización de la máscara binaria del CHM "),
    paste0("(umbral de altura = ", cob_umbral, " m), indica que las copas cubren "),
    paste0("**", cob_pct, "%** del área total, equivalente a **", cob_area_ha, " ha**. "),
    "Esta métrica es fundamental para evaluar el cierre de copas, la competencia entre individuos y la eficiencia en el uso del espacio. También resulta clave para el monitoreo de sistemas silvopastoriles, donde la cobertura de copas determina el momento en que deben realizarse las podas y raleos.",
    "",
    if (file.exists(file.path(dir_inf, "fig_cobertura.png"))) {
      c(
        "```{r fig-cobertura, fig.cap='Cobertura de copas vectorizada. Los contornos negros delimitan las áreas con vegetación superior al umbral de altura definido.'}",
        paste0("knitr::include_graphics('", file.path(dir_inf,"fig_cobertura.png"), "')"),
        "```",
        ""
      )
    } else {
      c("")
    },
    "",
    "# Productos Generados",
    "",
    "Todos los productos fueron exportados a la carpeta de salida seleccionada con la siguiente estructura:",
    "",
    "**Salidas_NUBES/**",
    "",
    "- `puntos_clasificados.laz`: Nube de puntos con clasificación de suelo (Clase 2) y vegetación.",
    "- `puntos_normalizados.laz`: Nube normalizada al nivel del terreno.",
    "",
    "**Salidas_RASTER/**",
    "",
    "- `DEM.tif`: Modelo Digital de Elevación suavizado, recortado al área de interés.",
    "- `CHM.tif`: Modelo de Altura de Copas, recortado al área de interés.",
    "- `Hillshade.tif`: Sombreado del relieve.",
    "",
    "**Salidas_VECTORIALES/**",
    "",
    "- `Curvas_Nivel.shp`: Curvas de nivel en formato shapefile.",
    "- `Arboles.shp`: Puntos de árboles detectados con altura del ápice.",
    "- `Area_Interes.shp`: Polígono del área de interés analizada.",
    "- `Cobertura_Copas.shp`: Polígonos de cobertura de copas (si disponible).",
    "",
    "# Referencias",
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
    "*Informe generado automáticamente con la Herramienta INTA para Análisis LiDAR y Fotogrametría Forestal (ForestMap - INTA EEA Montecarlo). Los valores estadísticos corresponden al procesamiento realizado en la fecha indicada.*"
  ))

  rmarkdown::render(rmd_p, output_file=pdf_p,
                    envir=new.env(parent=globalenv()), quiet=TRUE)
  unlink(rmd_p)  # Eliminar el .Rmd temporal

  log_fn(paste("✅ PDF guardado en:", pdf_p))
  invisible(pdf_p)
}
