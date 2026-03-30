# ==============================================================================
# SERVER - Lógica del servidor
# ==============================================================================
# Este archivo requiere que se carguen primero:
# - Bibliotecas: shiny, terra, sf, lidR, ggplot2, DT, plotly
# - Módulos: processing.R, visualization.R, export.R
# - Variables de colores desde colors_light.R
# ==============================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    ruta_las       = NULL,
    ruta_shp       = NULL,
    ruta_dir       = NULL,
    # Última carpeta conocida — arranca en Home, se actualiza tras cada selección
    ultima_carpeta = path.expand("~"),
    configurado    = FALSE,
    las_raw        = NULL, roi          = NULL,
    las_filtrado   = NULL, las_clasf    = NULL,
    dem            = NULL, dem_suav     = NULL,
    curvas         = NULL, hillshade    = NULL,
    datos_norm     = NULL, chm          = NULL,
    arboles        = NULL,
    # Estadísticas para el informe narrativo
    dens_orig      = NA,   area_ha      = NA,
    dem_min        = NA,   dem_max      = NA,   dem_mean = NA,
    chm_min        = NA,   chm_max      = NA,   chm_mean = NA,
    log_config     = character(0), log_prepro  = character(0),
    log_modelos    = character(0), log_arboles = character(0),
    log_export     = character(0),
    # Configuración del proyecto (editable)
    cfg_autor      = "Autor del análisis",
    cfg_institucion = "Institución",
    cfg_email      = "email@email.com",
    cfg_destinatario = "Destinatario del informe"
  )

  ts_log <- function(m) paste(format(Sys.time(),"[%H:%M:%S]"), m)
  ag <- function(campo, m) rv[[campo]] <- c(rv[[campo]], ts_log(m))

  # ── Guardar Configuración del Proyecto ───────────────────────────────────────
  observeEvent(input$btn_guardar_cfg, {
    rv$cfg_autor       <- input$cfg_autor
    rv$cfg_institucion <- input$cfg_institucion
    rv$cfg_email       <- input$cfg_email
    rv$cfg_destinatario <- input$cfg_destinatario
    showNotification("✅ Configuración del proyecto guardada.", type="message")
  })

  # ── Helpers de diálogo nativo (rstudioapi con fallback a tcltk) ─────────────
  # Usa rstudioapi si está disponible (RStudio), si no cae a tcltk (R base)
  elegir_archivo <- function(caption, filtros_rstudio, dir_inicio) {
    if (rstudioapi::isAvailable()) {
      tryCatch(
        rstudioapi::selectFile(
          caption = caption,
          filter  = filtros_rstudio,
          path    = dir_inicio
        ),
        error = function(e) character(0)
      )
    } else {
      tryCatch({
        tcltk::tkfocus(tcltk::tktoplevel())  # inicializa Tcl/Tk si hace falta
        tcltk::tk_choose.files(
          default = dir_inicio,
          caption = caption,
          multi   = FALSE
        )
      }, error = function(e) character(0))
    }
  }

  elegir_carpeta <- function(caption, dir_inicio) {
    if (rstudioapi::isAvailable()) {
      tryCatch(
        rstudioapi::selectDirectory(
          caption = caption,
          path    = dir_inicio
        ),
        error = function(e) NA_character_
      )
    } else {
      tryCatch({
        tcltk::tk_choose.dir(default = dir_inicio, caption = caption)
      }, error = function(e) NA_character_)
    }
  }

  # ── LAS ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_las, {
    ruta <- elegir_archivo(
      caption          = "Seleccionar nube de puntos (LAS/LAZ)",
      filtros_rstudio  = "LAS/LAZ (*.las *.laz)",
      dir_inicio       = rv$ultima_carpeta
    )
    if (length(ruta) > 0 && !is.na(ruta) && nchar(ruta) > 0) {
      rv$ruta_las       <- ruta
      rv$ultima_carpeta <- dirname(ruta)
    }
  })

  # ── SHP ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_shp, {
    ruta <- elegir_archivo(
      caption          = "Seleccionar área de interés (Shapefile)",
      filtros_rstudio  = "Shapefile (*.shp)",
      dir_inicio       = rv$ultima_carpeta
    )
    if (length(ruta) > 0 && !is.na(ruta) && nchar(ruta) > 0) {
      rv$ruta_shp       <- ruta
      rv$ultima_carpeta <- dirname(ruta)
    }
  })

  # ── DIR salida ────────────────────────────────────────────────────────────
  observeEvent(input$btn_dir, {
    ruta <- elegir_carpeta(
      caption    = "Seleccionar carpeta de salida",
      dir_inicio = rv$ultima_carpeta
    )
    if (!is.na(ruta) && nchar(ruta) > 0)
      rv$ruta_dir <- ruta
  })

  # Outputs de rutas
  output$txt_ruta_las <- renderText(rv$ruta_las %||% "Sin seleccionar")
  output$txt_ruta_shp <- renderText(rv$ruta_shp %||% "Sin seleccionar")
  output$txt_ruta_dir <- renderText(rv$ruta_dir %||% "Sin seleccionar")
  output$met_las <- renderText(if(!is.null(rv$ruta_las))"✅"else"❌")
  output$met_shp <- renderText(if(!is.null(rv$ruta_shp))"✅"else"❌")
  output$met_dir <- renderText(if(!is.null(rv$ruta_dir))"✅"else"❌")

  # ── E1: Confirmar Configuración ─────────────────────────────────────────
  observeEvent(input$btn_configurar, {
    req(rv$ruta_las, rv$ruta_shp, rv$ruta_dir)
    # Si n_hilos es 0, usar todos los núcleos disponibles
    n_threads <- if(input$n_hilos == 0) N_CORES_MAX else input$n_hilos
    set_lidr_threads(n_threads)
    crear_estructura_carpetas(rv$ruta_dir)
    rv$configurado <- TRUE
    ag("log_config", sprintf("LAS  : %s", basename(rv$ruta_las)))
    ag("log_config", sprintf("ROI  : %s", basename(rv$ruta_shp)))
    ag("log_config", sprintf("Dir  : %s", rv$ruta_dir))
    ag("log_config", sprintf("Hilos: %s | Buffer: %d m | Densidad: %.0f pts/m²",
                              ifelse(input$n_hilos == 0, paste0("Todos (", N_CORES_MAX, ")"), input$n_hilos),
                              input$buffer_m, input$densidad))
    ag("log_config", "Carpetas: NUBES / RASTER / VECTORIALES / INFORME — ✅ listas")
    ag("log_config", "✅ Configuración confirmada. Continúe con la Etapa 2.")
    showNotification("✅ Configuración guardada.", type="message")
  })
  output$log_config <- renderText(paste(rv$log_config, collapse="\n"))

  # ── E2: Pre-procesamiento ───────────────────────────────────────────────
  observeEvent(input$btn_preprocesar, {
    req(rv$ruta_las, rv$ruta_shp, rv$ruta_dir)
    rv$log_prepro <- character(0)
    withProgress(message="Pre-procesando...", value=0, {
      tryCatch({
        setProgress(0.10, detail="Cargando...")
        res <- cargar_recortar_las(rv$ruta_las, rv$ruta_shp,
                                    buffer_m=input$buffer_m,
                                    log_fn=function(m) ag("log_prepro",m))
        rv$las_raw   <- res$las
        rv$roi       <- res$roi
        rv$dens_orig <- res$dens_orig
        rv$area_ha   <- res$area_ha

        setProgress(0.40, detail="Filtrando...")
        rv$las_filtrado <- filtrar_nube(rv$las_raw, densidad=input$densidad,
                                         log_fn=function(m) ag("log_prepro",m))
        setProgress(0.70, detail="Clasificando suelo...")
        rv$las_clasf <- clasificar_suelo(rv$las_filtrado,
                          rigidness=as.integer(input$csf_rigidness),
                          class_threshold=input$csf_threshold,
                          cloth_resolution=input$csf_cloth_res,
                          sloop_smooth=input$csf_sloop_smooth,
                          log_fn=function(m) ag("log_prepro",m))
        setProgress(1)
        showNotification("✅ Pre-procesamiento completado.", type="message")
      }, error=function(e){
        ag("log_prepro", paste("❌ ERROR:", conditionMessage(e)))
        showNotification(paste("Error:", conditionMessage(e)), type="error")
      })
    })
  })
  output$log_prepro    <- renderText(paste(rv$log_prepro, collapse="\n"))
  output$met_pts_orig  <- renderText(if(is.null(rv$las_raw))      "—" else format(nrow(rv$las_raw@data),     big.mark="."))
  output$met_pts_filt  <- renderText(if(is.null(rv$las_filtrado)) "—" else format(nrow(rv$las_filtrado@data),big.mark="."))
  output$met_pts_suelo <- renderText(if(is.null(rv$las_clasf))    "—" else format(sum(rv$las_clasf@data$Classification==2L,na.rm=TRUE),big.mark="."))
  output$met_pts_veg   <- renderText(if(is.null(rv$las_clasf))    "—" else format(sum(rv$las_clasf@data$Classification!=2L,na.rm=TRUE),big.mark="."))

  output$plot3d_orig  <- renderPlotly({
    req(rv$las_raw)
    plotly_nube_3d(rv$las_raw, color_var="Z",
                   titulo="Nube Original — coloreada por Elevación (Z)")
  })
  output$plot3d_clasf <- renderPlotly({
    req(rv$las_clasf)
    plotly_nube_3d(rv$las_clasf, color_var="Classification",
                   titulo="Nube Clasificada — Suelo (amarillo) / Vegetación (verde)")
  })

  # ── E3: Modelos Digitales ───────────────────────────────────────────────
  observeEvent(input$btn_modelos, {
    req(rv$las_clasf, rv$roi)
    rv$log_modelos <- character(0)
    withProgress(message="Generando modelos...", value=0, {
      tryCatch({
        setProgress(0.10, detail="DEM...")
        rd <- generar_dem(rv$las_clasf,
                          res_dem=input$dem_res,
                          win_min=input$dem_win_min,
                          win_mean=input$dem_win_mean,
                          log_fn=function(m) ag("log_modelos",m))
        rv$dem      <- rd$dem
        rv$dem_suav <- rd$dem_suav
        rv$dem_min  <- rd$dem_min
        rv$dem_max  <- rd$dem_max
        rv$dem_mean <- rd$dem_mean

        setProgress(0.35, detail="Curvas de nivel...")
        rv$curvas <- generar_curvas_nivel(rv$dem_suav, rv$roi,
                       equidistancia=input$cn_equidist,
                       log_fn=function(m) ag("log_modelos",m))

        setProgress(0.50, detail="Hillshade...")
        rv$hillshade <- generar_hillshade(rv$dem_suav,
                         angulo_sol=input$hs_angulo, azimut=input$hs_azimut,
                         log_fn=function(m) ag("log_modelos",m))

        setProgress(0.70, detail="Normalización + CHM...")
        rn <- normalizar_y_chm(rv$las_clasf, rv$dem_suav, rv$roi,
                res_chm=input$chm_res, subcircle=input$chm_subcirc,
                log_fn=function(m) ag("log_modelos",m))
        rv$datos_norm <- rn$datos_norm
        rv$chm        <- rn$chm
        rv$chm_min    <- rn$chm_min
        rv$chm_max    <- rn$chm_max
        rv$chm_mean   <- rn$chm_mean

        setProgress(1)
        showNotification("✅ Modelos generados.", type="message")
      }, error=function(e){
        ag("log_modelos", paste("❌ ERROR:", conditionMessage(e)))
        showNotification(paste("Error:", conditionMessage(e)), type="error")
      })
    })
  })
  output$log_modelos <- renderText(paste(rv$log_modelos, collapse="\n"))

  output$plot_dem_bruto <- renderPlotly({
    req(rv$dem)
    plotly_raster(raster_to_df(rv$dem), "DEM bruto (TIN)", PAL_TERRAIN, "Elev.(m)")
  })
  output$plot_dem_suav <- renderPlotly({
    req(rv$dem_suav)
    plotly_raster(raster_to_df(rv$dem_suav), "DEM suavizado", PAL_TERRAIN, "Elev.(m)")
  })
  output$plot_hillshade <- renderPlotly({
    req(rv$hillshade)
    plotly_raster(raster_to_df(rv$hillshade), "Hillshade — Sombreado del Relieve", PAL_HILLSHADE, "Luz")
  })
  output$plot_chm <- renderPlotly({
    req(rv$chm)
    plotly_raster(raster_to_df(rv$chm), "CHM — Modelo de Altura de Copas", PAL_CHM, "Altura(m)")
  })

  # Curvas de nivel: Se usa renderPlot con terra::plot para máxima eficiencia
  # (el código Plotly original tenía problemas con líneas espurias entre curvas)
  
  output$plot_curvas <- renderPlot({
    # 1. Requerir los datos
    req(rv$dem_suav, rv$curvas)
    
    # 2. Configurar el área de dibujo (opcional: márgenes)
    par(mar = c(4, 4, 3, 5)) 
    
    # 3. Dibujar el Raster (DEM) de fondo
    # terra::plot es extremadamente eficiente con escenas grandes
    plot(rv$dem_suav, 
         main = "DEM Suavizado + Curvas de Nivel", 
         col = terrain.colors(50), # O tu variable PAL_TERRAIN
         mar = c(3, 1, 3, 5))
    
    # 4. Superponer las curvas
    # Al ser rv$curvas un objeto SpatVector o SF, plot(..., add=TRUE) es instantáneo
    plot(rv$curvas, 
         add = TRUE, 
         col = "black", 
         lwd = 0.8)
    
  }, height = 530) # Puedes ajustar la altura fija para que se vea bien

  output$plot_hist_z <- renderPlot({
    req(rv$datos_norm)
    z <- rv$datos_norm@data$Z
    if (length(z) > 100000) z <- sample(z, 100000)
    ggplot(data.frame(Z=z), aes(x=Z)) +
      geom_histogram(bins=60, fill=GREEN, color=BG_CARD, alpha=0.85) +
      labs(title="Distribución Vertical — Nube Normalizada",
           x="Altura sobre el terreno (m)", y="Frecuencia") +
      theme_minimal() +
      theme(plot.background=element_rect(fill=BG_CARD,colour=NA),
            panel.background=element_rect(fill=BG_CARD,colour=NA),
            panel.grid=element_line(color=BORDER_COLOR),
            text=element_text(color=TEXT_PRIMARY),
            axis.text=element_text(color=TEXT_SECONDARY),
            plot.title=element_text(color=ACCENT_PRIMARY,face="bold"))
  })

  # ── E4: Árboles ─────────────────────────────────────────────────────────
# ==============================================================================
# Función para calcular cobertura de copas usando máscara binaria del CHM
# ==============================================================================
# Calcula métricas de cobertura de copas a partir de un umbral de altura en el CHM.
# Método: Crea una máscara binaria donde CHM > umbral, luego vectoriza y calcula áreas.
#
# PARÁMETROS:
#   chm              : SpatRaster con el Canopy Height Model
#   roi              : Area de interes
#   umbral_altura    : Altura mínima (m) para considerar cobertura de copa (default = 3.0)
#
# RETORNO (lista con):
#   porc_cobertura   : Porcentaje de cobertura de copas (0-100)
#   area_copa_total  : Suma de áreas cubiertas por copas (m²)
#   n_poligonos      : Número de polígonos de copa detectados
#   area_polig_media : Área promedio por polígono (m²)
#   area_polig_sd    : Desviación estándar del área de polígonos (m²)
#   area_polig_min   : Área mínima de polígono (m²)
#   area_polig_max   : Área máxima de polígono (m²)
#   area_total       : Área total del área de estudio (m²)
#   copas_vect       : SpatVector con polígonos de copas
#
# MÉTODO:
#   1. Crea máscara binaria: CHM > umbral_altura
#   2. Vectoriza la máscara (as.polygons con dissolve = TRUE)
#   3. Filtra polígonos donde valor == 1 (copas)
#   4. Calcula áreas usando expanse()
# ------------------------------------------------------------------------------
calcular_cobertura <- function(chm, roi, umbral_altura = 3.0) {
  
  if (is.null(chm) || is.null(roi)) {
    stop("chm o roi NULL")
  }
  
  # Máscara binaria
  mask <- chm > umbral_altura
  
  # Convertir máscara (SpatRaster de terra) a dataframe
  df_mascara <- as.data.frame(mask, xy = TRUE)
  names(df_mascara)[3] <- "valor"
  
  # Vectorización
  copas <- as.polygons(mask, dissolve = TRUE)
  
  # Filtrar solo valor 1
  copas <- copas[copas[[1]] == 1, ]
  
  if (is.null(copas) || nrow(copas) == 0) {
    return(list(
      porc_cobertura = 0,
      area_copa_total = 0,
      n_poligonos = 0,
      area_total = as.numeric(expanse(roi, unit = "m")),
      copas_vect = NULL,
      mascara_df = df_mascara,
      umbral_altura = umbral_altura
    ))
  }
  
  # Áreas
  area_total <- as.numeric(expanse(roi, unit = "m"))
  areas <- expanse(copas, unit = "m")
  
  area_copa_total <- sum(areas, na.rm = TRUE)
  porc_cobertura <- min(area_copa_total / area_total * 100, 100)
  
  list(
    porc_cobertura = porc_cobertura,
    area_copa_total = area_copa_total,
    n_poligonos = nrow(copas),
    area_total = area_total,
    copas_vect = copas,
    mascara_df = df_mascara,
    umbral_altura = umbral_altura
  )
}

observeEvent(input$btn_arboles, {
  req(rv$datos_norm, rv$chm, rv$roi)
  
  rv$log_arboles <- character(0)
  
  withProgress(message="Detectando árboles...", value=0, {
    tryCatch({
      
      setProgress(0.3)
      rv$arboles <- detectar_arboles(
        rv$datos_norm,
        ws = input$lmf_ws,
        hmin = input$lmf_hmin,
        log_fn = function(m) ag("log_arboles", m)
      )
      
      ag("log_arboles", sprintf("✅ %d árboles detectados", nrow(rv$arboles)))
      
      setProgress(0.7)
      rv$cobertura_copas <- calcular_cobertura(
        rv$chm,
        rv$roi,
        input$canopy_height_cutoff
      )
      
      ag("log_arboles", sprintf("🌳 Cobertura: %.1f%%",
                               rv$cobertura_copas$porc_cobertura))
      
      setProgress(1)
      
    }, error=function(e){
      ag("log_arboles", paste("❌ ERROR:", conditionMessage(e)))
    })
  })
})

output$log_arboles <- renderText(paste(rv$log_arboles, collapse="\n"))
output$met_n_arb   <- renderText(if(is.null(rv$arboles)) "—" else as.character(nrow(rv$arboles)))
output$met_alt_med <- renderText(if(is.null(rv$arboles)) "—" else sprintf("%.1f", mean(rv$arboles$Z, na.rm=TRUE)))
output$met_alt_max <- renderText(if(is.null(rv$arboles)) "—" else sprintf("%.1f", max(rv$arboles$Z, na.rm=TRUE)))


output$met_canopy_pct <- renderText({
  req(rv$cobertura_copas)
  sprintf("%.1f%%", rv$cobertura_copas$porc_cobertura)
})

output$met_canopy_area <- renderText({
  req(rv$cobertura_copas)
  sprintf("%.1f ha", rv$cobertura_copas$area_copa_total / 10000)
})


  # Mapa árboles sobre CHM
  output$plot_arb_chm <- renderPlotly({
    req(rv$chm, rv$arboles)
    # Convertir CHM a data frame con nombre estándar
    df_chm <- raster_to_df(rv$chm)   # columnas: x, y, valor
    # Coordenadas de los ápices
    co_arb <- as.data.frame(st_coordinates(rv$arboles))
    co_arb$altura <- rv$arboles$Z

    plotly_raster(df_chm, "Árboles identificados sobre CHM", PAL_CHM, "Altura(m)") |>
      add_trace(
        data = co_arb, x = ~X, y = ~Y,
        type="scatter", mode="markers",
        inherit = FALSE,
        marker=list(symbol="cross", size=9, color=MARKER_TREE_COLOR,
                    line=list(color=MARKER_TREE_LINE, width=1.2)),
        text  = ~paste0("Ápice: ", round(altura,1), " m"),
        hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE
      )
  })

output$plot_hist_arb <- renderPlot({
    req(rv$arboles)
    ggplot(data.frame(h=rv$arboles$Z), aes(x=h)) +
      geom_histogram(bins=30, fill=GREEN, color=BG_CARD, alpha=0.9) +
      labs(title="Distribución de Alturas de Árboles Detectados",
           x="Altura del ápice (m)", y="Frecuencia") +
      theme_minimal() +
      theme(plot.background=element_rect(fill=BG_CARD,colour=NA),
            panel.background=element_rect(fill=BG_CARD,colour=NA),
            panel.grid=element_line(color=BORDER_COLOR),
            text=element_text(color=TEXT_PRIMARY),
            axis.text=element_text(color=TEXT_SECONDARY),
            plot.title=element_text(color=ACCENT_PRIMARY,face="bold"))
  })



output$plot_canopy_coverage <- renderPlotly({
  req(rv$cobertura_copas)
  
  # Forzar consistencia (como en CHM)
  df_cov <- rv$cobertura_copas$mascara_df
  
  # Normalizar nombres
  names(df_cov) <- tolower(names(df_cov))
  col_val <- setdiff(names(df_cov), c("x", "y"))[1]
  df_cov$valor <- df_cov[[col_val]]

  # Opcional: asegurar binario limpio
  df_cov$valor <- ifelse(df_cov$valor > 0, 1, 0)

  plotly_raster(
    df_cov,
    sprintf("Cobertura de copas: %.1f%% del área",
            rv$cobertura_copas$porc_cobertura),
    PAL_TERRENO_VEGETACION,
    "Cobertura"
  ) |>
    layout(
      coloraxis = list(
        colorbar = list(
          tickmode = "array",
          tickvals = c(0, 1),
          ticktext = c("Sin copa", "Copa")
        )
      ),
      showlegend = FALSE
    )
})

 
  # ── E5: Exportación ──────────────────────────────────────────────────────
  observeEvent(input$btn_exportar, {
    req(rv$ruta_dir, rv$las_clasf, rv$datos_norm, rv$dem_suav,
        rv$chm, rv$curvas, rv$hillshade, rv$arboles, rv$roi)
    rv$log_export <- character(0)
    withProgress(message="Exportando...", value=0.1, {
      tryCatch({
        # Extraer copas_vect (SpatVector) si está disponible
        copas_vect <- if (!is.null(rv$cobertura_copas)) {
          rv$cobertura_copas$copas_vect
        } else {
          NULL
        }
        
        exportar_todos(rv$ruta_dir, rv$las_clasf, rv$datos_norm, rv$dem_suav,
                       rv$chm, rv$curvas, rv$hillshade, rv$arboles, rv$roi,
                       copas_vect = copas_vect,
                       log_fn=function(m) ag("log_export",m))
        setProgress(1)
        showNotification("✅ Exportación completada.", type="message")
      }, error=function(e){
        ag("log_export", paste("❌ ERROR:", conditionMessage(e)))
        showNotification(paste("Error:", conditionMessage(e)), type="error")
      })
    })
  })

  # ── Informe descriptivo narrativo PDF ────────────────────────────────────
  observeEvent(input$btn_informe, {
    req(rv$ruta_dir)
    ag("log_export","📄 Generando informe descriptivo PDF...")
    tryCatch({
      generar_informe_descriptivo(rv, input, log_fn = function(m) ag("log_export", m))
      showNotification("✅ Informe PDF generado.", type="message")
    }, error = function(e) {
      ag("log_export", paste("❌ ERROR informe:", conditionMessage(e)))
      showNotification(paste("Error informe:", conditionMessage(e)), type="error")
    })
  })

  output$log_exportar <- renderText(paste(rv$log_export, collapse="\n"))

  # ── Abrir carpeta de resultados ──────────────────────────────────────────
  observeEvent(input$btn_abrir_carpeta, {
    req(rv$ruta_dir)
    carpeta <- rv$ruta_dir
    if (!dir.exists(carpeta)) {
      showNotification("❌ La carpeta de resultados no existe aún.", type="error")
      return()
    }
    os <- .Platform$OS.type
    sysname <- Sys.info()[["sysname"]]
    tryCatch({
      if (sysname == "Windows") {
        shell.exec(carpeta)
      } else if (sysname == "Darwin") {
        system(paste("open", shQuote(carpeta)))
      } else {
        system(paste("xdg-open", shQuote(carpeta)))
      }
      showNotification("📂 Carpeta de resultados abierta.", type="message")
    }, error = function(e) {
      showNotification(paste("No se pudo abrir la carpeta:", carpeta), type="warning")
    })
  })

}

# ══════════════════════════════════════════════════════════════════════════════
