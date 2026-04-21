# ==============================================================================
# MÓDULO DE PROCESAMIENTO LiDAR
# ==============================================================================
# Funciones para procesamiento de nubes de puntos, clasificación de suelo,
# generación de modelos digitales y detección de árboles
# ==============================================================================

# ──────────────────────────────────────────────────────────────────────────────
# GESTIÓN DE ARCHIVOS
# ──────────────────────────────────────────────────────────────────────────────
crear_estructura_carpetas <- function(carpeta_trabajo) {
  dirs <- c("Salidas_NUBES", "Salidas_RASTER", "Salidas_VECTORIALES", "Salidas_INFORME")
  for (d in dirs) {
    dir.create(file.path(carpeta_trabajo, d), showWarnings = FALSE, recursive = TRUE)
  }
  invisible(dirs)
}

# ──────────────────────────────────────────────────────────────────────────────
# CARGA Y PREPROCESAMIENTO
# ──────────────────────────────────────────────────────────────────────────────

# Verifica el estado del CRS del LAS comparado con el SHP.
# Lee solo el encabezado (rápido, no carga puntos).
# Retorna lista con $estado: "sin_crs" | "mismatch" | "ok"
verificar_crs_las <- function(ruta_las, ruta_shp) {
  hdr <- readLASheader(ruta_las)
  roi <- vect(ruta_shp)

  wkt_las <- tryCatch(sf::st_crs(hdr)$wkt, error = function(e) NULL)
  las_tiene_crs <- !is.null(wkt_las) && !is.na(wkt_las) && nchar(trimws(wkt_las)) > 0

  epsg_shp <- tryCatch(
    as.integer(crs(roi, describe = TRUE)$code),
    error = function(e) NA_integer_
  )

  if (!las_tiene_crs) {
    return(list(estado = "sin_crs", crs_shp = crs(roi), epsg_shp = epsg_shp))
  }

  misma_crs <- isTRUE(sf::st_crs(hdr) == sf::st_crs(roi))
  epsg_las  <- tryCatch(as.integer(sf::st_crs(hdr)$epsg), error = function(e) NA_integer_)

  if (!misma_crs) {
    return(list(
      estado   = "mismatch",
      crs_shp  = crs(roi),
      epsg_shp = epsg_shp,
      epsg_las = epsg_las
    ))
  }

  list(estado = "ok")
}

cargar_recortar_las <- function(ruta_las, ruta_shp, buffer_m = 20,
                                crs_override = NULL, log_fn = message) {
  log_fn("📂 Cargando nube de puntos...")
  las <- readLAS(ruta_las, select = "xyznr")

  bb   <- ext(las)
  area <- (bb[2]-bb[1]) * (bb[4]-bb[3])
  dens_orig <- round(nrow(las@data) / as.numeric(area), 1)
  log_fn(sprintf("✅ Nube cargada: %s pts | densidad ≈ %.1f pts/m²",
                 format(nrow(las@data), big.mark="."), dens_orig))

  log_fn("🗺️  Cargando área de interés...")
  roi <- vect(ruta_shp)
  area_roi_ha <- round(as.numeric(expanse(roi, unit="ha")), 2)
  log_fn(sprintf("   Área de interés: %.2f ha", area_roi_ha))

  # Asignar CRS elegido por el usuario si el LAS no tenía proyección
  if (!is.null(crs_override)) {
    log_fn(sprintf("📐 Asignando CRS al LAS: EPSG:%d", as.integer(crs_override)))
    projection(las) <- crs(paste0("epsg:", as.integer(crs_override)))
  }

  # Si ambos tienen CRS pero difieren, reproyectar el ROI al CRS del LAS
  wkt_las <- tryCatch(sf::st_crs(las)$wkt, error = function(e) NULL)
  las_tiene_crs <- !is.null(wkt_las) && !is.na(wkt_las) && nchar(trimws(wkt_las)) > 0
  if (las_tiene_crs && !isTRUE(sf::st_crs(las) == sf::st_crs(roi))) {
    log_fn("⚠️  CRS distintos. Reproyectando área de interés al CRS del LAS...")
    roi <- project(roi, crs(las))
    log_fn("   Reproyección completada.")
  }

  log_fn(sprintf("✂️  Recortando con buffer de %d m...", buffer_m))
  las_r <- clip_roi(las = las, geometry = st_as_sf(buffer(roi, buffer_m)))
  log_fn(sprintf("✅ Nube recortada: %s pts", format(nrow(las_r@data), big.mark=".")))

  list(las = las_r, roi = roi, dens_orig = dens_orig, area_ha = area_roi_ha)
}

filtrar_nube <- function(las, densidad = 10, log_fn = message) {
  log_fn(sprintf("🔽 Submuestreando a densidad %.0f pts/m²...", densidad))
  las <- decimate_points(las, algorithm = random(density = densidad))
  
  log_fn("🔍 Eliminando puntos duplicados...")
  las <- filter_duplicates(las)
  
  log_fn("🔇 Clasificando y eliminando ruido (SOR)...")
  las <- classify_noise(las, algorithm = sor())
  las <- filter_poi(las, Classification != LASNOISE)
  
  log_fn(sprintf("✅ Nube filtrada: %s pts", format(nrow(las@data), big.mark=".")))
  las
}

# ──────────────────────────────────────────────────────────────────────────────
# CLASIFICACIÓN DE SUELO
# ──────────────────────────────────────────────────────────────────────────────
clasificar_suelo <- function(las, rigidness = 2, class_threshold = 0.5,
                              cloth_resolution = 2, sloop_smooth = FALSE, 
                              log_fn = message) {
  rig_label <- switch(as.character(rigidness),
                      "1" = "1 — Quebrado",
                      "2" = "2 — Ondulado / Medio",
                      "3" = "3 — Llano")
  log_fn(sprintf("🏔️ Clasificando suelo — CSF (rigidez: %s)...", rig_label))
  
  las_c <- classify_ground(las,
                            algorithm = csf(sloop_smooth     = sloop_smooth,
                                            class_threshold  = class_threshold,
                                            cloth_resolution = cloth_resolution,
                                            rigidness        = rigidness,
                                            iterations       = 500,
                                            time_step        = 0.65))
  
  n <- sum(las_c@data$Classification == 2L, na.rm = TRUE)
  log_fn(sprintf("✅ Puntos de suelo clasificados: %s", format(n, big.mark=".")))
  las_c
}

# ──────────────────────────────────────────────────────────────────────────────
# MODELOS DIGITALES
# ──────────────────────────────────────────────────────────────────────────────
generar_dem <- function(las_clasf, res_dem = 1, win_min = 3, win_mean = 9, 
                        log_fn = message) {
  log_fn(sprintf("📐 Generando DEM — resolución %.2f m (TIN)...", res_dem))
  dem <- rasterize_terrain(las_clasf, res = res_dem, algorithm = tin())
  
  # Forzar valores impares >= 3
  win_min  <- max(3L, as.integer(win_min)  %/% 2L * 2L + 1L)
  win_mean <- max(3L, as.integer(win_mean) %/% 2L * 2L + 1L)
  log_fn(sprintf("🔀 Suavizando: ventana mín %dx%d → ventana media %dx%d...",
                 win_min, win_min, win_mean, win_mean))
  
  dem_s <- focal(dem,   w = matrix(1L, win_min,  win_min),  fun = "min")
  dem_s <- focal(dem_s, w = matrix(1L, win_mean, win_mean), fun = "mean")
  
  g_dem <- terra::global(dem_s, fun = c("min","max","mean"), na.rm = TRUE)
  log_fn(sprintf("   Elevación mín: %.1f m | máx: %.1f m | media: %.1f m",
                 g_dem$min, g_dem$max, g_dem$mean))
  log_fn("✅ DEM generado y suavizado.")
  
  list(dem = dem, dem_suav = dem_s,
       dem_min = round(g_dem$min,1), dem_max = round(g_dem$max,1),
       dem_mean = round(g_dem$mean,1))
}

generar_curvas_nivel <- function(dem_suav, roi, equidistancia = 0.5, 
                                  log_fn = message) {
  log_fn(sprintf("📏 Curvas de nivel (equidistancia %.2f m)...", equidistancia))
  
  g     <- terra::global(dem_suav, fun = c("min","max"), na.rm = TRUE)
  e_min <- floor(  as.numeric(g[["min"]]) / equidistancia) * equidistancia
  e_max <- ceiling(as.numeric(g[["max"]]) / equidistancia) * equidistancia
  niveles <- seq(e_min, e_max, by = equidistancia)
  
  log_fn(sprintf("   Rango: %.2f – %.2f m → %d niveles", e_min, e_max, length(niveles)))
  curvas <- as.contour(dem_suav, levels = niveles)
  curvas <- crop(curvas, roi)
  
  log_fn(sprintf("✅ %d curvas generadas.", length(curvas)))
  curvas
}

generar_hillshade <- function(dem_suav, angulo_sol = 45, azimut = 315, 
                               log_fn = message) {
  log_fn(sprintf("☀️  Hillshade (sol: %.0f° elevación, %.0f° azimut)...", 
                 angulo_sol, azimut))
  
  hs <- shade(terrain(dem_suav, v = "slope",  unit = "radians"),
              terrain(dem_suav, v = "aspect", unit = "radians"),
              angle = angulo_sol, direction = azimut)
  
  log_fn("✅ Hillshade generado.")
  hs
}

normalizar_y_chm <- function(las_clasf, dem_suav, roi, res_chm = 0.5,
                              subcircle = 0.025, log_fn = message) {
  log_fn("📉 Normalizando nube al nivel del terreno...")
  dn <- las_clasf - dem_suav
  dn <- dn[dn$Z >= 0]
  dn <- clip_roi(las = dn, geometry = st_as_sf(roi))
  log_fn(sprintf("✅ Nube normalizada: %s pts", format(nrow(dn@data), big.mark=".")))
  
  log_fn(sprintf("🌿 CHM — resolución %.2f m (p2r + knnidw)...", res_chm))
  chm <- rasterize_canopy(dn, res = res_chm,
                           algorithm = p2r(subcircle = subcircle,
                                           na.fill   = knnidw(k = 8, p = 2)))
  
  g_chm <- terra::global(chm, fun = c("min","max","mean"), na.rm = TRUE)
  log_fn(sprintf("   Altura mín: %.1f m | máx: %.1f m | media: %.1f m",
                 g_chm$min, g_chm$max, g_chm$mean))
  log_fn("✅ CHM generado.")
  
  list(datos_norm = dn, chm = chm,
       chm_min = round(g_chm$min,1), chm_max = round(g_chm$max,1),
       chm_mean = round(g_chm$mean,1))
}

# ──────────────────────────────────────────────────────────────────────────────
# DETECCIÓN DE ÁRBOLES
# ──────────────────────────────────────────────────────────────────────────────
detectar_arboles <- function(datos_norm, ws = 6, hmin = 6, log_fn = message) {
  log_fn(sprintf("🌲 Detectando árboles — LMF (ws=%.1f m, hmin=%.1f m)...", ws, hmin))
  
  ttops <- locate_trees(datos_norm, lmf(ws = ws, hmin = hmin))
  
  log_fn(sprintf("✅ %d árboles detectados.", nrow(ttops)))
  ttops
}
