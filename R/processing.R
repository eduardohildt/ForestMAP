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

validar_shp_un_poligono <- function(ruta_shp) {
  roi <- vect(ruta_shp)
  n   <- nrow(roi)
  list(ok = n == 1L, n_poligonos = n)
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

  epsg_las <- tryCatch(as.integer(sf::st_crs(hdr)$epsg), error = function(e) NA_integer_)

  wkt_shp       <- tryCatch(sf::st_crs(roi)$wkt, error = function(e) NULL)
  shp_tiene_crs <- !is.null(wkt_shp) && !is.na(wkt_shp) && nchar(trimws(wkt_shp)) > 0
  if (!shp_tiene_crs) {
    return(list(estado = "shp_sin_crs", crs_las = crs(hdr), epsg_las = epsg_las))
  }

  misma_crs <- isTRUE(sf::st_crs(hdr) == sf::st_crs(roi))

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

verificar_crs_las_solo <- function(ruta_las) {
  hdr   <- readLASheader(ruta_las)
  wkt   <- tryCatch(sf::st_crs(hdr)$wkt, error = function(e) NULL)
  tiene <- !is.null(wkt) && !is.na(wkt) && nchar(trimws(wkt)) > 0
  if (!tiene) list(estado = "sin_crs") else list(estado = "ok")
}

cargar_recortar_las <- function(ruta_las, ruta_shp = NULL, buffer_m = 20,
                                crs_override = NULL, shp_crs_override = NULL,
                                usar_bbox = FALSE,
                                log_fn = message, lang = "es") {
  log_fn(tr("processing.log.loading_cloud", lang))
  las <- readLAS(ruta_las, select = "xyznr")

  bb   <- ext(las)
  area <- (bb[2]-bb[1]) * (bb[4]-bb[3])
  dens_orig <- round(nrow(las@data) / as.numeric(area), 1)
  log_fn(tr("processing.log.cloud_loaded", lang, format(nrow(las@data), big.mark="."), dens_orig))

  if (usar_bbox) {
    if (!is.null(crs_override)) {
      log_fn(tr("processing.log.assigning_crs_las", lang, as.integer(crs_override)))
      projection(las) <- crs(paste0("epsg:", as.integer(crs_override)))
    }
    wkt_las <- tryCatch(sf::st_crs(las)$wkt, error = function(e) NULL)
    las_tiene_crs <- !is.null(wkt_las) && !is.na(wkt_las) && nchar(trimws(wkt_las)) > 0
    roi <- as.polygons(ext(las))
    if (las_tiene_crs) terra::crs(roi) <- wkt_las
    area_roi_ha <- round(as.numeric(expanse(roi, unit = "ha")), 2)
    log_fn(tr("processing.log.roi_area", lang, area_roi_ha))
    return(list(las = las, roi = roi, dens_orig = dens_orig, area_ha = area_roi_ha))
  }

  log_fn(tr("processing.log.loading_roi", lang))
  roi <- vect(ruta_shp)
  area_roi_ha <- round(as.numeric(expanse(roi, unit="ha")), 2)
  log_fn(tr("processing.log.roi_area", lang, area_roi_ha))

  if (!is.null(crs_override)) {
    log_fn(tr("processing.log.assigning_crs_las", lang, as.integer(crs_override)))
    projection(las) <- crs(paste0("epsg:", as.integer(crs_override)))
  }

  if (!is.null(shp_crs_override)) {
    log_fn(tr("processing.log.assigning_crs_shp", lang, as.integer(shp_crs_override)))
    terra::crs(roi) <- paste0("EPSG:", as.integer(shp_crs_override))
  }

  wkt_las <- tryCatch(sf::st_crs(las)$wkt, error = function(e) NULL)
  las_tiene_crs <- !is.null(wkt_las) && !is.na(wkt_las) && nchar(trimws(wkt_las)) > 0
  if (las_tiene_crs) {
    wkt_roi       <- tryCatch(sf::st_crs(roi)$wkt, error = function(e) NULL)
    roi_tiene_crs <- !is.null(wkt_roi) && !is.na(wkt_roi) && nchar(trimws(wkt_roi)) > 0
    crs_las_wkt   <- sf::st_crs(las)$wkt
    if (!roi_tiene_crs) {
      log_fn(tr("processing.log.shp_no_crs", lang))
      terra::crs(roi) <- crs_las_wkt
      log_fn(tr("processing.log.crs_assigned", lang, as.integer(sf::st_crs(las)$epsg)))
    } else if (!isTRUE(sf::st_crs(las) == sf::st_crs(roi))) {
      log_fn(tr("processing.log.crs_mismatch", lang))
      roi <- project(roi, crs_las_wkt)
      log_fn(tr("processing.log.reprojection_done", lang))
    }
  }

  log_fn(tr("processing.log.clipping", lang, buffer_m))
  las_r <- tryCatch(
    clip_roi(las = las, geometry = st_as_sf(buffer(roi, buffer_m))),
    error = function(e) {
      stop(tr("processing.log.no_points_in_roi", lang))
    }
  )
  if (nrow(las_r@data) == 0L) {
    stop(tr("processing.log.no_points_in_roi", lang))
  }
  log_fn(tr("processing.log.cloud_clipped", lang, format(nrow(las_r@data), big.mark=".")))

  list(las = las_r, roi = roi, dens_orig = dens_orig, area_ha = area_roi_ha)
}

filtrar_nube <- function(las, densidad = 10, tipo = "aleatorio", log_fn = message, lang = "es") {
  log_fn(tr("processing.log.subsampling", lang, densidad, tipo))
  algo <- if (identical(tipo, "uniforme"))
    homogenize(density = densidad, res = 1, use_pulse = FALSE)
  else
    random(density = densidad)
  las <- decimate_points(las, algorithm = algo)

  log_fn(tr("processing.log.removing_duplicates", lang))
  las <- filter_duplicates(las)

  log_fn(tr("processing.log.removing_noise", lang))
  las <- classify_noise(las, algorithm = sor())
  las <- filter_poi(las, Classification != LASNOISE)

  log_fn(tr("processing.log.cloud_filtered", lang, format(nrow(las@data), big.mark=".")))
  las
}

# ──────────────────────────────────────────────────────────────────────────────
# CLASIFICACIÓN DE SUELO
# ──────────────────────────────────────────────────────────────────────────────
clasificar_suelo <- function(las, rigidness = 2, class_threshold = 0.5,
                              cloth_resolution = 2, sloop_smooth = FALSE,
                              log_fn = message, lang = "es") {
  rig_label <- switch(as.character(rigidness),
                      "1" = tr("processing.csf.quebrado", lang),
                      "2" = tr("processing.csf.ondulado", lang),
                      "3" = tr("processing.csf.llano",    lang))
  log_fn(tr("processing.log.soil_classification", lang, rig_label))

  las_c <- classify_ground(las,
                            algorithm = csf(sloop_smooth     = sloop_smooth,
                                            class_threshold  = class_threshold,
                                            cloth_resolution = cloth_resolution,
                                            rigidness        = rigidness,
                                            iterations       = 500,
                                            time_step        = 0.65))

  n <- sum(las_c@data$Classification == 2L, na.rm = TRUE)
  log_fn(tr("processing.log.soil_classified", lang, format(n, big.mark=".")))
  las_c
}

# ──────────────────────────────────────────────────────────────────────────────
# MODELOS DIGITALES
# ──────────────────────────────────────────────────────────────────────────────
generar_dem <- function(las_clasf, res_dem = 1, win_min = 3, win_mean = 9,
                        log_fn = message, lang = "es") {
  log_fn(tr("processing.log.generating_dem", lang, res_dem))
  dem <- rasterize_terrain(las_clasf, res = res_dem, algorithm = tin())

  # Forzar valores impares >= 3
  win_min  <- max(3L, as.integer(win_min)  %/% 2L * 2L + 1L)
  win_mean <- max(3L, as.integer(win_mean) %/% 2L * 2L + 1L)
  log_fn(tr("processing.log.smoothing", lang, win_min, win_min, win_mean, win_mean))

  dem_s <- focal(dem,   w = matrix(1L, win_min,  win_min),  fun = "min")
  dem_s <- focal(dem_s, w = matrix(1L, win_mean, win_mean), fun = "mean")

  g_dem <- terra::global(dem_s, fun = c("min","max","mean"), na.rm = TRUE)
  log_fn(tr("processing.log.dem_stats", lang, g_dem$min, g_dem$max, g_dem$mean))
  log_fn(tr("processing.log.dem_done", lang))

  list(dem = dem, dem_suav = dem_s,
       dem_min = round(g_dem$min,1), dem_max = round(g_dem$max,1),
       dem_mean = round(g_dem$mean,1))
}

generar_curvas_nivel <- function(dem_suav, roi, equidistancia = 0.5,
                                  log_fn = message, lang = "es") {
  log_fn(tr("processing.log.contours", lang, equidistancia))

  g     <- terra::global(dem_suav, fun = c("min","max"), na.rm = TRUE)
  e_min <- floor(  as.numeric(g[["min"]]) / equidistancia) * equidistancia
  e_max <- ceiling(as.numeric(g[["max"]]) / equidistancia) * equidistancia
  niveles <- seq(e_min, e_max, by = equidistancia)

  log_fn(tr("processing.log.contour_range", lang, e_min, e_max, length(niveles)))
  curvas <- as.contour(dem_suav, levels = niveles)
  curvas <- crop(curvas, roi)

  log_fn(tr("processing.log.contours_done", lang, length(curvas)))
  curvas
}

generar_hillshade <- function(dem_suav, angulo_sol = 45, azimut = 315,
                               log_fn = message, lang = "es") {
  log_fn(tr("processing.log.hillshade", lang, angulo_sol, azimut))

  hs <- shade(terrain(dem_suav, v = "slope",  unit = "radians"),
              terrain(dem_suav, v = "aspect", unit = "radians"),
              angle = angulo_sol, direction = azimut)

  log_fn(tr("processing.log.hillshade_done", lang))
  hs
}

normalizar_y_chm <- function(las_clasf, dem_suav, roi, res_chm = 0.5,
                              subcircle = 0.025, log_fn = message, lang = "es") {
  log_fn(tr("processing.log.normalizing", lang))
  dn <- las_clasf - dem_suav
  dn <- dn[dn$Z >= 0]
  dn <- clip_roi(las = dn, geometry = st_as_sf(roi))
  log_fn(tr("processing.log.cloud_normalized", lang, format(nrow(dn@data), big.mark=".")))

  log_fn(tr("processing.log.chm", lang, res_chm))
  chm <- rasterize_canopy(dn, res = res_chm,
                           algorithm = p2r(subcircle = subcircle,
                                           na.fill   = knnidw(k = 8, p = 2)))

  g_chm <- terra::global(chm, fun = c("min","max","mean"), na.rm = TRUE)
  log_fn(tr("processing.log.chm_stats", lang, g_chm$min, g_chm$max, g_chm$mean))
  log_fn(tr("processing.log.chm_done", lang))

  list(datos_norm = dn, chm = chm,
       chm_min = round(g_chm$min,1), chm_max = round(g_chm$max,1),
       chm_mean = round(g_chm$mean,1))
}

# ──────────────────────────────────────────────────────────────────────────────
# DETECCIÓN DE ÁRBOLES
# ──────────────────────────────────────────────────────────────────────────────
detectar_arboles <- function(datos_norm, ws = 6, hmin = 6, log_fn = message, lang = "es") {
  log_fn(tr("processing.log.detecting_trees", lang, ws, hmin))

  ttops <- locate_trees(datos_norm, lmf(ws = ws, hmin = hmin))

  log_fn(tr("processing.log.trees_detected_proc", lang, nrow(ttops)))
  ttops
}
