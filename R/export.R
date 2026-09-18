# ==============================================================================
# MÓDULO DE EXPORTACIÓN
# ==============================================================================
# Funciones para guardar productos: rasters y vectoriales
# Los nombres de carpetas y archivos se traducen al idioma activo de la
# interfaz en el momento de la exportación (ver R/i18n.R, claves folders.*/files.*)
# ==============================================================================

exportar_todos <- function(carpeta, dem_suav, chm, curvas, hillshade, arboles, roi,
                            copas_vect = NULL, formato_vectorial = "shp",
                            lang = "es", log_fn = message) {
  log_fn(tr("export.log.exporting_all", lang))

  fmt <- switch(formato_vectorial,
    geojson = list(ext = ".geojson", filetype = "GeoJSON"),
    list(ext = ".shp", filetype = "ESRI Shapefile"))

  nombre_raster <- tr("folders.raster", lang)
  nombre_vector <- tr("folders.vector", lang)

  # Crear carpetas si no existen (recién ahora, al exportar)
  br <- file.path(carpeta, nombre_raster)
  bv <- file.path(carpeta, nombre_vector)

  if (!dir.exists(br)) {
    dir.create(br, recursive = TRUE, showWarnings = FALSE)
    log_fn(tr("export.log.folder_created", lang, nombre_raster))
  }
  if (!dir.exists(bv)) {
    dir.create(bv, recursive = TRUE, showWarnings = FALSE)
    log_fn(tr("export.log.folder_created", lang, nombre_vector))
  }

  # Rasters (recortados y enmascarados al ROI)
  log_fn(tr("export.log.exporting_rasters", lang))
  writeRaster(crop(dem_suav,  roi, mask=TRUE),
              file.path(br, paste0(tr("files.dem", lang), ".tif")),
              datatype="FLT4S", overwrite=TRUE)
  writeRaster(crop(chm, roi, mask=TRUE),
              file.path(br, paste0(tr("files.chm", lang), ".tif")),
              datatype="FLT4S", overwrite=TRUE)
  writeRaster(crop(hillshade, roi, mask=TRUE),
              file.path(br, paste0(tr("files.hillshade", lang), ".tif")),
              datatype="FLT4S", overwrite=TRUE)

  # Vectoriales
  log_fn(tr("export.log.exporting_vectors", lang, toupper(formato_vectorial)))
  writeVector(curvas, file.path(bv, paste0(tr("files.contour_lines", lang), fmt$ext)),
              filetype = fmt$filetype, overwrite=TRUE)
  writeVector(vect(arboles), file.path(bv, paste0(tr("files.trees", lang), fmt$ext)),
              filetype = fmt$filetype, overwrite=TRUE)
  writeVector(roi, file.path(bv, paste0(tr("files.roi", lang), fmt$ext)),
              filetype = fmt$filetype, overwrite=TRUE)

  # Cobertura de copas (opcional)
  if (!is.null(copas_vect) && nrow(copas_vect) > 0) {
    log_fn(tr("export.log.exporting_canopy", lang))
    nombre_copas <- paste0(tr("files.canopy_cover", lang), fmt$ext)
    tryCatch({
      writeVector(copas_vect, file.path(bv, nombre_copas),
                  filetype = fmt$filetype, overwrite = TRUE)
      log_fn(tr("export.log.canopy_saved", lang, nombre_copas))
    }, error = function(e) {
      log_fn(tr("export.log.canopy_error", lang, conditionMessage(e)))
    })
  } else {
    log_fn(tr("export.log.canopy_unavailable", lang))
  }

  log_fn(tr("export.log.export_complete", lang))
  invisible(TRUE)
}
