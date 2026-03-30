# ==============================================================================
# MÓDULO DE EXPORTACIÓN
# ==============================================================================
# Funciones para guardar productos: nubes de puntos, rasters y vectoriales
# ==============================================================================

exportar_todos <- function(carpeta, las_clasf, datos_norm, dem_suav,
                            chm, curvas, hillshade, arboles, roi, 
                            copas_vect = NULL,
                            log_fn = message) {
  log_fn("💾 Exportando todos los productos...")
  
  # Crear carpetas si no existen
  bn <- file.path(carpeta, "Salidas_NUBES")
  br <- file.path(carpeta, "Salidas_RASTER")
  bv <- file.path(carpeta, "Salidas_VECTORIALES")
  
  if (!dir.exists(bn)) {
    dir.create(bn, recursive = TRUE, showWarnings = FALSE)
    log_fn("  ✓ Creada carpeta: Salidas_NUBES")
  }
  if (!dir.exists(br)) {
    dir.create(br, recursive = TRUE, showWarnings = FALSE)
    log_fn("  ✓ Creada carpeta: Salidas_RASTER")
  }
  if (!dir.exists(bv)) {
    dir.create(bv, recursive = TRUE, showWarnings = FALSE)
    log_fn("  ✓ Creada carpeta: Salidas_VECTORIALES")
  }
  
  # Nubes de puntos
  log_fn("  → Exportando nubes LAZ...")
  writeLAS(las_clasf,  file.path(bn, "puntos_clasificados.laz"))
  writeLAS(datos_norm, file.path(bn, "puntos_normalizados.laz"))
  
  # Rasters (recortados y enmascarados al ROI)
  log_fn("  → Exportando rasters GeoTIFF...")
  writeRaster(crop(dem_suav,  roi, mask=TRUE), 
              file.path(br,"DEM.tif"), 
              datatype="FLT4S", overwrite=TRUE)
  writeRaster(crop(chm, roi, mask=TRUE), 
              file.path(br,"CHM.tif"), 
              datatype="FLT4S", overwrite=TRUE)
  writeRaster(crop(hillshade, roi, mask=TRUE), 
              file.path(br,"Hillshade.tif"), 
              datatype="FLT4S", overwrite=TRUE)
  
  # Vectoriales
  log_fn("  → Exportando shapefiles...")
  writeVector(curvas, file.path(bv,"Curvas_Nivel.shp"), overwrite=TRUE)
  writeVector(vect(arboles), file.path(bv,"Arboles.shp"), overwrite=TRUE)
  writeVector(roi, file.path(bv,"Area_Interes.shp"), overwrite=TRUE)
  
  # Cobertura de copas (opcional)
  if (!is.null(copas_vect) && nrow(copas_vect) > 0) {
    log_fn("  → Exportando cobertura de copas...")
    tryCatch({
      writeVector(copas_vect, 
                  file.path(bv, "Cobertura_Copas.shp"), 
                  overwrite = TRUE)
      log_fn("    ✓ Cobertura_Copas.shp guardado")
    }, error = function(e) {
      log_fn(paste("    ⚠ Error al exportar cobertura de copas:", conditionMessage(e)))
    })
  } else {
    log_fn("  ⊗ Cobertura de copas no disponible (omitido)")
  }
  
  log_fn("✅ Exportación completa.")
  invisible(TRUE)
}
