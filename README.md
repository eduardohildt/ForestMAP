# ForestMAP - Herramienta INTA para Análisis LiDAR y Fotogrametría Forestal

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue.svg)](https://www.r-project.org/) [![License](https://img.shields.io/badge/License-INTA-green.svg)](https://inta.gob.ar)

Aplicación Shiny para procesamiento y análisis de nubes de puntos LiDAR/fotogramétricos en ecosistemas forestales.

**Autor:** Dr. Eduardo Hildt\
**Institución:** INTA EEA Montecarlo - Grupo Forestal\
**Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar){.email}\
**Versión:** 2026.5

------------------------------------------------------------------------

## Descripción

ForestMAP es una herramienta interactiva basada en el paquete lidR (Roussel et al., 2020).

-   **Procesamiento LiDAR**: Filtrado, clasificación de suelo (CSF), normalización
-   **Modelos digitales**: DEM, DSM, CHM, hillshade, curvas de nivel
-   **Detección de árboles**: Local Maximum Filter (LMF) con parámetros ajustables
-   **Visualización 3D**: Nubes de puntos y rasters interactivos (Plotly)
-   **Exportación**: LAZ, GeoTIFF, Shapefile, informes PDF

Los datos de entrada necesarios son:
- 	**Nueve de puntos LiDAR o fotogramétrica, en formato LAZ**
-	**Archivo SHP que contenga el área de interés a analizar**
-   **¡Importante!: Ambos archivos deben contar con el mismo sistema de coordenadas (CRS)**

------------------------------------------------------------------------

## Requisitos del Sistema

### Software Requerido

| Componente | Versión Mínima | Notas |
|------------------------|-------------------------------|-----------------|
| **R** | 4.0+ | [Descargar](https://cloud.r-project.org/) |
| **RStudio** | *Recomendado* | [Descargar](https://posit.co/download/rstudio-desktop/) |

### Dependencias R (CRAN)

``` r
# Core
shiny, bslib, DT, plotly, htmlwidgets, ggplot2

# Geoespacial
lidR, terra, sf, RCSF

# Informes
knitr, rmarkdown, fancyhdr

# Sistema
parallel, rstudioapi
```

### Hardware Recomendado

-   **RAM**: ≥16 GB (nubes \>10M puntos)
-   **CPU**: 4+ núcleos (procesamiento paralelo)
-   **Almacenamiento**: 50+ GB libres (datos LiDAR son pesados)

------------------------------------------------------------------------

## Instalación

### 1. Clonar Repositorio

``` bash
git clone https://github.com/tu-usuario/ForestMAP.git
cd ForestMAP
```

### 2. Instalar Dependencias

**Opción A: Script automático** *(recomendado)*

``` bash
Rscript scripts/install.R
```

**Opción B: Manual en R**

``` r
install.packages(c(
  "shiny", "bslib", "DT", "plotly", "lidR", "terra", 
  "sf", "RCSF", "htmlwidgets", "ggplot2", "knitr", "rmarkdown", "fancyhdr"
))
```

------------------------------------------------------------------------

## Ejecución

### Opción 1: Terminal

``` bash
Rscript run.R
```

La aplicación se abrirá automáticamente en el navegador en `http://localhost:3838`

### Opción 2: RStudio

1.  Abrir `app.R` en RStudio
2.  Clic en **"Run App"** (esquina superior derecha)

------------------------------------------------------------------------

## Estructura del Proyecto

```         
ForestMAP/
├── app.R                      # Punto de entrada principal
├── run.R                      # Launcher desde terminal
├── R/
│   ├── colors_light.R         # Paleta de colores científica
│   ├── processing.R           # Pipeline LiDAR (CSF, DEM, CHM)
│   ├── visualization.R        # Plotly 2D/3D, helpers UI
│   ├── export.R               # Exportación LAZ/GeoTIFF/SHP
│   ├── report_generator.R     # Informes PDF automatizados
│   ├── ui.R                   # Interfaz Shiny
│   └── server.R               # Lógica servidor
├── assets/
│   └── logo_INTA.png          # Logo institucional
├── scripts/
│   └── install.R              # Instalador automático
├── .gitignore
└── README.md
```

------------------------------------------------------------------------

## Uso Básico

### Flujo de Trabajo

1.  **Configuración Inicial**
    -   Seleccionar archivo LAS/LAZ (nube de puntos)
    -   Seleccionar shapefile (área de interés)
    -   Elegir carpeta de salida
2.  **Preprocesamiento**
    -   Ajustar densidad de submuestreo
    -   Configurar parámetros CSF (clasificación suelo)
    -   Ejecutar filtrado + clasificación
3.  **Generación de Modelos**
    -   DEM: resolución, ventanas de suavizado
    -   CHM: resolución, algoritmo p2r
    -   Hillshade: ángulo solar, azimut
4.  **Detección de Árboles**
    -   Ventana móvil: ventana de búsqueda (ws), altura mínima (hmin)
5.  **Exportación**
    -   Productos espaciales: LAZ, GeoTIFF, Shapefile
    -   Informe PDF descriptivo con estadísticas

------------------------------------------------------------------------

## Notas Técnicas

### Algoritmos Clave

-   **CSF** (Cloth Simulation Filter): Clasificación de puntos de suelo

    -   Rigidez: 1=quebrado, 2=ondulado, 3=llano
    -   Umbral: distancia vertical de clasificación

-   **TIN** (Triangulated Irregular Network): Interpolación DEM

-   **p2r + knnidw**: Generación CHM con relleno adaptativo

-   **LMF** (Local Maximum Filter): Detección de ápices arbóreos

### Procesamiento Paralelo

La aplicación detecta automáticamente núcleos de CPU y permite procesamiento multi-hilo en: - Filtrado de ruido (SOR) - Generación de modelos digitales

### Límites

-   Tamaño máximo archivo: **25 GB** (configurable en `app.R`)

------------------------------------------------------------------------

## Solución de problemas

### Error: "No se encontró el paquete X"

``` bash
Rscript scripts/install.R
```

### Error: "Memory allocation failed"

-   Reducir densidad de submuestreo (ej: 5 pts/m²)
-   Cerrar otras aplicaciones
-   Aumentar RAM disponible

### Shiny no abre en navegador

Verificar puerto 3838:

``` bash
netstat -an | grep 3838  # Linux/Mac
netstat -an | findstr 3838  # Windows
```

### RStudio: "Object not found"

Ejecutar **completo** desde `app.R` (no ejecutar líneas individuales)

------------------------------------------------------------------------

## Créditos

**Desarrollado por:**\
Dr. Eduardo Hildt\
INTA EEA Montecarlo - Grupo Forestal\
Misiones, Argentina

**Red de Drones INTA**\
Herramienta para análisis forestal con datos aerotransportados

------------------------------------------------------------------------

## Licencia

Propiedad intelectual de INTA (Instituto Nacional de Tecnología Agropecuaria).\
Uso interno y académico. Para uso comercial, contactar: [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar){.email}

------------------------------------------------------------------------

## Contacto

-   **Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar){.email}
-   **Institución:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Soporte:** Abrir issue en este repositorio
