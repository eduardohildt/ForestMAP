# ForestMap - Herramienta INTA para Análisis LiDAR y Fotogrametría Forestal

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue.svg)](https://www.r-project.org/) [![lidR](https://img.shields.io/badge/powered%20by-lidR-2ecc71.svg)](https://github.com/r-lidar/lidR) [![License](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

> **Análisis de estructura forestal mediante nubes de puntos LiDAR/fotogramétricas**\
> *Construido sobre lidR: el estándar open-source para procesamiento LiDAR forestal*

**Autor:** Dr. Eduardo Hildt\
**Institución:** INTA EEA Montecarlo - Grupo Forestal\
**Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)\
**Versión:** 2026.5

------------------------------------------------------------------------

## Descripción

ForestMAP es una **plataforma de análisis geoespacial forestal** desarrollada íntegramente sobre el ecosistema [**lidR**](https://github.com/r-lidar/lidR) (Roussel et al., 2020, 2021, 2023), el paquete de referencia mundial para procesamiento de datos LiDAR aerotransportados en aplicaciones forestales.

### Capacidades

-   **Preprocesamiento avanzado**: Recorte, filtrado de ruido, clasificación de suelo CSF (Zhang et al., 2016), normalización altimétrica
-   **Modelado digital del terreno**: DEM con interpolación TIN, CHM a resoluciones configurables (0.25–5 m), hillshade, curvas de nivel
-   **Segmentación individual de árboles**: Algoritmo LMF (Local Maximum Filter) con ventanas adaptativas y umbrales de altura
-   **Métricas forestales**: Densidad (árboles/ha), cobertura de copas (%), distribución altimétrica, coeficiente de variación estructural
-   **Visualización científica**: Renderizado 3D interactivo (Plotly) y gráficos temáticos
-   **Exportación multiformato**: LAZ clasificado/normalizado, GeoTIFF georreferenciado, Shapefile con atributos dasométricos, informes PDF automatizados

### Requisitos de Entrada

| Archivo | Formato | Descripción |
|----|----|----|
| **Nube de puntos** | `.laz` / `.las` | LiDAR o fotogramétrica (estructura-desde-movimiento) |
| **Área de interés** | `.shp` | Polígono delimitador del rodal/parcela |

------------------------------------------------------------------------

## Requisitos del Sistema

### Software Requerido

| Componente | Versión Mínima | Notas |
|----|----|----|
| **R** | 4.0+ | [Descargar](https://cloud.r-project.org/) |
| **RStudio** | *Recomendado* | [Descargar](https://posit.co/download/rstudio-desktop/) |

### Dependencias R (CRAN)

``` r
# Core
shiny, shiny.i18n, bslib, DT, plotly, htmlwidgets, ggplot2

# Geoespacial
lidR, terra, sf, RCSF

# Informes
knitr, rmarkdown, tinytex

# Sistema
parallel, rstudioapi
```

### Hardware Recomendado

-   **RAM**: ≥16 GB (nubes \>10M puntos)
-   **CPU**: 4+ núcleos (procesamiento paralelo)
-   **Almacenamiento**: 50+ GB libres

------------------------------------------------------------------------

## Instalación

### 1. Clonar Repositorio

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Instalar Dependencias

**Script automático** *(recomendado)*

`PowerShell o CMD start 'C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe' .\scripts\install.R`

------------------------------------------------------------------------

## Ejecución

### Opción 1: RStudio

1.  Abrir `run.R` en RStudio
2.  Clic en **"Run App"** (esquina superior derecha)

### Opción 2: Terminal

`PowerShell o CMD start 'C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe' .\run.R`

La aplicación se abrirá automáticamente en el navegador en `http://localhost:3838`

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

Ejecutar **completo** desde `run.R` (no ejecutar líneas individuales)

------------------------------------------------------------------------

## Créditos

**Desarrollado por:**\
Dr. Eduardo Hildt\
INTA EEA Montecarlo - Grupo Forestal\
Misiones, Argentina

**Tecnología núcleo:**\
[**lidR package**](https://github.com/r-lidar/lidR) (Roussel et al., 2020, 2021, 2023)\
*The reference open-source software for airborne LiDAR data processing in forestry*

**En apoyo a la Red de Drones INTA**\
Herramienta institucional para facilitar el análisis de datos aerotransportados en el sector forestal argentino.

------------------------------------------------------------------------

## Referencias Clave

**lidR - Algoritmos Forestales:** - Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Clasificación de Suelo:** - Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dasometría:** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Contacto y Soporte

-   **Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Institución:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Soporte:** Abrir issue en este repositorio

**Licencia:** Propiedad intelectual de INTA (Instituto Nacional de Tecnología Agropecuaria). Uso institucional, académico y de investigación. Para uso comercial, contactar al autor.
