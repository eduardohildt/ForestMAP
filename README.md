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
|---------------------|---------------------|------------------------------|
| **Nube de puntos** | `.laz` / `.las` | LiDAR o fotogramétrica (estructura-desde-movimiento) |
| **Área de interés** | `.shp` | Polígono delimitador del rodal/parcela |

⚠️ **Crítico**: Ambos archivos deben compartir el mismo sistema de coordenadas (CRS/EPSG).

------------------------------------------------------------------------

## Requisitos del Sistema

### Software Requerido

| Componente | Versión Mínima | Notas |
|------------------------|------------------------------|------------------|
| **R** | 4.0+ | [Descargar](https://cloud.r-project.org/) |
| **RStudio** | *Recomendado* | [Descargar](https://posit.co/download/rstudio-desktop/) |

### Dependencias R (CRAN)

``` r
# Core
shiny, bslib, DT, plotly, htmlwidgets, ggplot2

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

**Opción A: Script automático** *(recomendado)*

``` PowerShell o CMD
start 'C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe' .\scripts\install.R
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

``` PowerShell o CMD
start 'C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe' .\run.R
```

La aplicación se abrirá automáticamente en el navegador en `http://localhost:3838`

### Opción 2: RStudio

1.  Abrir `run.R` en RStudio
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
└── README.md
```

------------------------------------------------------------------------

## Uso Básico

### Flujo de Trabajo

#### 1. Configuración Inicial

##### Panel: Configuración

```         
📂 Seleccionar archivo LAZ/LAS (nube de puntos)
📂 Seleccionar shapefile ROI (área de interés)
📂 Elegir carpeta de salida
```

#### 2. Preprocesamiento

##### Panel: Carga de datos

| Parámetro | Rango | Recomendado | Efecto |
|--------------|------------|--------------|-------------------------|
| **Densidad submuestreo** | 1–50 pts/m² | 5–10 | Reduce tiempo de cómputo manteniendo calidad |
| **CSF Rigidez** | 1–3 | 2 (ondulado) | 1=abrupto, 2=moderado, 3=llano |
| **CSF Umbral** | 0.1–2 m | 0.5 | Distancia vertical dentro de la que se clasifica el suelo |

**Ejecutar**: `[Filtrar y Clasificar]` → genera `puntos_clasificados.laz`

#### 3. Generación de Modelos Digitales

##### Panel: Modelos Digitales

| Modelo | Resolución | Suavizado | Producto |
|-----------|------------|----------------------|----------------------------|
| **DEM** | 0.5–5 m | Ventana 3×3 (mín) + 9×9 (media) | Topografía base y curvas de nivel |
| **CHM** | 0.25–2 m | Algoritmo p2r + knnidw | Alturas de copas |
| **Hillshade** | Igual DEM | Ángulo 45°, azimut 315° | Visualización relieve |

**Ejecutar**: `[Generar Modelos]` → visualización 3D interactiva

#### 4. Detección de Árboles Individuales

##### Panel: Árboles

| Parámetro | Función | Típico | Ajuste según... |
|-----------------|-----------------|-----------------|---------------------|
| **ws** (ventana) | Diámetro búsqueda máximos locales | 3–8 m | Densidad plantación: ↓ ws para alta densidad |
| **hmin** | Altura mínima ápice válido | 10–15 m | Madurez rodal: ↑ hmin para excluir regeneración |
| **umbral** | Cálculo de la cobertura | 4 m | Altura de la base de la copa |

**Ejecutar**: `[Detectar Árboles]` → shapefile `Arboles.shp` con atributo `Z` (altura)

#### 5. Exportación

##### Panel: Exportar

**Estructura de salidas**:

```         
📁 Salidas_NUBES/
   ├── puntos_clasificados.laz    # Clase 2=suelo, resto=vegetación
   └── puntos_normalizados.laz    # Z referido al terreno

📁 Salidas_RASTER/
   ├── DEM.tif                    # Modelo elevación (EPSG original)
   ├── CHM.tif                    # Modelo altura copas
   └── Hillshade.tif              # Sombreado relieve

📁 Salidas_VECTORIALES/
   ├── Curvas_Nivel.shp           # Isolíneas equidistancia 1 m
   ├── Arboles.shp                # Puntos con atributo altura
   ├── Area_Interes.shp           # Polígono ROI analizado
   └── Cobertura_Copas.shp        # Polígonos vectorizados

📄 Informe_Analisis.pdf           # Reporte técnico automatizado
```

------------------------------------------------------------------------

## Productos del Informe Automatizado

El informe PDF generado (`Informe_Analisis.pdf`) incluye:

### Sección 1: Metadatos del Relevamiento

-   Usuario, fecha de análisis, archivos procesados
-   Autor institucional, contacto técnico

### Sección 2: Características del Área

-   Superficie analizada (hectáreas)
-   Densidad original/submuestreada de la nube de puntos
-   Puntos clasificados como suelo (algoritmo CSF)

### Sección 3: Topografía

-   **DEM con curvas de nivel**: Elevación absoluta, rango altitudinal
-   **Hillshade**: Visualización de micro-relieve y pendientes

### Sección 4: Estructura Forestal

-   **CHM (Canopy Height Model)**: Distribución espacial de alturas
-   **Vista de árboles detectados**: Cruces sobre CHM con gradiente de color
-   **Mapa de cobertura de copas**

## Notas Técnicas

### Algoritmos Clave

-   **CSF** (Cloth Simulation Filter): Clasificación de puntos de suelo

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
