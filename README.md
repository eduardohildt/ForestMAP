<h1>
  <span style="color:#E63946; font-weight:900; font-family:'Exo 2',sans-serif;">INTA</span>
  <span style="color:#2d7a3d; font-weight:700;"> ForestMap</span>
</h1>

**Herramienta para Análisis LiDAR y Fotogrametría Forestal**

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue.svg)](https://www.r-project.org/) [![lidR](https://img.shields.io/badge/powered%20by-lidR-2ecc71.svg)](https://github.com/r-lidar/lidR) [![License](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

> **Análisis de estructura forestal mediante nubes de puntos LiDAR/fotogramétricas**\
> *Construido sobre lidR: el estándar open-source para procesamiento LiDAR forestal*

**Autor:** Dr. Eduardo Hildt\
**Institución:** INTA EEA Montecarlo - Grupo Forestal\
**Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)\
**Versión:** 2026.10 — Last Update: Octubre 2026

[Español](#español) | [English](#english) | [Português](#português) | [Deutsch](#deutsch) | [Français](#français) | [Italiano](#italiano) | [Svenska](#svenska)

------------------------------------------------------------------------

# Español

## Descripción

ForestMap es una **plataforma de análisis geoespacial forestal** desarrollada íntegramente sobre el ecosistema [**lidR**](https://github.com/r-lidar/lidR) (Roussel et al., 2020, 2021, 2023), el paquete de referencia mundial para procesamiento de datos LiDAR aerotransportados en aplicaciones forestales.

### Capacidades

-   **Preprocesamiento avanzado**: Recorte, submuestreo con reporte de densidad de puntos, filtrado de ruido, clasificación de suelo CSF (Zhang et al., 2016), normalización de la nube de puntos
-   **Modelado digital del terreno**: DEM con interpolación TIN, curvas de nivel desde 0.1 m de equidistancia, CHM a resoluciones configurables desde 0.05 m, hillshade
-   **Segmentación individual de árboles**: Algoritmo LMF (Local Maximum Filter) con ventanas adaptativas y umbrales de altura
-   **Métricas forestales**: Densidad (árboles/ha), cobertura de copas (%), distribución altimétrica, coeficiente de variación estructural
-   **Visualización científica**: Renderizado 3D interactivo (Plotly) y gráficos temáticos
-   **Proyectos organizados**: cada análisis se guarda en una carpeta `Proyecto_<nombre>/` (con el nombre definido por el usuario) dentro del directorio de salida elegido, con subcarpetas `RASTER/`, `VECTORIALES/` e `INFORME/`
-   **Exportación multiformato**: GeoTIFF georreferenciado (DEM/CHM/Hillshade) y capas vectoriales a elección en **Shapefile o GeoJSON**, con informes HTML/PDF automatizados
-   **Multi-idioma**: interfaz, nombres de carpetas/archivos de salida e informe totalmente traducidos a 7 idiomas (Español, English, Português, Deutsch, Français, Italiano, Svenska), según el idioma activo al momento de exportar

### Requisitos de Entrada

| Archivo | Formato | Descripción |
|----|----|----|
| **Nube de puntos** | `.laz` / `.las` | LiDAR o fotogramétrica (estructura-desde-movimiento) |
| **Área de interés** | `.shp` / `.kml` / `.geojson` | Polígono delimitador del rodal/parcela |

------------------------------------------------------------------------

## Requisitos del Sistema

### Software Requerido

| Componente | Versión Mínima | Notas |
|----|----|----|
| **R** | 4.0+ | [Descargar](https://cloud.r-project.org/) |

**Recomendación de hardware**
-   **RAM**: ≥16 GB (nubes \>10M puntos)
-   **CPU**: 4+ núcleos (procesamiento paralelo)
-   **Almacenamiento**: 50+ GB libres

------------------------------------------------------------------------

## Instalación

### 1. Obtener el repositorio

Descargar en formato ZIP y descomprimir, o clonar:

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Instalar Dependencias

**Windows:** ejecutar `Instalar_Dependencias.cmd` (doble clic).

**Linux/macOS:** ejecutar `Rscript dev/install.R`. Para que los cuadros de diálogo de selección de archivos funcionen, instale también el soporte Tcl/Tk de su sistema (ej. `sudo apt install r-cran-tcltk` en Debian/Ubuntu).

------------------------------------------------------------------------

## Ejecución

**Windows:** ejecutar `Ejecutar_ForestMap.cmd` (doble clic).

**Linux/macOS:** ejecutar `Rscript dev/run.R`.

La aplicación se abrirá automáticamente en el navegador en `http://localhost:3838`

------------------------------------------------------------------------

## Solución de problemas

### Error: "No se encontró el paquete X"

**Windows:** ejecutar `Instalar_Dependencias.cmd`
**Linux/macOS:** ejecutar `Rscript dev/install.R`

### Error: no aparece el cuadro de diálogo de selección de archivos (Linux)

Instale el soporte Tcl/Tk de R (ej. `sudo apt install r-cran-tcltk` en Debian/Ubuntu) o ejecute la aplicación desde RStudio.

### Error: "Memory allocation failed"

-   Reducir densidad de submuestreo (ej: 5 pts/m²)
-   Cerrar otras aplicaciones
-   Aumentar RAM disponible

------------------------------------------------------------------------


## Referencias Clave

**lidR - Algoritmos Forestales:** 

- Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Clasificación de Suelo:** 

- Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dasometría:** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Contacto y Soporte

-   **Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Institución:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Soporte:** Abrir issue en este repositorio
-   **Comentarios y sugerencias:** [Completar formulario](https://forms.gle/8YokYp4K3CCrzggG8)

**Licencia:** [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0) — libre uso, modificación y distribución. Las obras derivadas deben mantener la misma licencia. Copyright © INTA / Dr. Eduardo Hildt.

------------------------------------------------------------------------

# English

## Description

ForestMap is a **forest geospatial analysis platform** built entirely on the [**lidR**](https://github.com/r-lidar/lidR) ecosystem (Roussel et al., 2020, 2021, 2023), the world's reference package for processing airborne LiDAR data in forestry applications.

### Capabilities

-   **Advanced preprocessing**: Clipping, subsampling with point density reporting, noise filtering, CSF ground classification (Zhang et al., 2016), point cloud normalization
-   **Digital terrain modeling**: DEM with TIN interpolation, contour lines from 0.1 m equidistance, CHM at configurable resolutions from 0.05 m, hillshade
-   **Individual tree segmentation**: LMF (Local Maximum Filter) algorithm with adaptive windows and height thresholds
-   **Forest metrics**: Density (trees/ha), canopy cover (%), height distribution, structural coefficient of variation
-   **Scientific visualization**: Interactive 3D rendering (Plotly) and thematic charts
-   **Organized projects**: each analysis is saved in a `Project_<name>/` folder (with the name defined by the user) inside the chosen output directory, with `RASTER/`, `VECTOR/`, and `REPORT/` subfolders
-   **Multi-format export**: Georeferenced GeoTIFF (DEM/CHM/Hillshade) and vector layers of your choice in **Shapefile or GeoJSON**, with automated HTML/PDF reports
-   **Multi-language**: interface, output folder/file names, and report fully translated into 7 languages (Español, English, Português, Deutsch, Français, Italiano, Svenska), according to the active language at export time

### Input Requirements

| File | Format | Description |
|----|----|----|
| **Point cloud** | `.laz` / `.las` | LiDAR or photogrammetric (structure-from-motion) |
| **Area of interest** | `.shp` / `.kml` / `.geojson` | Boundary polygon of the stand/plot |

------------------------------------------------------------------------

## System Requirements

### Required Software

| Component | Minimum Version | Notes |
|----|----|----|
| **R** | 4.0+ | [Download](https://cloud.r-project.org/) |

**Hardware recommendation**
-   **RAM**: ≥16 GB (clouds \>10M points)
-   **CPU**: 4+ cores (parallel processing)
-   **Storage**: 50+ GB free

------------------------------------------------------------------------

## Installation

### 1. Get the repository

Download as a ZIP file and extract it, or clone:

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Install Dependencies

**Windows:** run `Instalar_Dependencias.cmd` (double-click).

**Linux/macOS:** run `Rscript dev/install.R`. For the file selection dialogs to work, also install your system's Tcl/Tk support (e.g. `sudo apt install r-cran-tcltk` on Debian/Ubuntu).

------------------------------------------------------------------------

## Running the Application

**Windows:** run `Ejecutar_ForestMap.cmd` (double-click).

**Linux/macOS:** run `Rscript dev/run.R`.

The application will open automatically in your browser at `http://localhost:3838`

------------------------------------------------------------------------

## Troubleshooting

### Error: "Package X not found"

**Windows:** run `Instalar_Dependencias.cmd`
**Linux/macOS:** run `Rscript dev/install.R`

### Error: the file selection dialog does not appear (Linux)

Install R's Tcl/Tk support (e.g. `sudo apt install r-cran-tcltk` on Debian/Ubuntu) or run the application from RStudio.

### Error: "Memory allocation failed"

-   Reduce the subsampling density (e.g., 5 pts/m²)
-   Close other applications
-   Increase available RAM

------------------------------------------------------------------------

## Key References

**lidR - Forestry Algorithms:**

- Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Ground Classification:**

- Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dendrometry:** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Contact and Support

-   **Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Institution:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Support:** Open an issue in this repository
-   **Feedback and suggestions:** [Fill out the form](https://forms.gle/8YokYp4K3CCrzggG8)

**License:** [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0) — free to use, modify, and distribute. Derivative works must keep the same license. Copyright © INTA / Dr. Eduardo Hildt.

------------------------------------------------------------------------

# Português

## Descrição

O ForestMap é uma **plataforma de análise geoespacial florestal** desenvolvida integralmente sobre o ecossistema [**lidR**](https://github.com/r-lidar/lidR) (Roussel et al., 2020, 2021, 2023), o pacote de referência mundial para o processamento de dados LiDAR aerotransportados em aplicações florestais.

### Capacidades

-   **Pré-processamento avançado**: Recorte, subamostragem com relatório de densidade de pontos, filtragem de ruído, classificação de solo CSF (Zhang et al., 2016), normalização da nuvem de pontos
-   **Modelagem digital do terreno**: MDT (DEM) com interpolação TIN, curvas de nível a partir de 0,1 m de equidistância, MDC (CHM) em resoluções configuráveis a partir de 0,05 m, hillshade
-   **Segmentação individual de árvores**: Algoritmo LMF (Local Maximum Filter) com janelas adaptativas e limiares de altura
-   **Métricas florestais**: Densidade (árvores/ha), cobertura de copas (%), distribuição altimétrica, coeficiente de variação estrutural
-   **Visualização científica**: Renderização 3D interativa (Plotly) e gráficos temáticos
-   **Projetos organizados**: cada análise é salva em uma pasta `Projeto_<nome>/` (com o nome definido pelo usuário) dentro do diretório de saída escolhido, com subpastas `RASTER/`, `VETORIAL/` e `RELATORIO/`
-   **Exportação multiformato**: GeoTIFF georreferenciado (DEM/CHM/Hillshade) e camadas vetoriais à escolha em **Shapefile ou GeoJSON**, com relatórios HTML/PDF automatizados
-   **Multi-idioma**: interface, nomes de pastas/arquivos de saída e relatório totalmente traduzidos para 7 idiomas (Español, English, Português, Deutsch, Français, Italiano, Svenska), conforme o idioma ativo no momento da exportação

### Requisitos de Entrada

| Arquivo | Formato | Descrição |
|----|----|----|
| **Nuvem de pontos** | `.laz` / `.las` | LiDAR ou fotogramétrica (estrutura a partir de movimento) |
| **Área de interesse** | `.shp` / `.kml` / `.geojson` | Polígono delimitador do talhão/parcela |

------------------------------------------------------------------------

## Requisitos do Sistema

### Software Necessário

| Componente | Versão Mínima | Notas |
|----|----|----|
| **R** | 4.0+ | [Baixar](https://cloud.r-project.org/) |

**Recomendação de hardware**
-   **RAM**: ≥16 GB (nuvens \>10M pontos)
-   **CPU**: 4+ núcleos (processamento paralelo)
-   **Armazenamento**: 50+ GB livres

------------------------------------------------------------------------

## Instalação

### 1. Obter o repositório

Baixe em formato ZIP e descompacte, ou clone:

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Instalar Dependências

**Windows:** execute `Instalar_Dependencias.cmd` (duplo clique).

**Linux/macOS:** execute `Rscript dev/install.R`. Para que as caixas de diálogo de seleção de arquivos funcionem, instale também o suporte Tcl/Tk do seu sistema (ex.: `sudo apt install r-cran-tcltk` no Debian/Ubuntu).

------------------------------------------------------------------------

## Execução

**Windows:** execute `Ejecutar_ForestMap.cmd` (duplo clique).

**Linux/macOS:** execute `Rscript dev/run.R`.

A aplicação será aberta automaticamente no navegador em `http://localhost:3838`

------------------------------------------------------------------------

## Solução de problemas

### Erro: "Pacote X não encontrado"

**Windows:** execute `Instalar_Dependencias.cmd`
**Linux/macOS:** execute `Rscript dev/install.R`

### Erro: a caixa de diálogo de seleção de arquivos não aparece (Linux)

Instale o suporte Tcl/Tk do R (ex.: `sudo apt install r-cran-tcltk` no Debian/Ubuntu) ou execute a aplicação a partir do RStudio.

### Erro: "Memory allocation failed"

-   Reduzir a densidade de subamostragem (ex.: 5 pts/m²)
-   Fechar outras aplicações
-   Aumentar a RAM disponível

------------------------------------------------------------------------

## Referências-Chave

**lidR - Algoritmos Florestais:**

- Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Classificação de Solo:**

- Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dasometria:** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Contato e Suporte

-   **Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Instituição:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Suporte:** Abra uma issue neste repositório
-   **Comentários e sugestões:** [Preencher formulário](https://forms.gle/8YokYp4K3CCrzggG8)

**Licença:** [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0) — livre uso, modificação e distribuição. As obras derivadas devem manter a mesma licença. Copyright © INTA / Dr. Eduardo Hildt.

------------------------------------------------------------------------

# Deutsch

## Beschreibung

ForestMap ist eine **geoforstliche Analyseplattform**, die vollständig auf dem [**lidR**](https://github.com/r-lidar/lidR)-Ökosystem (Roussel et al., 2020, 2021, 2023) aufbaut, dem weltweiten Referenzpaket für die Verarbeitung luftgestützter LiDAR-Daten in forstlichen Anwendungen.

### Funktionen

-   **Fortschrittliche Vorverarbeitung**: Zuschnitt, Subsampling mit Bericht zur Punktdichte, Rauschfilterung, CSF-Bodenklassifizierung (Zhang et al., 2016), Normalisierung der Punktwolke
-   **Digitale Geländemodellierung**: DGM (DEM) mit TIN-Interpolation, Höhenlinien ab 0,1 m Äquidistanz, Kronenhöhenmodell (CHM) in konfigurierbaren Auflösungen ab 0,05 m, Schummerung (Hillshade)
-   **Einzelbaumsegmentierung**: LMF-Algorithmus (Local Maximum Filter) mit adaptiven Fenstern und Höhenschwellenwerten
-   **Forstliche Kennzahlen**: Dichte (Bäume/ha), Kronendeckungsgrad (%), Höhenverteilung, struktureller Variationskoeffizient
-   **Wissenschaftliche Visualisierung**: Interaktive 3D-Darstellung (Plotly) und thematische Grafiken
-   **Organisierte Projekte**: jede Analyse wird in einem Ordner `Projekt_<Name>/` (mit vom Benutzer festgelegtem Namen) im gewählten Ausgabeverzeichnis gespeichert, mit den Unterordnern `RASTER/`, `VEKTOR/` und `BERICHT/`
-   **Multiformat-Export**: georeferenziertes GeoTIFF (DEM/CHM/Hillshade) und Vektorebenen nach Wahl als **Shapefile oder GeoJSON**, mit automatisierten HTML/PDF-Berichten
-   **Mehrsprachigkeit**: Oberfläche, Namen der Ausgabeordner/-dateien und Bericht vollständig in 7 Sprachen übersetzt (Español, English, Português, Deutsch, Français, Italiano, Svenska), entsprechend der beim Export aktiven Sprache

### Eingabeanforderungen

| Datei | Format | Beschreibung |
|----|----|----|
| **Punktwolke** | `.laz` / `.las` | LiDAR oder fotogrammetrisch (Structure from Motion) |
| **Untersuchungsgebiet** | `.shp` / `.kml` / `.geojson` | Begrenzungspolygon des Bestands/der Parzelle |

------------------------------------------------------------------------

## Systemanforderungen

### Erforderliche Software

| Komponente | Mindestversion | Hinweise |
|----|----|----|
| **R** | 4.0+ | [Herunterladen](https://cloud.r-project.org/) |

**Hardware-Empfehlung**
-   **RAM**: ≥16 GB (Punktwolken \>10 Mio. Punkte)
-   **CPU**: 4+ Kerne (parallele Verarbeitung)
-   **Speicherplatz**: 50+ GB frei

------------------------------------------------------------------------

## Installation

### 1. Repository beziehen

Als ZIP-Datei herunterladen und entpacken, oder klonen:

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Abhängigkeiten installieren

**Windows:** `Instalar_Dependencias.cmd` ausführen (Doppelklick).

**Linux/macOS:** `Rscript dev/install.R` ausführen. Damit die Dateiauswahldialoge funktionieren, installieren Sie zusätzlich die Tcl/Tk-Unterstützung Ihres Systems (z. B. `sudo apt install r-cran-tcltk` unter Debian/Ubuntu).

------------------------------------------------------------------------

## Ausführung

**Windows:** `Ejecutar_ForestMap.cmd` ausführen (Doppelklick).

**Linux/macOS:** `Rscript dev/run.R` ausführen.

Die Anwendung öffnet sich automatisch im Browser unter `http://localhost:3838`

------------------------------------------------------------------------

## Fehlerbehebung

### Fehler: "Paket X wurde nicht gefunden"

**Windows:** `Instalar_Dependencias.cmd` ausführen
**Linux/macOS:** `Rscript dev/install.R` ausführen

### Fehler: Der Dateiauswahldialog erscheint nicht (Linux)

Installieren Sie die Tcl/Tk-Unterstützung von R (z. B. `sudo apt install r-cran-tcltk` unter Debian/Ubuntu) oder führen Sie die Anwendung über RStudio aus.

### Fehler: "Memory allocation failed"

-   Subsampling-Dichte reduzieren (z. B. 5 Pkt./m²)
-   Andere Anwendungen schließen
-   Verfügbaren RAM erhöhen

------------------------------------------------------------------------

## Wichtige Referenzen

**lidR - Forstliche Algorithmen:**

- Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Bodenklassifizierung:**

- Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dendrometrie:** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Kontakt und Support

-   **E-Mail:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Institution:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Support:** Ein Issue in diesem Repository eröffnen
-   **Kommentare und Vorschläge:** [Formular ausfüllen](https://forms.gle/8YokYp4K3CCrzggG8)

**Lizenz:** [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0) — freie Nutzung, Änderung und Verbreitung. Abgeleitete Werke müssen unter derselben Lizenz stehen. Copyright © INTA / Dr. Eduardo Hildt.

------------------------------------------------------------------------

# Français

## Description

ForestMap est une **plateforme d'analyse géospatiale forestière** entièrement développée sur l'écosystème [**lidR**](https://github.com/r-lidar/lidR) (Roussel et al., 2020, 2021, 2023), le package de référence mondial pour le traitement des données LiDAR aéroportées dans les applications forestières.

### Fonctionnalités

-   **Prétraitement avancé** : Découpage, sous-échantillonnage avec rapport de densité de points, filtrage du bruit, classification du sol CSF (Zhang et al., 2016), normalisation du nuage de points
-   **Modélisation numérique du terrain** : MNT (DEM) avec interpolation TIN, courbes de niveau à partir de 0,1 m d'équidistance, MNC (CHM) à des résolutions configurables à partir de 0,05 m, ombrage (hillshade)
-   **Segmentation individuelle des arbres** : Algorithme LMF (Local Maximum Filter) avec fenêtres adaptatives et seuils de hauteur
-   **Métriques forestières** : Densité (arbres/ha), couverture des houppiers (%), distribution altimétrique, coefficient de variation structurelle
-   **Visualisation scientifique** : Rendu 3D interactif (Plotly) et graphiques thématiques
-   **Projets organisés** : chaque analyse est enregistrée dans un dossier `Projet_<nom>/` (avec le nom défini par l'utilisateur) dans le répertoire de sortie choisi, avec les sous-dossiers `RASTER/`, `VECTEUR/` et `RAPPORT/`
-   **Export multiformat** : GeoTIFF géoréférencé (DEM/CHM/Hillshade) et couches vectorielles au choix en **Shapefile ou GeoJSON**, avec rapports HTML/PDF automatisés
-   **Multilingue** : interface, noms des dossiers/fichiers de sortie et rapport entièrement traduits en 7 langues (Español, English, Português, Deutsch, Français, Italiano, Svenska), selon la langue active au moment de l'export

### Données d'entrée requises

| Fichier | Format | Description |
|----|----|----|
| **Nuage de points** | `.laz` / `.las` | LiDAR ou photogrammétrique (structure from motion) |
| **Zone d'intérêt** | `.shp` / `.kml` / `.geojson` | Polygone délimitant le peuplement/la parcelle |

------------------------------------------------------------------------

## Configuration requise

### Logiciels requis

| Composant | Version minimale | Remarques |
|----|----|----|
| **R** | 4.0+ | [Télécharger](https://cloud.r-project.org/) |

**Recommandation matérielle**
-   **RAM** : ≥16 Go (nuages \>10M points)
-   **CPU** : 4+ cœurs (traitement parallèle)
-   **Stockage** : 50+ Go libres

------------------------------------------------------------------------

## Installation

### 1. Obtenir le dépôt

Téléchargez au format ZIP et décompressez, ou clonez :

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Installer les dépendances

**Windows :** exécuter `Instalar_Dependencias.cmd` (double-clic).

**Linux/macOS :** exécuter `Rscript dev/install.R`. Pour que les boîtes de dialogue de sélection de fichiers fonctionnent, installez également le support Tcl/Tk de votre système (ex. `sudo apt install r-cran-tcltk` sur Debian/Ubuntu).

------------------------------------------------------------------------

## Exécution

**Windows :** exécuter `Ejecutar_ForestMap.cmd` (double-clic).

**Linux/macOS :** exécuter `Rscript dev/run.R`.

L'application s'ouvrira automatiquement dans le navigateur à l'adresse `http://localhost:3838`

------------------------------------------------------------------------

## Dépannage

### Erreur : « Package X introuvable »

**Windows :** exécuter `Instalar_Dependencias.cmd`
**Linux/macOS :** exécuter `Rscript dev/install.R`

### Erreur : la boîte de dialogue de sélection de fichiers n'apparaît pas (Linux)

Installez le support Tcl/Tk de R (ex. `sudo apt install r-cran-tcltk` sur Debian/Ubuntu) ou exécutez l'application depuis RStudio.

### Erreur : « Memory allocation failed »

-   Réduire la densité de sous-échantillonnage (ex. : 5 pts/m²)
-   Fermer les autres applications
-   Augmenter la RAM disponible

------------------------------------------------------------------------

## Références clés

**lidR - Algorithmes forestiers :**

- Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Classification du sol :**

- Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dendrométrie :** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Contact et support

-   **E-mail :** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Institution :** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Support :** Ouvrir une issue sur ce dépôt
-   **Commentaires et suggestions :** [Remplir le formulaire](https://forms.gle/8YokYp4K3CCrzggG8)

**Licence :** [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0) — utilisation, modification et distribution libres. Les œuvres dérivées doivent conserver la même licence. Copyright © INTA / Dr. Eduardo Hildt.

------------------------------------------------------------------------

# Italiano

## Descrizione

ForestMap è una **piattaforma di analisi geospaziale forestale** sviluppata interamente sull'ecosistema [**lidR**](https://github.com/r-lidar/lidR) (Roussel et al., 2020, 2021, 2023), il pacchetto di riferimento mondiale per l'elaborazione di dati LiDAR aviotrasportati nelle applicazioni forestali.

### Funzionalità

-   **Preelaborazione avanzata**: Ritaglio, sottocampionamento con report sulla densità dei punti, filtraggio del rumore, classificazione del suolo CSF (Zhang et al., 2016), normalizzazione della nuvola di punti
-   **Modellazione digitale del terreno**: DEM con interpolazione TIN, curve di livello a partire da 0,1 m di equidistanza, CHM con risoluzioni configurabili a partire da 0,05 m, hillshade
-   **Segmentazione dei singoli alberi**: Algoritmo LMF (Local Maximum Filter) con finestre adattive e soglie di altezza
-   **Metriche forestali**: Densità (alberi/ha), copertura delle chiome (%), distribuzione altimetrica, coefficiente di variazione strutturale
-   **Visualizzazione scientifica**: Rendering 3D interattivo (Plotly) e grafici tematici
-   **Progetti organizzati**: ogni analisi viene salvata in una cartella `Progetto_<nome>/` (con il nome definito dall'utente) all'interno della directory di output scelta, con le sottocartelle `RASTER/`, `VETTORIALE/` e `RAPPORTO/`
-   **Esportazione multiformato**: GeoTIFF georeferenziato (DEM/CHM/Hillshade) e layer vettoriali a scelta in **Shapefile o GeoJSON**, con report HTML/PDF automatizzati
-   **Multilingua**: interfaccia, nomi di cartelle/file di output e report completamente tradotti in 7 lingue (Español, English, Português, Deutsch, Français, Italiano, Svenska), in base alla lingua attiva al momento dell'esportazione

### Requisiti di Input

| File | Formato | Descrizione |
|----|----|----|
| **Nuvola di punti** | `.laz` / `.las` | LiDAR o fotogrammetrica (structure-from-motion) |
| **Area di interesse** | `.shp` / `.kml` / `.geojson` | Poligono delimitante del popolamento/particella |

------------------------------------------------------------------------

## Requisiti di Sistema

### Software Richiesto

| Componente | Versione Minima | Note |
|----|----|----|
| **R** | 4.0+ | [Scaricare](https://cloud.r-project.org/) |

**Requisiti hardware consigliati**
-   **RAM**: ≥16 GB (nuvole \>10M punti)
-   **CPU**: 4+ core (elaborazione parallela)
-   **Archiviazione**: 50+ GB liberi

------------------------------------------------------------------------

## Installazione

### 1. Ottenere il repository

Scaricare in formato ZIP ed estrarre, oppure clonare:

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Installare le Dipendenze

**Windows:** eseguire `Instalar_Dependencias.cmd` (doppio clic).

**Linux/macOS:** eseguire `Rscript dev/install.R`. Affinché le finestre di dialogo per la selezione dei file funzionino, installare anche il supporto Tcl/Tk del proprio sistema (es. `sudo apt install r-cran-tcltk` su Debian/Ubuntu).

------------------------------------------------------------------------

## Esecuzione

**Windows:** eseguire `Ejecutar_ForestMap.cmd` (doppio clic).

**Linux/macOS:** eseguire `Rscript dev/run.R`.

L'applicazione si aprirà automaticamente nel browser all'indirizzo `http://localhost:3838`

------------------------------------------------------------------------

## Risoluzione dei problemi

### Errore: "Pacchetto X non trovato"

**Windows:** eseguire `Instalar_Dependencias.cmd`
**Linux/macOS:** eseguire `Rscript dev/install.R`

### Errore: la finestra di selezione dei file non appare (Linux)

Installare il supporto Tcl/Tk di R (es. `sudo apt install r-cran-tcltk` su Debian/Ubuntu) oppure eseguire l'applicazione da RStudio.

### Errore: "Memory allocation failed"

-   Ridurre la densità di sottocampionamento (es.: 5 pts/m²)
-   Chiudere le altre applicazioni
-   Aumentare la RAM disponibile

------------------------------------------------------------------------

## Riferimenti Chiave

**lidR - Algoritmi Forestali:**

- Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Classificazione del Suolo:**

- Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dasometria:** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Contatti e Supporto

-   **Email:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Istituzione:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issue/Supporto:** Aprire una issue in questo repository
-   **Commenti e suggerimenti:** [Compilare il modulo](https://forms.gle/8YokYp4K3CCrzggG8)

**Licenza:** [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0) — libero uso, modifica e distribuzione. Le opere derivate devono mantenere la stessa licenza. Copyright © INTA / Dr. Eduardo Hildt.

------------------------------------------------------------------------

# Svenska

## Beskrivning

ForestMap är en **geospatial analysplattform för skogsbruk** som är helt utvecklad ovanpå [**lidR**](https://github.com/r-lidar/lidR)-ekosystemet (Roussel et al., 2020, 2021, 2023), det globala referenspaketet för bearbetning av flygburen LiDAR-data inom skogliga tillämpningar.

### Funktioner

-   **Avancerad förbehandling**: Beskärning, subsampling med rapport om punkttäthet, brusfiltrering, CSF-markklassificering (Zhang et al., 2016), normalisering av punktmolnet
-   **Digital terrängmodellering**: DEM med TIN-interpolation, höjdkurvor från 0,1 m ekvidistans, CHM i konfigurerbara upplösningar från 0,05 m, skuggning (hillshade)
-   **Segmentering av enskilda träd**: LMF-algoritm (Local Maximum Filter) med adaptiva fönster och höjdtrösklar
-   **Skogliga mått**: Täthet (träd/ha), kronslutenhet (%), höjdfördelning, strukturell variationskoefficient
-   **Vetenskaplig visualisering**: Interaktiv 3D-rendering (Plotly) och tematiska diagram
-   **Organiserade projekt**: varje analys sparas i en mapp `Projekt_<namn>/` (med namn definierat av användaren) inuti den valda utdatakatalogen, med undermapparna `RASTER/`, `VEKTOR/` och `RAPPORT/`
-   **Export i flera format**: georefererad GeoTIFF (DEM/CHM/Hillshade) och vektorlager efter val i **Shapefile eller GeoJSON**, med automatiserade HTML/PDF-rapporter
-   **Flerspråkighet**: gränssnitt, namn på utdatamappar/filer och rapport helt översatta till 7 språk (Español, English, Português, Deutsch, Français, Italiano, Svenska), beroende på vilket språk som är aktivt vid export

### Indatakrav

| Fil | Format | Beskrivning |
|----|----|----|
| **Punktmoln** | `.laz` / `.las` | LiDAR eller fotogrammetrisk (structure-from-motion) |
| **Intresseområde** | `.shp` / `.kml` / `.geojson` | Avgränsande polygon för beståndet/provytan |

------------------------------------------------------------------------

## Systemkrav

### Nödvändig mjukvara

| Komponent | Minsta version | Anmärkningar |
|----|----|----|
| **R** | 4.0+ | [Ladda ner](https://cloud.r-project.org/) |

**Rekommenderad maskinvara**
-   **RAM**: ≥16 GB (moln \>10M punkter)
-   **CPU**: 4+ kärnor (parallell bearbetning)
-   **Lagring**: 50+ GB ledigt

------------------------------------------------------------------------

## Installation

### 1. Hämta repositoriet

Ladda ner som ZIP-fil och packa upp, eller klona:

``` bash
git clone https://github.com/eduardohildt/ForestMAP
cd ForestMAP
```

### 2. Installera beroenden

**Windows:** kör `Instalar_Dependencias.cmd` (dubbelklick).

**Linux/macOS:** kör `Rscript dev/install.R`. För att filvalsdialogerna ska fungera behöver du även installera systemets Tcl/Tk-stöd (t.ex. `sudo apt install r-cran-tcltk` på Debian/Ubuntu).

------------------------------------------------------------------------

## Körning

**Windows:** kör `Ejecutar_ForestMap.cmd` (dubbelklick).

**Linux/macOS:** kör `Rscript dev/run.R`.

Applikationen öppnas automatiskt i webbläsaren på `http://localhost:3838`

------------------------------------------------------------------------

## Felsökning

### Fel: "Paketet X hittades inte"

**Windows:** kör `Instalar_Dependencias.cmd`
**Linux/macOS:** kör `Rscript dev/install.R`

### Fel: filvalsdialogen visas inte (Linux)

Installera R:s Tcl/Tk-stöd (t.ex. `sudo apt install r-cran-tcltk` på Debian/Ubuntu) eller kör applikationen från RStudio.

### Fel: "Memory allocation failed"

-   Minska subsamplingstätheten (t.ex. 5 pts/m²)
-   Stäng andra applikationer
-   Öka tillgängligt RAM-minne

------------------------------------------------------------------------

## Nyckelreferenser

**lidR - Skogliga algoritmer:**

- Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2020). *lidR: An R package for analysis of Airborne LiDAR Data*. Remote Sensing of Environment, 251, 112061. [DOI: 10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061)

-   Roussel J-R, Auty D (2023). *Airborne LiDAR Data Manipulation and Visualization for Forestry Applications*. R package version 4.x. [CRAN](https://cran.r-project.org/package=lidR)

**CSF - Markklassificering:**

- Zhang W, Qi J, Wan P, Wang H, Xie D, Wang X, Yan G (2016). *An easy-to-use airborne LiDAR data filtering method based on cloth simulation*. Remote Sensing, 8(6), 501. [DOI: 10.3390/rs8060501](https://doi.org/10.3390/rs8060501)

**Dendrometri:** - Avery TE, Burkhart HE (2002). *Forest Measurements* (5th ed.). McGraw-Hill.

------------------------------------------------------------------------

## Kontakt och support

-   **E-post:** [hildt.eduardo\@inta.gob.ar](mailto:hildt.eduardo@inta.gob.ar)
-   **Institution:** [INTA EEA Montecarlo](https://inta.gob.ar/montecarlo)
-   **Issues/Support:** Öppna ett issue i detta repository
-   **Kommentarer och förslag:** [Fyll i formuläret](https://forms.gle/8YokYp4K3CCrzggG8)

**Licens:** [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0) — fri användning, modifiering och distribution. Härledda verk måste behålla samma licens. Copyright © INTA / Dr. Eduardo Hildt.
