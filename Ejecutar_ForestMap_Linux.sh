#!/bin/bash

# Ir a la carpeta scripts ubicada junto al script
cd "$(dirname "$0")" || exit 1

# Verificar que Rscript exista
if ! command -v Rscript >/dev/null 2>&1; then
    echo "No se encontró Rscript."
    exit 1
fi

# Ejecutar la aplicación
Rscript dev/run.R

read -p "Presione Enter para continuar..."