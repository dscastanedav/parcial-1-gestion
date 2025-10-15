
# 02_change_point_test.R
# Structural change test before/after Zero-Trust adoption (2022)
# Output: outputs/figures/02_changepoint.png

# Si tienes problemas de permisos o R no encuentra los paquetes instalados,
# puedes especificar manualmente la ruta a tu carpeta de librerías personal.
# Descomenta y edita la siguiente línea según tu usuario y versión de R:
# .libPaths("C:/Users/tu_usuario/AppData/Local/R/win-library/4.x")
.libPaths("C:/Users/santi/AppData/Local/R/win-library/4.5")
library(readr); library(dplyr); library(ggplot2); library(strucchange)

inc <- read_csv("incidents.csv") %>%
  group_by(year) %>%
  summarize(incidents = sum(incidents)) %>%
  arrange(year)

# Fit simple linear model
fm <- lm(incidents ~ year, data = inc)
bp <- breakpoints(incidents ~ year, data = inc, h = 3)

png("outputs/figures/02_changepoint.png", width = 800, height = 500)
plot(bp)
lines(bp)
title("Prueba de cambio estructural (Bai-Perron)")
dev.off()

cat("Número óptimo de rupturas:", breakpoints(fm)$breakpoints, "\n")
