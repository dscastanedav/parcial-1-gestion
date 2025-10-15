
# 02_change_point_test.R
# Structural change test before/after Zero-Trust adoption (2022)
# Output: outputs/figures/02_changepoint.png

library(readr); library(dplyr); library(ggplot2); library(strucchange)

inc <- read_csv("incidents.csv") %>%
  group_by(year) %>%
  summarize(incidents = sum(incidents)) %>%
  arrange(year)

# Fit simple linear model
fm <- lm(incidents ~ year, data = inc)
bp <- breakpoints(incidents ~ year, data = inc)

png("outputs/figures/02_changepoint.png", width = 800, height = 500)
plot(bp)
lines(bp)
title("Prueba de cambio estructural (Bai-Perron)")
dev.off()

cat("Número óptimo de rupturas:", breakpoints(fm)$breakpoints, "\n")
