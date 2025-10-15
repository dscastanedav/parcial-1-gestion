
# 01_trend_analysis.R
# Time trend of incidents by sector with pre/post IA/ISMS overlay
# Output: outputs/figures/01_trend_incidents.png

# Si tienes problemas de permisos o R no encuentra los paquetes instalados,
# puedes especificar manualmente la ruta a tu carpeta de librerías personal.
# Descomenta y edita la siguiente línea según tu usuario y versión de R:
# .libPaths("C:/Users/tu_usuario/AppData/Local/R/win-library/4.x")

library(readr); library(dplyr); library(ggplot2)

inc <- read_csv("incidents.csv")

p <- inc %>%
  group_by(year) %>%
  summarize(incidents = sum(incidents),
            adopted = max(adopt_IA_ISMS)) %>%
  ggplot(aes(x = year, y = incidents)) +
  geom_line(size = 1.1) +
  geom_point() +
  geom_vline(xintercept = 2022, linetype = "dashed") +
  annotate("text", x = 2022.2, y = max(inc$incidents), label = "Adopción IA/ISMS", hjust = 0) +
  labs(title = "Tendencia de incidentes (total sectores)",
       subtitle = "Línea punteada: inicio de adopción IA/ISMS (2022)",
       x = "Año", y = "Incidentes reportados")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/figures/01_trend_incidents.png", plot = p, width = 7, height = 4.5, dpi = 150)

# Simple numeric: % reducción 2021 -> 2025
base_2021 <- inc %>% filter(year==2021) %>% summarize(n=sum(incidents)) %>% pull(n)
now_2025  <- inc %>% filter(year==2025) %>% summarize(n=sum(incidents)) %>% pull(n)
reduction <- (base_2021 - now_2025)/base_2021 * 100
cat(sprintf("Reducción estimada 2021->2025: %.1f%%\n", reduction))
