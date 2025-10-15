
# 04_anomaly_detection.R
# Telemetry anomaly detection as proxy for improvements in MTTD/MTTR after 2023
# Output: outputs/figures/04_anomalies.png

# Si tienes problemas de permisos o R no encuentra los paquetes instalados,
# puedes especificar manualmente la ruta a tu carpeta de librerías personal.
# Descomenta y edita la siguiente línea según tu usuario y versión de R:
# .libPaths("C:/Users/tu_usuario/AppData/Local/R/win-library/4.x")
library(readr); library(dplyr); library(ggplot2); library(zoo)

tel <- read_csv("ai_defense_metrics.csv")
tel$date <- as.Date(tel$date)

# Simple rolling mean / sd anomaly flag on events
tel <- tel %>%
  arrange(date) %>%
  mutate(roll_mean = zoo::rollmean(events, k = 6, fill = NA, align = "right"),
         roll_sd   = zoo::rollapply(events, width = 6, FUN = sd, fill = NA, align = "right"),
         upper = roll_mean + 2*roll_sd,
         lower = pmax(0, roll_mean - 2*roll_sd),
         anomaly = ifelse(!is.na(roll_mean) & (events > upper | events < lower), 1, 0))

p <- ggplot(tel, aes(x = date, y = events)) +
  geom_line(size = 1.0) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
  geom_point(aes(color = anomaly == 1)) +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed") +
  labs(title = "Detección de anomalías en eventos (proxy MTTD/MTTR)",
       subtitle = "Banda = media ± 2*sd (ventana 6 meses). Línea punteada: 2023 (adopción).",
       x = "Fecha", y = "Eventos / mes", color = "Anomalía")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/figures/04_anomalies.png", plot = p, width = 7.5, height = 4.5, dpi = 150)

# Compare average MTTD/MTTR before vs after 2023
b4 <- tel %>% filter(date < as.Date("2023-01-01")) %>% summarize(MTTD=mean(MTTD_hours), MTTR=mean(MTTR_hours))
af <- tel %>% filter(date >= as.Date("2023-01-01")) %>% summarize(MTTD=mean(MTTD_hours), MTTR=mean(MTTR_hours))
print(b4); print(af)
cat(sprintf("ΔMTTD: %.2f h | ΔMTTR: %.2f h\n", b4$MTTD - af$MTTD, b4$MTTR - af$MTTR))
