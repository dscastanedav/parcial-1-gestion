
# 05_api_graph.R
# API exposure network; simulate micro-segmentation by removing risky edges
# Output: outputs/figures/05_api_graph_before_after.png

# Si tienes problemas de permisos o R no encuentra los paquetes instalados,
# puedes especificar manualmente la ruta a tu carpeta de librerías personal.
# Descomenta y edita la siguiente línea según tu usuario y versión de R:
# .libPaths("C:/Users/tu_usuario/AppData/Local/R/win-library/4.x")

library(readr); library(dplyr); library(igraph); library(ggplot2); library(ggraph)

api <- read_csv("apis_exposure.csv")

# Build graph (before micro-segmentation)
g_before <- graph_from_data_frame(api, directed = FALSE)

# Micro-segmentation: drop 'high' risk or 'none' auth edges
api_after <- api %>% filter(!(risk == "high" | auth == "none"))
g_after <- graph_from_data_frame(api_after, directed = FALSE)

png("outputs/figures/05_api_graph_before_after.png", width = 1200, height = 500)
par(mfrow=c(1,2), mar=c(0,0,2,0))
plot(g_before, main="Red de APIs (antes)", vertex.size=15, vertex.label.cex=0.8)
plot(g_after, main="Red de APIs (después de microsegmentación)", vertex.size=15, vertex.label.cex=0.8)
dev.off()

cat(sprintf("Grado medio antes: %.2f\n", mean(degree(g_before))))
cat(sprintf("Grado medio después: %.2f\n", mean(degree(g_after))))
