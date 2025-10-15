
# 03_poisson_model.R
# (Quasi)Poisson/NegBin regression: incidents ~ adoption + sector + year
# Output: results printed to console

# Si tienes problemas de permisos o R no encuentra los paquetes instalados,
# puedes especificar manualmente la ruta a tu carpeta de librerías personal.
# Descomenta y edita la siguiente línea según tu usuario y versión de R:
# .libPaths("C:/Users/tu_usuario/AppData/Local/R/win-library/4.x")
library(readr); library(dplyr); library(MASS)

inc <- read_csv("incidents.csv")

# Fit Negative Binomial (overdispersion-friendly)
inc$sector <- factor(inc$sector)
m <- glm.nb(incidents ~ adopt_IA_ISMS + sector + year, data = inc)

summary(m)

# Incident Rate Ratio for adoption
irr <- exp(coef(m)["adopt_IA_ISMS"])
cat(sprintf("IRR (adopción IA/ISMS): %.3f\n", irr))
