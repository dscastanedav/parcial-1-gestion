
# 03_poisson_model.R
# (Quasi)Poisson/NegBin regression: incidents ~ adoption + sector + year
# Output: results printed to console

library(readr); library(dplyr); library(MASS)

inc <- read_csv("incidents.csv")

# Fit Negative Binomial (overdispersion-friendly)
inc$sector <- factor(inc$sector)
m <- glm.nb(incidents ~ adopt_IA_ISMS + sector + year, data = inc)

summary(m)

# Incident Rate Ratio for adoption
irr <- exp(coef(m)["adopt_IA_ISMS"])
cat(sprintf("IRR (adopción IA/ISMS): %.3f\n", irr))
