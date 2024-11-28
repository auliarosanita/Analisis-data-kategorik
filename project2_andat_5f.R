library(MASS)
library(AER)
library(lmtest)
library(knitr) 

df <- data.frame(
  C = factor(c(2, 3, 3, 4,
               3, 2, 3, 2,
               1, 3, 2, 1)),
  S = factor(c(3, 3, 3, 2,
               3, 3, 3, 1,
               1, 2, 3, 1)),
  W = c(28.3, 26, 25.6, 21,
        22.5, 23.8, 24.3, 26,
        26, 24.7, 25.8, 27.1),
  Wt = c(3.05, 2.6, 2.15, 1.85,
         1.55, 2.1, 2.15, 2.3,
         2.3, 1.9, 2.65, 2.95),
  Sa = c(8, 4, 0, 0,
         0, 0, 0, 14,
         9, 0, 0, 8)
)

df$C <- relevel(df$C, ref = "1")
df$S <- relevel(df$S, ref = "1")

# Regresion Poisson Log-linear
model <- glm(Sa ~ W + Wt + C + S, data = df, family = poisson("link"=log))
summary(model)

#Dispersion Test
dispersiontest(model, trafo = 2, alternative = "greater")

suppressWarnings({ 
  # Negative Binomial model
  modelnb <- glm.nb(Sa ~ W + Wt + C + S, data = df)
  }) 

# Compare models
lrtest(model, modelnb)
AIC(model, modelnb)
