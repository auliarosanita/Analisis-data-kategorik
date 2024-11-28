#Package
library(MASS)
library(AER)
library(lmtest)
library(knitr)

# Data
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

df

# Menentukan baseline untuk kategori C dan S
df$C <- relevel(df$C, ref = "1")  
df$S <- relevel(df$S, ref = "1")  

# Model Poisson log-linier
model <- glm(Sa ~ W + Wt + C + S, data = df, family = poisson("link"=log))

# Hasil model
summary(model)

# Pengujian Overdispersi
t <- dispersiontest(model, alternative = "greater")

# Tabel hasil
hasil_tabel <- data.frame(
  Keterangan = c("Metode", 
                 "Estimasi Dispersi", 
                 "Nilai Z", "P-value", 
                 "Kesimpulan"),
  Nilai = c("Dispersion Test (AER)",
            round(t$estimate, 4),
            round(t$statistic, 4),
            round(t$p.value, 4),
            ifelse(t$p.value < 0.05,
                   "Model menunjukkan indikasi overdispersi",
                   "Tidak ada indikasi overdispersi"))
)

t <- dispersiontest(model, alternative = "greater")
print(hasil_tabel, row.names = FALSE)

# Pengujian overdispersi Pearson Chi-Square
deviance <- deviance(model)
pearson <- sum(residuals(model, type = "pearson")^2)
p_value <- pchisq(pearson, 4, lower.tail = FALSE)

hasil_tabel <- data.frame(
  Keterangan = c("Metode", 
                 "Deviance", 
                 "Pearson", 
                 "P-value", 
                 "Kesimpulan"),
  Nilai = c("Pearson Chi-Square",
            round(deviance, 4),
            round(pearson, 4),
            round(p_value, 4),
            ifelse(t$p.value < 0.05,
                   "Model tidak fit",
                   "Model sudah fit"))
)

print(hasil_tabel, row.names = FALSE)

suppressWarnings({ 
  # Model Binomial Negatif
  modelnb <- glm.nb(Sa ~ W + Wt + C + S, data = df)
  }) 

# Membandingkan model
lrtest(model, modelnb) 
AIC(model, modelnb)
