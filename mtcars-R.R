install.packages("ggplot2")
install.packages("gridExtra")
install.packages("MASS")
install.packages("car")
library(GGally)
library(corrplot)
library(car)
library(ggplot2)
library(gridExtra)
library(MASS) 


# Nalaganje podatkov
data(mtcars)

getwd()
write.csv(mtcars, file = "mtcars", row.names = FALSE)

# Pretvorba kategoričnih spremenljivk v faktorje
categorical_vars <- c("cyl", "vs", "am", "gear", "carb")
mtcars[categorical_vars] <- lapply(mtcars[categorical_vars], as.factor)

# pregled ali so normalno distributed
shapiro_results <- lapply(mtcars[, sapply(mtcars, is.numeric)], shapiro.test)

for (var in names(shapiro_results)) {
  cat("\nShapiro-Wilk test za spremenljivko:", var, "\n")
  print(shapiro_results[[var]])
}

# transformacija
mtcars$hp_log <- log(mtcars$hp)
mtcars$disp_log <- log(mtcars$disp)


# Shapiro-Wilk test za log-transformirani spremenljivki
shapiro_hp_log <- shapiro.test(mtcars$hp_log)
shapiro_disp_log <- shapiro.test(mtcars$disp_log)

mtcars <- mtcars[, !(colnames(mtcars) %in% c("disp", "hp"))]

names(mtcars)

# Prikaz rezultatov
cat("Shapiro-Wilk test za hp_log:\n")
print(shapiro_hp_log)

cat("\nShapiro-Wilk test za disp_log:\n")
print(shapiro_disp_log)

categorical_vars <- c("cyl", "vs", "am", "gear", "carb")

for (cat_var in categorical_vars) {
  plot <- ggplot(mtcars, aes(x = .data[[cat_var]], y = mpg, fill = .data[[cat_var]])) +
    geom_boxplot() +
    labs(title = paste("Povezanost med", cat_var, "in mpg"),
         x = cat_var,
         y = "mpg") +
    theme_minimal() +
    theme(legend.position = "none")

  # Prikaz posameznega grafa
  print(plot)

}

# Seznam binarnih spremenljivk
binary_vars <- c("vs", "am")

# Izvedba T-testa za binarne spremenljivke
for (var in binary_vars) {
  t_test <- t.test(mpg ~ mtcars[[var]], data = mtcars)
  cat("\nT-test za spremenljivko:", var, "\n")
  print(t_test)
}

# Seznam večnivojskih spremenljivk
multi_level_vars <- c("cyl", "gear", "carb")

# Izvedba ANOVA testa za večnivojske spremenljivke
for (var in multi_level_vars) {
  anova_model <- aov(mpg ~ mtcars[[var]], data = mtcars)
  cat("\nANOVA za spremenljivko:", var, "\n")
  print(summary(anova_model))
}

### MODEL Z VSEMI SPREMENLJIVKAMI

names(mtcars)
model <- lm(mpg ~ ., data = mtcars)
summary(model)
AIC(model)
par(mfrow = c(2, 2))
plot(model)

# MULTIKOLINEARNSOT
library(car)
vif_values <- vif(model)
print(vif_values)
barplot(vif_values[, 3],
        names.arg = rownames(model),
        horiz = TRUE,
        col = "lightblue",
        main = "Vrednosti VIF za neodvisne spremenljivke",
        xlab = "VIF",
        las = 2,         # Zavrtite imena osi Y za boljši prikaz
        cex.names = 0.8) # Pomanjšajte pisavo imen osi

# Dodajte mejne vrednosti za VIF
abline(v = 5, col = "red", lty = 2, lwd = 2)  # Meja VIF = 5
abline(v = 10, col = "darkred", lty = 2, lwd = 2)  # Visoka VIF = 10


# stepwise

model_step <- stepAIC(model, direction = "both", trace = FALSE)
summary(model_step)
AIC(model_step)

# opt
model_opt <- lm(mpg ~ wt + hp_log, data = mtcars)
summary(model_opt)
AIC(model_opt)


par(mfrow = c(2, 2))
plot(model_opt)




#linearnost
# Nastavitev grafične postavitve za dva grafa
par(mfrow = c(1, 2))

# Razsevni grafikon: mpg ~ wt
plot(mtcars$wt, mtcars$mpg,
     main = "mpg vs wt",
     xlab = "Teža vozila (wt)",
     ylab = "Poraba goriva (mpg)",
     pch = 19, col = "blue")
abline(lm(mpg ~ wt, data = mtcars), col = "red", lwd = 2)

# Razsevni grafikon: mpg ~ hp_log
plot(mtcars$hp_log, mtcars$mpg,
     main = "mpg vs hp_log",
     xlab = "Log-transformirana moč motorja (hp_log)",
     ylab = "Poraba goriva (mpg)",
     pch = 19, col = "blue")
abline(lm(mpg ~ hp_log, data = mtcars), col = "red", lwd = 2)

# Ponastavimo postavitev na eno samo grafično okno
par(mfrow = c(1, 1))

# normalna porazdelitev rezidualov
shapiro.test(resid(model_opt))


# Izvedba Non-constant Variance Score Test
ncvTest(model_opt)

# LOG MPG ZA RESIDUALS

mtcars$mpg_log <-log(mtcars$mpg)
mtcars$wt_log <- log(mtcars$wt)

shapiro_mpg_log <- shapiro.test(mtcars$mpg_log)
shapiro_wt_log <- shapiro.test(mtcars$wt_log)

names(mtcars)


# opt1
model_opt1 <- lm(mpg_log ~ wt_log + hp_log, data = mtcars)
summary(model_opt1)
AIC(model_opt1)


par(mfrow = c(2, 2))
plot(model_opt1)


shapiro.test(resid(model_opt1))

ncvTest(model_opt1)



confint(model_opt1, level = 0.95)


#linearnost
# Nastavitev grafične postavitve za dva grafa
par(mfrow = c(1, 2))

# Razsevni grafikon: log(mpg) ~ log(wt)
plot(mtcars$wt_log, mtcars$mpg_log,
     main = "Log(mpg) vs Log(wt)",
     xlab = "Log(Teža vozila) (log(wt))",
     ylab = "Log(Poraba goriva) (log(mpg))",
     pch = 19, col = "blue")
abline(lm(mpg_log ~ wt_log, data = mtcars), col = "red", lwd = 2)

# Razsevni grafikon: log(mpg) ~ log(hp)
plot(mtcars$hp_log, mtcars$mpg_log,
     main = "Log(mpg) vs Log(hp)",
     xlab = "Log(Moč motorja) (log(hp))",
     ylab = "Log(Poraba goriva) (log(mpg))",
     pch = 19, col = "blue")
abline(lm(mpg_log ~ hp_log, data = mtcars), col = "red", lwd = 2)

par(mfrow = c(1, 1))


# Graf 1: Vpliv log(wt) na log(mpg)
p1 <- ggplot(mtcars, aes(x = wt_log, y = mpg_log)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Vpliv log(wt) na log(mpg)",
       x = "Log(Teža vozila)",
       y = "Log(Poraba goriva)")

# Graf 2: Vpliv log(hp) na log(mpg)
p2 <- ggplot(mtcars, aes(x = hp_log, y = mpg_log)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Vpliv log(hp) na log(mpg)",
       x = "Log(Moč motorja)",
       y = "Log(Poraba goriva)")

grid.arrange(p1, p2, nrow = 2)

