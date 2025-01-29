par(mfrow = c(2, 4))

# Histogram in QQ-plot za hp (original)
hist(mtcars$hp, main = "Histogram hp (original)", xlab = "hp", col = "skyblue", border = "black")
qqnorm(mtcars$hp, main = "QQ-plot hp (original)")
qqline(mtcars$hp, col = "red")

# Histogram in QQ-plot za hp (log-transformacija)
hist(mtcars$hp_log, main = "Histogram hp (log)", xlab = "log(hp)", col = "lightgreen", border = "black")
qqnorm(mtcars$hp_log, main = "QQ-plot hp (log)")
qqline(mtcars$hp_log, col = "red")

# Histogram in QQ-plot za disp (original)
hist(mtcars$disp, main = "Histogram disp (original)", xlab = "disp", col = "skyblue", border = "black")
qqnorm(mtcars$disp, main = "QQ-plot disp (original)")
qqline(mtcars$disp, col = "red")

# Histogram in QQ-plot za disp (log-transformacija)
hist(mtcars$disp_log, main = "Histogram disp (log)", xlab = "log(disp)", col = "lightgreen", border = "black")
qqnorm(mtcars$disp_log, main = "QQ-plot disp (log)")
qqline(mtcars$disp_log, col = "red")




numerical_vars <- c("mpg", "disp_log", "hp_log", "drat", "wt", "qsec")
cor_matrix <- cor(mtcars[, numerical_vars])
print(cor_matrix)

# 3. Vizualizacija korelacij za numerične spremenljivke
ggpairs(
  mtcars[, numerical_vars],
  lower = list(continuous = "smooth"),
  upper = list(continuous = "cor"),
  diag = list(continuous = "densityDiag"),
  title = "Vizualizacija korelacij za numerične spremenljivke"
) +
  ggtitle("Korelacijska analiza: numerične spremenljivke") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
