# Bootstrap de prediccion individual 

rm(list = ls())

# Paquetes
pkgs <- c("readr", "dplyr", "ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Ruta 
file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"

# Cargar datos 
df <- read_csv(file_path, show_col_types = FALSE)

# Homologar
if ("attendance_rate" %in% names(df)) {
} else if ("attendance_percentage" %in% names(df)) {
  df <- df %>% rename(attendance_rate = attendance_percentage)
} else if ("attendance" %in% names(df)) {
  df <- df %>% rename(attendance_rate = attendance)
}

# Asegurar
df <- df %>%
  mutate(
    exam_score = suppressWarnings(as.numeric(exam_score)),
    study_hours_per_day = suppressWarnings(as.numeric(study_hours_per_day)),
    sleep_hours = suppressWarnings(as.numeric(sleep_hours)),
    attendance_rate = suppressWarnings(as.numeric(attendance_rate))
  )

# Filtrar
df_use <- df %>%
  filter(
    is.finite(exam_score),
    is.finite(study_hours_per_day),
    is.finite(sleep_hours),
    is.finite(attendance_rate)
  )

n <- nrow(df_use)

# Modelo
m0 <- lm(exam_score ~ study_hours_per_day + sleep_hours + attendance_rate, data = df_use)

x_star <- data.frame(
  study_hours_per_day = 6,
  sleep_hours = 7,
  attendance_rate = 70
)

pred_point <- as.numeric(predict(m0, newdata = x_star))


# Bootstrap 

set.seed(123)        
B <- 1000
pred_boot <- numeric(B)

for (b in 1:B) {
  idx <- sample.int(n, size = n, replace = TRUE)     
  db  <- df_use[idx, ]
  
  fit_b <- lm(exam_score ~ study_hours_per_day + sleep_hours + attendance_rate, data = db)
  
  pred_boot[b] <- as.numeric(predict(fit_b, newdata = x_star))
}

print(summary(pred_boot))

# Distribución empírica

desktop <- file.path(Sys.getenv("HOME"), "Desktop")
if (!dir.exists(desktop)) desktop <- getwd()  # fallback

plot_path <- file.path(desktop, "g2_bootstrap_pred_distribution.png")

p_hist <- ggplot(data.frame(pred = pred_boot), aes(x = pred)) +
  geom_histogram(bins = 30, color = "white") +
  geom_vline(xintercept = pred_point, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribución bootstrap de la predicción (exam_score)",
    subtitle = "Línea punteada: predicción puntual en la muestra original",
    x = "Predicción bootstrap",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 13)

ggsave(plot_path, p_hist, width = 9, height = 6, dpi = 300)
print(p_hist)


# IC 93%

alpha <- 0.07
ci <- quantile(pred_boot, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE, names = FALSE)
names(ci) <- c("IC_93_Lower", "IC_93_Upper")

print(round(ci, 4))

# Guardar 
csv_path <- file.path(desktop, "g_bootstrap_predicciones.csv")
write.csv(data.frame(pred_boot = pred_boot), csv_path, row.names = FALSE)


