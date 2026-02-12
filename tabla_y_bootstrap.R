# Tarea Microeconometría 
rm(list = ls())

# Paquetes
pkgs <- c("readr", "dplyr", "tibble", "ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

#Rutas
file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"
desktop   <- path.expand("~/Desktop")

# Leer datos
df <- read_csv(file_path, show_col_types = FALSE)

cat("\nColumnas disponibles:\n")
print(names(df))

# Homologar nombres 
# attendance_rate vs attendance_percentage
if (!("attendance_rate" %in% names(df)) && ("attendance_percentage" %in% names(df))) {
  df <- df %>% rename(attendance_rate = attendance_percentage)
}
# study_hours_per_day esperado
# exam_score esperado
stopifnot("exam_score" %in% names(df))
stopifnot("study_hours_per_day" %in% names(df))
stopifnot("sleep_hours" %in% names(df))
stopifnot("social_media_hours" %in% names(df))
stopifnot("exercise_frequency" %in% names(df))
stopifnot("mental_health_rating" %in% names(df))
stopifnot("attendance_rate" %in% names(df))

#Asegurar tipos numéricos
num_vars <- c("exam_score","study_hours_per_day","sleep_hours","attendance_rate",
              "social_media_hours","exercise_frequency","mental_health_rating")

df <- df %>%
  mutate(across(all_of(num_vars), ~ suppressWarnings(as.numeric(.x))))

# Tabla descriptiva
desc_num <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    mean = mean(x),
    var  = var(x),
    sd   = sd(x),
    min  = min(x),
    max  = max(x)
  )
}

desc_table <- bind_rows(
  exam_score          = desc_num(df$exam_score),
  study_hours_per_day = desc_num(df$study_hours_per_day),
  sleep_hours         = desc_num(df$sleep_hours),
  attendance_rate     = desc_num(df$attendance_rate),
  social_media_hours  = desc_num(df$social_media_hours),
  exercise_frequency  = desc_num(df$exercise_frequency),
  mental_health_rating= desc_num(df$mental_health_rating),
  .id = "variable"
)

print(desc_table)

# Para 'gender' (categórica)
gender_col <- NULL
if ("gender" %in% names(df)) gender_col <- "gender"
if (is.null(gender_col) && "female" %in% names(df)) gender_col <- "female"  # por si solo hay dummy

cat("\n==================== (a) Gender (distribución) ====================\n")
if (!is.null(gender_col) && gender_col == "gender") {
  gender_tab <- df %>%
    mutate(gender = as.factor(gender)) %>%
    count(gender, name = "n") %>%
    mutate(prop = n / sum(n))
  print(gender_tab)
} else if (!is.null(gender_col) && gender_col == "female") {
  # Si solo existe dummy female (0/1)
  female_tab <- df %>%
    mutate(female = as.numeric(female)) %>%
    count(female, name = "n") %>%
    mutate(prop = n / sum(n))
  print(female_tab)
  cat("\nNota: 'female' es dummy; su media es proporción de mujeres.\n")
  cat("Media(female) =", mean(df$female, na.rm = TRUE), "\n")
} else {
  cat("No se encontró columna 'gender' ni dummy 'female'.\n")
}

# Guardar descriptivas en CSV 
desc_out <- file.path(desktop, "tabla_descriptiva.csv")
write_csv(desc_table, desc_out)

# Histograma de exam_score
p_hist_exam <- ggplot(df, aes(x = exam_score)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  labs(
    title = "Histograma de exam_score",
    x = "exam_score",
    y = "Frecuencia"
  )

print(p_hist_exam)

hist_out <- file.path(desktop, "hist_exam_score.png")
ggsave(hist_out, plot = p_hist_exam, width = 8, height = 5, dpi = 300)

# Bootstrap de la media poblacional de exam_score (500 reps) 
set.seed(123)  

y <- df$exam_score
y <- y[!is.na(y)]
n <- length(y)
B <- 500

boot_means <- replicate(B, mean(sample(y, size = n, replace = TRUE)))

mu_hat <- mean(y)
boot_mean <- mean(boot_means)
boot_se   <- sd(boot_means)

# Intervalos de confianza 
ci_perc <- quantile(boot_means, probs = c(0.025, 0.975))
ci_norm <- c(mu_hat - 1.96 * boot_se, mu_hat + 1.96 * boot_se)

cat("n =", n, "\n")
cat("Media muestral (mu_hat) =", mu_hat, "\n")
cat("Media de bootstrap =", boot_mean, "\n")
cat("SE bootstrap =", boot_se, "\n")
cat("IC 95% Percentil: [", ci_perc[1], ", ", ci_perc[2], "]\n", sep="")
cat("IC 95% Normal:    [", ci_norm[1], ", ", ci_norm[2], "]\n", sep="")

# Distribución empírica del estimador bootstrap
boot_df <- tibble(boot_means = boot_means)

p_boot_dist <- ggplot(boot_df, aes(x = boot_means)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  labs(
    title = "Distribución bootstrap de la media (exam_score)",
    x = "Media bootstrap",
    y = "Frecuencia"
  )

print(p_boot_dist)

boot_out <- file.path(desktop, "bootstrap_dist_media_exam_score.png")
ggsave(boot_out, plot = p_boot_dist, width = 8, height = 5, dpi = 300)
cat("\nDistribución bootstrap guardada en:", boot_out, "\n")

