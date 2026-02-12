# Z-score de exam_score

rm(list = ls())

# Paquetes
library(readr)
library(dplyr)
library(ggplot2)

# Ruta
file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"
desktop   <- path.expand("~/Desktop")

# Leer datos
df <- read_csv(file_path, show_col_types = FALSE)

# Limpiar y convertir
df2 <- df %>%
  mutate(
    exam_score = as.numeric(exam_score),
    study_hours_per_day = as.numeric(study_hours_per_day)
  ) %>%
  filter(!is.na(exam_score), !is.na(study_hours_per_day))

# Estandarizar exam_score (z-score) 
df2 <- df2 %>%
  mutate(
    exam_score_z = (exam_score - mean(exam_score)) / sd(exam_score)
  )

cat("\nResumen exam_score_z:\n")
print(summary(df2$exam_score_z))

# Regresión OLS estandarizada
lm_z <- lm(exam_score_z ~ study_hours_per_day, data = df2)

print(summary(lm_z))

# Scatterplot + recta OLS
p_z <- ggplot(df2, aes(x = study_hours_per_day, y = exam_score_z)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "exam_score (z) vs study_hours_per_day",
    x = "study_hours_per_day",
    y = "exam_score (z-score)"
  )

print(p_z)

# Exportar
out_plot <- file.path(desktop, "scatter_exam_score_z_vs_study_hours.png")
ggsave(out_plot, plot = p_z, width = 8, height = 5, dpi = 300)


# OLS 

rm(list = ls())

pkgs <- c("readr", "dplyr", "sandwich")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Ruta 
file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"

# Leer 
df <- read_csv(file_path, show_col_types = FALSE)

needed <- c(
  "exam_score", "study_hours_per_day", "social_media_hours",
  "attendance_percentage", "sleep_hours",
  "age", "gender", "part_time_job", "parental_education_level"
)
missing <- needed[!needed %in% names(df)]
if (length(missing) > 0) stop(paste("Faltan columnas:", paste(missing, collapse = ", ")))

# Preparar 
df2 <- df %>%
  mutate(
    exam_score = suppressWarnings(as.numeric(exam_score)),
    study_hours_per_day = suppressWarnings(as.numeric(study_hours_per_day)),
    social_media_hours = suppressWarnings(as.numeric(social_media_hours)),
    attendance_percentage = suppressWarnings(as.numeric(attendance_percentage)),
    sleep_hours = suppressWarnings(as.numeric(sleep_hours)),
    age = suppressWarnings(as.numeric(age)),
    
    Z_score = as.numeric(scale(exam_score)),
    log_exam_score = ifelse(exam_score > 0, log(exam_score), NA_real_),
    
    female = ifelse(tolower(gender) == "female", 1, 0),
    part_time = ifelse(tolower(part_time_job) == "yes", 1, 0),
    
    
    # Referencia: high school
    parental_master   = ifelse(tolower(parental_education_level) == "master", 1, 0),
    parental_bachelor = ifelse(tolower(parental_education_level) == "bachelor", 1, 0)
  ) %>%
  
  filter(
    is.finite(exam_score),
    is.finite(study_hours_per_day),
    is.finite(social_media_hours),
    is.finite(attendance_percentage),
    is.finite(sleep_hours),
    is.finite(age),
    is.finite(female),
    is.finite(part_time),
    is.finite(parental_master),
    is.finite(parental_bachelor),
    is.finite(Z_score),
    is.finite(log_exam_score)
  )


#  Modelos 
m1 <- lm(exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage,
         data = df2)

m2 <- lm(exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age,
         data = df2)

m3 <- lm(exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age + female + sleep_hours + parental_master + parental_bachelor,
         data = df2)

m4 <- lm(Z_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age + female + sleep_hours + parental_master + parental_bachelor,
         data = df2)

m5 <- lm(log_exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age + female,
         data = df2)

models <- list(`(1)`=m1, `(2)`=m2, `(3)`=m3, `(4)`=m4, `(5)`=m5)

# coef + SE robustos HC1 
get_robust <- function(mod) {
  V <- sandwich::vcovHC(mod, type = "HC1")
  b <- coef(mod)
  se <- sqrt(diag(V))
  
  # Alinear
  alln <- union(names(b), names(se))
  b2  <- setNames(rep(NA_real_, length(alln)), alln); b2[names(b)]  <- b
  se2 <- setNames(rep(NA_real_, length(alln)), alln); se2[names(se)] <- se
  list(b=b2, se=se2)
}

rob <- lapply(models, get_robust)

# Orden y etiquetas de variables 
var_order <- c(
  "study_hours_per_day",
  "social_media_hours",
  "attendance_percentage",
  "age",
  "female",
  "sleep_hours",
  "parental_master",
  "parental_bachelor",
  "part_time",
  "(Intercept)"
)

var_labels <- c(
  study_hours_per_day    = "study_hrs_pday",
  social_media_hours     = "social_media_hrs",
  attendance_percentage  = "attendance_pct",
  age                    = "edad",
  female                 = "female",
  sleep_hours            = "sleep_hrs",
  parental_master        = "parental_master",
  parental_bachelor      = "parental_bachelor",
  part_time              = "part_time",
  `(Intercept)`          = "constante"
)

# Construir tabla 
fmt_cell <- function(beta, se) {
  if (!is.finite(beta) || !is.finite(se)) return("—")
  paste0(round(beta, 3), "\n(", round(se, 3), ")")
}

table_mat <- sapply(names(models), function(mn) {
  b  <- rob[[mn]]$b
  se <- rob[[mn]]$se
  sapply(var_order, function(v) fmt_cell(b[v], se[v]))
})

# Convertir a data.frame con nombres 
out <- data.frame(
  Variable = unname(var_labels[var_order]),
  table_mat,
  check.names = FALSE
)

#  filas de Observations y R2
obs <- sapply(models, nobs)
r2  <- sapply(models, function(m) summary(m)$r.squared)

out2 <- rbind(
  out,
  data.frame(Variable = "Observations", t(as.data.frame(obs)), check.names = FALSE),
  data.frame(Variable = "R2",           t(as.data.frame(round(r2, 3))), check.names = FALSE)
)


print(out2, row.names = FALSE)



