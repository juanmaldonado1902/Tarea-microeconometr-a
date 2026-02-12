# Prueba conjunta 
rm(list = ls())

# Paquetes 
pkgs <- c("readr", "dplyr", "sandwich", "lmtest")
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
  "age", "gender", "parental_education_level"
)
missing <- needed[!needed %in% names(df)]
if (length(missing) > 0)
  stop(paste("Faltan columnas:", paste(missing, collapse = ", ")))

#Preparar datos
df2 <- df %>%
  mutate(
    exam_score = as.numeric(exam_score),
    study_hours_per_day = as.numeric(study_hours_per_day),
    social_media_hours = as.numeric(social_media_hours),
    attendance_percentage = as.numeric(attendance_percentage),
    sleep_hours = as.numeric(sleep_hours),
    age = as.numeric(age),
    
    female = ifelse(tolower(gender) == "female", 1, 0),
    
    # Dummies 
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
    is.finite(parental_master),
    is.finite(parental_bachelor)
  )

cat("N =", nrow(df2), "\n")


#  Estimación de la especificación (3)

m3 <- lm(
  exam_score ~ study_hours_per_day + social_media_hours +
    attendance_percentage + age + female + sleep_hours +
    parental_master + parental_bachelor,
  data = df2
)

print(summary(m3))


# Prueba conjunta Wald robusta 

V_HC1 <- sandwich::vcovHC(m3, type = "HC1")

wald_res <- lmtest::waldtest(
  m3,
  vcov = V_HC1,
  Terms = c("parental_master", "parental_bachelor")
)

print(wald_res)


# Reporte 

F_stat <- wald_res$F[2]
p_val  <- wald_res$`Pr(>F)`[2]

cat("F =", round(F_stat, 4),
    " | p-value =", round(p_val, 5), "\n")