#  Comparar OLS (columna 3) vs FGLS

rm(list = ls())

# Paquetes 
pkgs <- c("readr","dplyr","ggplot2","sandwich","lmtest")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install)>0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Ruta
file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"

# Leer
df <- read_csv(file_path, show_col_types = FALSE)

needed <- c("exam_score","study_hours_per_day","social_media_hours",
            "attendance_percentage","sleep_hours","age","gender",
            "part_time_job","parental_education_level")
missing <- needed[!needed %in% names(df)]
if(length(missing)>0) stop(paste("Faltan columnas:", paste(missing, collapse=", ")))

# Preparar 
df2 <- df %>%
  mutate(
    exam_score = suppressWarnings(as.numeric(exam_score)),
    study_hours_per_day = suppressWarnings(as.numeric(study_hours_per_day)),
    social_media_hours = suppressWarnings(as.numeric(social_media_hours)),
    attendance_percentage = suppressWarnings(as.numeric(attendance_percentage)),
    sleep_hours = suppressWarnings(as.numeric(sleep_hours)),
    age = suppressWarnings(as.numeric(age)),
    female = ifelse(tolower(gender) == "female", 1, 0),
    part_time = ifelse(tolower(part_time_job) == "yes", 1, 0),
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


include_interaction <- FALSE

if (!include_interaction) {
  f3 <- exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
    age + female + sleep_hours + parental_master + parental_bachelor
} else {
  f3 <- exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
    age + female + study_hours_per_day:female +
    sleep_hours + parental_master + parental_bachelor
}

# OLS 

ols3 <- lm(f3, data = df2)

# EE homocedásticos 
V_homo <- vcov(ols3)
se_homo <- sqrt(diag(V_homo))

# EE robustos HC1
V_HC1 <- sandwich::vcovHC(ols3, type = "HC1")
se_HC1 <- sqrt(diag(V_HC1))


# FGLS
e_hat <- resid(ols3)
# Evitar log(0)
eps <- 1e-12
log_e2 <- log(pmax(e_hat^2, eps))

# Modelo auxiliar (varianza) 
aux <- lm(update(f3, log_e2 ~ .), data = df2)

sigma2_hat <- as.numeric(exp(predict(aux, newdata = df2)))
w_hat <- 1 / sigma2_hat

fgls <- lm(f3, data = df2, weights = w_hat)

# EE del WLS 
V_fgls <- vcov(fgls)
se_fgls <- sqrt(diag(V_fgls))


# Tabla comparativa OLS (homo), OLS (HC1), FGLS

coef_ols <- coef(ols3)
coef_fgls <- coef(fgls)

# Alinear nombres 
all_terms <- union(names(coef_ols), names(coef_fgls))

get_vec <- function(x, alln){
  out <- setNames(rep(NA_real_, length(alln)), alln)
  out[names(x)] <- x
  out
}

b_ols   <- get_vec(coef_ols, all_terms)
b_fgls  <- get_vec(coef_fgls, all_terms)
se_h    <- get_vec(se_homo, all_terms)
se_r    <- get_vec(se_HC1, all_terms)
se_fg   <- get_vec(se_fgls, all_terms)

fmt <- function(b,se){
  if(!is.finite(b) || !is.finite(se)) return("—")
  paste0(round(b,3), "\n(", round(se,3), ")")
}

tab <- data.frame(
  Variable = all_terms,
  `OLS (SE homo)` = sapply(all_terms, \(t) fmt(b_ols[t],  se_h[t])),
  `OLS (HC1)`     = sapply(all_terms, \(t) fmt(b_ols[t],  se_r[t])),
  `FGLS (WLS)`    = sapply(all_terms, \(t) fmt(b_fgls[t], se_fg[t])),
  check.names = FALSE
)

# filas de N y R2
add_stats <- function(mod){
  c(N = nobs(mod), R2 = summary(mod)$r.squared)
}
st_ols <- add_stats(ols3)
st_fg  <- add_stats(fgls)

tab2 <- rbind(
  tab,
  data.frame(Variable="Observations",
             `OLS (SE homo)`=as.character(st_ols["N"]),
             `OLS (HC1)`=as.character(st_ols["N"]),
             `FGLS (WLS)`=as.character(st_fg["N"]),
             check.names = FALSE),
  data.frame(Variable="R2",
             `OLS (SE homo)`=as.character(round(st_ols["R2"],3)),
             `OLS (HC1)`=as.character(round(st_ols["R2"],3)),
             `FGLS (WLS)`=as.character(round(st_fg["R2"],3)),
             check.names = FALSE)
)

print(tab2, row.names = FALSE)

# Gráficas

df_plot <- df2 %>%
  mutate(
    fitted_ols = fitted(ols3),
    resid_ols  = resid(ols3),
    abs_resid  = abs(resid_ols),
    sqrt_abs_resid = sqrt(abs_resid),
    fitted_fgls = fitted(fgls),
    resid_fgls  = resid(fgls)
  )

p1 <- ggplot(df_plot, aes(x = fitted_ols, y = resid_ols)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "OLS (3): Residuales vs Ajustados", x = "Ajustados (OLS)", y = "Residuales (OLS)")
print(p1)

p2 <- ggplot(df_plot, aes(x = fitted_ols, y = sqrt_abs_resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OLS (3): Scale-Location (√|resid| vs Ajustados)",
       x = "Ajustados (OLS)", y = "√|Residuales|")
print(p2)

p3 <- ggplot(df_plot, aes(x = fitted_fgls, y = resid_fgls)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "FGLS/WLS: Residuales vs Ajustados", x = "Ajustados (FGLS)", y = "Residuales (FGLS)")
print(p3)


