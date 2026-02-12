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

cat("\n==================== (a) Descriptivas (numéricas) ====================\n")
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
cat("\nTabla descriptiva guardada en:", desc_out, "\n")

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
cat("\nHistograma guardado en:", hist_out, "\n")

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

cat("\n==================== (c) Bootstrap media exam_score ====================\n")
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


# Kernel

rm(list = ls())

# Paquetes

pkgs <- c("readr", "dplyr", "ggplot2", "tidyr", "purrr", "knitr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Rutas

file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"
desktop   <- path.expand("~/Desktop")

# Leer y limpiar datos

df <- read_csv(file_path, show_col_types = FALSE)
stopifnot("exam_score" %in% names(df))
stopifnot("study_hours_per_day" %in% names(df))

df2 <- df %>%
  mutate(
    exam_score = suppressWarnings(as.numeric(exam_score)),
    study_hours_per_day = suppressWarnings(as.numeric(study_hours_per_day))
  ) %>%
  filter(!is.na(exam_score), !is.na(study_hours_per_day))

x <- df2$study_hours_per_day
y <- df2$exam_score
n <- length(y)

cat("\n==================== Resumen ====================\n")
cat("N =", n, "\n")
cat("Rango X:", paste(range(x), collapse = " a "), "\n")
cat("Rango Y:", paste(range(y), collapse = " a "), "\n")


# OLS (línea de referencia)

lm_fit <- lm(exam_score ~ study_hours_per_day, data = df2)
R2_ols <- summary(lm_fit)$r.squared

cat("\n==================== OLS ====================\n")
print(summary(lm_fit))

# Grid para curvas suaves
xgrid <- seq(min(x), max(x), length.out = 300)

ols_line <- data.frame(
  x = xgrid,
  yhat = as.numeric(predict(lm_fit, newdata = data.frame(study_hours_per_day = xgrid)))
)

# Kernels K(u)

K_uniform <- function(u) 0.5 * (abs(u) <= 1)
K_tri     <- function(u) pmax(0, 1 - abs(u))
K_gauss   <- function(u) dnorm(u)

kernels <- list(
  "Uniforme"   = K_uniform,
  "Triangular" = K_tri,
  "Gaussiano"  = K_gauss
)

# Estimadores Kernel: NW y LL

nw_est <- function(x, y, xgrid, h, Kfun) {
  sapply(xgrid, function(x0) {
    w <- Kfun((x - x0) / h)
    sw <- sum(w)
    if (sw == 0) return(NA_real_)
    sum(w * y) / sw
  })
}

ll_est <- function(x, y, xgrid, h, Kfun) {
  sapply(xgrid, function(x0) {
    u <- x - x0
    w <- Kfun(u / h)
    
    S0 <- sum(w)
    if (S0 == 0) return(NA_real_)
    
    S1 <- sum(w * u)
    S2 <- sum(w * u^2)
    T0 <- sum(w * y)
    T1 <- sum(w * u * y)
    
    denom <- (S0 * S2 - S1^2)
    if (denom == 0) return(NA_real_)
    
    a_hat <- (S2 * T0 - S1 * T1) / denom
    a_hat
  })
}

# R2 no paramétrico (tipo ajuste): evalúa yhat en x_i
R2_from_yhat <- function(y, yhat) {
  ok <- is.finite(yhat) & is.finite(y)
  y_ok <- y[ok]; yhat_ok <- yhat[ok]
  sse <- sum((y_ok - yhat_ok)^2)
  sst <- sum((y_ok - mean(y_ok))^2)
  1 - sse / sst
}

#  NW, h=0.4, kernel UNIFORME + OLS

h0 <- 0.4

nw_u_h04 <- nw_est(x, y, xgrid, h0, K_uniform)

df_q7 <- data.frame(
  x = xgrid,
  OLS = ols_line$yhat,
  NW  = nw_u_h04
)

p_q7 <- ggplot(df2, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.25) +
  geom_line(data = df_q7, aes(x = x, y = OLS), linewidth = 1) +
  geom_line(data = df_q7, aes(x = x, y = NW),  linewidth = 1) +
  labs(
    title = "Q7: NW (kernel uniforme, h = 0.4) + OLS",
    x = "study_hours_per_day",
    y = "exam_score"
  )

print(p_q7)
ggsave(file.path(desktop, "Q7_NW_uniform_h0.4_plus_OLS.png"),
       plot = p_q7, width = 9, height = 5.5, dpi = 300)


#  LL, h=0.4, kernel UNIFORME + OLS

ll_u_h04 <- ll_est(x, y, xgrid, h0, K_uniform)

df_q8 <- data.frame(
  x = xgrid,
  OLS = ols_line$yhat,
  LL  = ll_u_h04
)

p_q8 <- ggplot(df2, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.25) +
  geom_line(data = df_q8, aes(x = x, y = OLS), linewidth = 1) +
  geom_line(data = df_q8, aes(x = x, y = LL),  linewidth = 1) +
  labs(
    title = "Q8: Local Linear (kernel uniforme, h = 0.4) + OLS",
    x = "study_hours_per_day",
    y = "exam_score"
  )

print(p_q8)
ggsave(file.path(desktop, "Q8_LL_uniform_h0.4_plus_OLS.png"),
       plot = p_q8, width = 9, height = 5.5, dpi = 300)


# Cambiar bandwidths (h=0.1,0.4,1.0) con kernel UNIFORME

hs <- c(0.1, 0.4, 1.0)

# NW curvas por h (kernel uniforme)
nw_h_curves <- lapply(hs, function(h) nw_est(x, y, xgrid, h, K_uniform))
names(nw_h_curves) <- paste0("h=", hs)

nw_h_df <- data.frame(
  x = rep(xgrid, times = length(hs)),
  yhat = unlist(nw_h_curves, use.names = FALSE),
  h = rep(names(nw_h_curves), each = length(xgrid))
)

p_q9_nw <- ggplot(df2, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.25) +
  geom_line(data = ols_line, aes(x = x, y = yhat), linewidth = 1) +
  geom_line(data = nw_h_df, aes(x = x, y = yhat, linetype = h), linewidth = 1) +
  labs(
    title = "Q9: NW (kernel uniforme) con h = 0.1, 0.4, 1.0 + OLS",
    x = "study_hours_per_day",
    y = "exam_score",
    linetype = "Bandwidth"
  )

print(p_q9_nw)
ggsave(file.path(desktop, "Q9_NW_uniform_h0.1_h0.4_h1_plus_OLS.png"),
       plot = p_q9_nw, width = 9, height = 5.5, dpi = 300)

# LL curvas por h (kernel uniforme)
ll_h_curves <- lapply(hs, function(h) ll_est(x, y, xgrid, h, K_uniform))
names(ll_h_curves) <- paste0("h=", hs)

ll_h_df <- data.frame(
  x = rep(xgrid, times = length(hs)),
  yhat = unlist(ll_h_curves, use.names = FALSE),
  h = rep(names(ll_h_curves), each = length(xgrid))
)

p_q9_ll <- ggplot(df2, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.25) +
  geom_line(data = ols_line, aes(x = x, y = yhat), linewidth = 1) +
  geom_line(data = ll_h_df, aes(x = x, y = yhat, linetype = h), linewidth = 1) +
  labs(
    title = "Q9: LL (kernel uniforme) con h = 0.1, 0.4, 1.0 + OLS",
    x = "study_hours_per_day",
    y = "exam_score",
    linetype = "Bandwidth"
  )

print(p_q9_ll)
ggsave(file.path(desktop, "Q9_LL_uniform_h0.1_h0.4_h1_plus_OLS.png"),
       plot = p_q9_ll, width = 9, height = 5.5, dpi = 300)

#  Mantener h=0.4, comparar kernels (Uniforme, Triangular, Gaussiano)

nw_k_curves <- lapply(names(kernels), function(kname) {
  nw_est(x, y, xgrid, h0, kernels[[kname]])
})
names(nw_k_curves) <- names(kernels)

nw_k_df <- data.frame(
  x = rep(xgrid, times = length(kernels)),
  yhat = unlist(nw_k_curves, use.names = FALSE),
  kernel = rep(names(kernels), each = length(xgrid))
)

p_q10_nw <- ggplot(df2, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.25) +
  geom_line(data = ols_line, aes(x = x, y = yhat), linewidth = 1) +
  geom_line(data = nw_k_df, aes(x = x, y = yhat, linetype = kernel), linewidth = 1) +
  labs(
    title = "Q10: NW (h = 0.4) comparación de kernels + OLS",
    x = "study_hours_per_day",
    y = "exam_score",
    linetype = "Kernel"
  )

print(p_q10_nw)
ggsave(file.path(desktop, "Q10_NW_h0.4_kernels_uniform_tri_gauss_plus_OLS.png"),
       plot = p_q10_nw, width = 9, height = 5.5, dpi = 300)

# LL por kernel (h=0.4)
ll_k_curves <- lapply(names(kernels), function(kname) {
  ll_est(x, y, xgrid, h0, kernels[[kname]])
})
names(ll_k_curves) <- names(kernels)

ll_k_df <- data.frame(
  x = rep(xgrid, times = length(kernels)),
  yhat = unlist(ll_k_curves, use.names = FALSE),
  kernel = rep(names(kernels), each = length(xgrid))
)

p_q10_ll <- ggplot(df2, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(alpha = 0.25) +
  geom_line(data = ols_line, aes(x = x, y = yhat), linewidth = 1) +
  geom_line(data = ll_k_df, aes(x = x, y = yhat, linetype = kernel), linewidth = 1) +
  labs(
    title = "Q10: LL (h = 0.4) comparación de kernels + OLS",
    x = "study_hours_per_day",
    y = "exam_score",
    linetype = "Kernel"
  )

print(p_q10_ll)
ggsave(file.path(desktop, "Q10_LL_h0.4_kernels_uniform_tri_gauss_plus_OLS.png"),
       plot = p_q10_ll, width = 9, height = 5.5, dpi = 300)


# Tabla (filas=kernel, columnas=bandwidth), incluye R2

combo_grid <- expand.grid(
  kernel = names(kernels),
  h = hs,
  stringsAsFactors = FALSE
) %>% as_tibble()

calc_R2_combo <- function(kernel_name, hval, which = c("NW","LL")) {
  which <- match.arg(which)
  Kfun <- kernels[[kernel_name]]
  if (which == "NW") {
    yhat <- nw_est(x, y, xgrid = x, h = hval, Kfun = Kfun)
  } else {
    yhat <- ll_est(x, y, xgrid = x, h = hval, Kfun = Kfun)
  }
  R2_from_yhat(y, yhat)
}

tab_long <- combo_grid %>%
  mutate(
    R2_NW = map2_dbl(kernel, h, ~ calc_R2_combo(.x, .y, "NW")),
    R2_LL = map2_dbl(kernel, h, ~ calc_R2_combo(.x, .y, "LL"))
  )

tab_NW <- tab_long %>%
  select(kernel, h, R2_NW) %>%
  mutate(h = paste0("h=", h)) %>%
  pivot_wider(names_from = h, values_from = R2_NW) %>%
  arrange(factor(kernel, levels = names(kernels)))

tab_LL <- tab_long %>%
  select(kernel, h, R2_LL) %>%
  mutate(h = paste0("h=", h)) %>%
  pivot_wider(names_from = h, values_from = R2_LL) %>%
  arrange(factor(kernel, levels = names(kernels)))

cat("\n==================== Q11: R2 OLS (referencia) ====================\n")
cat("R2 OLS =", round(R2_ols, 4), "\n")

cat("\n==================== Q11: TABLA R2 (NW): kernel x bandwidth ====================\n")
print(knitr::kable(tab_NW, digits = 4))

cat("\n==================== Q11: TABLA R2 (LL): kernel x bandwidth ====================\n")
print(knitr::kable(tab_LL, digits = 4))

# Guardar tablas en CSV 
write.csv(tab_NW, file.path(desktop, "Q11_tabla_R2_NW_kernel_x_bandwidth.csv"), row.names = FALSE)
write.csv(tab_LL, file.path(desktop, "Q11_tabla_R2_LL_kernel_x_bandwidth.csv"), row.names = FALSE)

cat("\nTablas guardadas en Desktop:\n")
cat(" - Q11_tabla_R2_NW_kernel_x_bandwidth.csv\n")
cat(" - Q11_tabla_R2_LL_kernel_x_bandwidth.csv\n")

cat("\n==================== Archivos de figuras (Desktop) ====================\n")
cat("Q7_NW_uniform_h0.4_plus_OLS.png\n")
cat("Q8_LL_uniform_h0.4_plus_OLS.png\n")
cat("Q9_NW_uniform_h0.1_h0.4_h1_plus_OLS.png\n")
cat("Q9_LL_uniform_h0.1_h0.4_h1_plus_OLS.png\n")
cat("Q10_NW_h0.4_kernels_uniform_tri_gauss_plus_OLS.png\n")
cat("Q10_LL_h0.4_kernels_uniform_tri_gauss_plus_OLS.png\n")


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

# Regresión OLS con variable estandarizada
lm_z <- lm(exam_score_z ~ study_hours_per_day, data = df2)

cat("\n==================== OLS con exam_score estandarizado ====================\n")
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

cat("\nGráfica guardada en:", out_plot, "\n")


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
    
    
    # Referencia: high school + none (y cualquier otro distinto de master/bachelor)
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

cat("\n==================== Resumen ====================\n")
cat("N =", nrow(df2), "\n")

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

# Función para sacar coef + SE robustos HC1 
get_robust <- function(mod) {
  V <- sandwich::vcovHC(mod, type = "HC1")
  b <- coef(mod)
  se <- sqrt(diag(V))
  
  # Alinear nombres 
  alln <- union(names(b), names(se))
  b2  <- setNames(rep(NA_real_, length(alln)), alln); b2[names(b)]  <- b
  se2 <- setNames(rep(NA_real_, length(alln)), alln); se2[names(se)] <- se
  list(b=b2, se=se2)
}

rob <- lapply(models, get_robust)

# Orden y etiquetas de variables como tu tabla 
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

# Convertir a data.frame con nombres bonitos
out <- data.frame(
  Variable = unname(var_labels[var_order]),
  table_mat,
  check.names = FALSE
)

# Agregar filas de Observations y R2
obs <- sapply(models, nobs)
r2  <- sapply(models, function(m) summary(m)$r.squared)

out2 <- rbind(
  out,
  data.frame(Variable = "Observations", t(as.data.frame(obs)), check.names = FALSE),
  data.frame(Variable = "R2",           t(as.data.frame(round(r2, 3))), check.names = FALSE)
)

cat("\n==================== Tabla MCO (HC1) en consola ====================\n")
print(out2, row.names = FALSE)

cat("\nNota: Errores heterocedásticos (HC1) entre paréntesis bajo los coeficientes.\n")
cat("Las '—' indican que esa variable no está incluida (o fue omitida por colinealidad).\n")
cat("Referencia parental: high school y none (es decir, parental_master=0 y parental_bachelor=0).\n")



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

cat("\n==================== Resumen ====================\n")
cat("N =", nrow(df2), "\n")


#  Estimación de la especificación (3)

m3 <- lm(
  exam_score ~ study_hours_per_day + social_media_hours +
    attendance_percentage + age + female + sleep_hours +
    parental_master + parental_bachelor,
  data = df2
)

cat("\n==================== Modelo (3) ====================\n")
print(summary(m3))


# Prueba conjunta Wald robusta 

V_HC1 <- sandwich::vcovHC(m3, type = "HC1")

wald_res <- lmtest::waldtest(
  m3,
  vcov = V_HC1,
  Terms = c("parental_master", "parental_bachelor")
)

cat("\n==================== Wald test conjunto (HC1) ====================\n")
print(wald_res)


# Reporte 

F_stat <- wald_res$F[2]
p_val  <- wald_res$`Pr(>F)`[2]

cat("\nH0: parental_master = 0 y parental_bachelor = 0\n")
cat("F =", round(F_stat, 4),
    " | p-value =", round(p_val, 5), "\n")



#  interacción female x study_hours_per_day
#  Matriz Var-Cov de estimadores del modelo (3) 


rm(list = ls())

# Paquetes
pkgs <- c("readr", "dplyr", "sandwich")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Ruta
file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"

# Leer
df <- read_csv(file_path, show_col_types = FALSE)

needed <- c("exam_score", "study_hours_per_day", "social_media_hours",
            "attendance_percentage", "sleep_hours",
            "age", "gender", "part_time_job", "parental_education_level")
missing <- needed[!needed %in% names(df)]
if (length(missing) > 0) stop(paste("Faltan columnas:", paste(missing, collapse = ", ")))

#Preparar
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
    is.finite(part_time),
    is.finite(parental_master),
    is.finite(parental_bachelor),
    is.finite(Z_score),
    is.finite(log_exam_score)
  )

cat("\n==================== Resumen ====================\n")
cat("N =", nrow(df2), "\n")

#Modelos 
m1 <- lm(exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage,
         data = df2)

m2 <- lm(exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age,
         data = df2)

#  interacción female x study_hours_per_day
m3 <- lm(exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age + female + study_hours_per_day:female +
           sleep_hours + parental_master + parental_bachelor,
         data = df2)

m4 <- lm(Z_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age + female + sleep_hours + parental_master + parental_bachelor,
         data = df2)

m5 <- lm(log_exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
           age + female,
         data = df2)

models <- list(`(1)`=m1, `(2)`=m2, `(3)`=m3, `(4)`=m4, `(5)`=m5)

#  coef + SE robustos HC1 
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

#  etiquetas de variables 
var_order <- c(
  "study_hours_per_day",
  "study_hours_per_day:female",   # interacción nueva en (3)
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
  study_hours_per_day          = "study_hrs_pday",
  `study_hours_per_day:female` = "study_hrs_pday × female",
  social_media_hours           = "social_media_hrs",
  attendance_percentage        = "attendance_pct",
  age                          = "edad",
  female                       = "female",
  sleep_hours                  = "sleep_hrs",
  parental_master              = "parental_master",
  parental_bachelor            = "parental_bachelor",
  part_time                    = "part_time",
  `(Intercept)`                = "constante"
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

out <- data.frame(
  Variable = unname(var_labels[var_order]),
  table_mat,
  check.names = FALSE
)

# filas de Observations y R2
obs <- sapply(models, nobs)
r2  <- sapply(models, function(m) summary(m)$r.squared)

out2 <- rbind(
  out,
  data.frame(Variable = "Observations", t(as.data.frame(obs)), check.names = FALSE),
  data.frame(Variable = "R2",           t(as.data.frame(round(r2, 3))), check.names = FALSE)
)

cat("\n==================== Tabla MCO (HC1) en consola ====================\n")
print(out2, row.names = FALSE)

#  Matriz Var-Cov de los estimadores del modelo (3)

V_m3_classic <- vcov(m3)
print(round(V_m3_classic, 6))

V_m3_HC1 <- sandwich::vcovHC(m3, type = "HC1")
print(round(V_m3_HC1, 6))


# Partial out 

rm(list = ls())

pkgs <- c("readr", "dplyr", "ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Ruta 
file_path <- "/Users/juanpablo/Desktop/student_habits_performance_dummies1.csv"

#  Leer 
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
    
    female = ifelse(tolower(gender) == "female", 1, 0),
    part_time = ifelse(tolower(part_time_job) == "yes", 1, 0),
    
    # Dummies parentales
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
    is.finite(parental_bachelor)
  )

cat("\n==================== Resumen ====================\n")
cat("N =", nrow(df2), "\n")


# Modelo completo 

full_mod <- lm(
  exam_score ~ study_hours_per_day + social_media_hours + attendance_percentage +
    age + female + sleep_hours + parental_master + parental_bachelor,
  data = df2
)

print(summary(full_mod))


#  Partial-out 

# Controles W (
W_formula <- ~ social_media_hours + attendance_percentage + age + female +
  sleep_hours + parental_master + parental_bachelor

# Residual de Y respecto a Intercept + W
y_res <- resid(lm(
  exam_score ~ social_media_hours + attendance_percentage + age + female +
    sleep_hours + parental_master + parental_bachelor,
  data = df2
))

# Residual de X respecto a Intercept + W
x_res <- resid(lm(
  study_hours_per_day ~ social_media_hours + attendance_percentage + age + female +
    sleep_hours + parental_master + parental_bachelor,
  data = df2
))

# Regresión partial-out 
po_mod <- lm(y_res ~ 0 + x_res)


print(summary(po_mod))


# Chequeo
beta_full <- coef(full_mod)["study_hours_per_day"]
beta_po   <- coef(po_mod)["x_res"]


# Scatterplot

plot_df <- data.frame(
  x_res = x_res,
  y_res = y_res
)

p <- ggplot(plot_df, aes(x = x_res, y = y_res)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Partial-out (FWL): exam_score vs study_hours_per_day (residualizados)",
    x = "Residual de study_hours_per_day (controlando por W)",
    y = "Residual de exam_score (controlando por W)"
  )

print(p)

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
cat("N usado =", n, "\n")

# Modelo
m0 <- lm(exam_score ~ study_hours_per_day + sleep_hours + attendance_rate, data = df_use)

x_star <- data.frame(
  study_hours_per_day = 6,
  sleep_hours = 7,
  attendance_rate = 70
)

pred_point <- as.numeric(predict(m0, newdata = x_star))
cat("\nPredicción puntual (muestra original):", round(pred_point, 4), "\n\n")


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

cat("Resumen predicciones bootstrap:\n")
print(summary(pred_boot))
cat("SD(pred_boot) =", sd(pred_boot), "\n")


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
cat("\nPlot guardado en:", plot_path, "\n")


# IC 93%

alpha <- 0.07
ci <- quantile(pred_boot, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE, names = FALSE)
names(ci) <- c("IC_93_Lower", "IC_93_Upper")

print(round(ci, 4))

#  Guardar 
csv_path <- file.path(desktop, "g_bootstrap_predicciones.csv")
write.csv(data.frame(pred_boot = pred_boot), csv_path, row.names = FALSE)
cat("\nPredicciones bootstrap guardadas en:", csv_path, "\n")


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

cat("\n==================== Resumen ====================\n")
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

cat("\n==================== Tabla: OLS vs FGLS ====================\n")
print(tab2, row.names = FALSE)
cat("\nNota: En OLS (HC1) los EE son robustos a heterocedasticidad.\n")
cat("      En FGLS (WLS) los EE mostrados son los clásicos del WLS (asumiendo pesos correctos).\n")

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


