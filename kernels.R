
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

cat("N =", n, "\n")
cat("Rango X:", paste(range(x), collapse = " a "), "\n")
cat("Rango Y:", paste(range(y), collapse = " a "), "\n")


# OLS (línea de referencia)

lm_fit <- lm(exam_score ~ study_hours_per_day, data = df2)
R2_ols <- summary(lm_fit)$r.squared

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
ggsave(file.path(desktop, "Q10_LL_h0.4_kernels_uniform_tri_gauss_OLS.png"),
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

cat("R2 OLS =", round(R2_ols, 4), "\n")

print(knitr::kable(tab_NW, digits = 4))

print(knitr::kable(tab_LL, digits = 4))

# Guardar  CSV 
write.csv(tab_NW, file.path(desktop, "Q11_tabla_R2_NW_kernel_x_bandwidth.csv"), row.names = FALSE)
write.csv(tab_LL, file.path(desktop, "Q11_tabla_R2_LL_kernel_x_bandwidth.csv"), row.names = FALSE)


