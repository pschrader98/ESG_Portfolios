detach("package:plyr", unload = TRUE)
library(dplyr)
library(tidyr)
library(fixest)


#-- Quintile Portfolios

# --- columns ---
ret_cols <- c("LS_Q_EW_resid_n_car_5_material",
              "LS_Q_VW_resid_n_car_5_material")
factors  <- c("MKT_RF","SMB","HML","MOM")

# --- read + scale BOTH LS returns to percent ---
port <- readr::read_csv(
  "E:/GermanBusinessPanelTeam/Schrader/Forschung/ESG_Portfolios/Output/portfolio_returns_quintiles.csv",
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(across(all_of(ret_cols), ~ .x * 100))   # convert to percent

# helper to build a regression df for a given return column
make_df <- function(ret_col) {
  port %>%
    transmute(
      date,
      return = .data[[ret_col]],
      MKT_RF = .data[["MKT_RF"]],
      SMB    = .data[["SMB"]],
      HML    = .data[["HML"]],
      MOM    = .data[["MOM"]]
    ) %>%
    drop_na() %>%
    arrange(date) %>%
    mutate(id = 1L, t = row_number())
}

df_ew <- make_df(ret_cols[1])
df_vw <- make_df(ret_cols[2])

# --- Newey–West HAC (12 lags) ---
vcNW <- NW(lag = 12)

# --- Spec 1 (alpha-only) and Spec 4 (FF4) for each return series ---
# EW
ew_s1 <- feols(return ~ 1,                           data = df_ew, panel.id = ~ id + t)
ew_s4 <- feols(return ~ MKT_RF + SMB + HML + MOM,    data = df_ew, panel.id = ~ id + t)
# VW
vw_s1 <- feols(return ~ 1,                           data = df_vw, panel.id = ~ id + t)
vw_s4 <- feols(return ~ MKT_RF + SMB + HML + MOM,    data = df_vw, panel.id = ~ id + t)

# --- Table with the four columns (EW/VW × Spec1/Spec4) ---
etable(
  ew_s1, ew_s4, vw_s1, vw_s4,
  coefstat = "tstat",
  se.below = TRUE,
  digits   = 3,
  dict     = c("(Intercept)"="Alpha","MKT_RF"="MKT−RF","SMB"="SMB","HML"="HML","MOM"="UMD"),
  headers  = c("EW α-only","EW FF4","VW α-only","VW FF4")
)

# --- 3) Pretty table with etable ---------------------------------------------
# Keep order/labels tight
ord <- c("^\\(Intercept\\)$",
         "^MKT_RF$",
         "^SMB$",
         "^HML$",
         "^MOM$")
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

dict     = c("return"="Portfolio Return","(Intercept)"="Alpha","MKT_RF"="MKT−RF","SMB"="SMB","HML"="HML","MOM"="UMD")

# Save LaTeX table; fixef_sizes = TRUE shows number of FE levels
etable(
  `(1) Sector + Year FE` = ew_s1,
  `(2) Sector + Year FE` = ew_s4,
  `(3) Sector + Year FE` = vw_s1,
  `(4) Sector + Year FE` = vw_s4,
  headers     = list(c("Equal Weighted" = 2, "Value Weighted" = 2)),
  fixef_sizes = TRUE,
  coefstat    = "tstat",
  keep        = keep_vec,
  order       = order_vec,
  dict        = dict,
  fitstat     = ~ r2 + n,
  se.row      = TRUE,                 # print a row describing SEs
  style.tex   = style.tex("aer",
                          yesNo = c("Yes","No"),
                          fixef.title  = "\\midrule",
                          fixef.where  = "stat",
                          fixef.suffix = " FE",
                          stats.title  = "\\midrule",
                          tabular      = "*"),
  file    = "./results/port_car_5_quin.tex",
  replace = TRUE,
  tex     = TRUE
)

#-- Decile Portfolios

# --- columns ---
ret_cols <- c("LS_D_EW_resid_n_car_5_material",
              "LS_D_VW_resid_n_car_5_material")
factors  <- c("MKT_RF","SMB","HML","MOM")

# --- read + scale BOTH LS returns to percent ---
port <- readr::read_csv(
  "E:/GermanBusinessPanelTeam/Schrader/Forschung/ESG_Portfolios/Output/portfolio_returns_deciles.csv",
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(across(all_of(ret_cols), ~ .x * 100))   # convert to percent

# helper to build a regression df for a given return column
make_df <- function(ret_col) {
  port %>%
    transmute(
      date,
      return = .data[[ret_col]],
      MKT_RF = .data[["MKT_RF"]],
      SMB    = .data[["SMB"]],
      HML    = .data[["HML"]],
      MOM    = .data[["MOM"]]
    ) %>%
    drop_na() %>%
    arrange(date) %>%
    mutate(id = 1L, t = row_number())
}

df_ew <- make_df(ret_cols[1])
df_vw <- make_df(ret_cols[2])

# --- Newey–West HAC (12 lags) ---
vcNW <- NW(lag = 12)

# --- Spec 1 (alpha-only) and Spec 4 (FF4) for each return series ---
# EW
ew_s1 <- feols(return ~ 1,                           data = df_ew, panel.id = ~ id + t)
ew_s4 <- feols(return ~ MKT_RF + SMB + HML + MOM,    data = df_ew, panel.id = ~ id + t)
# VW
vw_s1 <- feols(return ~ 1,                           data = df_vw, panel.id = ~ id + t)
vw_s4 <- feols(return ~ MKT_RF + SMB + HML + MOM,    data = df_vw, panel.id = ~ id + t)

# --- Table with the four columns (EW/VW × Spec1/Spec4) ---
etable(
  ew_s1, ew_s4, vw_s1, vw_s4,
  coefstat = "tstat",
  se.below = TRUE,
  digits   = 3,
  headers  = c("EW α-only","EW FF4","VW α-only","VW FF4")
)

# --- 3) Pretty table with etable ---------------------------------------------
# Keep order/labels tight
ord <- c("^\\(Intercept\\)$",
         "^MKT_RF$",
         "^SMB$",
         "^HML$",
         "^MOM$")
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

dict     = c("return"="Portfolio Return","(Intercept)"="Alpha","MKT_RF"="MKT−RF","SMB"="SMB","HML"="HML","MOM"="UMD")

# Save LaTeX table; fixef_sizes = TRUE shows number of FE levels
etable(
  `(1) Sector + Year FE` = ew_s1,
  `(2) Sector + Year FE` = ew_s4,
  `(3) Sector + Year FE` = vw_s1,
  `(4) Sector + Year FE` = vw_s4,
  headers     = list(c("Equal Weighted" = 2, "Value Weighted" = 2)),
  fixef_sizes = TRUE,
  coefstat    = "tstat",
  keep        = keep_vec,
  order       = order_vec,
  dict        = dict,
  fitstat     = ~ r2 + n,
  se.row      = TRUE,                 # print a row describing SEs
  style.tex   = style.tex("aer",
                          yesNo = c("Yes","No"),
                          fixef.title  = "\\midrule",
                          fixef.where  = "stat",
                          fixef.suffix = " FE",
                          stats.title  = "\\midrule",
                          tabular      = "*"),
  file    = "./results/port_car_5_dec.tex",
  replace = TRUE,
  tex     = TRUE
)

# use whatever factors exist in your file
factor_pool <- c("MKT_RF","SMB","HML","RMW","CMA","MOM","LIQ")
factors <- intersect(names(port), factor_pool)

# -------------- Generic spanning regression

fit_span <- function(df, y, x = factors, nw_lags = 12) {
  d <- df %>% drop_na(any_of(c(y, x)))
  if (nrow(d) == 0) return(NULL)
  fm <- as.formula(paste(y, "~", paste(x, collapse = " + ")))
  m  <- lm(fm, data = d)
  vc <- NeweyWest(m, lag = nw_lags, prewhite = FALSE, adjust = TRUE)
  list(model = m, vcov = vc, data = d)
}

summ_span <- function(fit, label) {
  if (is.null(fit)) return(tibble())
  ct <- coeftest(fit$model, vcov. = fit$vcov)
  td <- broom::tidy(ct)
  alpha_row <- td %>% filter(term == "(Intercept)") %>%
    transmute(series = label,
              alpha  = estimate,
              t_alpha = statistic)
  alpha_row %>%
    mutate(alpha_ann = (1 + alpha)^12 - 1,
           n = nobs(fit$model),
           start = min(fit$data$date),
           end   = max(fit$data$date),
           r2 = summary(fit$model)$r.squared)
}


# ----------------- Run all Regressions

# detect series
leg_cols <- names(port)[str_detect(names(port), "(_EW_ex$|_VW_ex$)")]
ls_cols_raw <- names(port)[str_starts(names(port), "LS_") & !str_ends(names(port), "_ex")]
ls_cols_ex  <- names(port)[str_starts(names(port), "LS_") &  str_ends(names(port), "_ex")]

# prefer *_ex if you exported it, otherwise raw LS
# key by "core name" (drop optional _ex)
core_name <- function(x) sub("_ex$", "", x)
best_ls <- tibble(col = c(ls_cols_ex, ls_cols_raw)) %>%
  distinct(core = core_name(col), .keep_all = TRUE) %>% pull(col)

# run
res_legs <- map_dfr(leg_cols,  ~ summ_span(fit_span(port, .x), .x))
res_ls   <- map_dfr(best_ls,   ~ summ_span(fit_span(port, .x), .x))

results_all <- bind_rows(res_ls, res_legs) %>%
  arrange(series)

print(results_all %>% select(series, alpha, t_alpha, alpha_ann, n, start, end, r2), n=50)


# ------------------ Print output


make_spanning_table <- function(port, low_col, high_col,
                                factors = c("MKT_RF","SMB","HML","MOM"),
                                nw_lag = 12,
                                out = "",
                                title = "Spanning Regressions (EW)") {
  # 1) build common sample with ONLY the columns we need
  df <- port %>%
    transmute(
      y_low  = .data[[low_col]],
      y_high = .data[[high_col]],
      !!!rlang::syms(factors)
    ) %>%
    drop_na()
  
  # 2) fit spanning (same sample for both legs; LS = high-low)
  fml <- as.formula(paste("y ~", paste(factors, collapse = " + ")))
  fit_low  <- lm(fml, data = transform(df, y = y_low))
  fit_high <- lm(fml, data = transform(df, y = y_high))
  fit_ls   <- lm(fml, data = transform(df, y = y_high - y_low))
  
  # after you fit the models & vcovs
  vc_ls <- sandwich::NeweyWest(fit_ls, lag = nw_lag, prewhite = FALSE, adjust = TRUE)
  ct_ls <- lmtest::coeftest(fit_ls, vcov. = vc_ls)
  t_ls  <- unname(ct_ls["(Intercept)","t value"])
  p_ls  <- unname(ct_ls["(Intercept)","Pr(>|t|)"])
  
  star <- function(p) ifelse(p < .01,"***",
                             ifelse(p < .05,"**",
                                    ifelse(p < .10,"*","")))
  
  alpha_low  <- coef(fit_low)[["(Intercept)"]]
  alpha_high <- coef(fit_high)[["(Intercept)"]]
  alpha_ls   <- coef(fit_ls)[["(Intercept)"]]
  
  alpha_low_ann  <- (1 + alpha_low )^12 - 1
  alpha_high_ann <- (1 + alpha_high)^12 - 1
  alpha_ls_ann   <- (1 + alpha_ls )^12 - 1
  
  extra_rows <- tibble::tribble(
    ~term,                 ~`Low Investment`,                       ~`High Investment`,
    "n",                   as.character(nobs(fit_low)),              as.character(nobs(fit_high)),
    "Annualized Alpha",    scales::percent(alpha_low_ann,  0.01),    scales::percent(alpha_high_ann, 0.01),
    "Difference in Alphas","",                                       paste0(scales::percent(alpha_ls_ann, 0.01), star(p_ls))
  )
  
  
  # 5) print with modelsummary (coeffs+stars and t-stats), drop AIC/BIC/LogLik/RMSE
  coef_map <- c("(Intercept)"="Intercept","MKT_RF"="Market","SMB"="SMB",
                "HML"="HML","MOM"="UMD","RMW"="RMW","CMA"="CMA","LIQ"="LIQ")
  models <- list("Low Investment"=fit_low, "High Investment"=fit_high)
  vcovs  <- list("Low Investment"=vc_low,  "High Investment"=vc_high)
  
  msummary(
    models,
    vcov      = vcovs,
    coef_map  = coef_map,
    estimate  = "{estimate}{stars}",
    statistic = "({statistic})",
    stars     = c('*'=.1,'**'=.05,'***'=.01),
    fmt       = 3,
    gof_omit  = "AIC|BIC|Log\\.Lik\\.|RMSE",  # keep n, R2, adj.R2, Std.Errors; drop these
    add_rows  = extra_rows,
    title     = title,
    output    = out
  )
  
  # sanity print so you see the numbers used in the footer
  message(sprintf("Alphas used — Low: %.6f, High: %.6f, LS: %.6f", alpha_low, alpha_high, alpha_ls))
  invisible(list(fit_low=fit_low, fit_high=fit_high, fit_ls=fit_ls))
}

# ---- call it with your EW legs (NOT VW) ----
make_spanning_table(
  port,
  low_col  = "botQ_resid_n_car_1_material_VW_ex",
  high_col = "topQ_resid_n_car_1_material_VW_ex",
  factors  = c("MKT_RF","SMB","HML","MOM"),
  nw_lag   = 12,
  out      = "results/spanning_table_VW_quintiles.tex",
  title    = "Spanning Regressions Quintiles (VW)"
)











