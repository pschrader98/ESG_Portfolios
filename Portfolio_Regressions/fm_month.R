# packages
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(sandwich)
library(lmtest)
library(roll)
library(lubridate)
library(data.table)
library(kableExtra)
library(data.table)
library(stargazer)



# ---------- Pass 1: Rolling betas ----------
roll_k  <- 36
min_obs <- 24

df <- readr::read_csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESG_Portfolios/Output/panel_industries.csv",
                      show_col_types = FALSE) %>%
  mutate(excess = ret_adj - RF) %>%
  arrange(permno, YearMonth)


setDT(df)

# map: firm-level column -> matching industry-level column
pairs <- c(
  n_material_24m          = "industry_n_material_24m",
  n_car_1_material_24m    = "industry_n_car_1_material_24m",
  n_car_5_material_24m    = "industry_n_car_5_material_24m",
  n_reach_material_24m    = "industry_n_reach_material_24m",
  n_severity_material_24m = "industry_n_severity_material_24m"
)

# create ex-self columns: industry_excl_<firm_col>
for (firm_col in names(pairs)) {
  ind_col <- pairs[[firm_col]]
  out_col <- paste0("industry_excl_", firm_col)
  
  # ex-self = industry total − firm’s own count
  df[, (out_col) := 
       get(ind_col) - get(firm_col)
  ]
  
}


setDT(df); setkey(df, permno, YearMonth)

get_betas_roll <- function(d){
  X <- as.matrix(d[, .(MKT_RF = as.numeric(MKT_RF),
                       SMB    = as.numeric(SMB),
                       HML    = as.numeric(HML),
                       MOM    = as.numeric(MOM))])
  y <- as.numeric(d$excess)
  fit <- roll::roll_lm(x = X, y = y, width = roll_k, intercept = TRUE,
                       min_obs = min_obs, complete_obs = TRUE, online = TRUE)
  co  <- as.data.table(fit$coefficients)
  setnames(co, c("alpha_hat","beta_mkt","beta_smb","beta_hml","beta_mom"))
  cbind(d, co)
}
betas <- df[, get_betas_roll(.SD), by = permno] # don't shift, returns are already merged for t+1

# ---------- Pass 2: Fama–MacBeth using the betas panel ----------

setDT(betas)

# 1) Prep FM dataset -----------------------------------------------------------
# Ensure a proper monthly date
if (!inherits(betas$YearMonth, "Date")) {
  betas[, YearMonth := lubridate::ym(as.character(YearMonth))]  # e.g., 2005-01-01
}


fm_vars <- c("permno","YearMonth","excess",
             "n_car_1_material_24m","industry_excl_n_car_1_material_24m",
             "size","bm_q","lev_q","roa_q","beta_mkt","beta_smb","beta_hml","beta_mom")

panel <- betas[, ..fm_vars]

# 2) Optional: winsorize ECON controls (not the incident counts) --------------
winsorize <- function(x) {
  q <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  DescTools::Winsorize(x, val = q)
}

wins_cols <- c("size","bm_q","lev_q","roa_q")
for (v in wins_cols) {
  panel[[v]] <- winsorize(panel[[v]])
}

# 3) Keep complete cases for the FM formula -----------------------------------
fm_formula <- excess ~ n_car_1_material_24m + industry_excl_n_car_1_material_24m +
  size + bm_q + lev_q + roa_q + beta_mkt + beta_smb + beta_hml + beta_mom

vars_needed <- all.vars(fm_formula)

panel <- panel %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(vars_needed), ~ !is.na(.)))

# 4) (Good practice) drop months with few firms ------------------
panel <- panel %>%
  dplyr::add_count(YearMonth, name = "n_month") %>%
  dplyr::filter(n_month >= 5) %>%
  dplyr::select(-n_month)

# 5) Two-step FM helper -------------------------------------------------------
fm_run <- function(data, formula, nw = TRUE, lag = NULL) {
  # First step: cross-sectional OLS by month
  by_m <- split(data, data$YearMonth)
  cs_fits <- lapply(by_m, function(df) {
    lm(formula, data = df)
  })
  # Collect monthly coefficient time series
  coef_ts <- do.call(rbind, lapply(names(cs_fits), function(d) {
    cbind.data.frame(YearMonth = as.Date(d), t(coef(cs_fits[[d]])))
  }))
  rownames(coef_ts) <- NULL
  coef_ts <- coef_ts[order(coef_ts$YearMonth), ]
  
  # Second step: average coeffs; NW SEs from time series of monthly coeffs
  coef_names <- setdiff(colnames(coef_ts), "YearMonth")
  means <- sapply(coef_names, function(k) mean(coef_ts[[k]], na.rm = TRUE))
  ses   <- sapply(coef_names, function(k) {
    y <- coef_ts[[k]]
    if (nw) {
      # Newey-West with automatic bandwidth when lag = NULL
      sqrt(sandwich::NeweyWest(lm(y ~ 1), lag = lag, prewhite = FALSE)[1,1])
    } else {
      sd(y, na.rm = TRUE) / sqrt(sum(!is.na(y)))
    }
  })
  list(
    coef_means = means,
    se = ses,
    Tm = nrow(coef_ts),
    monthly_coefs = coef_ts
  )
}

# 6) Run FM -------------------------------------------------------------------
# 0) Formulae
fm_formula_m1 <- excess ~ n_car_1_material_24m + industry_excl_n_car_1_material_24m
fm_formula_m2 <- update(fm_formula_m1, ~ . + size + bm_q + lev_q + roa_q)
fm_formula_m3 <- update(fm_formula_m2, ~ . + beta_mkt + beta_smb + beta_hml + beta_mom)

# 1) Run FM with NW(12)
fm_m1 <- fm_run(panel, fm_formula_m1, nw = TRUE, lag = 12)
fm_m2 <- fm_run(panel, fm_formula_m2, nw = TRUE, lag = 12)
fm_m3 <- fm_run(panel, fm_formula_m3, nw = TRUE, lag = 12)


# 7) Pretty print & Stargazer-compatible object -------------------------------
# Build real lm shells so stargazer will render nicely,
# then overwrite with FM means and supply NW(12) SEs
# --- Build stargazer-ready lm shells from FM outputs ---
# --- Build lm shells (as before) ---
mk_print_lm <- function(formula, data, fm_out) {
  fit  <- lm(formula, data = data)
  need <- names(fit$coefficients)
  fit$coefficients <- fm_out$coef_means[need]
  se <- as.numeric(fm_out$se[need]); names(se) <- need
  list(fit = fit, se = se)
}

m1 <- mk_print_lm(fm_formula_m1, panel, fm_m1)
m2 <- mk_print_lm(fm_formula_m2, panel, fm_m2)
m3 <- mk_print_lm(fm_formula_m3, panel, fm_m3)

t_of <- function(m) {
  co <- coef(m$fit); se <- m$se[names(co)]
  se[!is.finite(se) | se == 0] <- NA_real_
  co / se
}
t1 <- t_of(m1); t2 <- t_of(m2); t3 <- t_of(m3)

p_of <- function(tvec, df) 2*pt(-abs(tvec), df = df)

p1 <- p_of(t1, df = fm_m1$Tm - 1)
p2 <- p_of(t2, df = fm_m2$Tm - 1)
p3 <- p_of(t3, df = fm_m3$Tm - 1)

# --- LaTeX output (no covariate.labels) ---
out_path <- 'results/fm_car_1_t1.tex'
stargazer(m1$fit, m2$fit, m3$fit,
          type = "latex", float = FALSE, header = FALSE,
          omit.table.layout = "n",
          digits = 3, omit.stat = c("ser","adj.rsq"), df = FALSE,
          table.layout = "-lmd-c#-t-a-s-", report = "vc*t",
          dep.var.labels = 'Ret$_{t}$',
          t = list(t1, t2, t3),                 # what to print below coefs
          p = list(p1, p2, p3),                 # what to use for stars
          star.cutoffs = c(0.1, 0.05, 0.01),
          add.lines = list(
            c("Controls", "No", "Yes", "Yes"),
            c("Betas",    "No", "No",  "Yes"),
            c("Months",   fm_m1$Tm, fm_m2$Tm, fm_m3$Tm)
          ),
          out = out_path
)

