
library(readr)
library(dplyr)
library(lubridate)
library(fixest)

# --- Rolling Sum industry level ---

ret_cols <- c(
  "industry_car_1_decile_ls_ew",
  "industry_car_1_decile_ls_vw",
  "industry_car_1_quintile_ls_ew",
  "industry_car_1_quintile_ls_vw"
)
factors <- c("MKT_RF","SMB","HML","MOM")

# --- read + scale ALL LS returns to percent; ensure YearMonth is monthly Date ---
port <- readr::read_csv(
  "E:/GermanBusinessPanelTeam/Schrader/Forschung/ESG_Portfolios/Output/portfolio_sum.csv",
  show_col_types = FALSE
) %>%
  mutate(YearMonth = lubridate::ym(as.character(YearMonth))) %>%
  arrange(YearMonth) %>%
  mutate(across(all_of(ret_cols), ~ .x * 100))   # returns in percent

# --- helper to build a regression df for a given return column (uses YearMonth) ---
make_df <- function(ret_col) {
  port %>%
    transmute(
      YearMonth,
      return = .data[[ret_col]],
      MKT_RF  = .data[["MKT_RF"]],
      SMB     = .data[["SMB"]],
      HML     = .data[["HML"]],
      MOM     = .data[["MOM"]]
    ) %>%
    tidyr::drop_na() %>%
    arrange(YearMonth) %>%
    mutate(id = 1L, t = dplyr::row_number())
}

# build all four datasets
dfs <- lapply(ret_cols, make_df)
names(dfs) <- ret_cols

# --- Newey–West HAC (12 lags) ---
vcNW <- NW(lag = 12)

# --- fit α-only and FF4 for each portfolio (8 models total) ---
fit_two <- function(df) list(
  s1 = feols(return ~ 1,                        data = df, panel.id = ~ id + t, vcov = vcNW),
  s4 = feols(return ~ MKT_RF + SMB + HML + MOM, data = df, panel.id = ~ id + t, vcov = vcNW)
)

mods <- lapply(dfs, fit_two)

# flatten and order: EW peers, VW peers, EW own, VW own  × (α-only, FF4)
m_ew_peer_s1 <- mods[[ret_cols[1]]]$s1
m_ew_peer_s4 <- mods[[ret_cols[1]]]$s4
m_vw_peer_s1 <- mods[[ret_cols[2]]]$s1
m_vw_peer_s4 <- mods[[ret_cols[2]]]$s4
m_ew_own_s1  <- mods[[ret_cols[3]]]$s1
m_ew_own_s4  <- mods[[ret_cols[3]]]$s4
m_vw_own_s1  <- mods[[ret_cols[4]]]$s1
m_vw_own_s4  <- mods[[ret_cols[4]]]$s4

# --- table (8 columns) ---
etable(
  m_ew_peer_s1, m_ew_peer_s4,
  m_vw_peer_s1, m_vw_peer_s4,
  m_ew_own_s1,  m_ew_own_s4,
  m_vw_own_s1,  m_vw_own_s4,
  coefstat = "tstat",
  se.below = TRUE,
  digits   = 3,
  dict     = c("(Intercept)"="Alpha","MKT_RF"="MKT−RF","SMB"="SMB","HML"="HML","MOM"="UMD"),
  headers  = c(
    "Peers CAR-Quintile LS (EW): α-only", "Peers CAR-Quintile LS (EW): FF4",
    "Peers CAR-Quintile LS (VW): α-only", "Peers CAR-Quintile LS (VW): FF4",
    "Own CAR-Quintile LS (EW): α-only",   "Own CAR-Quintile LS (EW): FF4",
    "Own CAR-Quintile LS (VW): α-only",   "Own CAR-Quintile LS (VW): FF4"
  )
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
  m_ew_peer_s1, 
  m_ew_peer_s4,
  m_vw_peer_s1, 
  m_vw_peer_s4,
  m_ew_own_s1,  
  m_ew_own_s4,
  m_vw_own_s1,  
  m_vw_own_s4,
  headers     = list(c("Decile (EW)" = 2, "Decile (VW)" = 2, "Quintile (EW)" = 2, "Quintile (VW)" = 2)),
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
  file    = "./results/port_car_5_industry.tex",
  replace = TRUE,
  tex     = TRUE
)

#-- Binary firm level---------------

ret_cols <- c(
  "car_1_binary_ls_ew",
  "car_1_binary_ls_vw"
)
factors <- c("MKT_RF","SMB","HML","MOM")

# --- read + scale ALL LS returns to percent; ensure YearMonth is monthly Date ---
port <- readr::read_csv(
  "E:/GermanBusinessPanelTeam/Schrader/Forschung/ESG_Portfolios/Output/portfolio_sum.csv",
  show_col_types = FALSE
) %>%
  mutate(YearMonth = lubridate::ym(as.character(YearMonth))) %>%
  arrange(YearMonth) %>%
  mutate(across(all_of(ret_cols), ~ .x * 100))   # returns in percent

# --- helper to build a regression df for a given return column (uses YearMonth) ---
make_df <- function(ret_col) {
  port %>%
    transmute(
      YearMonth,
      return = .data[[ret_col]],
      MKT_RF  = .data[["MKT_RF"]],
      SMB     = .data[["SMB"]],
      HML     = .data[["HML"]],
      MOM     = .data[["MOM"]]
    ) %>%
    tidyr::drop_na() %>%
    arrange(YearMonth) %>%
    mutate(id = 1L, t = dplyr::row_number())
}

# build all four datasets
dfs <- lapply(ret_cols, make_df)
names(dfs) <- ret_cols

# --- Newey–West HAC (12 lags) ---
vcNW <- NW(lag = 12)

# --- fit α-only and FF4 for each portfolio (8 models total) ---
fit_two <- function(df) list(
  s1 = feols(return ~ 1,                        data = df, panel.id = ~ id + t, vcov = vcNW),
  s4 = feols(return ~ MKT_RF + SMB + HML + MOM, data = df, panel.id = ~ id + t, vcov = vcNW)
)

mods <- lapply(dfs, fit_two)

# flatten and order: EW peers, VW peers, EW own, VW own  × (α-only, FF4)
m_ew_peer_s1 <- mods[[ret_cols[1]]]$s1
m_ew_peer_s4 <- mods[[ret_cols[1]]]$s4
m_vw_peer_s1 <- mods[[ret_cols[2]]]$s1
m_vw_peer_s4 <- mods[[ret_cols[2]]]$s4


# --- table ---
etable(
  `Binary Incident LS (EW): α-only` = m_ew_peer_s1,
  `Binary Incident LS (EW): FF4`    = m_ew_peer_s4,
  `Binary Incident LS (VW): α-only` = m_vw_peer_s1,
  `Binary Incident LS (VW): FF4`    = m_vw_peer_s4,
  coefstat = "tstat",
  se.below = TRUE,
  digits   = 3,
  dict     = c("(Intercept)"="Alpha","MKT_RF"="MKT−RF","SMB"="SMB","HML"="HML","MOM"="UMD"),
  headers  = list(c("Equal Weighted" = 2, "Value Weighted" = 2))  # optional super-headers
)



# --- 3) Pretty table with etable ---------------------------------------------
# --- Pretty LaTeX table -------------------------------------------------------
ord <- c("^\\(Intercept\\)$", "^MKT_RF$", "^SMB$", "^HML$", "^MOM$")
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

dict <- c(
  "return"      = "Portfolio Return",
  "(Intercept)" = "Alpha",
  "MKT_RF"      = "MKT−RF",
  "SMB"         = "SMB",
  "HML"         = "HML",
  "MOM"         = "UMD"
)

etable(
  m_ew_peer_s1, m_ew_peer_s4, m_vw_peer_s1, m_vw_peer_s4,
  headers   = list(c("Binary (EW)" = 2, "Binary (VW)" = 2)),
  coefstat  = "tstat",
  se.below  = TRUE,
  digits    = 3,
  dict      = c("(Intercept)"="Alpha","MKT_RF"="MKT−RF","SMB"="SMB","HML"="HML","MOM"="UMD"),
  style.tex = style.tex(
    "aer",
    yesNo       = c("Yes","No"),
    fixef.title = "\\midrule",
    fixef.where = "stat",
    fixef.suffix= " FE",
    stats.title = "\\midrule",
    tabular     = "*"
  ),
  file    = "./results/port_car_1_bin.tex",
  replace = TRUE,
  tex     = TRUE
)



#------------- Residuals --------------



ret_cols <- c(
  "n_car_1_decile_ls_ew",
  "n_car_1_decile_ls_vw",
  "industry_n_car_1_industry_decile_ls_ew",
  "industry_n_car_1_industry_decile_ls_vw"
)
factors <- c("MKT_RF","SMB","HML","MOM")

# --- read + scale ALL LS returns to percent; ensure YearMonth is monthly Date ---
port <- readr::read_csv(
  "E:/GermanBusinessPanelTeam/Schrader/Forschung/ESG_Portfolios/Output/portfolio_residual.csv",
  show_col_types = FALSE
) %>%
  mutate(YearMonth = lubridate::ym(as.character(YearMonth))) %>%
  arrange(YearMonth) %>%
  mutate(across(all_of(ret_cols), ~ .x * 100))   # returns in percent

# --- helper to build a regression df for a given return column (uses YearMonth) ---
make_df <- function(ret_col) {
  port %>%
    transmute(
      YearMonth,
      return = .data[[ret_col]],
      MKT_RF  = .data[["MKT_RF"]],
      SMB     = .data[["SMB"]],
      HML     = .data[["HML"]],
      MOM     = .data[["MOM"]]
    ) %>%
    tidyr::drop_na() %>%
    arrange(YearMonth) %>%
    mutate(id = 1L, t = dplyr::row_number())
}

# build all four datasets
dfs <- lapply(ret_cols, make_df)
names(dfs) <- ret_cols

# --- Newey–West HAC (12 lags) ---
vcNW <- NW(lag = 12)

# --- fit α-only and FF4 for each portfolio (8 models total) ---
fit_two <- function(df) list(
  s1 = feols(return ~ 1,                        data = df, panel.id = ~ id + t, vcov = vcNW),
  s4 = feols(return ~ MKT_RF + SMB + HML + MOM, data = df, panel.id = ~ id + t, vcov = vcNW)
)

mods <- lapply(dfs, fit_two)

# flatten and order: EW peers, VW peers, EW own, VW own  × (α-only, FF4)
m_ew_peer_s1 <- mods[[ret_cols[1]]]$s1
m_ew_peer_s4 <- mods[[ret_cols[1]]]$s4
m_vw_peer_s1 <- mods[[ret_cols[2]]]$s1
m_vw_peer_s4 <- mods[[ret_cols[2]]]$s4
m_ew_own_s1  <- mods[[ret_cols[3]]]$s1
m_ew_own_s4  <- mods[[ret_cols[3]]]$s4
m_vw_own_s1  <- mods[[ret_cols[4]]]$s1
m_vw_own_s4  <- mods[[ret_cols[4]]]$s4

# --- table (8 columns) ---
etable(
  m_ew_peer_s1, m_ew_peer_s4,
  m_vw_peer_s1, m_vw_peer_s4,
  m_ew_own_s1,  m_ew_own_s4,
  m_vw_own_s1,  m_vw_own_s4,
  coefstat = "tstat",
  se.below = TRUE,
  digits   = 3,
  dict     = c("(Intercept)"="Alpha","MKT_RF"="MKT−RF","SMB"="SMB","HML"="HML","MOM"="UMD"),
  headers  = c(
    "Peers CAR-Quintile LS (EW): α-only", "Peers CAR-Quintile LS (EW): FF4",
    "Peers CAR-Quintile LS (VW): α-only", "Peers CAR-Quintile LS (VW): FF4",
    "Own CAR-Quintile LS (EW): α-only",   "Own CAR-Quintile LS (EW): FF4",
    "Own CAR-Quintile LS (VW): α-only",   "Own CAR-Quintile LS (VW): FF4"
  )
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
  m_ew_peer_s1, 
  m_ew_peer_s4,
  m_vw_peer_s1, 
  m_vw_peer_s4,
  m_ew_own_s1,  
  m_ew_own_s4,
  m_vw_own_s1,  
  m_vw_own_s4,
  headers     = list(c("Decile (EW)" = 2, "Decile (VW)" = 2, "Quintile (EW)" = 2, "Quintile (VW)" = 2)),
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
  file    = "./results/port_car_1_residual.tex",
  replace = TRUE,
  tex     = TRUE
)




