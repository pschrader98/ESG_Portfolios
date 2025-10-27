# packages
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(sandwich)
library(lmtest)
library(roll)
library(lubridate)
library(fixest)


df <- readr::read_csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESG_Portfolios/Output/events_daily.csv",
                      show_col_types = FALSE)

df <- df %>%
  mutate(
    date     = as.Date(date),
    industry = as.factor(`SICS Codified Industry`),
    cusip    = as.character(cusip),
    year     = as.integer(format(date, "%Y")),
    YearQuarter = factor(sprintf("%dQ%d",
                                 as.integer(format(date, "%Y")),
                                 as.integer((as.POSIXlt(date)$mon)%/%3 + 1))),
    i_ind_qtr   = interaction(industry, YearQuarter, drop = TRUE),
    
    # ensure 0/1 integers (don’t force NAs; keep them as NA)
    across(c(incident,
             material_flag, materiality_car_5, materiality_reach, materiality_car_1, materiality_severity,
             industry_incident,
             industry_material_flag, industry_materiality_car_5, industry_materiality_reach,
             industry_materiality_car_1, industry_materiality_severity),
           ~ as.integer(.)),
    
    # ---------- FIRM-LEVEL: disjoint dummies ----------
    mat_inc_flag   = as.integer(incident == 1 & material_flag == 1),
    nonmat_inc_flag= as.integer(incident == 1 & material_flag == 0),
    
    mat_inc_car1   = as.integer(incident == 1 & materiality_car_1 == 1),
    nonmat_inc_car1= as.integer(incident == 1 & materiality_car_1 == 0),
    
    mat_inc_car5   = as.integer(incident == 1 & materiality_car_5 == 1),
    nonmat_inc_car5= as.integer(incident == 1 & materiality_car_5 == 0),
    
    mat_inc_reach  = as.integer(incident == 1 & materiality_reach == 1),
    nonmat_inc_reach=as.integer(incident == 1 & materiality_reach == 0),
    
    mat_inc_sev    = as.integer(incident == 1 & materiality_severity == 1),
    nonmat_inc_sev = as.integer(incident == 1 & materiality_severity == 0),
    
    # ---------- INDUSTRY-LEVEL: disjoint dummies ----------
    ind_inc_only = as.integer(industry_incident == 1 & incident == 0),
    
    ind_car_1_only = as.integer(industry_materiality_car_1 == 1 & incident == 0),
    ind_car_5_only = as.integer(industry_materiality_car_5 == 1 & incident == 0),
    ind_reach_only = as.integer(industry_materiality_reach == 1 & incident == 0),
    ind_severity_only = as.integer(industry_materiality_severity == 1 & incident == 0),
    ind_flag_only = as.integer(industry_material_flag == 1 & incident == 0),
    
    ind_mat_inc_flag    = as.integer(industry_incident == 1 & industry_material_flag == 1 & incident == 0),
    ind_nonmat_inc_flag = as.integer(industry_incident == 1 & industry_material_flag == 0 & incident == 0),
    
    ind_mat_inc_car1    = as.integer(industry_incident == 1 & industry_materiality_car_1 == 1 & incident == 0),
    ind_nonmat_inc_car1 = as.integer(industry_incident == 1 & industry_materiality_car_1 == 0 & incident == 0),
    
    ind_mat_inc_car5    = as.integer(industry_incident == 1 & industry_materiality_car_5 == 1 & incident == 0),
    ind_nonmat_inc_car5 = as.integer(industry_incident == 1 & industry_materiality_car_5 == 0 & incident == 0),
    
    ind_mat_inc_reach   = as.integer(industry_incident == 1 & industry_materiality_reach == 1 & incident == 0),
    ind_nonmat_inc_reach= as.integer(industry_incident == 1 & industry_materiality_reach == 0 & incident == 0),
    
    ind_mat_inc_sev     = as.integer(industry_incident == 1 & industry_materiality_severity == 1 & incident == 0),
    ind_nonmat_inc_sev  = as.integer(industry_incident == 1 & industry_materiality_severity == 0 & incident == 0),
    
    # ---------- Event characteristics ----------
    
    high_car_peer_quin    = as.integer(bottom_quintile_industry == 1 & incident == 0), # There has been a high car incident for peer but not focal firm
    low_car_peer_quin  = as.integer(industry_incident == 1 & bottom_quintile_industry == 0 & incident == 0), # There has been an incident for peer firm, which was not material and not related to focal firm

    high_car_peer_dec    = as.integer(bottom_decile_industry == 1 & incident == 0), 
    low_car_peer_dec  = as.integer(industry_incident == 1 & bottom_decile_industry == 0 & incident == 0),
    
    high_reach_peer    = as.integer(peer_reach3 == 1 & incident == 0), 
    low_reach_peer  = as.integer(industry_incident == 1 & peer_reach3 == 0 & incident == 0),
    
    high_severity_peer   = as.integer(peer_severity3 == 1 & incident == 0), # There has been a high car incident for peer but not focal firm
    low_severity_peer  = as.integer(industry_incident == 1 & peer_severity3 == 0 & incident == 0),
    
    sasb_peer   = as.integer(industry_material_flag == 1 & incident == 0), # There has been a high car incident for peer but not focal firm
    no_sasb_peer  = as.integer(industry_incident == 1 & industry_material_flag == 0 & incident == 0),
    
      )


# --- Tests for different materiality classifications ------------------------------------------

# --- Incident ------------------------------------------
e1 <- feols(
  CAR_0_1 ~ incident,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e2 <- feols(
  CAR_0_1 ~ incident + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e3 <- feols(
  CAR_0_1 ~ incident + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | industry + year,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e4 <- feols(
  CAR_0_1 ~ incident + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | cusip + date,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# --- Material Incident ------------------------------------------
e1 <- feols(
  CAR_0_1 ~ materiality_car_1,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e2 <- feols(
  CAR_0_1 ~ materiality_car_1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e3 <- feols(
  CAR_0_1 ~ materiality_car_1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | industry + year,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e4 <- feols(
  CAR_0_1 ~ materiality_car_1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | cusip + date,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# --- Material vs. nonmaterial incident------------------------------------------
e1 <- feols(
  CAR_0_1 ~ mat_inc_car1 + nonmat_inc_car1,
  data    = df,
  cluster = ~ cusip + date    # two-way clustered SE (firm and month)
)

e2 <- feols(
  CAR_0_1 ~ mat_inc_car1 + nonmat_inc_car1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc,
  data    = df,
  cluster = ~ cusip + date    # two-way clustered SE (firm and month)
)

e3 <- feols(
  CAR_0_1 ~ mat_inc_car1 + nonmat_inc_car1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | industry + year,
  data    = df,
  cluster = ~ cusip + date    # two-way clustered SE (firm and month)
)

e4 <- feols(
  CAR_0_1 ~ mat_inc_car1 + nonmat_inc_car1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | cusip + date,
  data    = df,
  cluster = ~ cusip + date   # two-way clustered SE (firm and month)
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# --- 3) Pretty table with etable ---------------------------------------------
# Keep order/labels tight
ord <- c("^mat_inc_car1$",
         "^nonmat_inc_car1$")
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

dict <- c(
  "CAR_0_1" = "CAR[0;1]",
  "mat_inc_car1" = "Material Firm Incident",
  "nonmat_inc_car1" = "Non-Material Firm Incident",
  "cusip"                      = "Firm",
  "date"                        = "Day",
  "industry"                 = "Industry",
  "year"                  = "Year"
)

# Save LaTeX table; fixef_sizes = TRUE shows number of FE levels
etable(
  `(1) Sector + Year FE` = e1,
  `(2) Sector×Year FE`   = e2,
  `(3) Year FE only`     = e3,
  `(3) Year FE only`     = e4,
  fixef_sizes = TRUE,
  coefstat    = "tstat",
  keep        = keep_vec,
  order       = order_vec,
  extralines = list("Controls" = c("No", "Yes", "Yes", "Yes")),
  dict        = dict,
  fitstat     = ~ r2 + n,
  se.row      = TRUE,                 # print a row describing SEs
  cluster     = ~ cusip,       # prints the clustering type in the SE row
  style.tex   = style.tex("aer",
                          yesNo = c("Yes","No"),
                          fixef.title  = "\\midrule",
                          fixef.where  = "stat",
                          fixef.suffix = " FE",
                          stats.title  = "\\midrule",
                          tabular      = "*"),
  file    = "./results/day_reg_firm_car1.tex",
  replace = TRUE,
  tex     = TRUE
)

# --- Incident industry ------------------------------------------
e1 <- feols(
  CAR_0_1 ~ ind_inc_only,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e2 <- feols(
  CAR_0_1 ~ ind_inc_only + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e3 <- feols(
  CAR_0_1 ~ ind_inc_only + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | industry + year,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e4 <- feols(
  CAR_0_1 ~ ind_inc_only + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | cusip + date,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# --- Material incidents (industry)------------------------------------------
e1 <- feols(
  CAR_0_1 ~ ind_car_1_only,
  data    = df,
  cluster = ~ cusip + date    # two-way clustered SE (firm and month)
)

e2 <- feols(
  CAR_0_1 ~ ind_car_1_only + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc,
  data    = df,
  cluster = ~ cusip + date    # two-way clustered SE (firm and month)
)

e3 <- feols(
  CAR_0_1 ~ ind_car_1_only + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | industry + year,
  data    = df,
  cluster = ~ cusip + date    # two-way clustered SE (firm and month)
)

e4 <- feols(
  CAR_0_1 ~ ind_car_1_only + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | cusip + date,
  data    = df,
  cluster = ~ cusip + date   # two-way clustered SE (firm and month)
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# --- Material vs. nonmaterial incident (industry) ------------------------------------------
e1 <- feols(
  CAR_0_1 ~ ind_mat_inc_car1 + ind_nonmat_inc_car1,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e2 <- feols(
  CAR_0_1 ~ ind_mat_inc_car1 + ind_nonmat_inc_car1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e3 <- feols(
  CAR_0_1 ~ ind_mat_inc_car1 + ind_nonmat_inc_car1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | industry + year,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)

e4 <- feols(
  CAR_0_1 ~ ind_mat_inc_car1 + ind_nonmat_inc_car1 + size + btm + leverage + ad_intensity + rd_intensity + roa + InstOwn_Perc
  | cusip + date,
  data    = df,
  cluster = ~ cusip    # two-way clustered SE (firm and month)
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# --- 3) Pretty table with etable ---------------------------------------------
# Keep order/labels tight
ord <- c("^ind_mat_inc_car1$",
         "^ind_nonmat_inc_car1$")
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

dict <- c(
  "CAR_0_1" = "CAR[0;1]",
  "ind_mat_inc_car1" = "Material Firm Incident",
  "ind_nonmat_inc_car1" = "Non-Material Industry Incident",
  "cusip"                      = "Firm",
  "date"                        = "Day",
  "industry"                 = "Industry",
  "year"                  = "Year"
)

# Save LaTeX table; fixef_sizes = TRUE shows number of FE levels
etable(
  `(1) Sector + Year FE` = e1,
  `(2) Sector×Year FE`   = e2,
  `(3) Year FE only`     = e3,
  `(3) Year FE only`     = e4,
  fixef_sizes = TRUE,
  coefstat    = "tstat",
  keep        = keep_vec,
  order       = order_vec,
  extralines = list("Controls" = c("No", "Yes", "Yes", "Yes")),
  dict        = dict,
  fitstat     = ~ r2 + n,
  se.row      = TRUE,                 # print a row describing SEs
  cluster     = ~ cusip,       # prints the clustering type in the SE row
  style.tex   = style.tex("aer",
                          yesNo = c("Yes","No"),
                          fixef.title  = "\\midrule",
                          fixef.where  = "stat",
                          fixef.suffix = " FE",
                          stats.title  = "\\midrule",
                          tabular      = "*"),
  file    = "./results/day_reg_industry_car1.tex",
  replace = TRUE,
  tex     = TRUE
)

# --- All event characteristics in one table (a priori definition) ------------------------------------------

df_peer <- dplyr::filter(df, incident == 0)

e1 <- feols(
  CAR_0_1 ~ ind_mat_inc_car1 + ind_nonmat_inc_car1 + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)

e2 <- feols(
  CAR_0_1 ~ ind_mat_inc_reach    + ind_nonmat_inc_reach + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)

e3 <- feols(
  CAR_0_1 ~ ind_mat_inc_sev  + ind_nonmat_inc_sev   + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)

e4 <- feols(
  CAR_0_1 ~ ind_mat_inc_flag   + ind_nonmat_inc_flag  + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# -------------------- Table formatting --------------------
# --- Select & order: refer to ORIGINAL names → prefix each pattern with '%'
patterns <- c(
  "^ind_mat_inc_car1$", "^ind_nonmat_inc_car1$",
  "^ind_mat_inc_reach$",    "^ind_nonmat_inc_reach$",
  "^ind_mat_inc_sev$", "^ind_nonmat_inc_sev$",
  "^ind_mat_inc_flag$",          "^ind_nonmat_inc_flag$"
)
keep_vec  <- paste0("%", patterns)
order_vec <- keep_vec

# --- Unified labels
dict <- c(
  "CAR_0_1"            = "CAR[0;1]",
  "ind_mat_inc_car1" = "Material peer incident",
  "ind_nonmat_inc_car1"  = "Non-material peer incident",
  "ind_mat_inc_reach"    = "Material peer incident",
  "ind_nonmat_inc_reach"     = "Non-material peer incident",
  "ind_mat_inc_sev" = "Material peer incident",
  "ind_nonmat_inc_sev"  = "Non-material peer incident",
  "ind_mat_inc_flag"          = "Material peer incident",
  "ind_nonmat_inc_flag"       = "Non-material peer incident",
  # FE aliases in stats panel
  "cusip"    = "Firm",
  "date"     = "Day",
  "industry" = "Industry",
  "year"     = "Year"
)

# --- One table, side-by-side, with column descriptions of materiality
etable(
  "(1) Materiality = CAR[0;1] quintile" = e1,
  "(2) Materiality = Reach"             = e2,
  "(3) Materiality = Severity"          = e3,
  "(4) Materiality = SASB flag"         = e4,
  fixef_sizes = TRUE,
  coefstat    = "tstat",
  keep        = keep_vec,
  order       = order_vec,
  dict        = dict,
  fitstat     = ~ r2 + n,
  se.row      = TRUE,
  
  # NEW: remove the automatic "Dependent Var." header row
  depvar      = FALSE,
  
  # NEW: set custom per-column headers (what shows above the columns)
  headers     = c("CAR[0;1] quintile", "Reach", "Severity", "SASB flag"),
  
  extralines  = list(
    "Materiality definition" = c("CAR[0;1] quintile", "Reach", "Severity", "SASB flag"),
    "Controls"               = c("Yes","Yes","Yes","Yes")
  ),
  style.tex   = style.tex("aer",
                          yesNo = c("Yes","No"),
                          fixef.title  = "\\midrule",
                          fixef.where  = "stat",
                          fixef.suffix = " FE",
                          stats.title  = "\\midrule",
                          tabular      = "*"),
  file    = "./results/day_reg_materiality_peer_spillovers.tex",
  replace = TRUE,
  tex     = TRUE
)

# --- Tests for spillover of different event types ------------------------------------------

# --- Low CAR vs. high CAR incident of peers ------------------------------------------
df_peer <- dplyr::filter(df, incident == 0)

e1 <- feols(
  CAR_0_1 ~ high_car_peer_quin + low_car_peer_quin,
  data    = df_peer,
  cluster = ~ industry + YearQuarter
)

e2 <- feols(
  CAR_0_1 ~ high_car_peer_quin + low_car_peer_quin + size + bm_q + roa_q + lev_q,
  data    = df_peer,
  cluster = ~ industry + YearQuarter
)

e3 <- feols(
  CAR_0_1 ~ high_car_peer_quin + low_car_peer_quin + size + bm_q + roa_q + lev_q
  | industry + year,
  data    = df_peer,
  cluster = ~ industry + YearQuarter
)

e4 <- feols(
  CAR_0_1 ~ high_car_peer_quin + low_car_peer_quin + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ industry + YearQuarter
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')

# --- All event characteristics in one table ------------------------------------------

df_peer <- dplyr::filter(df, incident == 0)

e1 <- feols(
  CAR_0_1 ~ high_car_peer_quin + low_car_peer_quin + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)

e2 <- feols(
  CAR_0_1 ~ high_reach_peer + low_reach_peer + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)

e3 <- feols(
  CAR_0_1 ~ high_severity_peer + low_severity_peer + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)

e4 <- feols(
  CAR_0_1 ~ sasb_peer + no_sasb_peer + size + bm_q + roa_q + lev_q
  | cusip + date,
  data    = df_peer,
  cluster = ~ cusip + date
)


etable(e1, e2, e3, e4, fixef_sizes = TRUE, coefstat = 'tstat')


# -------------------- Table formatting --------------------
# --- Select & order: refer to ORIGINAL names → prefix each pattern with '%'
patterns <- c(
  "^high_car_peer_quin$", "^low_car_peer_quin$",
  "^high_reach_peer$",    "^low_reach_peer$",
  "^high_severity_peer$", "^low_severity_peer$",
  "^sasb_peer$",          "^no_sasb_peer$"
)
keep_vec  <- paste0("%", patterns)
order_vec <- keep_vec

# --- Unified labels
dict <- c(
  "CAR_0_1"            = "CAR[0;1]",
  "high_car_peer_quin" = "Material peer incident",
  "low_car_peer_quin"  = "Non-material peer incident",
  "high_reach_peer"    = "Material peer incident",
  "low_reach_peer"     = "Non-material peer incident",
  "high_severity_peer" = "Material peer incident",
  "low_severity_peer"  = "Non-material peer incident",
  "sasb_peer"          = "Material peer incident",
  "no_sasb_peer"       = "Non-material peer incident",
  # FE aliases in stats panel
  "cusip"    = "Firm",
  "date"     = "Day",
  "industry" = "Industry",
  "year"     = "Year"
)

# --- One table, side-by-side, with column descriptions of materiality
etable(
  "(1) Materiality = CAR[0;1] quintile" = e1,
  "(2) Materiality = Reach"             = e2,
  "(3) Materiality = Severity"          = e3,
  "(4) Materiality = SASB flag"         = e4,
  fixef_sizes = TRUE,
  coefstat    = "tstat",
  keep        = keep_vec,
  order       = order_vec,
  dict        = dict,
  fitstat     = ~ r2 + n,
  se.row      = TRUE,
  
  # NEW: remove the automatic "Dependent Var." header row
  depvar      = FALSE,
  
  # NEW: set custom per-column headers (what shows above the columns)
  headers     = c("CAR[0;1] quintile", "Reach", "Severity", "SASB flag"),
  
  extralines  = list(
    "Materiality definition" = c("CAR[0;1] quintile", "Reach", "Severity", "SASB flag"),
    "Controls"               = c("Yes","Yes","Yes","Yes")
  ),
  style.tex   = style.tex("aer",
                          yesNo = c("Yes","No"),
                          fixef.title  = "\\midrule",
                          fixef.where  = "stat",
                          fixef.suffix = " FE",
                          stats.title  = "\\midrule",
                          tabular      = "*"),
  file    = "./results/day_reg_peer_spillovers.tex",
  replace = TRUE,
  tex     = TRUE
)



