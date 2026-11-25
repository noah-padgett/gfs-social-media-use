# Time Media
# Replication code

# Script: main.R
# Created by: R. Noah Padgett
# Last edited on: 2026-11-25


# ---- Part 1. R environment preparation ----
# Installing the package

#install.packages("remotes")
#remotes::install_github("noah-padgett/Rglobalflourishing", force = TRUE)

library(Rglobalflourishing)
load_packages()
options( survey.lonely.psu = "certainty")

res.dir <- "results-timemedia"
if(!dir.exists(here(res.dir))){
  dir.create(here(res.dir))
}
data.dir <- "data"
dataset.name <- "gfs_all_countries_Y1_Y2_Y3_midyear.sav"

# Here is YOUR construct variable
#   MUST be the variable name from the GFS Codebook for the variable in the dataset.
#   The name must end in "_Y1" / "_Y2" / "_Y3"
# Variable ends in "_Y2" -->> OUTCOME-WIDE Analysis
#   Note. For now, choose only one type of analysis.
FOCAL_VARIABLE <-  c("TIME_MEDIA_MY")

# Label/title you want to give your variable.
#   Note, most table, by default already contain "... at Wave 1..." in the title so
#   you do not need to add that here.
#get_outcome_better_name( FOCAL_VARIABLE, include.name = FALSE)
FOCAL_VARIABLE_BETTER_NAME <- c(TIME_MEDIA_MY = "social media use >2 hours/day")
# what category labels or value (e.g., mean) represents the reference group/value for your focal predictor
FOCAL_VARIABLE_REFERENCE_VALUE <- c(TIME_MEDIA_MY = "social media use ≤2 hours/day or none")

# IF your predictor (focal exposure) is binary/categorical, use the code below to define how you
#   want it to be categorized. Categorization must result in a binary variable 0/1 for
#   consistency across studies.
VALUES_DEFINING_UPPER_CATEGORY <- list(c(5))
VALUES_DEFINING_LOWER_CATEGORY <- list(c(1, 2, 3, 4))

save(data.dir,dataset.name, FOCAL_VARIABLE, FOCAL_VARIABLE_BETTER_NAME, FOCAL_VARIABLE_REFERENCE_VALUE, VALUES_DEFINING_UPPER_CATEGORY, VALUES_DEFINING_LOWER_CATEGORY,
     file = "variable_globals.RData")

##

df_raw0 <- gfs_get_labelled_raw_data(
  file = here::here(data.dir, dataset.name), wave = 3,
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  reverse.code.cont = TRUE
)

df.raw <- df_raw0 |>
  filter(!is.na(RETENTION_WEIGHT_L3)) |>
  mutate(
    MIDYEAR_TYPE_MY = case_when(
      MIDYEAR_TYPE_MY == 1 ~ "Separate Midyear and Year 2 Surveys",
      MIDYEAR_TYPE_MY == 2 ~ "Combined Midyear and Year 2 Surveys"
    ),
    TIME_MEDIA_MY_BINARY = case_when(
      TIME_MEDIA_MY == "5. More than 2 hours" ~ ">2 hours/day",
      TIME_MEDIA_MY %in% c("1. None/I don’t use social media", "2. Less than 30 minutes", "3. 30 minutes to less than an hour", "4. 1 to 2 hours") ~ "≤2 hours/day or none",
      .default = "(Missing)"
    ),
    AGE_GRP_Y1_ALT = case_when(
      AGE_Y1 %in% 18:24 ~ "    18--24",
      AGE_Y1 %in% 25:29 ~ "    25--29",
      AGE_Y1 %in% 30:39 ~ "    30--39",
      AGE_Y1 %in% 40:49 ~ "    40--49",
      AGE_Y1 %in% 50:59 ~ "    50--59",
      AGE_Y1 %in% 60:69 ~ "    60--69",
      AGE_Y1 %in% 70:79 ~ "    70--79",
      AGE_Y1 %in% 80:99 ~ "    80+"
    )

  )

df.svy <- svydesign(
  data=df.raw,
  ids=~PSU, strata=~STRATA, weights=~RETENTION_WEIGHT_L3
)

# ---- Part 2. Summary table(s) ----
# Complete-case sample size: 89,488, from RETENTION_WEIGHT_L3

## --- Table 1 ----
tb <- tbl_svysummary(
  data = df.svy ,
  by = TIME_MEDIA_MY_BINARY,
  include = c(AGE_Y1, AGE_GRP_Y1, GENDER, MARITAL_STATUS_Y1, EDUCATION_3_Y1, EMPLOYMENT_Y1, ATTEND_SVCS_Y1, BORN_COUNTRY_Y1),
  #include = c(AGE_Y1, AGE_GRP_Y1_ALT, GENDER, MARITAL_STATUS_Y1, EDUCATION_3_Y1, EMPLOYMENT_Y1),
  label = list(
    AGE_Y1 ~ "Respondent age",
    AGE_GRP_Y1 ~ "Age group",
    #AGE_GRP_Y1_ALT ~ "Age group",
    GENDER ~ "Gender",
    MARITAL_STATUS_Y1 ~ "Marital status",
    EDUCATION_3_Y1 ~ "Years of education",
    EMPLOYMENT_Y1 ~ "Employment status",
    ATTEND_SVCS_Y1 ~ "Religious service attendance",
    BORN_COUNTRY_Y1 ~ "Immigrant status"
  ),
  type = list(all_continuous() ~ "continuous2"),
  statistic = list(all_continuous() ~
                     c("    {mean} ({sd})", "    {p25}, {p75}"),
                   all_categorical() ~
                     "{n} ({p}%)"),
  digits = list(all_continuous() ~ c(1, 1, 1, 1),
                all_categorical() ~ list(label_style_number(digits = 0), Rglobalflourishing:::label_style_percent0(digits = 1))),
  missing_text = "    (Missing)",
  missing_stat = "{N_miss} ({p_miss}%)"
) %>%
  add_overall(last = TRUE) %>%
  italicize_labels() %>%
  modify_header(label ~ "**Characteristic**") %>%
  #modify_spanning_header(c("stat_1", "stat_2", "stat_3")~ "**Time Media Use**") |>
  add_stat_label(
    label = all_continuous() ~ c("    Mean (Standard Deviation)", "    Q1, Q3")
  )

ft <- as_flex_table(tb) |>
  add_header_lines(
    "Table 1. Sociodemographic characteristics of the pooled sample combined across countries by social media use
categories (weighted to be nationally representative within each country)."
  ) |>
  add_footer_lines("Note. Additional information about missingness and sample characteristics are provided in our Supplement which stratifies by country.")

read_docx() |>
  body_add_flextable(ft) |>
  print(target = here(res.dir, "table_1.docx"))

## --- Supplemental File 2 ----
### A relatively large excel table
tb <- tbl_svysummary(
  data = df.svy ,
  by = COUNTRY,
  #include = c(AGE_Y1, AGE_GRP_Y1_ALT, GENDER, MARITAL_STATUS_Y1, EDUCATION_3_Y1, EMPLOYMENT_Y1),
  include = c(AGE_Y1, AGE_GRP_Y1, GENDER, MARITAL_STATUS_Y1,  EMPLOYMENT_Y1, EDUCATION_3_Y1, ATTEND_SVCS_Y1, BORN_COUNTRY_Y1),
  label = list(
    AGE_Y1 ~ "Respondent age",
    AGE_GRP_Y1 ~ "Age group",
    #AGE_GRP_Y1_ALT ~ "Age group",
    GENDER ~ "Gender",
    MARITAL_STATUS_Y1 ~ "Marital status",
    EDUCATION_3_Y1 ~ "Years of education",
    EMPLOYMENT_Y1 ~ "Employment status"
  ),
  type = list(all_continuous() ~ "continuous2"),
  statistic = list(all_continuous() ~
                     c("    {mean} ({sd})", "    {p25}, {p75}"),
                   all_categorical() ~
                     "{n} ({p}%)"),
  digits = list(all_continuous() ~ c(1, 1, 1, 1),
                all_categorical() ~ list(label_style_number(digits = 0), Rglobalflourishing:::label_style_percent0(digits = 1))),
  missing_text = "    (Missing)",
  missing_stat = "{N_miss} ({p_miss}%)"
) %>%
  italicize_labels() %>%
  modify_header(label ~ "**Characteristic**") %>%
  add_stat_label(
    label = all_continuous() ~ c("    Mean (Standard Deviation)", "    Q1, Q3")
  )

ft <- as_flex_table(tb) |>
  add_header_lines(
    "Table S1. Sociodemographic characteristics by country."
  )

Rglobalflourishing::gfs_append_to_xlsx(
  file = here(res.dir, "Supplemental File 2.xlsx"),
  ft = ft,
  tb = "Table S5. Summary of demographic characteristics by Country"
)

## --- Table 2----
## Summary of time media by country

# [2] "TIME_MEDIA_MY1. None/I don’t use social media"
# [3] "TIME_MEDIA_MY1. Less than 30 minutes"
# [4] "TIME_MEDIA_MY3. 30 minutes to less than an hour"
# [5] "TIME_MEDIA_MY4. 1 to 2 hours"
# [6] "TIME_MEDIA_MY5. More than 2 hours"
# [7] "TIME_MEDIA_MY(Missing)"

counts.country <- svytotal(~COUNTRY, design = df.svy)

# 1st, w/ missing to get count + proportion missing
time.media.counts.miss <- svyby(~TIME_MEDIA_MY, by = ~COUNTRY, svytotal, design = df.svy)
time.media.props.miss <- svyby(~TIME_MEDIA_MY, by = ~COUNTRY, svymean, design = df.svy)

svymean(~TIME_MEDIA_MY, design = df.svy |> subset(TIME_MEDIA_MY != "(Missing)"))
svytotal(~TIME_MEDIA_MY, design = df.svy |> subset(TIME_MEDIA_MY != "(Missing)"))


# 2nd, subpopulation without missing to get count + proportion of each category
time.media.counts <- svyby(~TIME_MEDIA_MY, by = ~COUNTRY, svytotal, design = df.svy |> subset(TIME_MEDIA_MY != "(Missing)"))
time.media.props <- svyby(~TIME_MEDIA_MY, by = ~COUNTRY, svymean, design = df.svy |> subset(TIME_MEDIA_MY != "(Missing)"))

sum.tb.w.counts <- data.frame(
  Country = time.media.counts[,1],
  N =  format(round(counts.country,0), big.mark=","),
  c5 = paste0(round(time.media.counts[,6],0), " (",.round(time.media.props[,6]*100,1), "%)"),
  c4 = paste0(round(time.media.counts[,5],0), " (",.round(time.media.props[,5]*100,1), "%)"),
  c3 = paste0(round(time.media.counts[,4],0), " (",.round(time.media.props[,4]*100,1), "%)"),
  c2 = paste0(round(time.media.counts[,3],0), " (",.round(time.media.props[,3]*100,1), "%)"),
  c1 = paste0(round(time.media.counts[,2],0), " (",.round(time.media.props[,2]*100,1), "%)"),
  miss = paste0(round(time.media.counts.miss[,7],0), " (",.round(time.media.props.miss[,7]*100,1), "%)"),
  ord.var = time.media.props[,6]
)

sum.tb <- data.frame(
  Country = time.media.counts[,1],
  N = format(round(counts.country,0), big.mark=","),
  c5 = paste0(.round(time.media.props[,6]*100,1)),
  c4 = paste0(.round(time.media.props[,5]*100,1)),
  c3 = paste0(.round(time.media.props[,4]*100,1)),
  c2 = paste0(.round(time.media.props[,3]*100,1)),
  c1 = paste0(.round(time.media.props[,2]*100,1)),
  miss = paste0(round(time.media.counts.miss[,7],0), " (",.round(time.media.props.miss[,7]*100,1), "%)"),
  ord.var = time.media.props[,6]
)
colnames(sum.tb) <- c("Country", "N", ">2 hours/day (%)", ">1 to 2 hours/day (%)", "30min to <1 hours/day (%)", ">0 to <30min/day (%)", "0 hours/I don’t use social media (%)", "Missing N (%)", "ord.var")
# ">2 hours/day", "≤2 hours/day or none

ft <- sum.tb |>
  arrange(-ord.var) |>
  select(-ord.var) |>
  flextable() |>
  add_header_lines(
    "Table 2. Weighted prevalence of social media use by country."
  ) |>
  add_footer_lines(
    "Note. %'s estimated from complete-case data using the subpopulation within each country responding to the time-media question. N (%) missing reported for completeness and clarity."
  )

read_docx() |>
  body_add_flextable(ft) |>
  print(target = here(res.dir, "table_2.docx"))


# ----- Part 3. Imputing + recoding data ----
# The following is set to FALSE to not run since the imputed data objected are provided. The code, shown in greater detail in the "2_imputation.R" script actual conducts this imputation
run.imp = FALSE
if(run.imp){
  # Run data imputation
  source("2_imputation.R")

}

run.recode = TRUE
if(run.recode){
  source("3_recoding_data.R")
}

# ----- Part 4. Demographic variation analyses ----
# The following runs the Wave 1 analyses of the proportion of population within group using social median >2hrs/day

demo.vars <- c(paste0("COV_",c("AGE_GRP_Y1", "GENDER", "MARITAL_STATUS_Y1",
                               "EMPLOYMENT_Y1", "ATTEND_SVCS_Y1", "EDUCATION_3_Y1",
                               "BORN_COUNTRY_Y1")))

fit.demo.analysis <- map(seq_len(length(demo.vars)),\(i){
  d = demo.vars[i]
  gfs_svyglm_mi(
    fx = as.formula(paste0("I(TIME_MEDIA_MY) ~ 0 + ", d)),
    data.dir = paste0(data.dir, "/recoded"),
    direct.subset = quote(!is.na(RETENTION_WEIGHT_L3)),
    wgt = RETENTION_WEIGHT_L3,
    psu = PSU,
    strata = STRATA,
    force.linear = TRUE,
    res.dir = res.dir,
    appnd.txt.to.filename = paste0("_", d)
  )
})
fit.demo.analysis <- lapply(demo.vars, \(x){
  local({
    load(here(res.dir, paste0("TIME_MEDIA_MY_svyglm_saved_results_",x,".RData")))
    output$Variable <- x
    return(output)
  })
})
fit.demo.analysis <- fit.demo.analysis |>
  bind_rows()
df_country_results <- fit.demo.analysis |>
  select(Variable,COUNTRY:miss.info,p.value,outcome.sd,predictor.sd,wald.fvalue:wald.p.value)
# export country specific results
## --- Supplemental File 3 ----
write_csv(
  df_country_results,
  file = here(res.dir,"results", "TIME_MEDIA_MY_Demographic_Variation_by_Country.csv")
)

df_country_results |>
  select(-outcome.sd) |>
  select(-predictor.sd) |>
  mutate(
    group = str_remove(term, Variable),
    Variable = str_remove(Variable, "COV_"),
    Outcome = "Social Media Use (> 2hr/day)"
  ) |>
  select(Outcome, COUNTRY, Variable, group, nimp, estimate.pooled:wald.p.value)  |>
  rename(
    "Proportion" = "estimate.pooled",
    "std.error" = "se.pooled"
  ) |>
  mutate(
    Excluded_from_MetaAnalysis = ifelse(df.approx < 30, "*", " ")
  ) |>
  write_xlsx(file=here::here(res.dir, paste0("Supplemental File 3.xlsx")), overwrite = TRUE)

df_country_results <- readr::read_csv(here(res.dir, "results", "TIME_MEDIA_MY_Demographic_Variation_by_Country.csv"))
# identify failures:
df_country_results |>
  mutate(
    AnyNA = rowMeans(across(everything(), ~is.na(.)))
  ) |>
  filter(AnyNA > 0) |> View()

df_country_results |>
  filter(df.approx < 30) |> View()

# Meta-analytic summary
# filter out estimates where df (thus number of cases in cell less than 30)
fit.meta <- df_country_results |>
  filter(df.approx >= 30) |>
  mutate(df = df.approx) |>
  group_by(Variable,term) |>
  nest() |>
  gfs_meta_analysis(yi=estimate.pooled, sei=se.pooled, grpi = COUNTRY, pvalue=wald.p.value)
write_rds(
  fit.meta ,
  file = here(res.dir, "results", "TIME_MEDIA_MY_Demographic_Variation_Meta_Analytic_Estimates.rds")
)

## --- Table 3 ----

fit.meta|>
  mutate(
    k = map_dbl(data,~nrow(.))
  ) |> select(k) |> print(n=34)

ft <- fit.meta |>
  select(Variable,term,theta.rma,theta.rma.se,theta.lb,theta.ub,I2,tau,theta.pred.int.lb,theta.pred.int.ub) |>
  mutate(
    term = str_remove(term, Variable),
    theta.ci = paste0("(",.round(theta.lb,2,F),", ",.round(theta.ub,2,F),")" ),
    theta.pi = paste0("(",.round(theta.pred.int.lb,2,F),", ",.round(theta.pred.int.ub,2,F),")" ),
    I2 = ifelse(I2 == 0, "0.0%*", paste0(.round(I2,1,F),"%")),
    tau = ifelse(tau <0.001500001, "<0.001*", .round(tau, 3, F)),
    theta.rma = .round(theta.rma, 2, F),
    theta.rma.se = .round(theta.rma.se, 2, F)
  ) |>
  rowwise()|>
  mutate(
    Variable = get_outcome_better_name(str_remove(Variable,"COV_"), FALSE)
  ) |> ungroup() |>
  select(Variable, term, theta.rma, theta.rma.se, theta.ci, tau, theta.pi, I2) |>
  rename(
    "Prop." = "theta.rma",
    "SE" = "theta.rma.se",
    "95% CI" = "theta.ci",
    "Pred. Int." = "theta.pi",
    "Heterogeneity (tau)" = "tau"
  ) |>
  flextable() |>
  add_header_lines(
    "Table S6. Meta-analytic estimated subgroup means of >2hours/day of social media use."
  ) |>
  add_footer_lines(
    "Note. All Prop. (proportions) are the random-effects meta-analytic average proportion across countries; SE, standard error of the proportion; CI, confidence interval;  τ (tau, heterogeneity), estimated standard deviation of the distribution of group proportions; Pred. Int., a 95% prediction interval for estimated effect size for a new country, a prediction interval can have near zero-width when the estimate of heterogeneity (tau) is nearly zero, pulling the calibrated effect sizes to the meta-analytic average proportion; I2, the percent of variance in estimates not attributable to sampling variance.

*Estimate may not be stable due to lack of heterogeneity. Age group 80+ based on 10 estimates with group size > 30, gender--other estimate based on 7 estimates with group sizes > 30.
    "
  )

read_docx() |>
  body_add_flextable(ft)|>
  print(target = here(res.dir, "table_3.docx"))

# ----- Part 5. Outcome-wide analyses ----

# NOTE, need to use the "BIN_TIME_MEDIA" variable that forces the binary 0/1 needed for analysis
outcome.vec <- get_variable_codes(what = "VARIABLES.VEC") |> paste0("_Y3")
predictor.vec <- "BIN_TIME_MEDIA_MY"

pca.variables <- get_variable_codes(what = "VARS0") |> paste0("_Y1") # includes all outcomes/exposures assessed at wave 1 + other wave 1 specific variables
fixed.variables <- c(
  "MODE_ANNUAL", ## WILL ONLY BE INCLUDED IN COUNTRIES WHERE MODE VARIES
  "COV_AGE_GRP_Y1", "COV_GENDER", "COV_RACE_PLURALITY",
  "COV_EMPLOYMENT_Y1", "COV_MARITAL_STATUS_Y1",
  "COV_ATTEND_SVCS_Y1", "COV_BORN_COUNTRY_Y1", "COV_REL2_Y1",
  "COV_PARENTS_12YRS_Y1", "COV_MOTHER_RELATN_Y1", "COV_FATHER_RELATN_Y1",
  "COV_OUTSIDER_Y1", "COV_ABUSED_Y1","COV_HEALTH_GROWUP_Y1",
  "COV_INCOME_12YRS_Y1","COV_SVCS_12YRS_Y1",
  "COV_MOTHER_NA", "COV_FATHER_NA"
)
iterator.list <- expand.grid(outcome = outcome.vec, predictor = predictor.vec)
vec.x <- as.character(iterator.list$predictor)
vec.y <- as.character(iterator.list$outcome)
vec.iter <- seq_len(nrow(iterator.list))
# Y3(i) -> focal outcome (i) at wave 3
# Y1(i) -> focal outcome (i) at wave 1
# X2(j) -> focal exposure (j) at wave 2/midyear survey
# X0    -> set of constant predictors from wave 1
# Z(#)  -> principal components (#, number of retained components) from entered variables

# Focal Analysis: Y3 ~ X2 + X0 + Z(7)
plan("multisession", workers = 16)
with_progress({
  p <- progressor( along=vec.iter)
  furrr::future_pwalk(list(vec.iter, vec.x, vec.y), \(iter,x,y){
    load_packages()
    options(survey.lonely.psu = "certainty")
    gfs_wave_3_coordinated_analysis(
      your.outcome = y,
      your.pred = x,
      data.dir = paste0(data.dir, "/recoded"),
      direct.subset = expr(!is.na(RETENTION_WEIGHT_L3)),
      wgt = RETENTION_WEIGHT_L3,
      psu = PSU,
      strata = STRATA,
      covariates = fixed.variables,
      pca.variables = pca.variables,
      pc.rule = "constant",
      pc.cutoff = 7,
      res.dir = res.dir
    )
    p(sprintf("iter= %s", iter))
  }, .options = furrr_options(seed = TRUE))
})
future::resetWorkers(plan())

lst.results <- get_saved_results(
  res.dir = res.dir,
  outcomes =  outcome.vec,
  predictors = predictor.vec,
  what = "output"
)

meta.input <- lst.results %>%
  bind_rows() %>%
  filter(term %in% predictor.vec) |>
  filter(df > 100) |>
  group_by(outcome, term) %>%
  nest()

## Primary result - standardized effect sizes
meta.results <- gfs_meta_analysis( meta.input, grpi = COUNTRY, yi = std.est, sei = std.se, estimator = "PM")
readr::write_rds( meta.results, file = here::here(res.dir, paste0("TIME_MEDIA_MY_OutcomeWide_Meta_Analytic_Estimates.rds")), compress = "gz")

# ## Supplemental result - UNstandardized effect sizes
# meta.results <- gfs_meta_analysis( meta.input, grpi = COUNTRY, yi = est, sei = se )
# readr::write_rds(
#   meta.results,
#   file = here::here(res.dir, paste0("0_meta_analyzed_results_primary_unstd.rds")),
#   compress = "gz"
# ) # res.dir, paste0("0 Country Specific Results.xlsx")

# Country-specific equivalence test + printed-out results
# Equivalence test of standardized effect sizes
# Continuous outcomes --
#   H0: beta0 < -0.10 U beta0 >  0.10
# Binary outcomes --
#   H0: RR > 0.90  U RR < 1.10
#   H0: log(RR) < -0.1053 U log(RR) >  0.0953

## ---- Table S7 ----
lst.results %>%
  bind_rows() %>%
  filter(term %in% predictor.vec) |>
  select(-estimates.by.imp) |>
  rowwise() |>
  mutate(
    predictor.better.name = "Social Media Time >2 hours/day", #get_outcome_better_name(term,include.name = FALSE),
    outcome.better.name = get_outcome_better_name(outcome,include.name = FALSE),
    predictor.grp = "Social Media Use", #get_variable_group_labels(term),
    outcome.grp = get_variable_group_labels(outcome),
    space = " ", space2 = " ",
    equiv.test.t.above = case_when(
      outcome.scale == "bin" ~ (std.est - 0.0953)/std.se,
      outcome.scale != "bin" ~ (std.est - 0.10)/std.se
    ),
    equiv.test.t.below = case_when(
      outcome.scale == "bin" ~ (std.est + 0.1053)/std.se,
      outcome.scale != "bin" ~ (std.est + 0.10)/std.se
    ),
    equiv.test.p.value = max( pt(equiv.test.t.above, df=df, lower.tail = T),
                              pt(equiv.test.t.below, df=df, lower.tail = F))
  ) |> ungroup() |>
  # filter(!is.na(predictor.grp)) |>
  filter(!is.na(outcome.grp)) |>
  mutate(
    equiv.test.q.value.fdr = p.adjust(equiv.test.p.value, "fdr")
  ) |>
  select(COUNTRY, outcome.grp, outcome.better.name, outcome, predictor.grp, predictor.better.name,
         term,  std.est, std.se, std.ci.lb, std.ci.ub, EE, ECI, space, est, se, df,
         t.statistic, f.statistic, ci.lb, ci.ub, p.value, miss.info, outcome.sd,
         predictor.sd, space2, equiv.test.t.above, equiv.test.t.below, equiv.test.p.value, equiv.test.q.value.fdr) |>
  rename(
    Country = "COUNTRY",
    `Evalue for Estimate` = EE,
    `Evalue for Confidence Limit` = ECI
  ) |>
  write_xlsx(file=here::here(res.dir, paste0("Table_S3.xlsx")), overwrite = TRUE)

my.fn <- "
Note. 23 country-specific estimates pooled for each row; N(total)=89,488 across all countryies. ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00; CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country, A prediction interval can have near zero-width when the estimate of heterogeneity (tau) is nearly zero, pulling the calibrated effect sizes to the meta-analytic mean effect size; Metric (%<lb | %>ub), percent of effect sizes below a lower bound (<lb) and above an upper bound (>ub), for ES, the bounds are lb=-0.10, ub=0.10, and for RR, the bounds are lb=0.90 and ub=1.10; τ (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; Equivalence test, the p-value for a test of the null hypothesize that the effect is meaningful,for ES, the null is H0: (ES<-0.10 or ES>0.10), and for RR, H0: (RR<0.90 or RR>1.10).

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.

A semi-complete-case analyses was conducted by only using the participants who particpated in Wave 1, the mid-year survey, and Wave 3. Remaining within participate missingness was imputed with 20 imputed datasets. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

†Estimate of τ (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.
"

my.vars <- Rglobalflourishing:::get_defaults_w3(list(study="outcome"),"supp")
meta.results.sub <- meta.results |>
  mutate(
    outcome0 = factor(outcome, levels=unique(my.vars$tbl.row.vec), ordered=TRUE),
    outcome.scale = get_outcome_scale(outcome),
    equiv.test.t.above = case_when(
      outcome.scale != "cont" ~ (theta.rma - 0.0953)/theta.rma.se,
      outcome.scale == "cont" ~ (theta.rma - 0.10)/theta.rma.se
    ),
    equiv.test.t.below = case_when(
      outcome.scale != "cont" ~ (theta.rma + 0.1053)/theta.rma.se,
      outcome.scale == "cont" ~ (theta.rma + 0.10)/theta.rma.se
    ),
    equiv.test.p.value = max( pnorm(equiv.test.t.above, lower.tail = T),
                              pnorm(equiv.test.t.below, lower.tail = F))
    ) |> ungroup() |>
  mutate(
    equiv.test.q.value.fdr = format.pval(p.adjust(equiv.test.p.value, "fdr"),digits=3),
    equiv.test.p.value = format.pval(equiv.test.p.value,digits=3),
    ES = ifelse(outcome.scale == "cont", .round(theta.rma, 3, F), " "),
    RR = ifelse(outcome.scale != "cont", .round(exp(theta.rma), 3, F),  " "),
    ci = ifelse(outcome.scale != "cont",
                paste0("(", .round(exp(theta.lb),3,F), ", ", .round(exp(theta.ub), 3, F),")"),
                paste0("(", .round(theta.lb,3,F), ", ", .round(theta.ub, 3, F),")")),
    pi = ifelse(outcome.scale != "cont",
                paste0("(", .round(exp(theta.pred.int.lb),3,F), ", ", .round(exp(theta.pred.int.ub), 3, F),")"),
                paste0("(", .round(theta.pred.int.lb,3,F), ", ", .round(theta.pred.int.ub, 3, F),")")),
    prop.metric = ifelse(outcome.scale != "cont",
                         paste0(.round(rr.prob.0.90*100,1,F), "% | ", .round(rr.prob.1.10*100, 1, F), "%"),
                         paste0(.round(prob.leqneq0.1*100,1,F), "% | ", .round(prob.geq0.1*100, 1, F), "%")),
    tau = ifelse(tau < 0.0015, "< 0.001†", .round(tau, 3, F)),
    global.pvalue = format.pval(global.pvalue,digits=3, small.interval=3L, drop0trailing=F, nsmall=3)
  ) |>
  rowwise() |>
  mutate(
    outcome.bn = get_outcome_better_name(outcome, F)
  ) |> ungroup()


# main text
## ---- Table 4 ----
my.vars <- Rglobalflourishing:::get_defaults_w3(list(study="outcome"),"main")
tb <- meta.results.sub|>
  filter(outcome %in% my.vars$tbl.row.vec) |>
  mutate() |>
  arrange(outcome0 )|>
  select(outcome.bn, ES, RR, ci, prop.metric, pi, tau, global.pvalue, equiv.test.p.value, equiv.test.q.value.fdr) |>
  mutate( "\r" = " ", .after=8 ) |>
  rename(
    "Outcome" = "outcome.bn",
    "95% CI" = "ci",
    "Prop. Metric" = "prop.metric",
    "Pred. Int." = "pi",
    "Heterogeneity (tau)" =  "tau",
    "Global p-value" = "global.pvalue",
    "Equiv. Test p-value" = "equiv.test.p.value"
    ,"FDR p-value" = "equiv.test.q.value.fdr"
  )

insertRow <- function(data, new_row, r) {
  data_new <- rbind(data[1:r, ],
                    new_row,
                    data[- (1:r), ])
  rownames(data_new) <- 1:nrow(data_new)
  return(data_new)
}
tb0 <- tb
blank.row <- tb[1,,drop=FALSE]
blank.row[,] <- " "
my.rows <- which(my.vars$tbl.row.vec == "blank_Y3")
for(i in 1:length(my.vars$mylabels)){
  blank.row$Outcome <- my.vars$mylabels[i]
  if( my.rows[i]-1 == 0){
    tb0 <- insertRow( blank.row, tb0, 1)
  } else {
    tb0 <- insertRow(tb0, blank.row, my.rows[i]-1)
  }

}
ft <- tb0 |>
  flextable() |>
  add_header_row(values = c("", "Heterogeneity Metrics","", "Equivalence Test"),
                 colwidths = c(4, 4,1, 2)) |>
  add_header_lines(
    "Table 4. Meta-analyzed cross-national associations between social media use (“>2 hours/day” versus “≤2 hours/day or none”) and subsequent well-being and related outcomes."
  ) |>
  add_footer_lines(my.fn) |>
  theme_apa() |>
  align(align = "right", part = "all") |>
  align(j = 1, align = "left", part = "all")  |>
  border_remove() |>
  hline_bottom(part = "body") |>
  hline_bottom(part = "header") |>
  hline(i = 1, part = "header") |>
  hline(i = 2, j=5:8, part = "header")|>
  hline(i = 2, j=10:11, part = "header")|>
  italic(i = my.rows)
ft

read_docx() |>
  body_add_flextable(ft)|>
  print(target = here(res.dir, "table_4.docx"))


## ---- Table S8 ----
## Expanded supplemental table
my.vars <- Rglobalflourishing:::get_defaults_w3(list(study="outcome"),"supp")

my.fn  <- paste0(
  my.fn ,
  "The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates."
)
tb <- meta.results.sub |>
  filter(outcome %in% my.vars$tbl.row.vec) |>
  mutate() |>
  arrange(outcome0 )|>
  mutate(
    theta.rma.EE = .round(theta.rma.EE, 2, F),
    theta.rma.ECI = .round(theta.rma.ECI, 2, F),
    sens.reliability = map(sens.reliability, \(x){
      x |>
        mutate(
          out = paste0(.round(corrected.theta, 3, F), " (",.round(corrected.theta.lb,3,T),", ", .round(corrected.theta.ub, 3, T),")")
        )
    }),
    rcor70 = map_chr(sens.reliability,\(x){
      x |> filter(reliability == 0.7) |> select(out) |> as.character()
    }),
    rcor55 = map_chr(sens.reliability,\(x){
      x |> filter(reliability == 0.55) |> select(out) |> as.character()
    }),
    rcor40 = map_chr(sens.reliability, \(x){
      x |> filter(reliability == 0.40) |> select(out) |> as.character()
    })
  ) |>
  select(outcome.bn, ES, RR, ci, prop.metric, pi, tau, global.pvalue,
         equiv.test.p.value, equiv.test.q.value.fdr, theta.rma.EE, theta.rma.ECI,
         rcor70, rcor55, rcor40
  ) |>
  mutate( "\r" = " ", .after=8 ) |>
  mutate( "\r\r" = " ", .after=11 ) |>
  mutate( "\r\r\r" = " ", .after=14 ) |>
  rename(
    "Outcome" = "outcome.bn",
    "95% CI" = "ci",
    "Prop. Metric" = "prop.metric",
    "Pred. Int." = "pi",
    "Heterogeneity (tau)" =  "tau",
    "Global p-value" = "global.pvalue",
    "Equiv. Test p-value" = "equiv.test.p.value"
    ,"FDR p-value" = "equiv.test.q.value.fdr",
    "E-values for Estimate" = "theta.rma.EE",
    "E-values for Confidence Limit" = "theta.rma.ECI",
    "High (r=0.70)" = "rcor70",
    "Moderate (r=0.55)" = "rcor55",
    "Low (r=0.40)" = "rcor40"
  )

tb0 <- tb
blank.row <- tb[1,,drop=FALSE]
blank.row[,] <- " "
my.rows <- which(my.vars$tbl.row.vec == "blank_Y3")
for(i in 1:length(my.vars$mylabels)){
  blank.row$Outcome <- my.vars$mylabels[i]
  if( my.rows[i]-1 == 0){
    tb0 <- insertRow( blank.row, tb0, 1)
  } else {
    tb0 <- insertRow(tb0, blank.row, my.rows[i]-1)
  }

}

ft <- tb0 |>
  flextable() |>
  add_header_row(values = c("", "Heterogeneity Metrics", "", "Equivalence Tests for Meta-analytic Effect", "", "Sensitivity to Unmeasured Confounding","", "Reliability Corrected Est. (95% CI)"),
                 colwidths = c(4, 4, 1, 2, 1, 2, 1, 3)) |>
  add_header_lines(
    "Table S4. Supplemental meta-analytic results"
  ) |>
  add_footer_lines(my.fn) |>
  theme_apa() |>
  align(align = "right", part = "all") |>
  align(j = 1, align = "left", part = "all")  |>
  align(i = 1, align = "center", part = "header")  |>
  border_remove() |>
  hline_bottom(part = "body") |>
  hline_bottom(part = "header") |>
  hline(i = 1, part = "header") |>
  hline(i = 2, j=c(5:8, 10:11, 13:14, 16:17), part = "header")|>
  italic(i = my.rows)
ft
Rglobalflourishing::gfs_append_to_xlsx(
  file = here(res.dir, "Table_S4.xlsx"),
  ft = ft,
  tb = "Table S4. Supplemental meta-analytic results"
)


## ---- Supplemental analysis: explicit outcome control -----

outcome.vec <- get_variable_codes(what = "VARIABLES.VEC") |> paste0("_Y3")
predictor.vec <- "BIN_TIME_MEDIA_MY"

pca.variables <- get_variable_codes(what = "VARS0") |> paste0("_Y1") # includes all outcomes/exposures assessed at wave 1 + other wave 1 specific variables
fixed.variables <- c(
  "MODE_ANNUAL", ## WILL ONLY BE INCLUDED IN COUNTRIES WHERE MODE VARIES
  "COV_AGE_GRP_Y1", "COV_GENDER", "COV_RACE_PLURALITY",
  "COV_EMPLOYMENT_Y1", "COV_MARITAL_STATUS_Y1",
  "COV_ATTEND_SVCS_Y1", "COV_BORN_COUNTRY_Y1", "COV_REL2_Y1",
  "COV_PARENTS_12YRS_Y1", "COV_MOTHER_RELATN_Y1", "COV_FATHER_RELATN_Y1",
  "COV_OUTSIDER_Y1", "COV_ABUSED_Y1","COV_HEALTH_GROWUP_Y1",
  "COV_INCOME_12YRS_Y1","COV_SVCS_12YRS_Y1",
  "COV_MOTHER_NA", "COV_FATHER_NA"
)
iterator.list <- expand.grid(outcome = outcome.vec, predictor = predictor.vec)
vec.x <- as.character(iterator.list$predictor)
vec.y <- as.character(iterator.list$outcome)
vec.iter <- seq_len(nrow(iterator.list))

# Focal Analysis: Y3 ~ X2 + X0 + Y1 + Z(7)
plan("multisession", workers = 16)
with_progress({
  p <- progressor( along=vec.iter)
  furrr::future_pwalk(list(vec.iter, vec.x, vec.y), \(iter,x,y){
    load_packages()
    options(survey.lonely.psu = "certainty")

    y1 <- y |> str_remove("_Y3") |> paste0("_Y1")
    y1 <- check_wave1_overlap(y1, fixed.variables)


    gfs_wave_3_coordinated_analysis(
      your.outcome = y,
      your.pred = x,
      data.dir = paste0(data.dir, "/recoded"),
      direct.subset = expr(!is.na(RETENTION_WEIGHT_L3)),
      wgt = RETENTION_WEIGHT_L3,
      psu = PSU,
      strata = STRATA,
      covariates = c(y1, fixed.variables),
      pca.variables = pca.variables[!(pca.variables %in% c(y1))],
      pc.rule = "constant",
      pc.cutoff = 7,
      res.dir = res.dir,
      appnd.txt.to.filename = "_supp_mi_mod2"
    )
    p(sprintf("iter= %s", iter))
    #})
  },.options = furrr_options(seed = TRUE))
})
future::resetWorkers(plan())

lst.results <- get_saved_results(
  res.dir = res.dir,
  outcomes =  outcome.vec,
  predictors = predictor.vec,
  appnd.txt = "_supp_mi_mod2",
  what = "output"
)

meta.input <- lst.results %>%
  bind_rows() %>%
  filter(term %in% predictor.vec) |>
  filter(df > 100) |>
  group_by(outcome, term) %>%
  nest()

## Primary result - standardized effect sizes
meta.results <- gfs_meta_analysis( meta.input, grpi = COUNTRY, yi = std.est, sei = std.se, estimator = "PM")
readr::write_rds( meta.results, file = here::here(res.dir, paste0("TIME_MEDIA_MY_OutcomeWide_Meta_Analytic_Estimates_Model2.rds")), compress = "gz")


### ---- Too add to Table S8 ----
lst.results %>%
  bind_rows() %>%
  filter(term %in% predictor.vec) |>
  select(-estimates.by.imp) |>
  rowwise() |>
  mutate(
    predictor.better.name = "Social Media Time >2 hours/day", #get_outcome_better_name(term,include.name = FALSE),
    outcome.better.name = get_outcome_better_name(outcome,include.name = FALSE),
    predictor.grp = "Social Media Use", #get_variable_group_labels(term),
    outcome.grp = get_variable_group_labels(outcome),
    space = " ", space2 = " ",
    equiv.test.t.above = case_when(
      outcome.scale == "bin" ~ (std.est - 0.0953)/std.se,
      outcome.scale != "bin" ~ (std.est - 0.10)/std.se
    ),
    equiv.test.t.below = case_when(
      outcome.scale == "bin" ~ (std.est + 0.1053)/std.se,
      outcome.scale != "bin" ~ (std.est + 0.10)/std.se
    ),
    equiv.test.p.value = max( pt(equiv.test.t.above, df=df, lower.tail = T),
                              pt(equiv.test.t.below, df=df, lower.tail = F))
  ) |> ungroup() |>
  # filter(!is.na(predictor.grp)) |>
  filter(!is.na(outcome.grp)) |>
  mutate(
    equiv.test.q.value.fdr = p.adjust(equiv.test.p.value, "fdr")
  ) |>
  select(COUNTRY, outcome.grp, outcome.better.name, outcome, predictor.grp, predictor.better.name,
         term,  std.est, std.se, std.ci.lb, std.ci.ub, EE, ECI, space, est, se, df,
         t.statistic, f.statistic, ci.lb, ci.ub, p.value, miss.info, outcome.sd,
         predictor.sd, space2, equiv.test.t.above, equiv.test.t.below, equiv.test.p.value, equiv.test.q.value.fdr) |>
  rename(
    Country = "COUNTRY",
    `Evalue for Estimate` = EE,
    `Evalue for Confidence Limit` = ECI
  ) |>
  write_xlsx(file=here::here(res.dir, paste0("Table_S5.xlsx")), overwrite = TRUE)

## Expanded supplemental table
my.vars <- Rglobalflourishing:::get_defaults_w3(list(study="outcome"),"supp")
my.fn <- "
Note. 23 country-specific estimates pooled for each row; N(total)=89,488 across all countryies. ES, effect size measure for standardized regression coefficient, null effect is 0.00; RR, risk-ratio, null effect is 1.00; CI, confidence interval; Pred. Int., a 95% prediction interval for estimated effect size for a new country, A prediction interval can have near zero-width when the estimate of heterogeneity (tau) is nearly zero, pulling the calibrated effect sizes to the meta-analytic mean effect size; Metric (%<lb | %>ub), percent of effect sizes below a lower bound (<lb) and above an upper bound (>ub), for ES, the bounds are lb=-0.10, ub=0.10, and for RR, the bounds are lb=0.90 and ub=1.10; τ (tau, heterogeneity), estimated standard deviation of the distribution of effects; Global p-value, joint test of the null hypothesis that the country-specific Wald tests are null in all countries; Equivalence test, the p-value for a test of the null hypothesize that the effect is meaningful,for ES, the null is H0: (ES<-0.10 or ES>0.10), and for RR, H0: (RR<0.90 or RR>1.10).

An outcome-wide analytic approach was used, and a separate model was run for each outcome. A different type of model was run depending on the nature of the outcome: (1) for each binary outcome, a weighted generalized linear model (with a log link and Poisson distribution) was used to estimate a RR; and (2) for each continuous outcome, a weighted linear regression model was used to estimate an ES. All effect sizes were standardized.

A semi-complete-case analyses was conducted by only using the participants who particpated in Wave 1, the mid-year survey, and Wave 3. Remaining within participate missingness was imputed with 20 imputed datasets. All analyses controlled for sociodemographic and childhood factors assessed at Wave 1: relationship with mother growing up; relationship with father growing up; parent marital status around age 12; experienced abuse growing up (except for Israel); felt like an outsider in family growing up; self-rated health growing up; subjective financial status growing up; frequency of religious service attendance around age 12; year of birth; gender; education, employment status, marital status, immigration status; religious affiliation; frequency of religious service attendance; racial/ethnic identity when available; and the first seven principal components of the entire set of potential confounders assessed at Wave 1.

†Estimate of τ (tau, heterogeneity) is likely unstable. See our online supplement forest plots for more detail on heterogeneity of effects. Line-printer style abbreviations for small p-values (e.g., '2.22e-16') are used to help conserve space, given the table and font size, to aid in readability.

The formula for calculating E-values can be found in VanderWeele and Ding (2017). E-values for estimate are the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to fully explain away the observed association between the exposure and outcome, conditional on the measured covariates. E-values for the 95% CI closest to the null denote the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome to shift the CI to include the null value, conditional on the measured covariates.
"

my.vars <- Rglobalflourishing:::get_defaults_w3(list(study="outcome"),"supp")
meta.results.sub <- meta.results |>
  mutate(
    outcome0 = factor(outcome, levels=unique(my.vars$tbl.row.vec), ordered=TRUE),
    outcome.scale = get_outcome_scale(outcome),
    equiv.test.t.above = case_when(
      outcome.scale != "cont" ~ (theta.rma - 0.0953)/theta.rma.se,
      outcome.scale == "cont" ~ (theta.rma - 0.10)/theta.rma.se
    ),
    equiv.test.t.below = case_when(
      outcome.scale != "cont" ~ (theta.rma + 0.1053)/theta.rma.se,
      outcome.scale == "cont" ~ (theta.rma + 0.10)/theta.rma.se
    ),
    equiv.test.p.value = max( pnorm(equiv.test.t.above, lower.tail = T),
                              pnorm(equiv.test.t.below, lower.tail = F))
  ) |> ungroup() |>
  mutate(
    equiv.test.q.value.fdr = format.pval(p.adjust(equiv.test.p.value, "fdr"),digits=3),
    equiv.test.p.value = format.pval(equiv.test.p.value,digits=3),
    ES = ifelse(outcome.scale == "cont", .round(theta.rma, 3, F), " "),
    RR = ifelse(outcome.scale != "cont", .round(exp(theta.rma), 3, F),  " "),
    ci = ifelse(outcome.scale != "cont",
                paste0("(", .round(exp(theta.lb),3,F), ", ", .round(exp(theta.ub), 3, F),")"),
                paste0("(", .round(theta.lb,3,F), ", ", .round(theta.ub, 3, F),")")),
    pi = ifelse(outcome.scale != "cont",
                paste0("(", .round(exp(theta.pred.int.lb),3,F), ", ", .round(exp(theta.pred.int.ub), 3, F),")"),
                paste0("(", .round(theta.pred.int.lb,3,F), ", ", .round(theta.pred.int.ub, 3, F),")")),
    prop.metric = ifelse(outcome.scale != "cont",
                         paste0(.round(rr.prob.0.90*100,1,F), "% | ", .round(rr.prob.1.10*100, 1, F), "%"),
                         paste0(.round(prob.leqneq0.1*100,1,F), "% | ", .round(prob.geq0.1*100, 1, F), "%")),
    tau = ifelse(tau < 0.0015, "< 0.001†", .round(tau, 3, F)),
    global.pvalue = format.pval(global.pvalue,digits=3, small.interval=3L, drop0trailing=F, nsmall=3)
  ) |>
  rowwise() |>
  mutate(
    outcome.bn = get_outcome_better_name(outcome, F)
  ) |> ungroup()

tb <- meta.results.sub |>
  filter(outcome %in% my.vars$tbl.row.vec) |>
  mutate() |>
  arrange(outcome0 )|>
  mutate(
    theta.rma.EE = .round(theta.rma.EE, 2, F),
    theta.rma.ECI = .round(theta.rma.ECI, 2, F),
    sens.reliability = map(sens.reliability, \(x){
      x |>
        mutate(
          out = paste0(.round(corrected.theta, 3, F), " (",.round(corrected.theta.lb,3,T),", ", .round(corrected.theta.ub, 3, T),")")
        )
    }),
    rcor70 = map_chr(sens.reliability,\(x){
      x |> filter(reliability == 0.7) |> select(out) |> as.character()
    }),
    rcor55 = map_chr(sens.reliability,\(x){
      x |> filter(reliability == 0.55) |> select(out) |> as.character()
    }),
    rcor40 = map_chr(sens.reliability, \(x){
      x |> filter(reliability == 0.40) |> select(out) |> as.character()
    })
  ) |>
  select(outcome.bn, ES, RR, ci, prop.metric, pi, tau, global.pvalue,
         equiv.test.p.value, equiv.test.q.value.fdr, theta.rma.EE, theta.rma.ECI,
         rcor70, rcor55, rcor40
  ) |>
  mutate( "\r" = " ", .after=8 ) |>
  mutate( "\r\r" = " ", .after=11 ) |>
  mutate( "\r\r\r" = " ", .after=14 ) |>
  rename(
    "Outcome" = "outcome.bn",
    "95% CI" = "ci",
    "Prop. Metric" = "prop.metric",
    "Pred. Int." = "pi",
    "Heterogeneity (tau)" =  "tau",
    "Global p-value" = "global.pvalue",
    "Equiv. Test p-value" = "equiv.test.p.value"
    ,"FDR p-value" = "equiv.test.q.value.fdr",
    "E-values for Estimate" = "theta.rma.EE",
    "E-values for Confidence Limit" = "theta.rma.ECI",
    "High (r=0.70)" = "rcor70",
    "Moderate (r=0.55)" = "rcor55",
    "Low (r=0.40)" = "rcor40"
  )

tb0 <- tb
blank.row <- tb[1,,drop=FALSE]
blank.row[,] <- " "
my.rows <- which(my.vars$tbl.row.vec == "blank_Y3")
for(i in 1:length(my.vars$mylabels)){
  blank.row$Outcome <- my.vars$mylabels[i]
  if( my.rows[i]-1 == 0){
    tb0 <- insertRow( blank.row, tb0, 1)
  } else {
    tb0 <- insertRow(tb0, blank.row, my.rows[i]-1)
  }

}

ft <- tb0 |>
  flextable() |>
  add_header_row(values = c("", "Heterogeneity Metrics", "", "Equivalence Tests for Meta-analytic Effect", "", "Sensitivity to Unmeasured Confounding","", "Reliability Corrected Est. (95% CI)"),
                 colwidths = c(4, 4, 1, 2, 1, 2, 1, 3)) |>
  add_header_lines(
    "Table S4. Supplemental meta-analytic results"
  ) |>
  add_footer_lines(my.fn) |>
  theme_apa() |>
  align(align = "right", part = "all") |>
  align(j = 1, align = "left", part = "all")  |>
  align(i = 1, align = "center", part = "header")  |>
  border_remove() |>
  hline_bottom(part = "body") |>
  hline_bottom(part = "header") |>
  hline(i = 1, part = "header") |>
  hline(i = 2, j=c(5:8, 10:11, 13:14, 16:17), part = "header")|>
  italic(i = my.rows)
ft
Rglobalflourishing::gfs_append_to_xlsx(
  file = here(res.dir, "Table_S6.xlsx"),
  ft = ft,
  tb = "Table S6. Supplemental meta-analytic results - Explicit control of Wave 1 value of each outcome"
)

# ---- Part 6. Forest plots ----

plot.dat <- read_rds(
  here::here(res.dir, "results", paste0("0_meta_analyzed_results_primary.rds"))
)

# SFI, self-rated mental health, depression composite, having a close friend, loneliness, and financial security)

library(patchwork)
my.vars <- c("COMPOSITE_FLOURISHING_SECURE_Y3",  "LONELY_Y3", "WB_FIVEYRS_Y3") #c("SHOW_LOVE_Y3")

make_plot <- function(data, var){
  data |>
    select(outcome, data, theta.rma, theta.lb, theta.ub) |>
    filter(outcome %in% c(var)) |>
    unnest(c(data)) |>
    select(outcome,yi:theta.ub) |>
    rowwise()|>
    mutate(
      outcome.bn = get_outcome_better_name(outcome, F)
    ) |> ungroup() |>
    mutate(
      ni=1:n(),
      meta.lb = paste0(.round(theta.rma, 2, F) ),
      coun.lb = paste0("(",.round(ci.lb.i, 2, F), ", ", .round(ci.ub.i, 2, F),")")
    ) |>
    ggplot(aes(x=yi, y=reorder(group, yi))) +
    geom_point() +
    geom_segment(
      aes(x=ci.lb.i, xend=ci.ub.i)
    ) +
    geom_vline(aes(xintercept = theta.rma), linetype="dashed", color="grey20")+
    #geom_text(aes(x = -0.3, label = coun.lb), color="grey20", size=4.5) +
    lims(x = c(-0.3,0.3)) +
    labs(x="Standardized ES", y=NULL, title=NULL, subtitle=NULL) +
    facet_wrap(.~outcome.bn) +
    theme_Publication() +
    theme(plot.margin = unit(c(2, 2, 2, 2), "mm"))
}

p <- lapply(my.vars, \(x) make_plot(plot.dat, x))
p0 <- p[[1]] + p[[2]] + p[[3]]
p0
ggsave(here(res.dir, "figure_1.pdf"), p0, units="in", width=14, height=5)



my.vars <- c("COMPOSITE_DEPRESSION_Y3", "COMPOSITE_ANXIETY_Y3", "DONATED_Y3") #c()

make_plot <- function(data, var){
  data |>
    select(outcome, data, theta.rma, theta.lb, theta.ub) |>
    filter(outcome %in% c(var)) |>
    unnest(c(data)) |>
    select(outcome,yi:theta.ub) |>
    rowwise()|>
    mutate(
      outcome.bn = get_outcome_better_name(outcome, F)
    ) |> ungroup() |>
    mutate(
      ni=1:n(),
      meta.lb = paste0(.round(theta.rma, 2, F) ),
      coun.lb = paste0("(",.round(ci.lb.i, 2, F), ", ", .round(ci.ub.i, 2, F),")")
    ) |>
    ggplot(aes(x=yi, y=reorder(group, yi))) +
    geom_point() +
    geom_segment(
      aes(x=ci.lb.i, xend=ci.ub.i)
    ) +
    geom_vline(aes(xintercept = theta.rma), linetype="dashed", color="grey20")+
    #geom_text(aes(x = -0.3, label = coun.lb), color="grey20", size=4.5) +
   # lims(x = c(-0.5,0.5)) +
    labs(x="log(Risk-Ratio)", y=NULL, title=NULL, subtitle=NULL) +
    facet_wrap(.~outcome.bn) +
    theme_Publication() +
    theme(plot.margin = unit(c(2, 2, 2, 2), "mm"))
}

p <- lapply(my.vars, \(x) make_plot(plot.dat, x))
p0 <- p[[1]] + p[[2]] + p[[3]]
p0
ggsave(here(res.dir, "figure_2.pdf"), p0, units="in", width=14, height=5)


##---- Figure S2 -----

# Heterogeneity within and across countries

p <- plot.dat |>
  select(outcome, data, theta.rma, theta.lb, theta.ub) |>
  unnest(c(data)) |>
  filter(df > 100, yi > -50) |>
  select(outcome,yi:theta.ub) |>
  group_by(group) |>
  mutate(
    median_es = median(yi, na.rm=TRUE)
  ) |>
  ggplot(aes(x=yi, y=reorder(group, median_es))) +
  geom_boxplot(outliers = F) +
  #geom_point(alpha = 0.15, position=position_jitter(width = 0, height=0.1, seed=1)) +
  #geom_text(aes(x = -0.3, label = coun.lb), color="grey20", size=4.5) +
  # lims(x = c(-0.5,0.5)) +
  labs(x="ES/log(RR) Estimate", y=NULL, title=NULL,
       subtitle="Distribution of estimated effects") +
  theme_Publication() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm"))
p
ggsave(here(res.dir, "figure_S1.pdf"), p, units="in", width=5, height=6)



##---- Figure S1 -----
#3 violin plot of survey times

plot_dat <- df.raw |>
  select(COUNTRY, DOI_ANNUAL_Y1, DOI_MY, DOI_ANNUAL_Y3) |>
  pivot_longer(
    cols = DOI_ANNUAL_Y1:DOI_ANNUAL_Y3,
    names_to = "year",
    values_to = "DOI"
  ) |>
  mutate(
    year = case_when(
      year == "DOI_ANNUAL_Y1" ~ "Wave 1",
      year == "DOI_MY" ~ "Mid-year",
      year == "DOI_ANNUAL_Y3" ~ "Wave 3"
    ),
    year = factor(year, levels=c("Wave 1", "Mid-year", "Wave 3"), ordered=TRUE),
    grp = paste0(COUNTRY,"_", year)
  )
scale.fill <- c("Wave 1"="#d35400", "Mid-year"="#85c2f9", "Wave 3"="#feaaf1")
p <- plot_dat |>
  ggplot(aes(y = COUNTRY, x = DOI, group = grp, fill=year)) +
    geom_violin(scale="width", "position"="identity") +
    scale_fill_manual(values=scale.fill)+
    labs(x="Dates of data collection", y = NULL)+
    theme_Publication() +
    theme(
      plot.margin = unit(c(2, 2, 2, 2), "mm")
    )
p
ggsave(here("results-timemedia", "figure_S1.pdf"), p, units="in", width=10, height=8)
