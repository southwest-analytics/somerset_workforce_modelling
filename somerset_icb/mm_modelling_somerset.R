# 0. Load libraries and declare functions ----
# ════════════════════════════════════════════
library(tidyverse)
library(readxl)

if(tolower(basename(getwd()))!="somerset_icb")
  setwd("./somerset_icb")
source("../mm_modelling.R")

# 1. Load data ----
# ═════════════════

QOF <- FALSE

if(QOF){
  filename <- "./input/qof_input.xlsx"
} else {
  filename <- "./input/gbd_input.xlsx"
}
  
popn_sheet <- "POPN"
popn_growth_sheet <- "GROWTH"
prev_sheet <- "PREV"
odds_ratio_sheet <- "OR"

# • 1.1. Load population data ----
# ────────────────────────────────

# The population data should consist of IDX and SIZE fields. The IDX field 
# should uniquely identify the strata of the population and the SIZE field 
# should give the number of people in that strata.

# For example:

# ┌───────────┬──────┐
# │ IDX       │ SIZE │
# ╞═══════════╧══════╡
# │ M|00-04   │  714 │
# ├───────────┼──────┤
# │ ...       │  ... │
# ├───────────┼──────┤
# │ F|90_PLUS │  355 │
# └───────────┴──────┘

# The IDX of "M|00_04" would identify the male population aged 0 to 4 years
# and the SIZE of 714 shows the number of people in that strata, and the IDX 
# of "F|90_PLUS" would identify the female population aged 90 years 
# and over and the SIZE of 355 shows the number of people in that group.

df_popn <- readxl::read_xlsx(path = filename, sheet = popn_sheet)

# • 1.2. Load population growth data ----
# ───────────────────────────────────────

# The population growth data should consist of IDX, YEAR and PCT_GROWTH 
# fields. The IDX field should uniquely identify the strata of the population 
# and should match that of the population data, the YEAR should identify the 
# year that this growth figure relates to, and the PCT_GROWTH field should give
# the percentage growth from the baseline year.

# For example:

# ┌───────────┬──────┬────────────┐
# │ IDX       │ YEAR │ PCT_GROWTH │
# ╞═══════════╧══════╧════════════╡
# │ M|00-04   │ 2025 │      0.000 │
# ├───────────┼──────┼────────────┤
# │ M|00-04   │ 2026 │     -0.022 │
# ├───────────┼──────┼────────────┤
# │ M|00-04   │ 2027 │     -0.034 │
# ├───────────┼──────┼────────────┤
# │ ...       │  ... │        ... │
# ├───────────┼──────┼────────────┤
# │ F|90_PLUS │ 2025 │      0.000 │
# ├───────────┼──────┼────────────┤
# │ F|90_PLUS │ 2026 │      0.013 │
# ├───────────┼──────┼────────────┤
# │ F|90_PLUS │ 2027 │      0.040 │
# └───────────┴──────┴────────────┘

# The IDX of "M|00_04" would identify the male population aged 0 to 4 years
# and the YEAR of 2027 indicates the population growth for the year 2027, and 
# the PCT_GROWTH of -0.034 shows there is a forecast reduction in the number of 
# people in this group of -3.4% from the baseline year 2025. Conversely the IDX 
# of "F|90_PLUS" would identify the female population aged 90 years and above 
# and the YEAR of 2027 again indicates the population growth for the year 2027, 
# and the PCT_GROWTH of 0.040 shows there is a forecast increase in the number 
# of people in this group of +4.0% from the baseline year 2025.

df_popn_growth <- readxl::read_xlsx(path = filename, sheet = popn_growth_sheet)

# • 1.3. Load prevalence data ----
# ────────────────────────────────

# The prevalence data should consist of IDX, COND and PREV fields. The IDX 
# field should uniquely identify the strata of the population and should match 
# that of the population data, the COND should identify the condition for which
# the PREV fields gives the prevalence for.

# For example:

# ┌───────────┬────────┬───────┐
# │ IDX       │ COND   │ PREV  │
# ╞═══════════╧════════╧═══════╡
# │ M|00-04   │ Asthma │ 0.165 │
# ├───────────┼────────┼───────┤
# │ M|00-04   │ Stroke │ 0.000 │
# ├───────────┼────────┼───────┤
# │ ...       │ ...    │   ... │
# ├───────────┼────────┼───────┤
# │ F|90_PLUS │ Asthma │ 0.085 │
# ├───────────┼────────┼───────┤
# │ F|90_PLUS │ Stroke │ 0.104 │
# └───────────┴────────┴───────┘

# The IDX of "M|00_04" would identify the male population aged 0 to 4 years
# and the COND of "Asthma" indicates the condition for which the prevalence of 
# 0.165 (or 16.5%) is given, in comparison the IDX of "F|90_PLUS" indicating 
# the female population aged 90 years and above have a estimated prevalence of 
# 0.085 (or 8.5%) for the COND "Asthma".
# Whereas the prevalence of the condition (COND) "Stroke" is estimated as zero
# for the male population aged 0 to 4 years of age and 10.4% for females aged 90
# and over.

df_prev <- readxl::read_xlsx(path = filename, sheet = prev_sheet) %>%
  pivot_wider(id_cols = "IDX", names_from = "COND", values_from = "PREV", values_fill = 0)

# • 1.4. Load odds ratio data ----
# ────────────────────────────────

# The prevalence data should consist of IDX, COND and PREV fields. The IDX 
# field should uniquely identify the strata of the population and should match 
# that of the population data, the COND should identify the condition for which
# the PREV fields gives the prevalence for.

# For example:

# ┌───────────┬────────┬───────┐
# │ IDX       │ COND   │ PREV  │
# ╞═══════════╧════════╧═══════╡
# │ M|00-04   │ Asthma │ 0.165 │
# ├───────────┼────────┼───────┤
# │ M|00-04   │ Stroke │ 0.000 │
# ├───────────┼────────┼───────┤
# │ ...       │ ...    │   ... │
# ├───────────┼────────┼───────┤
# │ F|90_PLUS │ Asthma │ 0.085 │
# ├───────────┼────────┼───────┤
# │ F|90_PLUS │ Stroke │ 0.104 │
# └───────────┴────────┴───────┘

df_or <- readxl::read_xlsx(path = filename, sheet = odds_ratio_sheet)

age_bands <- c(paste0(str_sub(paste0("0", seq(0, 89, 5)), -2), "_", str_sub(paste0("0", seq(4, 89, 5)), -2)), "90_PLUS")
names(age_bands) <- c(paste0(seq(0, 89, 5), "-", seq(4, 90, 5)), "90+")


# 2. Create matrices ----
# ═══════════════════════

# • 2.1. Population matrix including growth ----
# ──────────────────────────────────────────────
df_tmp <- df_popn %>% 
  left_join(df_popn_growth, by = "IDX") %>%
  mutate(SIZE = round(SIZE * (1+PCT_GROWTH), 0)) %>%
  select(YEAR, IDX, SIZE) %>%
  # If required filter by year to return just a few focus years
  # this might be necessary for large population datasets
  # dplyr::filter(YEAR %in% c(2025, 2030, 2035, 2040, 2045)) %>%
  pivot_wider(names_from = YEAR, values_from = SIZE)

m_popn <- as.matrix(df_tmp %>% select(-IDX))
rownames(m_popn) <- df_tmp$IDX

# • 2.2. Prevalence matrix ----
# ─────────────────────────────
m_prev <- as.matrix(df_prev %>% select(-IDX))
rownames(m_prev) <- df_prev$IDX

# • 2.3. Odds ratios matrix ----
# ──────────────────────────────
conditions <- setdiff(df_prev %>% colnames(), "IDX")
n <- length(conditions)
m_or <- matrix(NA_real_, nrow = n, ncol = n, dimnames = list(conditions, conditions))
idx_i <- match(df_or$COND_I, conditions)
idx_j <- match(df_or$COND_J, conditions)
m_or[cbind(idx_i, idx_j)] <- df_or$OR
m_or[cbind(idx_j, idx_i)] <- df_or$OR # Symmetrize

# 3. Display Matrices ----
# ════════════════════════
m_popn
m_prev
m_or

# 4. Simulate Population ----
# ═══════════════════════════

# • 4.1. Example of multiple year and single simulation run ----
# ──────────────────────────────────────────────────────────────

cl <- parallel::makeCluster(parallel::detectCores()-1)
parallel::clusterEvalQ(cl, library(dplyr))
parallel::clusterExport(cl, varlist = c("fnSimulatePopulation", "fnGetLatentCorrelationMatrix",
                                        "fnGetRhoFromOR", "fnGetP11FromOR", "fnGetRhoFromP11",
                                        "fnFrechetHoeffdingBounds", "fnGetP11FromRho",
                                        "fnMakePD", "fnSimulateStrata",
                                        "m_popn", "m_prev", "m_or"))
dt_start <- Sys.time()
sim <- do.call("rbind",
               parallel::clusterApply(cl, x = colnames(m_popn), fun = function(x){
                 sim <- fnSimulatePopulation(m_popn[,x], m_prev, m_or)
                 sim$YEAR = as.integer(x)
                 return(sim)}))
Sys.time() - dt_start
parallel::stopCluster(cl)

# Add in the COUNT field, condition count and reorder columns
sim <- sim %>% 
  select(all_of(c("STRATA", "YEAR", conditions))) %>%
  mutate(COUNT = rowSums(select(., all_of(conditions))), .after = "YEAR")

if(QOF){
  output_filename <- "./output/qof/simulation_qof.csv"
  output_grouped_filename <- "./output/qof/simulation_grouped_qof.csv"
  output_summary_filename <- "./output/qof/simulation_summary_qof.csv"
} else {
  output_filename <- "./output/gbd/simulation_gbd.csv"
  output_grouped_filename <- "./output/gbd/simulation_grouped_gbd.csv"
  output_summary_filename <- "./output/gbd/simulation_summary_gbd.csv"
}

# BE AWARE THIS IS A VERY LARGE FILE SO THIS SECTION HAS BEEN COMMENTED OUT 
# # Output detailed data
# write.csv(sim, output_filename, row.names = FALSE)

# Group and add in SIZE field before output
write.csv(sim %>% 
            group_by(pick(everything())) %>%
            summarise(SIZE = n(), .groups = "keep") %>% 
            relocate(SIZE, .after = "COUNT") %>% 
            ungroup(), output_grouped_filename, row.names = FALSE)

# Summarise and output for workforce modelling input
write.csv(sim %>% 
            group_by(YEAR, COUNT) %>%
            summarise(SIZE = n(), .groups = "keep") %>%
            ungroup(), output_summary_filename, row.names = FALSE)

