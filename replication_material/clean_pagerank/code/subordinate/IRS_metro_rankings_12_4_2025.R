library(tidyverse)
library(knitr)
library(data.table)
library(kableExtra)
library(xtable)
library(tigris)
library(sf)

# ---------------------------------------------------------------------------
# Paths (set wd before running)
# ---------------------------------------------------------------------------

wd <- "G:/.shortcut-targets-by-id/1boJDCakyAAS94F5KjSRVfXd4ICmDvdRP/Measuring and Pricing Neighborhood Characteristics/clean_pagerank"

input_dir  <- file.path(wd, "output")
output_dir <- file.path(wd, "output", "12_5_2025")
raw_data <- file.path(wd, "data/raw")

# ---------------------------------------------------------------------------
# 1. METRO-LEVEL IRS PAGERANK
# ---------------------------------------------------------------------------

library(tidyverse)
library(knitr)

# Read and keep needed columns
irs_metro <- read.csv(file.path(input_dir, "irs_pagerank_combined.csv")) %>%
  select(
    name,
    cbsa,
    irs_metro_eigen_2001,
    irs_metro_eigen_2011,
    irs_metro_eigen_2021
  )

# Build a rank–by–year table of top 50 metros
top_n <- 50

top_table <- tibble(
  Rank = 1:top_n,
  `2001-2002` = irs_metro %>%
    arrange(desc(irs_metro_eigen_2001)) %>%
    slice_head(n = top_n) %>%
    pull(name),
  `2011-2012` = irs_metro %>%
    arrange(desc(irs_metro_eigen_2011)) %>%
    slice_head(n = top_n) %>%
    pull(name),
  `2021-2022` = irs_metro %>%
    arrange(desc(irs_metro_eigen_2021)) %>%
    slice_head(n = top_n) %>%
    pull(name)
)

# Print as a nice table
top_table %>%
  knitr::kable(
    format = "latex", 
    table.envir = "tabular",     
    booktabs = TRUE,
    col.names = c("Rank", 
                  "Top 50 Metros, 2001-2002", 
                  "Top 50 Metros, 2011-2012", 
                  "Top 50 Metros, 2021-2022")
  ) %>%
  kable_styling(
    full_width = FALSE
  )

# Build the LaTeX table object (same as your print)
top_table_tex <- top_table %>%
  knitr::kable(
    format = "latex",
    table.envir = "tabular",     
    booktabs = TRUE,
    col.names = c("Rank",
                  "Top 50 Metros, 2001-2002",
                  "Top 50 Metros, 2011-2012",
                  "Top 50 Metros, 2021-2022")
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE
  )

# Print
top_table_tex

# Save
kableExtra::save_kable(
  top_table_tex,
  file = file.path(output_dir, "top50_irs_metro_table.tex")
)


################################################################################
# IRS 2022 ranking
################################################################################

irs_county <- fread(paste0(wd, "/output/irs_county_pagerank_combined.csv"))

irs_county <- irs_county %>%
  select(c(name, state, fips, irs_county_rank_2021))

cbsa2fips <- fread(paste0(wd,"/data/raw/cbsa2fipsxw.csv"))

# Ensure FIPS in cbsa2fips is a proper 5-digit code
cbsa2fips <- cbsa2fips %>%
  mutate(fips = fipsstatecode * 1000 + fipscountycode)

# Select only needed CBSA info
cbsa2fips <- cbsa2fips %>%
  select(fips, cbsatitle)

# Merge into irs_county
irs_county <- irs_county %>%
  left_join(cbsa2fips, by = "fips")

# Sort and take top 50
top50 <- irs_county %>%
  arrange(irs_county_rank_2021) %>%   # lower rank = higher priority
  slice(1:50) %>%
  transmute(
    Ranking = irs_county_rank_2021,
    FIPS = fips,
    County = name,
    MSA = cbsatitle
  )

# Build LaTeX table
latex_top50 <- c(
 # "\\centering",
 # "\\caption{Top 50 Counties by IRS County Page Rank (2021-2022).}",
  "\\begin{tabular}{llll}",
  "\\toprule",
  "Ranking & FIPS Code & County Name & MSA Name \\\\",
  "\\midrule",
  apply(top50, 1, function(row) {
    sprintf("%s & %s & %s & %s \\\\",
            row[['Ranking']],
            row[['FIPS']],
            row[['County']],
            row[['MSA']])
  }),
  "\\bottomrule",
  "\\end{tabular}"
)

# Print
cat(latex_top50, sep = "\n")

# Optional: save to file
writeLines(latex_top50, paste0(output_dir, "/top50_irs_county_table.tex"))





################################################################################
# Now pop growth
################################################################################

# get county populations

library(data.table)
library(dplyr)

# load each year (keep only YEAR, GEO_ID, pop)
r1 <- fread(paste0(wd, "/data/raw/nhgis0014_csv/nhgis0014_ds239_20185_county.csv")) %>%
  transmute(YEAR, GEO_ID = GEOID, pop = AJWME001)

r2 <- fread(paste0(wd, "/data/raw/nhgis0014_csv/nhgis0014_ds244_20195_county.csv")) %>%
  transmute(YEAR, GEO_ID = GEOID, pop = ALUBE001)

r3 <- fread(paste0(wd, "/data/raw/nhgis0014_csv/nhgis0014_ds249_20205_county.csv")) %>%
  transmute(YEAR, GEO_ID = GEOID, pop = AMPVE001)

r4 <- fread(paste0(wd, "/data/raw/nhgis0014_csv/nhgis0014_ds254_20215_county.csv")) %>%
  transmute(YEAR, GEO_ID, pop = AON4E001)

r5 <- fread(paste0(wd, "/data/raw/nhgis0014_csv/nhgis0014_ds262_20225_county.csv")) %>%
  transmute(YEAR, GEO_ID, pop = AQNFE001)

r6 <- fread(paste0(wd, "/data/raw/nhgis0014_csv/nhgis0014_ds267_20235_county.csv")) %>%
  transmute(YEAR, GEO_ID, pop = ASN1E001)

# stack
census_raw <- bind_rows(r1, r2, r3, r4, r5) %>%
  mutate(
    # take last 5 characters: SSCCC
    tail5 = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)),
    STATEFIP  = as.integer(substr(tail5, 1, 2)),
    COUNTYFIP = as.integer(substr(tail5, 3, 5))
  ) %>%
  select(-tail5) %>%
  mutate(YEAR = substr(YEAR, 6, 9))


# population change table
pop_change <- census_raw %>%
  group_by(COUNTYFIP, STATEFIP) %>%
  reframe(
    p2018 = pop[YEAR == 2018],
    p2019 = pop[YEAR == 2019],
    p2020 = pop[YEAR == 2020],
    p2021 = pop[YEAR == 2021],
    p2022 = pop[YEAR == 2022],
    .groups = "drop"
  ) %>%
  mutate(
    pop_change_2018_2019 = 100 * (p2019 / p2018 - 1),
    pop_change_2019_2020 = 100 * (p2020 / p2019 - 1),
    pop_change_2021_2022 = 100 * (p2022 / p2021 - 1),
    pop_change_2018_2022 = 100 * (p2022 / p2018 - 1),
    fips = as.integer(sprintf("%02d%03d", STATEFIP, COUNTYFIP))
  )

irs_county <- read.csv(file.path(input_dir, "irs_county_pagerank_combined.csv")) %>%
  select(
    name,
    fips,
    state,
    irs_county_eigen_2001,
    irs_county_eigen_2011,
    irs_county_eigen_2021,
    irs_county_eigen_2019
  )  

irs_county <- full_join(irs_county, pop_change)

# get county-level movers

irs_movers <- fread(paste0(wd, "/data/processed/irs_Out_2122.csv"))
irs_movers$irs_pop <- irs_movers$Return #+ irs_movers$Exemption

hh_leaving <- irs_movers %>%
  filter(Source_County != Dest_County | Source_State != Dest_State) %>%     # exclude non-migrants
  group_by(Source_State, Source_County) %>%
  summarise(hh_leave = sum(Return, na.rm = TRUE), .groups = "drop") %>%
  mutate(fips = sprintf("%02d%03d", Source_State, Source_County))

hh_coming <- irs_movers %>%
  filter(Source_County != Dest_County | Source_State != Dest_State) %>%
  group_by(Dest_State, Dest_County) %>%
  summarise(hh_come = sum(Return, na.rm = TRUE), .groups = "drop") %>%
  mutate(fips = sprintf("%02d%03d", Dest_State, Dest_County))

hh_nonmig <- irs_movers %>%
  filter(Source_County == Dest_County, Source_State == Dest_State) %>%
  group_by(Dest_State, Dest_County) %>%
  summarise(hh_nonmig = sum(Return, na.rm = TRUE), .groups = "drop") %>%
  mutate(fips = sprintf("%02d%03d", Dest_State, Dest_County))

county_flows <- full_join(hh_leaving, hh_coming, by = "fips") %>%
  select(fips, hh_leave, hh_come) %>%
  full_join(hh_nonmig, by = "fips") %>%
  select(fips, hh_leave, hh_come, hh_nonmig) %>%
  mutate(fips = as.integer(fips))

irs_county <- full_join(irs_county, county_flows, by = "fips")

irs_county$change_perc_movers <- 100* (irs_county$hh_come - irs_county$hh_leave) / (irs_county$hh_come + irs_county$hh_leave)

irs_county <- irs_county %>%
  filter(!is.na(pop_change_2021_2022), !is.na(irs_county_eigen_2021), !is.na(change_perc_movers))

irs_county$irs_county_eigen_2021_std <- (irs_county$irs_county_eigen_2021 - mean(irs_county$irs_county_eigen_2021, na.rm = TRUE)) / sd(irs_county$irs_county_eigen_2021, na.rm = TRUE)


plot1 <- irs_county %>%
  filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(x = pop_change_2018_2022, y = log(irs_county_eigen_2021) )) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "County % Growth", y = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change, 2018-2022")
plot1  

plot2 <- irs_county %>%
  filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(x = pop_change_2018_2022, y = log(irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "County % Growth", y = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change, 2018-2022")
plot2  

plot3 <- irs_county %>%
  filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = change_perc_movers, y = log(irs_county_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  #geom_point() +
  theme_minimal() +
  #xlim(-40, 40) +
  labs(x = "County Growth as a % of Movers", y = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change as a % of Movers, 2021-2022")
plot3  

plot4 <- irs_county %>%
  filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = change_perc_movers, y = log(irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  #xlim(-40, 40)  +
  labs(x = "County Growth as a % of Movers", y = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change as a % of Movers, 2021-2022")
plot4  


plot5 <- irs_county %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(x = rank(-irs_county_eigen_2021), y = pop_change_2018_2022)) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Page Rank", y = "County % Growth",
       title = "County-level Population Change v.s. Page Rank, 2018–2022")
plot5


plot6 <- irs_county %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(x = rank(-irs_county_eigen_2021), y = pop_change_2018_2022)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Page Rank", y = "County % Growth",
       title = "County-level Population Change v.s. Page Rank, 2018–2022")
plot6


plot7 <- irs_county %>%
  #filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = rank(-irs_county_eigen_2021), y = change_perc_movers)) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Page Rank", y = "County Growth as a % of Movers",
       title = "County-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")
plot7


plot8 <- irs_county %>%
  filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = rank(-irs_county_eigen_2021), y = change_perc_movers)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Page Rank", y = "County Growth as a % of Movers",
       title = "County-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")
plot8




ggsave(file.path(output_dir, "pagerank_log_vs_pop_change_binned.png"),
       plot1, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pagerank_log_vs_pop_change_scatter.png"),
       plot2, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pagerank_log_vs_pct_movers_binned.png"),
       plot3, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pagerank_log_vs_pct_movers_scatter.png"),
       plot4, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pop_change_vs_pagerank_rank_binned.png"),
       plot5, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pop_change_vs_pagerank_rank_scatter.png"),
       plot6, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pct_movers_vs_pagerank_rank_binned.png"),
       plot7, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pct_movers_vs_pagerank_rank_scatter.png"),
       plot8, width = 8, height = 6, dpi = 300)


# -----------------------------
# LOG PageRank — flipped
# -----------------------------

plot1_flip <- irs_county %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(y = pop_change_2018_2022,
             x = log(irs_county_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(y = "County % Growth", x = "Log Page Rank",
       title = "County-level Population Change v.s. Page Rank, 2018–2022")

plot2_flip <- irs_county %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(y = pop_change_2018_2022,
             x = log(irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "County % Growth", x = "Log Page Rank",
       title = "County-level Population Change v.s. Page Rank, 2018–2022")

plot3_flip <- irs_county %>%
  #filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(y = change_perc_movers,
             x = log(irs_county_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "County Growth as a % of Movers", x = "Log Page Rank",
       title = "County-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")

plot4_flip <- irs_county %>%
  #filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(y = change_perc_movers,
             x = log(irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "County Growth as a % of Movers", x = "Log Page Rank",
       title = "County-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")


# -----------------------------
# RANK PageRank — flipped
# -----------------------------

plot5_flip <- irs_county %>%
  ggplot(aes(x = pop_change_2018_2022,
             y = rank(-irs_county_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "County % Growth", y = "Page Rank",
       title = "County-level Population Change v.s. Page Rank, 2018–2022")

plot6_flip <- irs_county %>%
  ggplot(aes(x = pop_change_2018_2022,
             y = rank(-irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "County % Growth", y = "Page Rank",
       title = "County-level Population Change v.s. Page Rank, 2018–2022")

plot7_flip <- irs_county %>%
  ggplot(aes(x = change_perc_movers,
             y = rank(-irs_county_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "County Growth as a % of Movers", y = "Page Rank",
       title = "County-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")

plot8_flip <- irs_county %>%
  filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = change_perc_movers,
             y = rank(-irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "County Growth as a % of Movers", y = "Page Rank",
       title = "County-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")

ggsave(file.path(output_dir, "pop_change_vs_pagerank_log_binned.png"),
       plot1_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pop_change_vs_pagerank_log_scatter.png"),
       plot2_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pct_movers_vs_pagerank_log_binned.png"),
       plot3_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pct_movers_vs_pagerank_log_scatter.png"),
       plot4_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pagerank_rank_vs_pop_change_binned.png"),
       plot5_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pagerank_rank_vs_pop_change_scatter.png"),
       plot6_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pagerank_rank_vs_pct_movers_binned.png"),
       plot7_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "pagerank_rank_vs_pct_movers_scatter.png"),
       plot8_flip, width = 8, height = 6, dpi = 300)


################################################################################
# Special requests 1-year
################################################################################

plotsp1 <- irs_county %>%
  filter(pop_change_2021_2022 > -10, pop_change_2021_2022 < 15) %>%
  ggplot(aes(x = pop_change_2021_2022, y = log(irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "County % Growth", y = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change, 2021-2022")
plotsp1  

binplotsp1 <- irs_county %>%
  filter(pop_change_2021_2022 > -10, pop_change_2021_2022 < 15) %>%
  ggplot(aes(x = pop_change_2021_2022, y = log(irs_county_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "County % Growth", y = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change, 2021-2022")
binplotsp1  



plotsp2 <- irs_county %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2021_2022 < 15) %>%
  ggplot(aes(y = pop_change_2021_2022, x = log(irs_county_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "County % Growth", x = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change, 2021-2022")
plotsp2  

binplotsp2 <- irs_county %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2021_2022 < 15) %>%
  ggplot(aes(y = pop_change_2021_2022, x = log(irs_county_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "County % Growth", x = "Log Page Rank", 
       title = "County-level Page Rank v.s. Population Change, 2021-2022")
binplotsp2  

ggsave(
  file.path(output_dir, "1_9_2026_pagerank_log_vs_pop_change_scatter.png"),
  plotsp1, width = 8, height = 6, dpi = 300
)

ggsave(
  file.path(output_dir, "1_9_2026_pagerank_log_vs_pop_change_binned.png"),
  binplotsp1, width = 8, height = 6, dpi = 300
)

ggsave(
  file.path(output_dir, "1_9_2026_pop_change_vs_pagerank_log_scatter.png"),
  plotsp2, width = 8, height = 6, dpi = 300
)

ggsave(
  file.path(output_dir, "1_9_2026_pop_change_vs_pagerank_log_binned.png"),
  binplotsp2, width = 8, height = 6, dpi = 300
)


################################################################################
# Now the same IRS plots but at the metro level
################################################################################


library(data.table)
library(dplyr)
library(ggplot2)

cbsa2fips <- fread(file = paste0(wd, "/data/raw/cbsa2fipsxw.csv"))
cbsa2fips$fips <- cbsa2fips$fipsstatecode * 1000 + cbsa2fips$fipscountycode

cbsa2fips <- full_join(cbsa2fips, pop_change, by = "fips")
names(cbsa2fips)[1] <- "cbsa"
cbsa2fips <- full_join(cbsa2fips, county_flows, by = "fips")

irs_metro <- read.csv(file.path(input_dir, "irs_pagerank_combined.csv")) %>%
  select(
    name,
    cbsa,
    irs_metro_eigen_2001,
    irs_metro_eigen_2011,
    irs_metro_eigen_2021,
    irs_metro_eigen_2019
  )

# ------------------------------------------------------------
# Aggregate to CBSA
#   - population growth: pop-weighted averages using p2022 if available,
#     otherwise simple averages
#   - movers: sum hh_leave / hh_come / hh_nonmig then compute percent
# ------------------------------------------------------------

metro_flows <- cbsa2fips %>%
  filter(!is.na(cbsa)) %>%
  group_by(cbsa) %>%
  reframe(
    metro_hh_leave  = sum(hh_leave, na.rm = TRUE),
    metro_hh_come   = sum(hh_come, na.rm = TRUE),
    metro_hh_nonmig = sum(hh_nonmig, na.rm = TRUE),
    
    pop_change_2018_2019 = ifelse(sum(!is.na(p2022)) > 0 & sum(p2022, na.rm = TRUE) > 0,
                                  sum(pop_change_2018_2019 * p2022, na.rm = TRUE) / sum(p2022, na.rm = TRUE),
                                  mean(pop_change_2018_2019, na.rm = TRUE)),
    
    pop_change_2019_2020 = ifelse(sum(!is.na(p2022)) > 0 & sum(p2022, na.rm = TRUE) > 0,
                                  sum(pop_change_2019_2020 * p2022, na.rm = TRUE) / sum(p2022, na.rm = TRUE),
                                  mean(pop_change_2019_2020, na.rm = TRUE)),
    
    pop_change_2021_2022 = ifelse(sum(!is.na(p2022)) > 0 & sum(p2022, na.rm = TRUE) > 0,
                                  sum(pop_change_2021_2022 * p2022, na.rm = TRUE) / sum(p2022, na.rm = TRUE),
                                  mean(pop_change_2021_2022, na.rm = TRUE)),
    
    pop_change_2018_2022 = ifelse(sum(!is.na(p2022)) > 0 & sum(p2022, na.rm = TRUE) > 0,
                                  sum(pop_change_2018_2022 * p2022, na.rm = TRUE) / sum(p2022, na.rm = TRUE),
                                  mean(pop_change_2018_2022, na.rm = TRUE))
  )

metro_flows$change_perc_movers <- 100 * (metro_flows$metro_hh_come - metro_flows$metro_hh_leave) /
  (metro_flows$metro_hh_come + metro_flows$metro_hh_leave)

irs_metro <- full_join(irs_metro, metro_flows, by = "cbsa")

irs_metro <- irs_metro %>%
  filter(!is.na(pop_change_2021_2022), !is.na(irs_metro_eigen_2021), !is.na(change_perc_movers))

irs_metro$irs_metro_eigen_2021_std <- (irs_metro$irs_metro_eigen_2021 - mean(irs_metro$irs_metro_eigen_2021, na.rm = TRUE)) /
  sd(irs_metro$irs_metro_eigen_2021, na.rm = TRUE)

# ------------------------------------------------------------
# Metro plots (same style as your county plots)
#   Note: "Log Page Rank" means log(irs_metro_eigen_2021)
# ------------------------------------------------------------

plot1_metro <- irs_metro %>%
  filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(x = pop_change_2018_2022, y = log(irs_metro_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Metro % Growth", y = "Log Page Rank",
       title = "Metro-level Page Rank v.s. Population Change, 2018-2022")
plot1_metro

plot2_metro <- irs_metro %>%
  filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(x = pop_change_2018_2022, y = log(irs_metro_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Metro % Growth", y = "Log Page Rank",
       title = "Metro-level Page Rank v.s. Population Change, 2018-2022")
plot2_metro

plot3_metro <- irs_metro %>%
  filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = change_perc_movers, y = log(irs_metro_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Metro Growth as a % of Movers", y = "Log Page Rank",
       title = "Metro-level Page Rank v.s. Population Change as a % of Movers, 2021-2022")
plot3_metro

plot4_metro <- irs_metro %>%
  filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = change_perc_movers, y = log(irs_metro_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Metro Growth as a % of Movers", y = "Log Page Rank",
       title = "Metro-level Page Rank v.s. Population Change as a % of Movers, 2021-2022")
plot4_metro

plot5_metro <- irs_metro %>%
  ggplot(aes(x = rank(-irs_metro_eigen_2021), y = pop_change_2018_2022)) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Page Rank", y = "Metro % Growth",
       title = "Metro-level Population Change v.s. Page Rank, 2018–2022")
plot5_metro

plot6_metro <- irs_metro %>%
  ggplot(aes(x = rank(-irs_metro_eigen_2021), y = pop_change_2018_2022)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Page Rank", y = "Metro % Growth",
       title = "Metro-level Population Change v.s. Page Rank, 2018–2022")
plot6_metro

plot7_metro <- irs_metro %>%
  ggplot(aes(x = rank(-irs_metro_eigen_2021), y = change_perc_movers)) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Page Rank", y = "Metro Growth as a % of Movers",
       title = "Metro-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")
plot7_metro

plot8_metro <- irs_metro %>%
  #filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = rank(-irs_metro_eigen_2021), y = change_perc_movers)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Page Rank", y = "Metro Growth as a % of Movers",
       title = "Metro-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")
plot8_metro

# ------------------------------------------------------------
# Save (same pattern as your county code)
# ------------------------------------------------------------

ggsave(file.path(output_dir, "metro_pagerank_log_vs_pop_change_binned.png"),
       plot1_metro, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pagerank_log_vs_pop_change_scatter.png"),
       plot2_metro, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pagerank_log_vs_pct_movers_binned.png"),
       plot3_metro, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pagerank_log_vs_pct_movers_scatter.png"),
       plot4_metro, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pop_change_vs_pagerank_rank_binned.png"),
       plot5_metro, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pop_change_vs_pagerank_rank_scatter.png"),
       plot6_metro, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pct_movers_vs_pagerank_rank_binned.png"),
       plot7_metro, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pct_movers_vs_pagerank_rank_scatter.png"),
       plot8_metro, width = 8, height = 6, dpi = 300)


# -----------------------------
# LOG PageRank — flipped axes
# -----------------------------

plot1_metro_flip <- irs_metro %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(y = pop_change_2018_2022, x = log(irs_metro_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(y = "Metro % Growth", x = "Log Page Rank",
       title = "Metro-level Population Change v.s. Page Rank, 2018–2022")

plot2_metro_flip <- irs_metro %>%
  #filter(pop_change_2018_2022 > -10, pop_change_2018_2022 < 15) %>%
  ggplot(aes(y = pop_change_2018_2022, x = log(irs_metro_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "Metro % Growth", x = "Log Page Rank",
       title = "Metro-level Population Change v.s. Page Rank, 2018–2022")

plot3_metro_flip <- irs_metro %>%
  #filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(y = change_perc_movers, x = log(irs_metro_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "Metro Growth as a % of Movers", x = "Log Page Rank",
       title = "Metro-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")

plot4_metro_flip <- irs_metro %>%
  #filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(y = change_perc_movers, x = log(irs_metro_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(y = "Metro Growth as a % of Movers", x = "Log Page Rank",
       title = "Metro-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")


# -----------------------------
# RANK PageRank — flipped axes
# -----------------------------

plot5_metro_flip <- irs_metro %>%
  ggplot(aes(x = pop_change_2018_2022, y = rank(-irs_metro_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Metro % Growth", y = "Page Rank",
       title = "Metro-level Population Change v.s. Page Rank, 2018–2022")

plot6_metro_flip <- irs_metro %>%
  ggplot(aes(x = pop_change_2018_2022, y = rank(-irs_metro_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Metro % Growth", y = "Page Rank",
       title = "Metro-level Population Change v.s. Page Rank, 2018–2022")

plot7_metro_flip <- irs_metro %>%
  ggplot(aes(x = change_perc_movers, y = rank(-irs_metro_eigen_2021))) +
  geom_point(stat = "summary_bin", bins = 40, fun = "mean") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Metro Growth as a % of Movers", y = "Page Rank",
       title = "Metro-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")

plot8_metro_flip <- irs_metro %>%
  #filter(change_perc_movers < 20, change_perc_movers > -20) %>%
  ggplot(aes(x = change_perc_movers, y = rank(-irs_metro_eigen_2021))) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Metro Growth as a % of Movers", y = "Page Rank",
       title = "Metro-level Population Change (% of Movers) v.s. Page Rank, 2021–2022")

# Save the flipped plots

ggsave(file.path(output_dir, "metro_pop_change_vs_pagerank_log_binned.png"),
       plot1_metro_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pop_change_vs_pagerank_log_scatter.png"),
       plot2_metro_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pct_movers_vs_pagerank_log_binned.png"),
       plot3_metro_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pct_movers_vs_pagerank_log_scatter.png"),
       plot4_metro_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pagerank_rank_vs_pop_change_binned.png"),
       plot5_metro_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pagerank_rank_vs_pop_change_scatter.png"),
       plot6_metro_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pagerank_rank_vs_pct_movers_binned.png"),
       plot7_metro_flip, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "metro_pagerank_rank_vs_pct_movers_scatter.png"),
       plot8_metro_flip, width = 8, height = 6, dpi = 300)




################################################################################
# IRS vs ACS comparison
################################################################################

irs_acs_subset <- fread(paste0(wd, "/data/irs_acs_subset/pagerank_irs_Out_2021_acs_subset.csv"))
names(irs_acs_subset)[1] <- "metro"

acs <- fread(paste0(wd,"/output/acs_rolling5_pagerank.csv")) 

acs <- full_join(acs, irs_acs_subset); rm(irs_acs_subset)

cor(acs$total_flow_household_2019, acs$rank, use = "complete.obs")

# IRS ranking: order by Pagerank 2021
irs_rank <- acs %>%
  filter(!is.na(rank)) %>%
  arrange(desc(rank)) %>%
  transmute(IRS_2021_2022 = metro_name)

# ACS ranking: order by 2019 household flows
acs_rank <- acs %>%
  filter(!is.na(total_flow_household_2019)) %>%
  arrange(desc(total_flow_household_2019)) %>%
  transmute(ACS_2023_5yr = metro_name)

# Combine into one table
n <- min(nrow(irs_rank), nrow(acs_rank))

combined <- tibble(
  Metro_Rank = 1:n,
  IRS_2021_2022 = irs_rank$IRS_2021_2022[1:n],
  ACS_2023_5yr = acs_rank$ACS_2023_5yr[1:n]
)

# Save
fwrite(combined, file.path(output_dir, "/irs_acs_rank_compare_5_12_2025.csv"))

library(dplyr)
library(xtable)

# take top 30 rows
top30 <- combined %>% slice(1:30)

# build LaTeX table manually
latex_table <- c(
 # "\\centering",
 # "\\caption{Top 30 Metros by IRS Page Rank (2021--2022) and ACS Migration Flows (2023 5-year).}",
 # "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lll}",
  "\\toprule",
  "Metro Rank & IRS 2021--2022 Metro & ACS 2023 5-Year Metro \\\\",
  "\\midrule",
  apply(top30, 1, function(row) {
    sprintf("%s & %s & %s \\\\",
            row[["Metro_Rank"]],
            row[["IRS_2021_2022"]],
            row[["ACS_2023_5yr"]])
  }),
  "\\bottomrule",
  "\\end{tabular}"
 # "}",     # close resizebox
)


# print LaTeX code
cat(latex_table, sep = "\n")

# optionally save to file
writeLines(latex_table, file.path(output_dir, "top30_irs_acs_table.tex"))

# take top 50 rows
top50 <- combined %>% slice(1:50)

# build LaTeX table manually
latex_table <- c(
  # "\\centering",
  # "\\caption{Top 30 Metros by IRS Page Rank (2021--2022) and ACS Migration Flows (2023 5-year).}",
  # "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lll}",
  "\\toprule",
  "Metro Rank & IRS 2021--2022 Metro & ACS 2023 5-Year Metro \\\\",
  "\\midrule",
  apply(top50, 1, function(row) {
    sprintf("%s & %s & %s \\\\",
            row[["Metro_Rank"]],
            row[["IRS_2021_2022"]],
            row[["ACS_2023_5yr"]])
  }),
  "\\bottomrule",
  "\\end{tabular}"
  # "}",     # close resizebox
)


# print LaTeX code
cat(latex_table, sep = "\n")

# optionally save to file
writeLines(latex_table, file.path(output_dir, "top50_irs_acs_table.tex"))

################################################################################
# ACS correlation plots and top 30
################################################################################

acs <- fread(paste0(wd,"/output/acs_rolling5_PageRank.csv")) 

library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)

cor_heatmap <- function(df, vars, labels, title) {
  df <- as.data.frame(df)
  
  # correlation matrix
  mat <- cor(df[, vars, drop = FALSE], use = "pairwise.complete.obs")
  colnames(mat) <- rownames(mat) <- labels
  
  # keep upper triangle (incl diagonal); set lower triangle to NA
  mat[lower.tri(mat)] <- NA_real_
  
  cor_df <- as.data.frame(as.table(mat)) %>%
    dplyr::filter(!is.na(Freq))
  
  ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile() +
    geom_text(
      aes(label = sprintf("%.2f", Freq)),
      color = "white",
      fontface = "bold"
    ) +
    scale_fill_gradient2(
      limits = c(-1, 1),
      low = "#b2182b",
      mid = "white",
      high = "#2166ac",
      midpoint = 0,
      name = "Corr."
    ) +
    scale_y_discrete(limits = rev(labels)) +   # <<< KEY FIX to flip the matrix correctly
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.grid = element_blank()
    )
}



make_top_table <- function(df, vars, labels, top_n = 30) {
  df <- as.data.frame(df)
  
  ranked <- lapply(vars, function(v) {
    df %>%
      arrange(desc(.data[[v]])) %>%
      slice_head(n = top_n) %>%
      pull(metro_name)
  })
  
  mat <- do.call(cbind, ranked)
  colnames(mat) <- labels
  
  tibble::tibble(
    Rank = 1:top_n,
    as.data.frame(mat, stringsAsFactors = FALSE)
  )
}




# ------------------------------------------------------------
# Variable sets (using 2019; change `year` if desired)
# ------------------------------------------------------------

year <- 2019

age_vars   <- c(paste0("age_25_35_", year),
                paste0("age_35_65_", year),
                paste0("age_65plus_", year))
age_labels <- c("Age 25–35", "Age 35–65", "Age 65+")

educ_vars   <- c(paste0("educ_NoCollege_", year),
                 paste0("educ_SomeCollege_", year),
                 paste0("educ_BA_", year),
                 paste0("educ_Grad_", year))
educ_labels <- c("No college", "Some college", "Bachelor's", "Graduate")

kids_vars   <- c(paste0("kids_flow_kids_u18_", year),
                 paste0("kids_flow_no_kids_", year))
kids_labels <- c("Has kid (< 18)", "No kid")

race_vars   <- c(paste0("race_White_", year),
                 paste0("race_Black_", year),
                 paste0("race_Hispanic_", year)
                 )
race_labels <- c("White", "Black", "Hispanic")

tenure_vars   <- c(paste0("tenure_Owner_", year),
                   paste0("tenure_Renter_", year))
tenure_labels <- c("Owner", "Renter")  # NA/unknown dropped by construction

# ------------------------------------------------------------
# Correlation plots
# ------------------------------------------------------------

age_cor_plot <- cor_heatmap(
  acs, age_vars, age_labels,
  title = paste0("Correlation of Age-specific Page Ranks, 2023 5-year ACS")
) + scale_fill_gradient(low = "lightgray", high = "black", name = "Corr.", limits = c(0.9, 1))

educ_cor_plot <- cor_heatmap(
  acs, educ_vars, educ_labels,
  title = paste0("Correlation of Education-specific Page Ranks, 2023 5-year ACS")
) + scale_fill_gradient(low = "lightgray", high = "black", name = "Corr.", limits = c(0.9, 1))

kids_cor_plot <- cor_heatmap(
  acs, kids_vars, kids_labels,
  title = paste0("Correlation of Kids-status Page Ranks, 2023 5-year ACS")
) + scale_fill_gradient(low = "lightgray", high = "black", name = "Corr.", limits = c(0.9, 1))

race_cor_plot <- cor_heatmap(
  acs, race_vars, race_labels,
  title = paste0("Correlation of Race-specific Page Ranks, 2023 5-year ACS")
) + scale_fill_gradient(low = "lightgray", high = "black", name = "Corr.", limits = c(0.9, 1))

tenure_cor_plot <- cor_heatmap(
  acs, tenure_vars, tenure_labels,
  title = paste0("Correlation of Tenure-specific Page Ranks, 2023 5-year ACS")
) + scale_fill_gradient(low = "lightgray", high = "black", name = "Corr.", limits = c(0.9, 1))

# Print (or save with ggsave)
age_cor_plot
educ_cor_plot
kids_cor_plot
race_cor_plot
tenure_cor_plot

ggsave(file.path(output_dir, "correlation_age_group.png"),
       age_cor_plot, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "correlation_education.png"),
       educ_cor_plot, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "correlation_children.png"),
       kids_cor_plot, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "correlation_race_ethnicity.png"),
       race_cor_plot, width = 8, height = 6, dpi = 300)

ggsave(file.path(output_dir, "correlation_home_tenure.png"),
       tenure_cor_plot, width = 8, height = 6, dpi = 300)


# ------------------------------------------------------------
# Top-30 LaTeX tables (by average eigenvalue within category)
# ------------------------------------------------------------
# --- build the tables ---
age_table    <- make_top_table(acs, age_vars,    age_labels,    top_n = 30)
educ_table   <- make_top_table(acs, educ_vars,   educ_labels,   top_n = 30)
kids_table   <- make_top_table(acs, kids_vars,   kids_labels,   top_n = 30)
race_table   <- make_top_table(acs, race_vars,   race_labels,   top_n = 30)
tenure_table <- make_top_table(acs, tenure_vars, tenure_labels, top_n = 30)

# --- DO NOT print kable() output here (that’s what caused duplicate tabular) ---
# age_table %>% kable(...)
# educ_table %>% kable(...) %>% kable_styling(...)
# ...

# --- helper: write a *single* tabular (no table float, no caption) ---
write_tabular_tex <- function(tbl, out_file) {
  tex <- knitr::kable(
    tbl,
    format = "latex",
    booktabs = TRUE
  )
  writeLines(tex, out_file)
}

# --- save files ---
write_tabular_tex(age_table,    file.path(output_dir, "top30_acs_age_table.tex"))
write_tabular_tex(educ_table,   file.path(output_dir, "top30_acs_educ_table.tex"))
write_tabular_tex(kids_table,   file.path(output_dir, "top30_acs_kids_table.tex"))
write_tabular_tex(race_table,   file.path(output_dir, "top30_acs_race_table.tex"))
write_tabular_tex(tenure_table, file.path(output_dir, "top30_acs_tenure_table.tex"))


################################################################################
# ACS Industry breakdown correlation plot + tables
################################################################################


library(dplyr)
library(knitr)
library(tibble)

#-----------------------------#
# 1. Single ranking table     #
#-----------------------------#

make_rank_table <- function(df, vars, labels, top_n = 30) {
  df <- as.data.frame(df)
  
  ranked <- lapply(vars, function(v) {
    df %>%
      arrange(desc(.data[[v]])) %>%
      slice_head(n = top_n) %>%
      pull(metro_name)
  })
  
  mat <- do.call(cbind, ranked)
  colnames(mat) <- labels
  
  tibble(
    Rank = 1:top_n,
    as.data.frame(mat, stringsAsFactors = FALSE)
  )
}




make_multi_rank_tables <- function(df,
                                   vars,
                                   labels,
                                   top_n = 30,
                                   max_cols_per_table = 3,
                                   output_dir,
                                   file_prefix = "top30_acs_industry") {
  stopifnot(length(vars) == length(labels))
  
  n_cat  <- length(vars)
  idx    <- seq_len(n_cat)
  groups <- split(idx, ceiling(idx / max_cols_per_table))
  
  for (g in seq_along(groups)) {
    sel        <- groups[[g]]
    vars_sub   <- vars[sel]
    labels_sub <- labels[sel]
    
    tbl <- make_rank_table(df, vars_sub, labels_sub, top_n = top_n)
    
    tex <- knitr::kable(
      tbl,
      format   = "latex",
      booktabs = TRUE
    )
    
    out_file <- file.path(output_dir, paste0(file_prefix, "_part", g, ".tex"))
    writeLines(tex, out_file)
  }
  
  invisible(NULL)
}

#-----------------------------#
# 3. Industry 2019 setup      #
#-----------------------------#

year <- 2019

industry_vars <- paste0("industry_", c(1:9, "NOLAB"), "_", year)


industry_labels <- c(
  "Natural resources (NAICS 11)",                     # industry_1
  "Mining, utilities, construction (NAICS 21–23)",         # industry_2
  "Manufacturing (NAICS 31–33)",                           # industry_3
  "Wholesale, retail, transport, warehousing (NAICS 42–49)", # industry_4
  "Information, finance, real estate, admin (NAICS 51–56)", # industry_5
  "Education & health (NAICS 61–62)",                      # industry_6
  "Arts, entertainment, food, accommodation (NAICS 71–72)",# industry_7
  "Other services (NAICS 81)",                             # industry_8
  "Public administration (NAICS 92)",                      # industry_9
  #"Not in labor force",                      # industry_X
  "Not in labor force"
  #"All workers"
  )


#-----------------------------#
# 4. Generate & print plot + tables  
#-----------------------------#


industry_cor_plot <- cor_heatmap(
  acs,
  industry_vars,
  industry_labels,
  title = paste0("Correlation of Industry-specific (NAICS) Page Ranks, 2023 5-year ACS")
)  #+ scale_fill_gradient(low = "lightgray", high = "black", name = "Corr.")

industry_cor_plot

ggsave(
  file.path(output_dir, "correlation_industry_pageranks.png"),
  industry_cor_plot,
  width = 16,
  height = 12,
  dpi = 600
)




make_multi_rank_tables(
  df        = acs,
  vars      = industry_vars,
  labels    = industry_labels,
  top_n     = 30,
  max_cols_per_table = 3,
  output_dir = output_dir,
  file_prefix = "top30_acs_industry"
)



################################################################################
# Flows summary
################################################################################


summ_stats <- function(x) {
  x <- x[is.finite(x)]
  tibble(
    Statistic        = c("Mean", "SD",
                         "25th Percentile", "Median", "75th Percentile",
                          "N"),
    value = c(
      round(mean(x)),
      round(sd(x)),
      round(quantile(x, 0.25)),
      round(median(x)),
      round(quantile(x, 0.75)),
      round(length(x))
    )
  )
}



### IRS

library(sf)
library(dplyr)
library(tidyr)
library(data.table)

# 1) County universe from GeoJSON
county_sf <- read_sf(file.path(input_dir, "irs_county_pagerank_combined.geojson"))

county_ids <- county_sf %>%
  st_drop_geometry() %>%
  transmute(GEOID = as.integer(GEOID)) %>%   # GEOID is 5-digit county FIPS, keep as int
  pull(GEOID)

# 2) Read IRS flows (do NOT filter to the universe)
irs_flow_2021 <- fread(paste0(wd, "/data/processed/irs_Out_2122.csv")) %>%
  filter(FIPS_Origin != FIPS_Dest)

# 3) Build full OD grid from GEOIDs and bring in flows; missing -> 0
irs_flow_2021 <- tidyr::expand_grid(
  FIPS_Origin = county_ids,
  FIPS_Dest   = county_ids
) %>%
  filter(FIPS_Origin != FIPS_Dest) %>%
  left_join(
    irs_flow_2021 %>% select(FIPS_Origin, FIPS_Dest, Return),
    by = c("FIPS_Origin", "FIPS_Dest")
  ) %>%
  mutate(Return = ifelse(is.na(Return), 0, Return))



# Outflows by origin county
irs_outflows <- irs_flow_2021 %>%
  group_by(FIPS_Origin) %>%
  reframe(flow = sum(Return, na.rm = TRUE)) %>%
  ungroup()

# Inflows by destination county
irs_inflows <- irs_flow_2021 %>%
  group_by(FIPS_Dest) %>%
  reframe(flow = sum(Return, na.rm = TRUE)) %>%
  ungroup()

irs_flow_2021 <- irs_flow_2021 %>%
  filter(Return > 0)

# Column summaries
col1 <- summ_stats(irs_outflows$flow)        %>% rename(`(1)` = value)
col2 <- summ_stats(irs_inflows$flow)         %>% rename(`(2)` = value)
col3 <- summ_stats(irs_flow_2021$Return)    %>% rename(`(3)` = value)

# Combine
irs_tab <- col1 %>%
  left_join(col2, by = "Statistic") %>%
  left_join(col3, by = "Statistic") %>%
  column_to_rownames("Statistic")

xt <- xtable(irs_tab, digits = c(0, 0, 0, 0))

# Build custom header ----
header <- paste(
  " & \\multicolumn{2}{c}{County} & \\multicolumn{1}{c}{Origin-Destination Pairs} \\\\",
  "\\cmidrule(l){2-3} \\cmidrule(l){4-4} \\\\",
  " & Outflows & Inflows & Migration Flows \\\\",
  "\\cmidrule(l){2-2} \\cmidrule(l){3-3} \\cmidrule(l){4-4} \\\\",
  " & (1) & (2) & (3) \\\\",
  sep = "\n"
)

add <- list(pos = list(0), command = header)

print(
  xt,
  include.rownames = TRUE,
  include.colnames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  add.to.row = add,
  digits = c(0, 0, 0, 0),
  align = c("l", "l", "l", "l"),
  file = paste0(output_dir, "/irs_2021_2022_migration_summary_table.tex")
)


################################################################################
# IRS flows metro table (OD pairs) from saved metro-metro flows (irs_metro_flows_2021)
################################################################################


library(sf)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(xtable)

# 1) Metro universe from GeoJSON (must contain cbsa codes)
metro_sf <- read_sf(file.path(input_dir, "irs_pagerank_combined.geojson"))

metro_ids <- metro_sf %>%
  st_drop_geometry() %>%
  transmute(cbsa = as.integer(cbsa)) %>%   # change 'cbsa' if your column name differs
  pull(cbsa) %>%
  unique()

# 2) Read metro flows
metro_file <- file.path(wd, "data/processed/irs_metro_flows_2021.csv")
metro_flow_raw <- fread(metro_file) %>%
  as_tibble() %>%
  filter(!is.na(CBSA_Origin), !is.na(CBSA_Dest), !is.na(Return)) %>%
  transmute(
    CBSA_Origin = as.integer(CBSA_Origin),
    CBSA_Dest   = as.integer(CBSA_Dest),
    Return      = as.numeric(Return)
  )

# (Recommended) collapse duplicates if any
metro_flow_raw <- metro_flow_raw %>%
  group_by(CBSA_Origin, CBSA_Dest) %>%
  summarise(Return = sum(Return, na.rm = TRUE), .groups = "drop")

# 3) Full OD grid over the GeoJSON metro universe + bring in flows; missing -> 0
metro_flow <- tidyr::expand_grid(
  CBSA_Origin = metro_ids,
  CBSA_Dest   = metro_ids
) %>%
  filter(CBSA_Origin != CBSA_Dest) %>%
  left_join(metro_flow_raw, by = c("CBSA_Origin", "CBSA_Dest")) %>%
  mutate(Return = ifelse(is.na(Return), 0, Return))

# 4) Outflows / inflows on the full grid
metro_outflows <- metro_flow %>%
  group_by(CBSA_Origin) %>%
  reframe(flow = sum(Return, na.rm = TRUE)) %>%
  ungroup()

metro_inflows <- metro_flow %>%
  group_by(CBSA_Dest) %>%
  reframe(flow = sum(Return, na.rm = TRUE)) %>%
  ungroup()

metro_flow <- metro_flow %>%
  filter(Return > 0)

# 5) Column summaries
col1 <- summ_stats(metro_outflows$flow) %>% rename(`(1)` = value)
col2 <- summ_stats(metro_inflows$flow)  %>% rename(`(2)` = value)
col3 <- summ_stats(metro_flow$Return)   %>% rename(`(3)` = value)

# Combine
metro_tab <- col1 %>%
  left_join(col2, by = "Statistic") %>%
  left_join(col3, by = "Statistic") %>%
  column_to_rownames("Statistic")

xt <- xtable(metro_tab, digits = c(0, 0, 0, 0))

# Header
header <- paste(
  " & \\multicolumn{2}{c}{Metro} & \\multicolumn{1}{c}{Origin-Destination Pairs} \\\\",
  "\\cmidrule(l){2-3} \\cmidrule(l){4-4} \\\\",
  " & Outflows & Inflows & Migration Flows \\\\",
  "\\cmidrule(l){2-2} \\cmidrule(l){3-3} \\cmidrule(l){4-4} \\\\",
  " & (1) & (2) & (3) \\\\",
  sep = "\n"
)

add <- list(pos = list(0), command = header)

print(
  xt,
  include.rownames = TRUE,
  include.colnames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  add.to.row = add,
  align = c("l", "l", "l", "l"),
  file = file.path(output_dir, "irs_metro_2021_2022_migration_summary_table.tex")
)



################################################################################
# IRS map
################################################################################

library(sf)


map_df <- read_sf(file.path(input_dir, "irs_county_pagerank_combined.geojson"))


# 3. Quantile bins 1–5, with labels that match the palette
map_df <- map_df %>%
  mutate(
    desirability_bin = ntile(irs_county_rank_2019, 5),
    desirability_bin = factor(
      desirability_bin,
      levels = 1:5,
      labels = c(
        "Least Desirable",
        "Less Desirable",
        "Mid",
        "More Desirable",
        "Most Desirable"
      )
    )
  )

# 4. Palette (same as earlier)
# Same palette
pal <- c(
  "Least Desirable" = "#f38765",
  "Less Desirable"  = "#fbbb7c",
  "Mid"             = "#F2F3D6",
  "More Desirable"  = "#C4DDBB",
  "Most Desirable"  = "#698D71"
)



# 5. Plot
rank_map <- ggplot(map_df) +
  geom_sf(aes(fill = desirability_bin), color = "white") +
  scale_fill_manual(values = pal, na.value = "grey") +
  labs(
    title = "IRS Page Rank for U.S. Counties, 2021-2022",
    fill  = "Desirability"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  xlim(-125, -66) +
  ylim(24, 50)

rank_map

ggsave(file.path(output_dir, "rank_map.png"),
       rank_map, width = 8, height = 6, dpi = 900)

################################################################################
# IRS metro map
################################################################################

library(sf)
library(dplyr)
library(ggplot2)

# Read metro geometries (must contain a cbsa code you can join on)
metro_sf <- read_sf(file.path(input_dir, "irs_pagerank_combined.geojson"))

# Quantile bins 1–5 (higher eigen = more desirable)
metro_sf <- metro_sf %>%
  mutate(
    desirability_bin = ntile(irs_metro_eigen_2021, 5),
    desirability_bin = factor(
      desirability_bin,
      levels = 1:5,
      labels = c(
        "Least Desirable",
        "Less Desirable",
        "Mid",
        "More Desirable",
        "Most Desirable"
      )
    )
  ) %>%
  filter(!is.na(desirability_bin))

# Same palette
pal <- c(
  "Least Desirable" = "#f38765",
  "Less Desirable"  = "#fbbb7c",
  "Mid"             = "#F2F3D6",
  "More Desirable"  = "#C4DDBB",
  "Most Desirable"  = "#698D71"
)

# Plot
metro_rank_map <- ggplot(metro_sf) +
  geom_sf(aes(fill = desirability_bin), color = "white") +
  scale_fill_manual(values = pal, na.value = "grey") +
  labs(
    title = "IRS Page Rank for U.S. Metros, 2021-2022",
    fill  = "Desirability"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  xlim(-125, -66) +
  ylim(24, 50)

metro_rank_map

ggsave(
  file.path(output_dir, "metro_rank_map.png"),
  metro_rank_map, width = 8, height = 6, dpi = 900
)

################################################################################
# Maps like website
################################################################################

################################################################################
# IRS map
################################################################################

library(sf)


map_df <- read_sf(file.path(input_dir, "irs_county_pagerank_combined.geojson"))
#irs_county <- fread(paste0(wd, "/output/irs_county_pagerank_combined.csv"))




library(dplyr)

# Compute percentile cutpoints
q_county <- quantile(
  -map_df$irs_county_rank_2019,
  probs = c(0, .50, .75, .80, .90, .95, .99, 1),
  na.rm = TRUE
)

map_df <- map_df %>%
  mutate(
    desirability_bin = cut(
      -irs_county_rank_2019,
      breaks = q_county,
      include.lowest = TRUE,
      labels = c(
        "0–50th",
        "50–75th",
        "75–80th",
        "80–90th",
        "90–95th",
        "95–99th",
        "99–100th"
      )
    )
  )


pal <- c(
  "0–50th"  = "#f38765",
  "50–75th" = "#fbbb7c",
  "75–80th" = "#fee08b",
  "80–90th" = "#F2F3D6",
  "90–95th" = "#C4DDBB",
  "95–99th" = "#8FBF9B",
  "99–100th"= "#698D71"
)


# 5. Plot
rank_map <- ggplot(map_df) +
  geom_sf(aes(fill = desirability_bin), color = "white") +
  scale_fill_manual(values = pal, na.value = "grey") +
  labs(
    title = "IRS Page Rank for U.S. Counties, 2021-2022",
    fill  = "Desirability Percentile"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  xlim(-125, -66) +
  ylim(24, 50)

rank_map

ggsave(file.path(output_dir, "rank_7bin_map.png"),
       rank_map, width = 8, height = 6, dpi = 900)

################################################################################
# IRS metro map
################################################################################

library(sf)
library(dplyr)
library(ggplot2)

# Read metro geometries (must contain a cbsa code you can join on)
metro_sf <- read_sf(file.path(input_dir, "irs_pagerank_combined.geojson"))

library(dplyr)

q_metro <- quantile(
  -metro_sf$irs_metro_eigen_2021,
  probs = c(0, .50, .75, .80, .90, .95, .99, 1),
  na.rm = TRUE
)

metro_sf <- metro_sf %>%
  mutate(
    desirability_bin = cut(
      -irs_metro_eigen_2021,
      breaks = q_metro,
      include.lowest = TRUE,
      labels = c(
        "0–50th",
        "50–75th",
        "75–80th",
        "80–90th",
        "90–95th",
        "95–99th",
        "99–100th"
      )
    )
  ) %>%
  filter(!is.na(desirability_bin))


pal <- c(
  "0–50th"  = "#f38765",
  "50–75th" = "#fbbb7c",
  "75–80th" = "#fee08b",
  "80–90th" = "#F2F3D6",
  "90–95th" = "#C4DDBB",
  "95–99th" = "#8FBF9B",
  "99–100th"= "#698D71"
)

pal <- gray.colors(9, start = 1, end = 0)[3:9]

# Plot
metro_rank_map <- ggplot(metro_sf) +
  geom_sf(aes(fill = desirability_bin), color = "white") +
  scale_fill_manual(values = pal, na.value = "white") +
  labs(
    title = "IRS Page Rank for U.S. Metros, 2021-2022",
    fill  = "Desirability Percentile"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  xlim(-125, -66) +
  ylim(24, 50)

metro_rank_map

ggsave(
  file.path(output_dir, "metro_rank_7bin_map.png"),
  metro_rank_map, width = 8, height = 6, dpi = 900
)

################################################################################
# IRS 2021–2022 metro ranking (Top 50) — LaTeX table like the county one
################################################################################

# Read metro pagerank (already in your script earlier, but keep this self-contained)
irs_metro <- fread(file.path(input_dir, "irs_pagerank_combined.csv")) %>%
  select(name, cbsa, irs_metro_eigen_2021)


# to get consistent names
acs_metro <- read.csv(file.path(input_dir, "acs_rolling5_pagerank.csv")) %>%
  select(c(metro_name, metro)) %>%
  rename(cbsa = metro)
irs_metro <- full_join(irs_metro, acs_metro); rm(acs_metro)
irs_metro$metro_name[is.na(irs_metro$metro_name)] <- irs_metro$name[is.na(irs_metro$metro_name)]
irs_metro <- irs_metro %>%
  select(-c(name)) %>%
  rename(name = metro_name)


# Compute rank: higher eigen = better (rank 1 = highest eigen)
irs_metro <- irs_metro %>%
  mutate(irs_metro_rank_2021 = rank(-irs_metro_eigen_2021, ties.method = "first"))

# Top 50
top50_metro <- irs_metro %>%
  arrange(irs_metro_rank_2021) %>%
  slice(1:50) %>%
  transmute(
    Ranking = irs_metro_rank_2021,
    CBSA = cbsa,
    Metro = name
  )

# Build LaTeX table (same style as your county table)
latex_top50_metro <- c(
 # "\\centering",
 # "\\caption{Top 50 Metros by IRS Metro Page Rank (2021--2022).}",
  "\\begin{tabular}{lll}",
  "\\toprule",
  "Ranking & CBSA Code & Metro Name \\\\",
  "\\midrule",
  apply(top50_metro, 1, function(row) {
    sprintf("%s & %s & %s \\\\",
            row[["Ranking"]],
            row[["CBSA"]],
            row[["Metro"]])
  }),
  "\\bottomrule",
  "\\end{tabular}"
)

# Print
cat(latex_top50_metro, sep = "\n")

# Save
writeLines(latex_top50_metro, file.path(output_dir, "top50_irs_metro_table_2021_2022.tex"))


################################################################################
# IRS metros: Top 30 in 2001–2002, show 2001 rank, 2021 rank, and improvement
################################################################################

library(dplyr)
library(tibble)
library(xtable)

irs_metro <- read.csv(file.path(input_dir, "irs_pagerank_combined.csv")) %>%
  select(name, cbsa, irs_metro_eigen_2001, irs_metro_eigen_2021)


# ranks (1 = best)
irs_metro_ranked <- irs_metro %>%
  filter(is.finite(irs_metro_eigen_2001), is.finite(irs_metro_eigen_2021)) %>%
  mutate(
    rank_2001 = rank(-irs_metro_eigen_2001, ties.method = "first"),
    rank_2021 = rank(-irs_metro_eigen_2021, ties.method = "first")
  )

n_top <- 30

# Subset to top 30 in 2001–2002 and compute improvement = rank_2001 - rank_2021
top30_2001 <- irs_metro_ranked %>%
  filter(rank_2001 <= n_top) %>%
  mutate(
    improvement = rank_2001 - rank_2021,
    improvement_fmt = ifelse(improvement > 0,
                             paste0("+", improvement),
                             as.character(improvement))
  ) %>%
  arrange(desc(improvement), rank_2001) %>%
  transmute(
    Metro = name,
    `Rank 2001--2002` = rank_2001,
    `Rank 2021--2022` = rank_2021,
    Improvement = improvement_fmt
  ) %>%
  mutate(row_id = row_number()) %>%
  column_to_rownames("row_id")

xt <- xtable(
  top30_2001,
  digits = 0,
  align  = c("l", rep("l", ncol(top30_2001)))
)

header <- paste(
  "\\multicolumn{4}{c}{Top 30 Metros in 2001--2002: Rank Change by 2021--2022} \\\\",
  "\\cmidrule(l){1-4}",
  "Metro & Rank 2001--2002 & Rank 2021--2022 & Improvement \\\\",
  sep = "\n"
)

add <- list(pos = list(0), command = header)

print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  add.to.row = add,
  file = file.path(output_dir, "irs_metro_top30_2001_rank_change_2021.tex")
)

################################################################################
# Now eigenvalue changes
################################################################################

library(dplyr)
library(tibble)
library(xtable)

irs_metro <- read.csv(file.path(input_dir, "irs_pagerank_combined.csv")) %>%
  select(name, cbsa, irs_metro_eigen_2001, irs_metro_eigen_2021)


# ranks (1 = best)
irs_metro_ranked <- irs_metro %>%
  filter(is.finite(irs_metro_eigen_2001), is.finite(irs_metro_eigen_2021)) %>%
  mutate(
    rank_2001 = rank(-irs_metro_eigen_2001, ties.method = "first"),
    rank_2021 = rank(-irs_metro_eigen_2021, ties.method = "first")
  )

n_top <- 30

# Subset to top 30 in 2001–2002 and compute improvement = rank_2001 - rank_2021
top30_2001 <- irs_metro_ranked %>%
  filter(rank_2001 <= n_top) %>%
  mutate(
    improvement = irs_metro_eigen_2021 - irs_metro_eigen_2001,
    improvement_fmt = ifelse(improvement > 0,
                             paste0("+", improvement),
                             as.character(improvement))
  ) %>%
  arrange(desc(improvement), rank_2001) %>%
  transmute(
    Metro = name,
    `Rank 2001--2002` = rank_2001,
    `Rank 2021--2022` = rank_2021,
    Improvement = improvement_fmt
  ) %>%
  mutate(row_id = row_number()) %>%
  column_to_rownames("row_id")

xt <- xtable(
  top30_2001,
  digits = 0,
  align  = c("l", rep("l", ncol(top30_2001)))
)

header <- paste(
  "\\multicolumn{4}{c}{Top 30 Metros in 2001--2002: Eigenvalue Change by 2021--2022} \\\\",
  "\\cmidrule(l){1-4}",
  "Metro & Rank 2001--2002 & Rank 2021--2022 & Improvement \\\\",
  sep = "\n"
)

add <- list(pos = list(0), command = header)

print(
  xt,
  include.rownames = FALSE,
  include.colnames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  add.to.row = add,
  file = file.path(output_dir, "irs_metro_top30_2001_eigenvalue_change_2021.tex")
)

################################################################################
# Eigenvalue change regression
################################################################################

library(dplyr)
library(tibble)
library(xtable)

irs_metro <- read.csv(file.path(input_dir, "irs_pagerank_combined.csv")) %>%
  select(name, cbsa, irs_metro_eigen_2001, irs_metro_eigen_2008, irs_metro_eigen_2021) %>%
  mutate(eigen_change = irs_metro_eigen_2021 - irs_metro_eigen_2001,
    rank_2001 = rank(-irs_metro_eigen_2001, ties.method = "first"),
    rank_2021 = rank(-irs_metro_eigen_2021, ties.method = "first"),
    rank_change = rank_2001 - rank_2021
  )

cor(irs_metro$rank_change, irs_metro$eigen_change, use = "complete.obs")

plot(y = irs_metro$rank_change, x = irs_metro$eigen_change)

zillow_df <- read.csv(file.path(raw_data, "County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")) %>%
  mutate(
    state_fips  = str_pad(StateCodeFIPS, 2, pad = "0"),
    county_fips = str_pad(MunicipalCodeFIPS, 3, pad = "0"),
    fips = as.integer(paste0(state_fips, county_fips))
  ) %>%
  select(fips, X2001.12.31, X2021.12.31) %>%
  mutate(price_change = X2021.12.31 - X2001.12.31,
         log_price_change = log(X2021.12.31) - log(X2001.12.31))

county_to_cbsa <- read.csv(file.path(raw_data, "cbsa2fipsxw_2023.csv")) %>%
  mutate(
    state_fips  = str_pad(fipsstatecode, 2, pad = "0"),
    county_fips = str_pad(fipscountycode, 3, pad = "0"),
    fips = paste0(state_fips, county_fips)
  ) %>%
  mutate(fips = as.integer(fips)) %>%
  rename(cbsa = cbsacode) %>%
  select(c(cbsa, fips))

zillow_df <- inner_join(zillow_df, county_to_cbsa)
irs_metro <- inner_join(irs_metro, zillow_df)

summary(lm(log_price_change  ~ eigen_change, irs_metro))
summary(lm(log(X2021.12.31)  ~ eigen_change, irs_metro))

reg1 <- lm(log_price_change ~ eigen_change, irs_metro)
reg2 <- lm(log(X2021.12.31) ~ eigen_change, irs_metro)

reg_table <- tibble(
  Term = c("Eigen change", "Constant", "Observations", "R-squared"),
  `Price change` = c(
    sprintf("%.4f\n(%.4f)", coef(summary(reg1))["eigen_change", "Estimate"], coef(summary(reg1))["eigen_change", "Std. Error"]),
    sprintf("%.4f\n(%.4f)", coef(summary(reg1))["(Intercept)", "Estimate"], coef(summary(reg1))["(Intercept)", "Std. Error"]),
    nobs(reg1),
    sprintf("%.3f", summary(reg1)$r.squared)
  ),
  `2021 price level` = c(
    sprintf("%.4f\n(%.4f)", coef(summary(reg2))["eigen_change", "Estimate"], coef(summary(reg2))["eigen_change", "Std. Error"]),
    sprintf("%.4f\n(%.4f)", coef(summary(reg2))["(Intercept)", "Estimate"], coef(summary(reg2))["(Intercept)", "Std. Error"]),
    nobs(reg2),
    sprintf("%.3f", summary(reg2)$r.squared)
  )
)

library(stargazer)

stargazer(
  reg1, reg2,
  type = "latex",
  title = "Regression results",
  column.labels = c("2001-2021 Log price change", "2021 Log price level"),
  dep.var.labels.include = FALSE,
  covariate.labels = c("Eigen change"),
  omit.stat = c("adj.rsq", "f", "ser"),
  digits = 4
)

ggplot(irs_metro, aes(x = eigen_change, y = log_price_change)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Change in IRS metro eigenvector centrality, 2001 to 2021",
    y = "Change in Zillow log price index, 2001 to 2021",
    title = "House price growth vs. change in metro centrality"
  ) +
  theme_minimal()

ggplot(irs_metro, aes(x = eigen_change, y = log(X2021.12.31))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Change in IRS metro eigenvector centrality, 2001 to 2021",
    y = "Zillow log price index in 2021",
    title = "2021 house prices vs. change in metro centrality"
  ) +
  theme_minimal()

# flip to eigen on log prices

summary(lm(eigen_change ~ log_price_change, irs_metro))
summary(lm(eigen_change ~ log(X2021.12.31), irs_metro))

reg1_flip <- lm(eigen_change ~ log_price_change, irs_metro)
reg2_flip <- lm(eigen_change ~ log(X2021.12.31), irs_metro)

stargazer(
  reg1_flip, reg2_flip,
  type = "latex",
  title = "Regression results",
  column.labels = c("Eigen change on log price change", "Eigen change on 2021 log price level"),
  dep.var.labels = c("Eigen change"),
  covariate.labels = c("Log price change", "2021 log price level"),
  omit.stat = c("adj.rsq", "f", "ser"),
  digits = 4
)

ggplot(irs_metro, aes(x = log_price_change, y = eigen_change)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Log Zillow price change, 2001 to 2021",
    y = "Change in IRS metro eigenvector centrality, 2001 to 2021",
    title = "Change in metro centrality vs. house price growth"
  ) +
  theme_minimal()

ggplot(irs_metro, aes(x = log(X2021.12.31), y = eigen_change)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Log Zillow price index in 2021",
    y = "Change in IRS metro eigenvector centrality, 2001 to 2021",
    title = "Change in metro centrality vs. 2021 house prices"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Industry correlation heatmap:
#   first column/row = Working people (NOT "Not in labor force"),
#   then industries ordered by corr with Working people
#   (uses your cor_heatmap() style; set `year` as above)
# ------------------------------------------------------------

# If you don't already have it loaded:
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- 1) pick the "working people" column (edit this to match your file) ----
# Common patterns I've seen in your outputs:
#   industry_flowPER_YYYY, industry_flowHH_YYYY, total_flowPER_YYYY, total_flowHH_YYYY, etc.
# Put the exact column name here.
working_var <- paste0("total_flow_individual_", year)   # <-- CHANGE if needed

# ---- 2) build industry vars, INCLUDING NOLAB, YESLAB ----
industry_vars_working   <- paste0("industry_", c(1:9, "NOLAB", "YESLAB"), "_", year)
industry_labels_working <- industry_labels  # includes "Not in labor force"

# ---- 3) order industries by correlation with "All workers"
#      (keep "All workers" fixed first; order the rest by corr w/ it)
df_sub <- acs %>%
  select(all_of(c(working_var, industry_vars_working))) %>%
  drop_na()

cors <- purrr::map_dbl(
  industry_vars_working,
  ~ cor(df_sub[[working_var]], df_sub[[.x]], use = "pairwise.complete.obs")
)

ord <- order(cors, decreasing = TRUE)

vars_ord   <- c(working_var, industry_vars_working[ord])
labels_ord <- c("All movers", industry_labels_working[ord])

# ---- 4) plot ----
industry_vs_working_cor_plot <- cor_heatmap(
  df     = acs,
  vars   = vars_ord,
  labels = labels_ord
)

industry_vs_working_cor_plot

ggsave(
  file.path(output_dir, paste0("correlation_working_then_industry_", year, ".png")),
  industry_vs_working_cor_plot,
  width = 16, height = 12, dpi = 600
)