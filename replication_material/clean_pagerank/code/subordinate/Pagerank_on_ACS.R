
################################################################################
# Generate ACS rankings
# August 2025
################################################################################

library(tidyverse)
library(sf)
library(tigris)
library(readr)
library(readxl)
library(reticulate)
library(corrplot)
library(purrr)
library(data.table)

options(tigris_use_cache = TRUE)


################################################################################
# Rolling 5 years
################################################################################


py$wd <- wd

# Paths

input_dir <- paste0(wd, "/data/rankings")
output_dir <- paste0(wd, "/output")
py_script <- paste0(wd, "/code/subordinate/run_acs_rolling5_pagerank.py")

# Run Python PageRank generator

use_python(python_path, required = FALSE)
reticulate::py_run_string(sprintf("wd = r'%s'", wd))
source_python(py_script)

output_dir <- paste0(wd, "/output") # undo python script change to path

# Get all rolling average files only

all_files <- list.files(input_dir, pattern = ".*_rolling5_\\d{4}_pagerank_.*\\.csv$", full.names = TRUE)


# Load and rename each file

dt_list <- list()

for (file in all_files) {
  fname <- basename(file)
  year <- str_extract(fname, "\\d{4}")
  var <- str_match(fname, "_pagerank_(.*)\\.csv$")[, 2]
  new_col <- paste0(var, "_", year)
  
  dt <- fread(file)
  setnames(dt, old = names(dt)[2], new = new_col)  # rename pagerank column
  
  # rename metro column to "metro"
  metro_col <- names(dt)[str_detect(names(dt), "metro|cbsa|GEOID")]
  setnames(dt, old = metro_col, new = "metro")
  dt[, metro := as.character(metro)]
  
  dt_list[[length(dt_list) + 1]] <- dt[, .(metro, get(new_col))]  # keep only metro + new column
  setnames(dt_list[[length(dt_list)]], old = "V2", new = new_col)
}

# Merge all data.tables by "metro"

merged_dt <- reduce(dt_list, full_join, by = "metro")

# Create rank columns (highest PageRank gets rank 1)
rank_cols <- names(merged_dt)[names(merged_dt) != "metro"]

merged_dt <- merged_dt %>%
  mutate(across(
    all_of(rank_cols),
    ~ rank(-., ties.method = "min"),
    .names = "rank_{.col}"
  ))


# Merge with CBSA geometry

metros <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  st_transform(4326) %>%
  select(GEOID, NAME, geometry)

geo_merged <- metros %>%
  inner_join(merged_dt, by = c("GEOID" = "metro"))

merged_dt <- metros %>%
  select(GEOID, NAME) %>%
  as.data.frame() %>%
  inner_join(merged_dt, by = c("GEOID" = "metro"))

names(merged_dt)[names(merged_dt) == "GEOID"] <- "metro"
names(merged_dt)[names(merged_dt) == "NAME"] <- "metro_name"

merged_dt <- merged_dt %>%
  select(!matches("^industry_ _\\d{4}$|^rank_industry_ _\\d{4}$")) %>%
  select(-c(geometry))

met_names_df <- read.csv(paste0(wd, "/data/raw/cbsa2fipsxw.csv" ))
met_names_df <- select(met_names_df, c(cbsacode, cbsatitle)) %>%
  distinct()

met_names_df$metro <- as.character(met_names_df$cbsacode)
met_names_df$GEOID <- as.character(met_names_df$cbsacode)

merged_dt <- left_join(merged_dt, met_names_df)
merged_dt$metro_name[!is.na(merged_dt$cbsatitle)] <- merged_dt$cbsatitle[!is.na(merged_dt$cbsatitle)]
merged_dt <- select(merged_dt, -c(cbsacode, cbsatitle, GEOID))

geo_merged <- left_join(geo_merged, met_names_df)
geo_merged$NAME[!is.na(geo_merged$cbsatitle)] <- geo_merged$cbsatitle[!is.na(geo_merged$cbsatitle)]
geo_merged <- select(geo_merged, -c(cbsacode, cbsatitle, metro))

# Save

write_csv(
  merged_dt %>%
    rename_with(~ str_replace_all(., c("PER" = "_individual", "HH" = "_household"))),
  file.path(output_dir, "acs_rolling5_pagerank.csv")
)

st_write(geo_merged, file.path(output_dir, "acs_rolling5_pagerank.geojson"),
         driver = "GeoJSON", delete_dsn = TRUE)


################################################################################
# Check consistency by year
################################################################################

# Identify relevant columns
flow_cols <- names(merged_dt)[str_detect(names(merged_dt), "^total_flowPER_\\d{4}$")]

# Create a wide-format table of top 20 metro names for each year
top20_table <- map(flow_cols, function(colname) {
  merged_dt %>%
    arrange(desc(.data[[colname]])) %>%
    slice_head(n = 20) %>%
    pull(metro_name)
}) %>%
  set_names(str_extract(flow_cols, "\\d{4}")) %>%  # Use years as column names
  as_tibble() %>%
  mutate(Rank = row_number()) %>%
  relocate(Rank)

write_csv(top20_table, file.path(output_dir, "acs_rolling5_top20.csv") )


# Identify relevant columns
Hisp_flow_cols <- names(merged_dt)[str_detect(names(merged_dt), "^total_flowPER_\\d{4}$")]

# Create a wide-format table of top 20 metro names for each year
Hisp_top20_table <- map(Hisp_flow_cols, function(colname) {
  merged_dt %>%
    arrange(desc(.data[[colname]])) %>%
    slice_head(n = 20) %>%
    pull(metro_name)
}) %>%
  set_names(str_extract(flow_cols, "\\d{4}")) %>%  # Use years as column names
  as_tibble() %>%
  mutate(Rank = row_number()) %>%
  relocate(Rank)
Hisp_top20_table

# Identify relevant columns
kids18_flow_cols <- names(merged_dt)[str_detect(names(merged_dt), "^kids_flow_kids_u18_\\d{4}$")]

# Create a wide-format table of top 20 metro names for each year
kids18_top20_table <- map(kids18_flow_cols, function(colname) {
  merged_dt %>%
    arrange(desc(.data[[colname]])) %>%
    slice_head(n = 20) %>%
    pull(metro_name)
}) %>%
  set_names(str_extract(flow_cols, "\\d{4}")) %>%  # Use years as column names
  as_tibble() %>%
  mutate(Rank = row_number()) %>%
  relocate(Rank)
kids18_top20_table


