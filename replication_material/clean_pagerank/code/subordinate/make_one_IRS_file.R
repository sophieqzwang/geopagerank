library(tidyverse)
library(sf)
library(corrplot)
library(tigris)
options(tigris_use_cache = TRUE)

input_dir <- paste0(wd, "/data/rankings")
output_dir <- paste0(wd, "/output")

################################################################################
# Process metro IRS data for website
################################################################################

# Load all yearly metro-level IRS PageRank files
irs_files <- list.files(input_dir, pattern = "^pagerank_irs_Out_\\d{4}_metro\\.csv$", full.names = TRUE)

# Combine into wide format
irs_data <- map(irs_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  varname <- file %>%
    basename() %>%
    str_remove("^pagerank_irs_Out_") %>%
    str_remove("_metro\\.csv$")
  df %>%
    rename(!!paste0("rank_", varname) := rank) %>%
    mutate(cbsa = as.character(cbsa))
}) %>%
  reduce(full_join, by = "cbsa") %>%
  filter(!is.na(cbsa))

# Load CBSA geometries from TIGER/Line
cbsa_geo <- core_based_statistical_areas(cb = TRUE, year = 2023) %>%
  st_transform(crs = 4326) %>%
  mutate(cbsa = as.character(as.integer(GEOID)))

# === METRO: Rename rank_* to metro_eigen_* and create metro_rank_* ===
irs_data <- irs_data %>%
  rename_with(~ str_replace(., "^rank_", "irs_metro_eigen_"), starts_with("rank_"))

eigen_cols <- names(irs_data)[str_starts(names(irs_data), "irs_metro_eigen_")]

for (col in eigen_cols) {
  rank_col <- str_replace(col, "eigen_", "rank_")
  irs_data[[rank_col]] <- rank(-irs_data[[col]], ties.method = "min")
}


# Join IRS PageRank data to geometries
irs_geo <- cbsa_geo %>%
  inner_join(irs_data, by = "cbsa")

# Extract name and state info
irs_data_export <- irs_geo %>%
  st_drop_geometry() %>%
  transmute(
    name = NAME,
    cbsa = cbsa,
    across(starts_with("irs_metro_"))
  )

# Save
write_csv(irs_data_export, file.path(output_dir, "irs_pagerank_combined.csv"))
st_write(irs_geo, file.path(output_dir, "irs_pagerank_combined.geojson"), delete_dsn = TRUE)
#write_csv(irs_data, file.path(output_dir, "irs_pagerank_combined.csv"))

# === METRO: Undo temporary changes ===
irs_data <- irs_data %>%
  select(-starts_with("irs_metro_rank_"))


################################################################################
# Cor plots sanity check
################################################################################

# Filter to rank columns only
rank_cols <- names(irs_data)[str_starts(names(irs_data), "irs_metro_eigen_")]
rank_matrix <- irs_data %>%
  select(all_of(rank_cols)) %>%
  drop_na()

# Extract years and sort
years <- str_extract(rank_cols, "\\d{4}") %>% as.integer()
sorted_indices <- order(years)
sorted_cols <- rank_cols[sorted_indices]
year_labels <- years[sorted_indices]

# Subset and rename columns
rank_matrix <- rank_matrix %>% select(all_of(sorted_cols))
colnames(rank_matrix) <- year_labels

# Compute correlation matrix
corr_mat <- cor(rank_matrix, use = "pairwise.complete.obs")

# Plot correlation heatmap
png(file = file.path(output_dir, "irs_rank_correlation_by_year.png"), width = 1000, height = 900, res = 150)
corrplot(corr_mat, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.9,
         title = "Correlations between IRS Metro PageRanks by Year",
         mar = c(0, 0, 3, 0))
dev.off()

# ===== Plot IRS Metro PageRank Correlation Matrix (1995 onward) =====

# Filter to years ≥ 1995
keep_years <- year_labels[year_labels >= 1995]
keep_cols <- colnames(rank_matrix)[year_labels >= 1995]

# Subset rank matrix and recalculate correlation
rank_matrix_1995 <- rank_matrix %>% select(all_of(keep_cols))
corr_mat_1995 <- cor(rank_matrix_1995, use = "pairwise.complete.obs")

# Plot
png(file = file.path(output_dir, "irs_rank_correlation_by_year_1995plus.png"),
    width = 1000, height = 900, res = 150)
corrplot(corr_mat_1995,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,
         title = "Correlations between IRS Metro PageRanks by Year",
         mar = c(0, 0, 3, 0))
dev.off()

# ===== Plot IRS Metro PageRank Correlation Matrix (1997, 2002, 2007, 2017 only) =====

# Define target years
target_years <- c(1997, 2002, 2007, 2017)

# Find columns matching target years
target_cols <- colnames(rank_matrix)[year_labels %in% target_years]

# Subset rank matrix and compute correlation
rank_matrix_subset <- rank_matrix %>% select(all_of(target_cols))
corr_mat_subset <- cor(rank_matrix_subset, use = "pairwise.complete.obs")

# Plot heatmap
png(file = file.path(output_dir, "irs_rank_correlation_selected_years.png"),
    width = 900, height = 800, res = 150)
corrplot(corr_mat_subset,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 1, 
         number.cex = 1.1, 
         addCoef.col = "white",
         title = "Correlations between IRS Metro PageRanks",
         mar = c(0, 0, 4, 0))
dev.off()



################################################################################
# Now process county data
################################################################################

# List relevant files (county-level IRS PageRank)
county_files <- list.files(
  path = input_dir,
  pattern = "^pagerank_irs_Out_\\d{4}\\.csv$",
  full.names = TRUE
)

# Combine into wide format: one row per county (FIPS), columns for each year
county_data <- map(county_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  year <- str_extract(basename(file), "\\d{4}")
  df <- df %>%
    rename(!!paste0("rank_", year) := rank) %>%
    mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"))
}) %>%
  reduce(full_join, by = "fips") %>%
  filter(!is.na(fips))

# Load all U.S. counties from TIGER/Line
options(tigris_use_cache = TRUE)
county_shapes <- counties(cb = TRUE, year = 2023) %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  st_transform(crs = 4326)

# === COUNTY: Rename rank_* to irs_county_eigen_* and create corresponding irs_county_rank_* columns ===
county_data <- county_data %>%
  rename_with(~ str_replace(., "^rank_", "irs_county_eigen_"), starts_with("rank_"))

eigen_cols <- names(county_data)[str_starts(names(county_data), "irs_county_eigen_")]

# Create rank columns with matching names: e.g. irs_county_rank_1990
for (col in eigen_cols) {
  rank_col <- str_replace(col, "eigen_", "rank_")
  county_data[[rank_col]] <- rank(-county_data[[col]], ties.method = "min")
}

# Join PageRank data to geometries
county_geo <- county_shapes %>%
  inner_join(county_data, by = "fips")

# Extract name and state info
county_data_export <- county_geo %>%
  st_drop_geometry() %>%
  transmute(
    name = NAME,
    fips = fips,
    state = STATE_NAME,
    statefp = STATEFP,
    across(starts_with("irs_county_"))
  )

# Save
write_csv(county_data_export, file.path(output_dir, "irs_county_pagerank_combined.csv"))
st_write(county_geo, file.path(output_dir, "irs_county_pagerank_combined.geojson"),
         driver = "GeoJSON", delete_dsn = TRUE)
#write_csv(county_data, file.path(output_dir, "irs_county_pagerank_combined.csv"))

# === COUNTY: Undo temporary changes ===
county_data <- county_data %>%
  select(-starts_with("irs_county_rank"))

library(corrplot)

# Select eigenvector PageRank columns and drop rows with missing data
eigen_cols <- names(county_data)[str_starts(names(county_data), "irs_county_eigen_")]

corr_data <- county_data %>%
  select(all_of(eigen_cols)) %>%
  drop_na()

# Clean column names for labeling (e.g., keep just the year)
colnames(corr_data) <- str_extract(eigen_cols, "\\d{4}")

# Compute correlation matrix
corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")

# Reorder by year (column names are years now, already in chronological order if numeric)
ordered_years <- order(as.numeric(colnames(corr_matrix)))
corr_matrix <- corr_matrix[ordered_years, ordered_years]

# Plot correlation matrix
png(file = file.path(output_dir, "irs_county_pagerank_correlation_by_year.png"),
    width = 1200, height = 1000, res = 150)
corrplot(corr_matrix, method = "color", type = "upper", order = "original",
         tl.col = "black", tl.cex = 1, number.cex = 0.7,
         title = "Correlations between IRS County PageRanks by Year",
         mar = c(0, 0, 3, 0))
dev.off()

# ===== Plot correlation matrix (1995 onward) =====

# Get numeric year labels from colnames
year_labels <- colnames(corr_matrix)
year_nums <- suppressWarnings(as.numeric(year_labels))

# Drop NAs and select only years ≥ 1995
valid_idx <- which(!is.na(year_nums) & year_nums >= 1995)
keep_years <- year_labels[valid_idx]

# Subset and reorder matrix
corr_matrix_1995 <- corr_matrix[keep_years, keep_years]

# Reorder chronologically just in case
ordered <- order(as.numeric(keep_years))
corr_matrix_1995 <- corr_matrix_1995[ordered, ordered]

# Plot
png(file = file.path(output_dir, "irs_county_pagerank_correlation_by_year_1995plus.png"),
    width = 1200, height = 1000, res = 150)
corrplot(corr_matrix_1995,
         method = "color",
         type = "upper",
         order = "original",
         tl.col = "black",
         tl.cex = 1,
         title = "Correlations between IRS County PageRanks by Year",
         mar = c(0, 0, 3, 0))
dev.off()


# ===== Plot IRS County PageRank Correlation Matrix (1997, 2002, 2007, 2017 only) =====

# Define target years
target_years <- c("1997", "2002", "2007", "2017")

# Check which of these years are present in the correlation matrix
present_years <- intersect(colnames(corr_matrix), target_years)

# Subset the correlation matrix
corr_matrix_subset <- corr_matrix[present_years, present_years]

# Plot heatmap
png(file = file.path(output_dir, "irs_county_pagerank_correlation_selected_years.png"),
    width = 1000, height = 850, res = 150)
corrplot(corr_matrix_subset,
         method = "color",
         type = "upper",
         order = "original",
         tl.col = "black",
         tl.cex = 1, 
         number.cex = 1.1, 
         addCoef.col = "white",
         title = "Correlations between IRS County PageRanks",
         mar = c(0, 0, 4, 0))
dev.off()
