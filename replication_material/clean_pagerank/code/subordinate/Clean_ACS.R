
################################################################################
# Take raw ACS data and make clean cuts
# August 2025
################################################################################

library(tidyverse)
library(data.table)

# Load raw ACS once
census_raw <- fread(paste0(wd, "/data/raw/acs_raw.csv"))

# Output directory
output_dir <- paste0(wd, "/data/processed")

years <- 2005:2019

for (start_year in years) {
  message("Processing window: ", start_year, "--", start_year + 4)
  
  census <- census_raw %>%
    filter(
      YEAR %in% start_year:(start_year + 4),
      MIGPUMA1 != 0,
      MIGPUMA1 != 1,
      MIGPLAC1 != 0,
      MIGPLAC1 < 99
    ) %>%
    mutate(
      AGEGRP = case_when(
        AGE >= 25 & AGE <= 35 ~ "25_35",
        AGE > 35 & AGE <= 65 ~ "35_65",
        AGE > 65 ~ "65plus",
        TRUE ~ NA_character_
      ),
      EDUCGRP = case_when(
        EDUC <= 6 ~ "NoCollege",
        EDUC %in% 7:9 ~ "SomeCollege",
        EDUC == 10 ~ "BA",
        EDUC == 11 ~ "Grad",
        TRUE ~ NA_character_
      ),
      RACEGRP = case_when(
        RACE == 2 ~ "Black",
        RACE == 1 & (HISPAN %in% 1:4) ~ "Hispanic",
        RACE == 1 & HISPAN == 0 ~ "White",
        TRUE ~ NA_character_
      ),
      LAB = case_when(
        LABFORCE == 1 ~ "NOLAB",
        LABFORCE == 2 ~ "YESLAB",
        TRUE ~ NA_character_
      ),
      IND1 = case_when(
        !is.na(INDNAICS) ~ substr(INDNAICS, 1, 1),
        TRUE ~ NA_character_
      ),
      MIGMET131 = ifelse(MIGMET131 == 0, STATEFIP + 100000, MIGMET131),
      MET2013 = ifelse(MET2013 == 0, STATEFIP + 100000, MET2013)
    )
  
  suffix <- paste0("_", start_year, ".csv")
  
  total_flows <- census %>%
    group_by(MIGMET131, MET2013) %>%
    summarise(flowHH = sum(HHWT, na.rm = TRUE), flowPER = sum(PERWT, na.rm = TRUE), .groups = "drop")
  
  industry_flows <- census %>%
    filter(!is.na(IND1), !is.na(as.integer(IND1))) %>%
    group_by(MIGMET131, MET2013, IND1) %>%
    summarise(flow = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = IND1, values_from = flow, values_fill = 0)
  
  lab_flows <- census %>%
    filter(!is.na(LAB)) %>%
    group_by(MIGMET131, MET2013, LAB) %>%
    summarise(flow = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = LAB, values_from = flow, values_fill = 0)
  
  industry_flows <- full_join(industry_flows, lab_flows)
  
  age_flows <- census %>%
    filter(!is.na(AGEGRP)) %>%
    group_by(MIGMET131, MET2013, AGEGRP) %>%
    summarise(flow = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = AGEGRP, values_from = flow, values_fill = 0)
  
  educ_flows <- census %>%
    filter(!is.na(EDUCGRP)) %>%
    group_by(MIGMET131, MET2013, EDUCGRP) %>%
    summarise(flow = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = EDUCGRP, values_from = flow, values_fill = 0)
  
  race_flows <- census %>%
    filter(!is.na(RACEGRP)) %>%
    group_by(MIGMET131, MET2013, RACEGRP) %>%
    summarise(flow = sum(PERWT, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = RACEGRP, values_from = flow, values_fill = 0)
  
  flow_kids_combined <- {
    hh <- census %>%
      group_by(SERIAL) %>%
      summarise(
        MIGMET131 = first(MIGMET131),
        MET2013 = first(MET2013),
        HHWT = first(HHWT),
        has_kid = any(RELATE %in% c(3, 4)),
        has_kid_u18 = any(RELATE %in% c(3, 4) & AGE < 18)
      )
    
    full_join(
      hh %>% filter(!has_kid) %>%
        group_by(MIGMET131, MET2013) %>%
        summarise(flow_no_kids = sum(HHWT), .groups = "drop"),
      hh %>% filter(has_kid) %>%
        group_by(MIGMET131, MET2013) %>%
        summarise(flow_with_kids = sum(HHWT), .groups = "drop"),
      by = c("MIGMET131", "MET2013")
    ) %>%
      full_join(
        hh %>% filter(has_kid_u18) %>%
          group_by(MIGMET131, MET2013) %>%
          summarise(flow_kids_u18 = sum(HHWT), .groups = "drop"),
        by = c("MIGMET131", "MET2013")
      ) %>%
      replace_na(list(flow_no_kids = 0, flow_with_kids = 0, flow_kids_u18 = 0))
  }
  
  hh_flags <- census %>%
    group_by(SERIAL) %>%
    summarise(
      MIGMET131 = first(MIGMET131),
      MET2013 = first(MET2013),
      HHWT = first(HHWT),
      head_over_50 = any(RELATE == 1 & AGE > 50, na.rm = TRUE),
      no_one_working = !any(LABFORCE == 2, na.rm = TRUE),
      someone_working = any(LABFORCE == 2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      group1 = head_over_50 & no_one_working,
      group2 = someone_working
    )
  
  flow_head50_no_work <- hh_flags %>%
    filter(group1) %>%
    group_by(MIGMET131, MET2013) %>%
    summarise(flow_head50_no_work = sum(HHWT), .groups = "drop")
  
  flow_someone_working <- hh_flags %>%
    filter(group2) %>%
    group_by(MIGMET131, MET2013) %>%
    summarise(flow_someone_working = sum(HHWT), .groups = "drop")
  
  flow_retired <- full_join(flow_head50_no_work, flow_someone_working,
                            by = c("MIGMET131", "MET2013")) %>%
    replace_na(list(flow_head50_no_work = 0, flow_someone_working = 0))
  
  flow_tenure <- census %>%
    filter(!is.na(OWNERSHP)) %>%
    mutate(TENURE = case_when(
      OWNERSHP == 1 ~ "Owner",
      OWNERSHP %in% c(2, 3) ~ "Renter",
      TRUE ~ NA_character_
    )) %>%
    group_by(MIGMET131, MET2013, TENURE) %>%
    summarise(flow = sum(PERWT), .groups = "drop") %>%
    pivot_wider(names_from = TENURE, values_from = flow, values_fill = 0)
  
  write.csv(total_flows,        file.path(output_dir, paste0("total_flows_metro_rolling5_", start_year, ".csv")), row.names = FALSE)
  write.csv(industry_flows,     file.path(output_dir, paste0("industry_flows_metro_rolling5_", start_year, ".csv")), row.names = FALSE)
  write.csv(age_flows,          file.path(output_dir, paste0("age_flows_metro_rolling5_", start_year, ".csv")), row.names = FALSE)
  write.csv(educ_flows,         file.path(output_dir, paste0("education_flows_metro_rolling5_", start_year, ".csv")), row.names = FALSE)
  write.csv(race_flows,         file.path(output_dir, paste0("race_flows_metro_rolling5_", start_year, ".csv")), row.names = FALSE)
  write.csv(flow_kids_combined, file.path(output_dir, paste0("flow_kids_rolling5_", start_year, ".csv")), row.names = FALSE)
  write.csv(flow_retired,       file.path(output_dir, paste0("flow_retired_rolling5_", start_year, ".csv")), row.names = FALSE)
  write.csv(flow_tenure,        file.path(output_dir, paste0("flow_tenure_rolling5_", start_year, ".csv")), row.names = FALSE)
}


