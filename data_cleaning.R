library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(readr)

# Set file paths
file_path <- "data/graduate_survey.csv"
cleaned_path <- "data/cleaned_data.csv"

# Verify input file exists
if (!file.exists(file_path)) {
  stop("Input file not found in data folder. Please verify the path and filename.")
}

# Read raw data
raw_data <- read_csv(file_path, show_col_types = FALSE)

# Process data
cleaned_data <- raw_data %>%
  mutate(respondent_id = row_number()) %>%
  rename_with(tolower) %>%
  select(
    respondent_id,
    campus, studyfield, branch, role, edulevel,
    proglang, databases, platform, webframework,
    industry, allsearch = aisearch, aitool, employment
  ) %>%
  mutate(
    across(c(campus, studyfield, branch, role, industry, employment),
           ~ ifelse(is.na(.) | . == "", "unknown", .)),
    across(c(proglang, databases, platform, webframework, allsearch, aitool),
           ~ ifelse(is.na(.) | . == "", "none", .))
  ) %>%
  separate_rows(employment, sep = ";\\s*") %>%
  mutate(employment = str_trim(employment)) %>%
  mutate(
    across(c(proglang, databases, platform, webframework, allsearch, aitool),
           ~ str_replace_all(.x, "\\s*;\\s*", ";"))
  ) %>%
  mutate(
    campus = case_when(
      str_detect(tolower(campus), "durban|umhlanga") ~ "Durban",
      str_detect(tolower(campus), "pretoria|hatfield") ~ "Pretoria",
      str_detect(tolower(campus), "cape town") ~ "Cape Town",
      campus %in% c("unknown", "na") ~ "Unknown",
      TRUE ~ str_to_title(campus)
    ),
    studyfield = case_when(
      str_detect(tolower(studyfield), "computer science|cs") ~ "Computer Science",
      str_detect(tolower(studyfield), "data science|ds") ~ "Data Science",
      str_detect(tolower(studyfield), "it|information technology") ~ "IT",
      TRUE ~ "Other"
    )
  ) %>%
  # Calculate campus counts and keep only top 3
  add_count(campus, name = "campus_count") %>%
  filter(campus_count >= quantile(campus_count, 0.75, na.rm = TRUE)) %>% 
  select(-campus_count) 

# Save outputs to data folder
write_csv(cleaned_data, cleaned_path)

# Summary function
tech_summary <- function(data, col) {
  data %>%
    distinct(respondent_id, {{col}}) %>%
    separate_rows({{col}}, sep = ";\\s*") %>%
    filter({{col}} != "none") %>%
    count({{col}}, name = "count") %>%
    arrange(desc(count))
}

# Generate summaries and save to data folder
write_csv(tech_summary(cleaned_data, proglang), "data/proglang_summary.csv")
write_csv(tech_summary(cleaned_data, databases), "data/databases_summary.csv")
write_csv(tech_summary(cleaned_data, webframework), "data/webframework_summary.csv")
write_csv(tech_summary(cleaned_data, aitool), "data/aitool_summary.csv")
