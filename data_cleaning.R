# Load necessary libraries for data manipulation and cleaning
library(dplyr)   # For data frame operations
library(tidyr)   # For reshaping and separating data
library(janitor) # For clean data frame handling
library(stringr) # For string manipulation
library(readr)   # For reading/writing CSV files

# Set file paths for input and output
file_path <- "data/graduate_survey.csv"    # Raw survey data location
cleaned_path <- "data/cleaned_data.csv"    # Cleaned data destination

# Verify input file exists to prevent errors
if (!file.exists(file_path)) {
  stop("Input file not found in data folder. Please verify the path and filename.")
}

# Read raw data while suppressing column type warnings
raw_data <- read_csv(file_path, show_col_types = FALSE)

# Process data through a pipeline of cleaning operations
cleaned_data <- raw_data %>%
  # Create unique respondent IDs to track individual records
  mutate(respondent_id = row_number()) %>%
  
  # Standardize column names to lowercase for consistency
  rename_with(tolower) %>%
  
  # Select relevant columns to focus on key variables
  select(
    respondent_id,
    campus, studyfield, branch, role, edulevel,
    proglang, databases, platform, webframework,
    industry, allsearch = aisearch, aitool, employment
  ) %>%
  
  # Handle missing values in categorical fields
  mutate(
    # Replace NA/empty values with "unknown" for demographic fields
    across(c(campus, studyfield, branch, role, industry, employment),
           ~ ifelse(is.na(.) | . == "", "unknown", .)),
    
    # Replace NA/empty values with "none" for technical tool fields
    across(c(proglang, databases, platform, webframework, allsearch, aitool),
           ~ ifelse(is.na(.) | . == "", "none", .))
  ) %>%
  
  # Split multiple employment statuses separated by semicolons
  separate_rows(employment, sep = ";\\s*") %>%
  mutate(employment = str_trim(employment)) %>%  # Remove extra whitespace
  
  # Standardize separators in technical tool columns
  mutate(
    across(c(proglang, databases, platform, webframework, allsearch, aitool),
           ~ str_replace_all(.x, "\\s*;\\s*", ";"))  # Normalize semicolon spacing
  ) %>%
  
  # Standardize campus names using pattern matching
  mutate(
    campus = case_when(
      str_detect(tolower(campus), "durban|umhlanga") ~ "Durban",  # Merge variants
      str_detect(tolower(campus), "pretoria|hatfield") ~ "Pretoria",
      str_detect(tolower(campus), "cape town") ~ "Cape Town",
      campus %in% c("unknown", "na") ~ "Unknown",  # Handle missing values
      TRUE ~ str_to_title(campus)  # Title case for remaining entries
    ),
    
    # Categorize study fields with common patterns
    studyfield = case_when(
      str_detect(tolower(studyfield), "computer science|cs") ~ "Computer Science",
      str_detect(tolower(studyfield), "data science|ds") ~ "Data Science",
      str_detect(tolower(studyfield), "it|information technology") ~ "IT",
      TRUE ~ "Other"  # Group uncommon fields
    )
  ) %>%
  
  # Calculate campus respondent counts and filter top campuses
  add_count(campus, name = "campus_count") %>%  # Count respondents per campus
  filter(campus_count >= quantile(campus_count, 0.75, na.rm = TRUE)) %>%  # Keep top 25% campuses
  select(-campus_count)  # Remove temporary count column

# Save cleaned data to specified location
write_csv(cleaned_data, cleaned_path)

# Function to create standardized tool usage summaries
tech_summary <- function(data, col) {
  data %>%
    distinct(respondent_id, {{col}}) %>%  # Remove duplicate tool entries per respondent
    separate_rows({{col}}, sep = ";\\s*") %>%  # Split multi-tool entries
    filter({{col}} != "none") %>%  # Remove placeholder values
    count({{col}}, name = "count") %>%  # Count tool occurrences
    arrange(desc(count))  # Sort by popularity
}

# Generate and save tool-specific summary reports
write_csv(tech_summary(cleaned_data, proglang), "data/proglang_summary.csv")  # Programming languages
write_csv(tech_summary(cleaned_data, databases), "data/databases_summary.csv")  # Databases
write_csv(tech_summary(cleaned_data, webframework), "data/webframework_summary.csv")  # Web frameworks
write_csv(tech_summary(cleaned_data, aitool), "data/aitool_summary.csv")  # AI tools
