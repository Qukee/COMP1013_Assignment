# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the CSV files
conditions <- read_csv("C:/Users/Mohamad/Documents/Cyber Security - UNI/Analytics Programming/Assignment/conditionsUG.csv")
encounters <- read_csv("C:/Users/Mohamad/Documents/Cyber Security - UNI/Analytics Programming/Assignment/encountersUG.csv")
patients <- read_csv("C:/Users/Mohamad/Documents/Cyber Security - UNI/Analytics Programming/Assignment/patientsUG.csv")

# Data cleaning
# Remove the unnecessary column `...1`
conditions <- conditions %>% select(-`...1`)
encounters <- encounters %>% select(-`...1`)
patients <- patients %>% select(-`...1`)

# Identify COVID-19 or Suspected COVID-19 patients
covid_conditions <- conditions %>% 
  filter(grepl("COVID|Suspected COVID", DESCRIPTION, ignore.case = TRUE))

# Join with patients data to get gender information
covid_conditions_with_gender <- covid_conditions %>%
  inner_join(patients %>% select(Id, GENDER), by = c("PATIENT" = "Id"))

# Join with encounters data to get Reason Description information by matching PATIENT number
covid_conditions_with_reason <- covid_conditions_with_gender %>%
  inner_join(encounters %>% select(PATIENT, REASONDESCRIPTION), by = "PATIENT")

# Remove rows with N/A Reason Description
covid_conditions_with_reason <- covid_conditions_with_reason %>%
  filter(!is.na(REASONDESCRIPTION))

# Count the frequency of each condition
condition_counts <- covid_conditions_with_reason %>%
  group_by(REASONDESCRIPTION, GENDER) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Get top 10 conditions for each gender
top_conditions_male <- condition_counts %>%
  filter(GENDER == "M") %>%
  top_n(10, count) %>%
  arrange(desc(count))

top_conditions_female <- condition_counts %>%
  filter(GENDER == "F") %>%
  top_n(10, count) %>%
  arrange(desc(count))

# Combine the top conditions into one table
top_conditions_combined <- full_join(
  top_conditions_male %>% rename(Male_Count = count),
  top_conditions_female %>% rename(Female_Count = count),
  by = "REASONDESCRIPTION"
) %>%
  arrange(desc(Male_Count), desc(Female_Count))

print(top_conditions_combined)
