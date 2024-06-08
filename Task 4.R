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

# Join with patients data to get demographic information and recovery status
covid_patients <- covid_conditions %>%
  inner_join(patients %>% select(Id, GENDER, BIRTHDATE, DEATHDATE, ZIP, COUNTY, STATE), by = c("PATIENT" = "Id"))

# Calculate age (assuming the data includes the current year for simplicity)
covid_patients <- covid_patients %>%
  mutate(AGE = as.integer(format(Sys.Date(), "%Y")) - as.integer(substr(BIRTHDATE, 1, 4)))

# Determine recovery status
covid_patients <- covid_patients %>%
  mutate(RECOVERY_STATUS = if_else(is.na(DEATHDATE), "Recovered", "Not Recovered"))

# Join with encounters data to get encounter class information
covid_encounters <- covid_patients %>%
  inner_join(encounters %>% select(PATIENT, REASONDESCRIPTION, START, STOP), by = "PATIENT")

# Filter out patients with no encounter data
covid_encounters <- covid_encounters %>%
  filter(!is.na(REASONDESCRIPTION))

# Analysis of demographics and symptoms by recovery status
demographics_analysis <- covid_encounters %>%
  group_by(RECOVERY_STATUS, GENDER, AGE, ZIP, COUNTY, STATE) %>%
  summarise(count = n()) %>%
  ungroup()

symptoms_analysis <- covid_encounters %>%
  group_by(RECOVERY_STATUS, REASONDESCRIPTION) %>%
  summarise(count = n()) %>%
  ungroup()

# Visualize demographics impact on recovery outcome
ggplot(demographics_analysis, aes(x = AGE, y = count, fill = RECOVERY_STATUS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Age on COVID-19 Recovery", x = "Age", y = "Count") +
  theme_minimal()

ggplot(demographics_analysis, aes(x = GENDER, y = count, fill = RECOVERY_STATUS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Gender on COVID-19 Recovery", x = "Gender", y = "Count") +
  theme_minimal()

ggplot(demographics_analysis, aes(x = COUNTY, y = count, fill = RECOVERY_STATUS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of County on COVID-19 Recovery", x = "County", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize symptoms impact on recovery outcome
ggplot(symptoms_analysis, aes(x = reorder(REASONDESCRIPTION, count), y = count, fill = RECOVERY_STATUS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Symptoms on COVID-19 Recovery", x = "Symptom", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Elaborate on the findings
