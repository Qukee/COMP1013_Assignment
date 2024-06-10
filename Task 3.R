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

# Join with patients data to get relevant information
covid_patients <- covid_conditions %>%
  inner_join(patients %>% select(Id, GENDER, AGE = BIRTHDATE, COUNTY, STATE), by = c("PATIENT" = "Id"))

# Calculate age (assuming the data includes the current year for simplicity)
covid_patients <- covid_patients %>%
  mutate(AGE = as.integer(format(Sys.Date(), "%Y")) - as.integer(substr(AGE, 1, 4)))

# Join with encounters data to get encounter class information
covid_encounters <- covid_patients %>%
  inner_join(encounters %>% select(PATIENT, ENCOUNTERCLASS), by = "PATIENT")

# Calculate hospitalisation rates by age and county
hospitalisation_rate <- covid_encounters %>%
  filter(ENCOUNTERCLASS %in% c("ambulatory", "emergency", "inpatient", "urgent care")) %>%
  group_by(AGE, COUNTY) %>%
  summarise(count = n()) %>%
  ungroup()

# Create heatmap
ggplot(hospitalisation_rate, aes(x = AGE, y = COUNTY, fill = count)) +
  geom_tile() +
  labs(title = "Hospitalisation Rate by Age and County for COVID-19 Patients", x = "Age", y = "County") +
  scale_fill_gradient(low ="white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualise the trends by age
ggplot(hospitalisation_rate, aes(x = AGE, y = count)) +
  geom_line() +
  labs(title = "Hospitalisation Rate by Age for COVID-19 Patients", x = "Age", y = "Count of Hospitalisations") +
  theme_minimal()

# Visualise the trends by county
ggplot(hospitalisation_rate, aes(x = COUNTY, y = count, fill = COUNTY)) +
  geom_bar(stat = "identity") +
  labs(title = "Hospitalisation Rate by County for COVID-19 Patients", x = "County", y = "Count of Hospitalisations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform a regression analysis to understand the impact of age and county on hospitalisation rates
model <- lm(count ~ AGE + COUNTY, data = hospitalisation_rate)

# Summary of the regression model
summary(model)

# Include an interaction term between age and county
model_interaction <- lm(count ~ AGE * COUNTY, data = hospitalisation_rate)

# Summary of the regression model with interaction
summary(model_interaction)


