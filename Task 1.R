# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the CSV files
conditions <- read_csv("C:/Users/Mohamad/Documents/Cyber Security - UNI/Analytics Programming/Assignment/conditionsUG.csv")
encounters <- read_csv("C:/Users/Mohamad/Documents/Cyber Security - UNI/Analytics Programming/Assignment/encountersUG.csv")
patients <- read_csv("C:/Users/Mohamad/Documents/Cyber Security - UNI/Analytics Programming/Assignment/patientsUG.csv")

# Inspect the data
glimpse(conditions)
glimpse(encounters)
glimpse(patients)

# Summary of the data
summary(conditions)
summary(encounters)
summary(patients)

# Data cleaning
# Remove the unnecessary column `...1`
conditions <- conditions %>% select(-`...1`)
encounters <- encounters %>% select(-`...1`)
patients <- patients %>% select(-`...1`)

# Identify COVID-19 patients (assuming 'COVID-19'is in the DESCRIPTION)
covid_conditions <- conditions %>% 
  filter(grepl("COVID|Suspected COVID", DESCRIPTION, ignore.case = TRUE))

# Join with patients data to get demographic information
covid_patients <- covid_conditions %>%
  inner_join(patients, by = c("PATIENT" = "Id"))

# Create age groups
covid_patients <- covid_patients %>%
  mutate(age = as.integer(format(Sys.Date(), "%Y")) - as.integer(substr(BIRTHDATE, 1, 4)),
         age_group = case_when(
           age <= 18 ~ "0-18",
           age <= 35 ~ "19-35",
           age <= 50 ~ "36-50",
           age > 50 ~ "51+"
         ))

# Analyze the top 10 most common conditions
top_conditions <- covid_patients %>%
  count(DESCRIPTION) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

# Analyze the distribution of COVID patients across age groups
covid_age_distribution <- covid_patients %>%
  count(age_group)

# Analyze the distribution of COVID patients across counties
covid_county_distribution <- covid_patients %>%
  count(COUNTY)

# Check for missing values
colSums(is.na(conditions))
colSums(is.na(encounters))
colSums(is.na(patients))

## The above R Code is for the creation, manipulation and cleaning of data 
## The below R Code is for the creation and displaying of our data
  
# Plot the distribution of COVID patients across age groups
ggplot(covid_age_distribution, aes(x = age_group, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of COVID Patients Across Age Groups", x = "Age Group", y = "Number of COVID Patients")

# Plot the distribution of COVID patients across counties
ggplot(covid_county_distribution, aes(x = reorder(COUNTY, n), y = n)) + 
  geom_bar(stat = "identity", fill = "red", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution of COVID Patients Across Counties", x = "County", y = "Number of COVID Patients")
