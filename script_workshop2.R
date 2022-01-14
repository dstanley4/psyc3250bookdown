# Date: YYYY-MM-DD
# Name: your name here
# Example: SWorkshop 2 PSYC 3250

# Activate packages  --------------------------------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(sjstats)

# Load data --------------------------------------------------------

my_missing_value_codes <- c("-999", "", "NA")

raw_data_survey <- read_csv(file = "data_aff_survey.csv",
                            na = my_missing_value_codes)


analytic_data_survey <- raw_data_survey


# Clean and screen --------------------------------------------------------

# Initial cleaning
analytic_data_survey <- analytic_data_survey %>%
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  clean_names()

# Numeric screening
analytic_data_survey %>%
  select(aff_com2_likert7, aff_com3_likert7, aff_com4_likert7rev) %>%
  skim()

# Flipping responses to reverse-key items --------------------------------------------------------

head(analytic_data_survey)

analytic_data_survey <- analytic_data_survey %>% 
  mutate(8 - across(.cols = ends_with("_likert7rev")) ) %>% 
  rename_with(.fn = str_replace,
              .cols = ends_with("_likert7rev"),
              pattern = "_likert7rev",
              replacement = "_likert7")

head(analytic_data_survey)

# Creating scale scores --------------------------------------------------------

analytic_data_survey <- analytic_data_survey %>% 
  rowwise() %>% 
  mutate(affect_mean = mean(c_across(starts_with("aff_com")),
                                     na.rm = TRUE)) %>%
  ungroup() 


head(analytic_data_survey)

# Descriptive statistics --------------------------------------------------------

analytic_data_survey %>%
  skim()


analytic_data_survey %>%
  summarise(mean_affect_mean_column = mean(affect_mean, na.rm = TRUE),
            var_affect_mean_column = var(affect_mean, na.rm = TRUE))

