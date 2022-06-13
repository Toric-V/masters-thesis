#install.packages("remotes")
remotes::install_github("rubenarslan/formr")

# Load libraries
library(groundhog)

groundhog.library("fastDummies", '2022-05-31')
groundhog.library("here", '2022-05-31')
groundhog.library("lubridate", '2022-05-31')
library(tidyverse)


# Change dplyr settings so I can view all columns 
options(dplyr.width = Inf)

source("G:/Mit drev/Universitet/Speciale/Coinjoint Survey Code/Credentials.R")

formr::formr_connect(credentials$email,
                     credentials$password)


# or do this to include question labels and 
# automatically reverse-score items whose name ends in "R":
survey_data_background <- formr::formr_results("DemocraticNorms_1")

survey_data_choice_tasks <- formr::formr_results("DemocraticNorms_2")


# Variables common to each survey: 
# session  = a unique ID for the Run - should be the same across all surveys
# created  = time stamp when survey was started
# modified = time stamp when survey was last modified by respondent
# ended    = time stamp when survey ended
# expired  = time stamp when survey expired (if respondent didn't reach end)

data <- merge(survey_data_background,survey_data_choice_tasks,by="session")



data$finished <- ifelse(data$ct20 == 1 | data$ct20 == 2, 1, 0)
data$finished[is.na(data$finished)] = 0


# Create choice data

# First convert the data to long format
choiceData <- data %>% 
  pivot_longer(
    cols = ct1:ct20,
    names_to = "qID",
    values_to = "choice") %>% 
    mutate(respondentID = parse_number(respondentID)) %>% 
  # Convert the qID variable to a number
  mutate(qID = parse_number(qID))


# Read in choice questions and join it to the choiceData
possible_questions <- read_csv("G:/Mit drev/Universitet/Speciale/Coinjoint Survey Code/choice_questions.csv")


# remove columns not used
survey_results <- choiceData [-c(2:8)]

survey_results <- survey_results %>% 
  select(!c(Tax, Police, Weed, created.y, modified.y, ended.y, expired.y, survey, df, ct)) %>%
  mutate(altID = choice) %>% 
  mutate(choice = 1) %>% 
  drop_na(altID)


# create data set of choices not made
alt_choice <- survey_results %>% 
  mutate(choice = 0) %>% 
  mutate(altID = ifelse(altID == 1, 2, 1))

# complete dataset 
temporary <- rbind(survey_results, alt_choice)


final <- temporary %>% 
  left_join(possible_questions, by = c("respondentID" = "respID", "qID" = "qID", "altID" = "altID"))


# Save cleaned data for modeling
write.csv(final, file.path('G:/Mit drev/Universitet/Speciale/Coinjoint Survey Code', 'final_data.csv'))





################################ Diagnostics of all partial responses

nrow(subset(data, Party_Id == 1)) #321 Very democratic
nrow(subset(data, Party_Id == 2)) #172 Mostly democratic
nrow(subset(data, Party_Id == 3)) #39 Identify with neither party
nrow(subset(data, Party_Id == 4)) #67 Mostly republican
nrow(subset(data, Party_Id == 5)) #91 Very republican

nrow(subset(data, Gender == 1)) #401 Males
nrow(subset(data, Gender == 2)) #288 Females
nrow(subset(data, Gender == 3)) #1 Other/Prefer not to say

mean(data$Age) #37.4 years old
median(data$Age) #35 years old

nrow(subset(data, Ethnicity == 1)) #625 white
nrow(subset(data, Ethnicity == 2)) #36 black
nrow(subset(data, Ethnicity == 3)) #13 hispanic
nrow(subset(data, Ethnicity == 4)) #14 asian
nrow(subset(data, Ethnicity == 5)) #2 other/prefer not to say



