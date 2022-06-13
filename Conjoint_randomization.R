# Make conjoint surveys using the conjointTools package

setwd("G:/Mit drev/Universitet/Speciale/Coinjoint Survey Code")

#Install
#install.packages("remotes")
#remotes::install_github("jhelvy/conjointTools")

# Load libraries
library(groundhog)

groundhog.library("tidyverse", '2022-05-31')
library(conjointTools)

# Define the attributes and levels
levels <- list(
  gender      = c('Male', 'Female'),
  age         = c('30s','40s','50s','60s','70s'),
  race        = c('White', 'Black', 'Hispanic', 'Asian'),
  previous_job= c('Farmer', 'Lawyer', 'Banker', 'Teacher'),
  party = c('Republican', 'Democrat'),
  political_experience = c('Some local-level experience', 'Some state-level experience', 'Some federal-level experience', 'Extensive political experience'),
  democracy = c("Worked on a plan to change their state legislature's committee structure", "Submitted a proposal that would change their state's record-keeping laws and practices", 'Participated in a working group on using program evaluation to inform policymaking',
                'Said the OWN PARTY governor should prosecute journalists who accuse him of misconduct without revealing sources', 'Supported a proposal to reduce the number of polling stations in areas that support OPPOSITE PARTYs', 'Supported a redistricting plan that gives OWN PARTYs 10 extra seats despite a decline in the polls'),
  tax = c('Increase the federal income tax on households earning over $250,000 and increase the federal corporate tax', 'Increase the federal income tax on households earning over $250,000', 'Cut the federal income tax for all households'),
  immigration = c('Local police should not help federal authorities to enforce immigration laws', 'Local police should turn over to federal authorities every illegal immigrant they encounter', 'Local police should aggressively search for illegal immigrants and turn them over to federal authorities'),
  marijuana = c('Marijuana should be legal for recreational use and sold freely', 'Marijuana should only be legal for medical use', 'Marijuana should be illegal for everyone. No exceptions')
)




# Make a full-factorial design of experiment
doe <- makeDoe(levels)
head(doe) # preview


# Recode the levels from numbers to their labels
doe <- recodeDoe(doe, levels)
head(doe) # preview


# Make a basic survey
survey <- makeSurvey(
  doe       = doe,  # Design of experiment
  nResp     = 350, # Total number of respondents (upper bound)
  nAltsPerQ = 2,    # Number of alternatives per question
  nQPerResp = 20     # Number of questions per respondent
)
head(survey) # preview

#Survey also gives the following info:
#respID: Identifies each survey respondent.
#qID: Identifies the choice question answered by the respondent.
#altID:Identifies the alternative in any one choice observation.
#obsID: Identifies each unique choice observation across all respondents.


# Save design
#write.csv(survey, file.path('G:/Mit drev/Universitet/Speciale/Coinjoint Survey Code', 'choice_questions.csv'))

