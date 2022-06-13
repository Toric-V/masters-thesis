setwd("G:/Mit drev/Universitet/Speciale/Coinjoint Survey Code")

final_data <- read.csv("G:/Mit drev/Universitet/Speciale/Coinjoint Survey Code/final_data.csv")



####################RECODING VARIABLES#########################

#create co-partisan variable.
#true/false dummy
final_data$copartisan <- ifelse(final_data$Party_Id<3 & final_data$party == "Democrat", 1, 0) | ifelse(final_data$Party_Id>3 & final_data$party == "Republican", 1, 0)
#true recoded to 1
final_data$copartisan <- ifelse(final_data$copartisan == TRUE, 1,0)


#recode democracy variable into dichotomous variable
final_data$democracy_binary <- ifelse(final_data$democracy == "Supported a proposal to reduce the number of polling stations in areas that support OPPOSITE PARTYs" |
                                      final_data$democracy == "Supported a redistricting plan that gives OWN PARTYs 10 extra seats despite a decline in the polls" |
                                      final_data$democracy == "Said the OWN PARTY governor should prosecute journalists who accuse him of misconduct without revealing sources", 0, 1) | 
                               ifelse(final_data$democracy == "Submitted a proposal that would change their state's record-keeping laws and practices" |
                                      final_data$democracy == "Worked on a plan to change their state legislature's committee structure" |
                                      final_data$democracy == "Participated in a working group on using program evaluation to inform policymaking", 1, 0)
#true recoded to 1
final_data$democracy_binary <- ifelse(final_data$democracy_binary == TRUE, 1,0)




#code policy proximity
tax1_q = "Increase the federal income tax on households earning over $250,000 and increase"
tax2_q = "Increase the federal income tax on households earning over $250,000"
tax3_q = "Decrease the federal income tax for all households"

police1_q = "Local police should not help federal authorities"
police2_q = "Local police should turn over to federal authorities"
police3_q = "Local police should aggressively"

weed1_q = "Marijuana should be legal for recreational use"
weed2_q = "Marijuana should only be legal for medical use"
weed3_q = "Marijuana should be illegal for everyone"


library(groundhog)
groundhog.library("dplyr", '2022-05-31')


final_data <- final_data %>% 
  mutate(tax_agree = ifelse(grepl(tax1_q, tax, fixed = TRUE), # test if tax1_q is in the tax column
                            Tax1, # if yes, the tax_agree column is set to Tax1 value
                            ifelse(grepl(tax2_q, tax, fixed = TRUE), # if no, test if tax2_q is in tax column
                                   Tax2, # if yes, tax_agree is set to Tax2
                                   Tax3))) %>%  # Otherwise set it to Tax3 
  mutate(police_agree = ifelse(grepl(police1_q, immigration, fixed = TRUE), 
                               Police1, 
                               ifelse(grepl(police2_q, immigration, fixed = TRUE),
                                      Police2, 
                                      Police3))) %>%
  mutate(weed_agree = ifelse(grepl(weed1_q, marijuana, fixed = TRUE), 
                             Weed1, 
                             ifelse(grepl(weed2_q, marijuana, fixed = TRUE),
                                    Weed2, 
                                    Weed3))) %>% 
  mutate(policy_proximity = tax_agree + police_agree + weed_agree)

final_data$policy_proximity <- final_data$policy_proximity - 3

median(final_data$policy_proximity) #9 is the median number from 0-12


############################################# ANALYSIS ####################################


final_data$choice <- as.numeric(final_data$choice)

final_data$gender <- as.factor(final_data$gender)
final_data$copartisan <- as.factor(final_data$copartisan)
final_data$age <- as.factor(final_data$age)
final_data$race <- as.factor(final_data$race)
final_data$previous_job <- as.factor(final_data$previous_job)
final_data$democracy_binary <- as.factor(final_data$democracy_binary)
final_data$political_experience <- as.factor(final_data$political_experience)




baselines <- list()
baselines$political_experience <- "Some local-level experience"
baselines$race <- "White"



#cjoint analysis
#amce function also enables interaction analysis - check package documentation y ~ var1 * var2

#install.packages("cjoint")

#groundhog.library("cjoint", '2022-05-31')

library("cjoint")

final_data$policy_proximity <- as.factor(final_data$policy_proximity)


Append_H1 <- amce(formula = choice ~  gender + copartisan + age + race + previous_job + democracy_binary + policy_proximity + political_experience,
           data = final_data, design = "uniform",
           respondent.varying = NULL, subset = NULL,
           respondent.id = "session", cluster = TRUE, na.ignore=FALSE,
           weights = NULL, baselines = baselines)


plot.amce(Append_H1)


final_data$policy_proximity <- as.numeric(final_data$policy_proximity)

#Making Policy Proximity into 2 categories instead of 13
final_data$policy_proximity <- ifelse(final_data$policy_proximity>8, "high", "low")
#below 9 is low - 9 and above is high

baselines$policy_proximity <- "low"


final_data$policy_proximity <- as.factor(final_data$policy_proximity)



H1 <- amce(formula = choice ~  gender + copartisan + age + race + previous_job + democracy_binary + policy_proximity + political_experience,
     data = final_data, design = "uniform",
     respondent.varying = NULL, subset = NULL,
     respondent.id = "session", cluster = TRUE, na.ignore=FALSE,
     weights = NULL, baselines = baselines)




H2 <- amce(formula = choice ~  gender + copartisan + age + race + previous_job + democracy_binary + policy_proximity + political_experience + democracy_binary*copartisan,
                data = final_data, design = "uniform",
                respondent.varying = NULL, subset = NULL,
                respondent.id = "session", cluster = TRUE, na.ignore=FALSE,
                weights = NULL, baselines = baselines)

H3 <- amce(formula = choice ~  gender + copartisan + age + race + previous_job + democracy_binary + policy_proximity + political_experience + democracy_binary*policy_proximity,
           data = final_data, design = "uniform",
           respondent.varying = NULL, subset = NULL,
           respondent.id = "session", cluster = TRUE, na.ignore=FALSE,
           weights = NULL, baselines = baselines)


########### Results

plot.amce(H1)

summary.amce(H2) # to check interaction coefficient

summary.amce(H3) # to check interaction coefficient


library("cregg")


final_data$copartisan <- as.factor(ifelse(final_data$copartisan == 1, 'true', 'false'))


H2_mm_by <- cj(data=final_data, formula = choice ~ copartisan, id = ~session, estimate = "mm", by = ~democracy_binary)
plot(H2_mm_by, group = "democracy_binary", vline = 0.5)

final_data$policy_proximity <- factor(final_data$policy_proximity, levels=c("low", "high"))
H3_mm_by <- cj(data=final_data, formula = choice ~ policy_proximity, id = ~session, estimate = "mm", by = ~democracy_binary)
plot(H3_mm_by, group = "democracy_binary", vline = 0.5)


############ Diagnostics
final_data$democracy <- as.factor(final_data$democracy)
final_data$party <- as.factor(final_data$party)
final_data$tax <- as.factor(final_data$tax)
final_data$immigration <- as.factor(final_data$immigration)
final_data$marijuana <- as.factor(final_data$marijuana)


plot(cj_freqs(final_data, choice ~  gender + party + age + race + previous_job + political_experience + democracy + tax + immigration + marijuana, id = ~session))
#plots the frequencies of each level of each attribute





#Check for carryover-effects
final_data$question_number <- final_data$qID

final_data$question_number <- ifelse(final_data$qID<6, "1-5", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==6, "6-10", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==7, "6-10", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==8, "6-10", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==9, "6-10", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==10, "6-10", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==11, "11-15", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==12, "11-15", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==13, "11-15", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==14, "11-15", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID==15, "11-15", final_data$question_number)
final_data$question_number <- ifelse(final_data$qID>15, "16-20", final_data$question_number)


final_data$question_number <- factor(final_data$question_number, levels=c("1-5", "6-10","11-15","16-20"))

final_data$finished <- factor(final_data$finished)




H_finshed <- cj(formula = choice ~  gender + copartisan + age + race + previous_job + democracy_binary + policy_proximity + political_experience,
           data = subset(final_data, finished==1), id =  ~  session, estimate = "amce", by=~question_number)
plot(H_finshed) + facet_wrap("question_number")


H_unfinished <- cj(formula = choice ~  gender + copartisan + age + race + previous_job + democracy_binary + policy_proximity + political_experience,
                data = subset(final_data, finished==0), id =  ~  session, estimate = "amce", by=~question_number)
plot(H_unfinished) + facet_wrap("question_number")


H_all <- cj(formula = choice ~  gender + copartisan + age + race + previous_job + democracy_binary + policy_proximity + political_experience,
                   data = final_data, id =  ~  session, estimate = "amce", by=~question_number)
plot(H_all) + facet_wrap("question_number")
