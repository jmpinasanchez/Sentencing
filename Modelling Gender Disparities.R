
#########################################
###Gender Disparities####################
#########################################

library(dplyr) #This is to be able to use %>%
library(car)  #This is to use vif
library(mice)
library(EValue) #https://louisahsmith.github.io/evalue/index.html


options(scipen=999) #To disable scientific notation
set.seed(7) #For the sampling process

#Importing the data
theft_post = read.csv("Post-guideline-main-dataset-Theft-from-a-shop-or-stall.csv",
                      fileEncoding="latin1")

#Quick exploratory analysis
head(theft_post)
prop.table(table(theft_post$outcome, useNA="ifany"))

#Dropping variables not relevant
vars = names(theft_post) %in% c("uniqueid","outcome_before_GP","GP_when","GP_fewerrequirements","GP_reduction",
                                "GP_percentreduction","GP_droppedthreshold","outcome_custody_measure","outcome_custody_length",
                                "outcome_custody_years","outcome_suspension_years","totality_adjustment","starting_point",
                                "GP_entered") 
theft_post = theft_post[!vars]
lapply(theft_post, table)

#Dropping cases not relevant
theft_post = theft_post[which(theft_post$outcome!="Crown Court"),]
lapply(theft_post, table)

#Recoding
#Suspended and immediate custody together
table(theft_post$outcome, useNA = "ifany")  
theft_post$cust = ifelse(theft_post$outcome=="Unknown/Missing",NA,
                  ifelse(theft_post$outcome=="Suspended sentence order"|
                         theft_post$outcome=="Immediate Custody",1,0))
theft_post$outcome = NULL 
table(theft_post$cust, useNA = "ifany")
prop.table(table(theft_post$cust))

#Harm category - we recode it so a higher value is more harm
table(theft_post$harm_category, useNA = "ifany")
theft_post$harm_category = ifelse(theft_post$harm_category=="Unknown/Missing",NA,
                           ifelse(theft_post$harm_category=="Category 3",1,
                           ifelse(theft_post$harm_category=="Category 2",2,3)))
table(theft_post$harm_category, useNA = "ifany")
#Culpability category
table(theft_post$culpability_category, useNA = "ifany")
theft_post$culpability_category = ifelse(theft_post$culpability_category=="Unknown/Missing",NA,
                                  ifelse(theft_post$culpability_category=="C - Lesser",1,
                                  ifelse(theft_post$culpability_category=="B - Medium",2,3)))
table(theft_post$culpability_category, useNA = "ifany")
#Value goods
table(theft_post$value_goods, useNA = "ifany")
theft_post$value_goods = ifelse(theft_post$value_goods=="Unknown/Missing",NA,
                         ifelse(theft_post$value_goods=="Up to £10",1,
                         ifelse(theft_post$value_goods=="£11-£50",2,
                         ifelse(theft_post$value_goods=="£51-£100",3,                                
                         ifelse(theft_post$value_goods=="£101-£200",4,                                
                         ifelse(theft_post$value_goods=="£201-£500",5,                                
                         ifelse(theft_post$value_goods=="£501-£1000",6,7)))))))
table(theft_post$value_goods, useNA = "ifany")
#Role
table(theft_post$culp_role, useNA = "ifany")
theft_post$culp_role = ifelse(theft_post$culp_role=="Unknown/Missing",NA, 
                       ifelse(theft_post$culp_role=="Lone"|theft_post$culp_role=="Limited",1,
                       ifelse(theft_post$culp_role=="Significant",2,3)))
table(theft_post$culp_role, useNA = "ifany")
#Force threat
table(theft_post$culp_forcethreat, useNA = "ifany")
theft_post$culp_forcethreat = ifelse(theft_post$culp_forcethreat=="Unknown/Missing",NA, 
                              ifelse(theft_post$culp_forcethreat=="None",1,
                              ifelse(theft_post$culp_forcethreat=="Limited or low level",2,3)))
table(theft_post$culp_forcethreat, useNA = "ifany")
#Planning
table(theft_post$culp_planning, useNA = "ifany")
theft_post$culp_planning = ifelse(theft_post$culp_planning=="Unknown/Missing",NA,
                           ifelse(theft_post$culp_planning=="Little or no planning",1,
                           ifelse(theft_post$culp_planning=="Some planning",2,3)))
table(theft_post$culp_planning, useNA = "ifany")
#Gender
table(theft_post$gender, useNA = "ifany")
theft_post$male = ifelse(theft_post$gender=="Unknown/Missing",NA,
                  ifelse(theft_post$gender=="Male",1,0))
theft_post$gender = NULL
table(theft_post$male, useNA = "ifany")
#Age
table(theft_post$age_band, useNA = "ifany")
theft_post$age = ifelse(theft_post$age_band=="Unknown/Missing",NA,
                 ifelse(theft_post$age_band=="30 to 39",3,
                 ifelse(theft_post$age_band=="18 to 21",1,    
                 ifelse(theft_post$age_band=="22 to 29",2,  
                 ifelse(theft_post$age_band=="40 to 49",4,  
                 ifelse(theft_post$age_band=="50 to 59",5, 6)))))) 
theft_post$age_band = NULL
table(theft_post$age, useNA = "ifany")
#Previous convictions
table(theft_post$agg_precons, useNA = "ifany")
theft_post$agg_precons = ifelse(theft_post$agg_precons=="No",0,
                         ifelse(theft_post$agg_precons=="Unknown/Missing",NA,1))
table(theft_post$agg_precons, useNA = "ifany")
#Previous convictions (amount)
table(theft_post$agg_precons_amount, useNA="ifany")
table(theft_post$agg_precons_amount, theft_post$agg_precons) #Not applicable means no previous convictions
theft_post$agg_precons = ifelse(theft_post$agg_precons_amount=="None"|theft_post$agg_precons_amount=="Not applicable", 0,
                         ifelse(theft_post$agg_precons_amount=="1 to 3",2, 
                         ifelse(theft_post$agg_precons_amount=="4 to 9",6,
                         ifelse(theft_post$agg_precons_amount=="10 to 19",14,
                         ifelse(theft_post$agg_precons_amount=="20 or more",25,NA))))) 
table(theft_post$agg_precons, useNA="ifany")
theft_post$agg_precons_amount = NULL
#First opportunity
table(theft_post$GP_firstopportunity, useNA = "ifany")
theft_post$first = ifelse(theft_post$GP_firstopportunity=="Yes", 1,
                   ifelse(theft_post$GP_firstopportunity=="Unknown/Missing", NA, 0))
table(theft_post$first, useNA = "ifany")
theft_post$GP_firstopportunity = NULL

#Removing factors with less than 1% prevalence
lapply(theft_post, function(x) prop.table(table(x)))
vars = names(theft_post) %in% c("culp_other_usedchild","culp_other_involveothers","harm_damage","agg_widerimpact") 
theft_post = theft_post[!vars]

#I remove "otherfactors" as this is a hotchpotch.
theft_post$harm_otherfactors = NULL
theft_post$agg_otherfactors = NULL
theft_post$mit_otherfactors = NULL

#Exploring associations with gender
lapply(theft_post, function(x) prop.table(table(x, theft_post$male),2)) 
#Culpability role =1 is 0.71 for women and 0.78 for men - this means that women are taken as more culpable
#Remorse is 0.20 for women and 0.15 for men, although first is 0.18 for women and 0.15 for men
#Offending whilst on bail is 0.12 for women and 0.19 for men - use this as an example of post-treatment bias
#No previous convictions is 0.22 for women and 0.16 for men - use this as an example of post-treatment bias
#Miti no conviction 0.11 for women and 0.7 for men
#The proportion who reoffend in the pivot tables is 0.53 for men and 0.42 for women
aggregate(agg_precons ~ male, theft_post, mean)  #This is 4.8 female and 5.1 male in the pivot tables

#Renaming the dataset
data = theft_post



###########################################
###Descriptive stats#######################
###########################################

lapply(data, function(x) prop.table(table(x, useNA = "ifany"))) #cust=0  70.9%
summary(data)



###########################################
###Imputation##############################
###########################################

#Missing data pattern
md.pattern(data)

#Imputation
data_mi = mice(data, m = 5, method = "pmm")



###########################################
###Models##################################
###########################################

#Empty model
model1 = glm(cust~male, data=data, family="binomial")
summary(model1)
exp(model1$coefficients)
#With imputed data
model1_mi = with(data_mi, glm(cust~male, family="binomial"))
summary(pool(model1_mi),conf.int = TRUE)
exp(summary(pool(model1_mi),conf.int = TRUE)[,c(2,7,8)])


#Model including pretreatment case characteristics
model2 = glm(cust ~ male + culp_other_banning + value_goods + agg_TICs + 
                    agg_failuretocomply + agg_bail + agg_stealing + mit_return + 
                    mit_carer + age + first, 
                    data=data, family="binomial")
summary(model2)
exp(model2$coefficients)
#With imputed data
model2_mi = with(data_mi, glm(cust~ male + culp_other_banning + value_goods + 
                                agg_TICs + agg_failuretocomply + agg_bail + agg_stealing + 
                                mit_return + mit_carer + age + first, 
                                family="binomial"))
summary(pool(model2_mi))
cbind(summary(pool(model2_mi),conf.int = TRUE)[,1], exp(summary(pool(model2_mi),conf.int = TRUE)[,c(2,7,8)]))

#Model including posttreatment case characteristics
model3 = glm(cust~., data=data, family="binomial")
summary(model3)
exp(model3$coefficients)
vif(model3)
#With imputed data
model3_mi = with(data_mi, glm(cust~male + culp_planning + culp_forcethreat + culp_role + culp_other_sophisticated + culp_other_banning + culp_other_coerced +
                                culp_other_disorder + value_goods + harm_emotional + harm_injury + harm_effect + 
                                agg_precons + agg_attemptstoconceal + agg_failuretocomply + agg_bail + agg_TICs + agg_prevalence +
                                agg_professional + agg_stealing + mit_age + mit_finhardship + mit_steps +
                                mit_disorder + mit_return + mit_condition + mit_carer + age + first +
                                mit_character + mit_remorse , 
                                family="binomial"))
summary(pool(model3_mi))
cbind(summary(pool(model3_mi),conf.int = TRUE)[,1], exp(summary(pool(model3_mi),conf.int = TRUE)[,c(2,7,8)]))



###########################################
###Evalue##################################
###########################################


#Translating OR into RR
P0 = 0.235 #Custody rate for the control group - women.

#Model 2
#The Point estimate
OR_m2 = exp(summary(pool(model2_mi))[2,2])   #point estimate model 3
RR_m2 = OR_m2/(1-P0+(P0*OR_m2)) #risk ratios: RR = OR / (1 - p + (p x OR))
#The lower 95%CI bound
OR_m2_lo = exp(summary(pool(model2_mi))[2,2] - 1.96*summary(pool(model2_mi))[2,3]) 
RR_m2_lo = OR_m2_lo/(1-P0+(P0*OR_m2_lo)) 
#The higher 95%CI bound
OR_m2_hi = exp(summary(pool(model2_mi))[2,2] + 1.96*summary(pool(model2_mi))[2,3]) 
RR_m2_hi = OR_m2_hi/(1-P0+(P0*OR_m2_hi)) 

#Model 3
#The Point estimate
OR_m3 = exp(summary(pool(model3_mi))[2,2])   #point estimate model 3
RR_m3 = OR_m3/(1-P0+(P0*OR_m3)) #risk ratios: RR = OR / (1 - p + (p x OR))
#The lower 95%CI bound
OR_m3_lo = exp(summary(pool(model3_mi))[2,2] - 1.96*summary(pool(model3_mi))[2,3]) 
RR_m3_lo = OR_m3_lo/(1-P0+(P0*OR_m3_lo)) 
#The higher 95%CI bound
OR_m3_hi = exp(summary(pool(model3_mi))[2,2] + 1.96*summary(pool(model3_mi))[2,3]) 
RR_m3_hi = OR_m3_hi/(1-P0+(P0*OR_m3_hi)) 

#The 95%CI for male in model3
evalues.RR(est = RR_m3, lo = RR_m3_lo, hi = RR_m3_hi, true=1) #E-Value = 1.64
bias_plot(1.257, xmax=7)





###########################################
###Specification Curve#####################
###########################################

library(gtools)
library(ggplot2)
library(ggpubr)

#Subjectively defined factors##############
subjective = c('culp_planning', 'culp_forcethreat', 'culp_role', 'culp_other_sophisticated', 
               'harm_injury', 'harm_emotional', 'harm_effect', 'agg_precons', 'agg_attemptstoconceal', 
               'agg_prevalence', 'agg_professional', 'mit_finhardship', 'mit_steps', 
               'mit_condition', 'mit_disorder') 

#Number of combinations####################
# Number of items
n <- length(subjective)
# Initialize an empty list to store all unique combinations
all_combinations <- list()
# Set the seed for reproducibility
set.seed(7)
# Loop over all possible k values (from 1 to n)
for (k in 1:n) {
  # Get combinations without replacement for each k
  combs <- combn(subjective, k)
  # If the combinations have fewer than n columns, pad them with NA
  if (k < n) {
    combs <- rbind(combs, matrix(NA, nrow = n - k, ncol = ncol(combs)))
  }
  # Transpose the combinations so each row represents a combination
  all_combinations[[k]] <- t(combs)
}
# Combine all results into one matrix
final_combinations <- do.call(rbind, all_combinations)
# Print the final combinations
length(final_combinations)
final_combinations

#Taking a random sample of 200 combinations
n <- 100
num_combinations <- nrow(final_combinations)
random_sample <- final_combinations[sample(1:num_combinations, n), ]
random_sample

#Model uncertainty##############################################################

# Initialize a vector to store the 'male' coefficients
male_coefficients <- numeric(nrow(random_sample))
# Loop through each combination in random_sample
for (i in 1:nrow(random_sample)) {
  # Extract the non-NA variables from the current row of random_sample
  additional_vars <- na.omit(random_sample[i, ])
  # Create the formula by adding the additional variables to the benchmark model
  formula_str <- paste("cust ~ male + culp_other_banning + value_goods + 
                        agg_TICs + agg_failuretocomply + agg_bail + agg_stealing  + 
                        mit_return + mit_carer + age + first +", 
                        paste(additional_vars, collapse = " + "))
  # Convert the formula string to a formula object
  model_formula <- as.formula(formula_str)
  # Fit the logistic regression model
  model <- with(data_mi, glm(eval(parse(text = formula_str)), family="binomial"))
  # Extract the coefficient for 'male'
  pooled_model <- pool(model)
  male_coefficients[i] <- pooled_model$pooled[2,3]
}
# Print the coefficients for 'male'
male_coefficients
#Turn them into odds ratios
male_coefficients = exp(male_coefficients)
#Calculating 95% CI
quantile(male_coefficients, probs = c(0.025, 0.975))


# The plot
# Create a data frame from the 'male' coefficients
male_coefficients_df <- data.frame(coefficient = male_coefficients)
# Create a density plot using ggplot
gender_plot = 
  ggplot(male_coefficients_df, aes(x = coefficient)) +
  geom_density(fill = "lightblue", alpha = 0.6, color = "black") +  
  geom_vline(xintercept = 1, color = "grey", linetype = "dashed", size = 0.8) +  
  labs(title = "",
       x = "odds ratio of custody between male and female offenders",
       y = "") +  
  xlim(c(0.9, 1.9)) +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(
    plot.title = element_blank(),  
    axis.title.x = element_text(family="serif", size = 12),  
    axis.text.y = element_blank(),
    #axis.title.y = element_text(family="serif", size = 12),  
    axis.text = element_text(family="serif", size = 10)  
  )
ggexport(gender_plot, width=1500, height=700, res=320,
         filename="gender.jpeg")

#To investigate the most influential predictors
#Creating a matrix to store the data
subjective_matrix <- matrix(0, nrow = n, ncol = length(subjective))
colnames(subjective_matrix) <- subjective
# Convert the matrix of samples into a data frame
random_sample_df <- as.data.frame(random_sample)
random_sample_df
#The loop
for (i in 1:n) {
  selected_items <- random_sample_df[i, ]  # Get the items selected in this iteration
  selected_items <- selected_items[!is.na(selected_items)]  # Remove NAs
  subjective_matrix[i, selected_items] <- 1  # Mark the selected items as 1
}
subjective_matrix
#Merging the disparities estimates and predictors
subjective_data = cbind(male_coefficients_df, subjective_matrix)
#The model
formula_str <- paste("coefficient ~ ", paste(subjective, collapse = " + "))
# Convert the formula string to a formula object
model_formula <- as.formula(formula_str)
#The model
summary(lm(model_formula, data=subjective_data))



