
#########################################
###Race Disparities######################
#########################################

library(dplyr) #This is to be able to use %>%
library(car)  #This is to use vif
library(mice)
library(EValue) #https://louisahsmith.github.io/evalue/index.html
library(haven) #to use read_sav
library(MASS) #to use glm.nb
library(lmtest) #to use lr.test



options(scipen=999) #To disable scientific notation
set.seed(7)

#Importing the data
drugs = read_sav("2014to2023.sav")

#Keeping only last five years worth of data
drugs = drugs[which(drugs$YEAR>2019),]

#Dropping non-drugs cases
drugs = drugs[which(as.character(drugs$OFFGUIDE)=="10"),]

#Dropping variables not relevant
vars = names(drugs) %in% c("MONSEX", "NEWRACE", "NEWEDUC", "AGE", "NEWCIT", "NEWCNVTN", 
                           "NOCOUNTS", "COMBDRG2", "CIRCDIST", "SUBASSIST", "XFOLSOR", 
                           "XCRHISSR", "SENT0CAP", "PRESENT") 
drugs = drugs[vars]
summary(drugs)
lapply(drugs, function(x) prop.table(table(x, useNA = "ifany")))

#Setting variables as characters
drugs$NEWCIT = as.character(drugs$NEWCIT)
drugs$CIRCDIST = as.character(drugs$CIRCDIST)
drugs$MONSEX = as.character(drugs$MONSEX)
drugs$NEWCNVTN = as.character(drugs$NEWCNVTN)

#Recoding NEWEDUC
table(drugs$NEWEDUC, useNA = "ifany")
drugs$NEWEDUC = ifelse(drugs$NEWEDUC<4,"0","1")
table(drugs$NEWEDUC, useNA = "ifany")

#Recoding NEWRACE
table(drugs$NEWRACE, useNA = "ifany")
drugs$NEWRACE = ifelse(drugs$NEWRACE==1, "white", 
                ifelse(drugs$NEWRACE==2, "black",
                ifelse(drugs$NEWRACE==3, "Hispanic",
                ifelse(drugs$NEWRACE==6, "other", "NA"))))
drugs$NEWRACE = relevel(as.factor(drugs$NEWRACE), ref = "white")
table(drugs$NEWRACE, useNA = "ifany")

#Recoding NOCOUNTS
table(drugs$NOCOUNTS, useNA = "ifany")  
drugs$NOCOUNTS = ifelse(drugs$NOCOUNTS<2,"0","1")
table(drugs$NOCOUNTS, useNA = "ifany")  

#Recoding COMBDRG2
table(drugs$COMBDRG2, useNA = "ifany")
drugs$COMBDRG2 = ifelse(drugs$COMBDRG2==1, "powder cocaine", 
                ifelse(drugs$COMBDRG2==2, "crack",
                ifelse(drugs$COMBDRG2==3, "heroin",
                ifelse(drugs$COMBDRG2==4, "marijuana", 
                ifelse(drugs$COMBDRG2==6, "meth",                        
                ifelse(drugs$COMBDRG2==7, "fentanyl", "other"))))))
table(drugs$COMBDRG2, useNA = "ifany")

#Recoding PRESENT
table(drugs$PRESENT, useNA = "ifany")  
drugs$detention = ifelse(drugs$PRESENT==1, 0,
                ifelse(drugs$PRESENT==2|drugs$PRESENT==3, 1, NA))
table(drugs$detention, useNA = "ifany")  
drugs$PRESENT = NULL

       

###########################################
###Descriptive stats#######################
###########################################

lapply(drugs, function(x) prop.table(table(x))) 
summary(drugs)
hist(drugs$SENT0CAP)



###########################################
###Imputation##############################
###########################################

#Missing data pattern
md.pattern(drugs)
#Imputation
data_mi = mice(drugs, m = 5, method = "pmm")



###########################################
###Models##################################
###########################################

#Model specification
#Poisson
model_poisson = glm(SENT0CAP ~ MONSEX, data=drugs, family="poisson")
summary(model_poisson)
#Negative binomial
model_negbin = glm.nb(SENT0CAP ~ MONSEX, data = drugs)
summary(model_negbin)
#Comparison
lrtest(model_poisson, model_negbin)

#Empty model
model1 = glm.nb(SENT0CAP ~ NEWRACE, data = drugs)
summary(model1)
exp(model1$coefficients)
#With imputed data
model1_mi = with(data_mi, glm.nb(SENT0CAP ~ NEWRACE, data = drugs))
summary(pool(model1_mi),conf.int = TRUE)
cbind(summary(pool(model1_mi),conf.int = TRUE)[,1], exp(summary(pool(model1_mi),conf.int = TRUE)[,c(2,7,8)]))

#Model including pretreatment case characteristics
model2 = glm.nb(SENT0CAP ~ NEWRACE + MONSEX + NEWEDUC + AGE + NEWCIT + NEWCNVTN +
                           NOCOUNTS + COMBDRG2, data = drugs)
summary(model2)
exp(model2$coefficients)
#With imputed data
model2_mi = with(data_mi, glm.nb(SENT0CAP ~ NEWRACE + MONSEX + NEWEDUC + AGE + 
                                 NEWCIT + NEWCNVTN + NOCOUNTS + COMBDRG2, data = drugs))
summary(pool(model2_mi),conf.int = TRUE)
cbind(summary(pool(model2_mi),conf.int = TRUE)[,1], exp(summary(pool(model2_mi),conf.int = TRUE)[,c(2,7,8)]))

#Model including posttreatment case characteristics
model3 = glm.nb(SENT0CAP ~ NEWRACE + MONSEX + AGE + NEWCIT + NEWEDUC + NEWCNVTN +
                           NOCOUNTS + COMBDRG2 + XCRHISSR + XFOLSOR + SUBASSIST + 
                           CIRCDIST + detention, data=drugs)
summary(model3)
exp(model3$coefficients)
vif(model3)
#With imputed data
model3_mi = with(data_mi, glm.nb(SENT0CAP ~ NEWRACE + MONSEX + NEWEDUC + AGE + 
                                   NEWCIT + NEWCNVTN + NOCOUNTS + COMBDRG2 + XCRHISSR + 
                                   XFOLSOR + SUBASSIST + CIRCDIST + detention, data = drugs))
summary(pool(model3_mi),conf.int = TRUE)
cbind(summary(pool(model3_mi),conf.int = TRUE)[,1], exp(summary(pool(model3_mi),conf.int = TRUE)[,c(2,7,8)]))



###########################################
###Robustness value########################
###########################################

library(sensemakr)

#Transforming sentence length using log(Y+1)
drugs$loglength = log(drugs$SENT0CAP + 1) 
#Running a linear model
linear = lm(drugs$loglength ~ NEWRACE + MONSEX + AGE + NEWCIT + NEWEDUC + NEWCNVTN +
                  NOCOUNTS + COMBDRG2 + XCRHISSR + XFOLSOR + SUBASSIST + 
                  CIRCDIST + detention, data=drugs)
summary(linear)

#The sensitivity analysis
sensitivity_analysis <- sensemakr(linear, treatment = "NEWRACEblack")
summary(sensitivity_analysis)



###########################################
###Specification Curve#####################
###########################################

library(gtools)
library(ggplot2)
library(ggpubr)

#Subjectively defined factors##############
subjective = c('XCRHISSR', 'XFOLSOR', 'SUBASSIST', 'CIRCDIST', 'detention') 

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


#Model uncertainty##############################################################
#Black offenders
# Initialize a vector to store the 'black' coefficients
race_coefficients <- numeric(nrow(final_combinations))
# Loop through each combination in random_sample
for (i in 1:nrow(final_combinations)) {
  # Extract the non-NA variables from the current row of random_sample
  additional_vars <- na.omit(final_combinations[i, ])
  # Create the formula by adding the additional variables to the benchmark model
  formula_str <- paste("SENT0CAP ~ NEWRACE + MONSEX + NEWEDUC + AGE + NEWCIT + NEWCNVTN +
                           NOCOUNTS + COMBDRG2 +", 
                        paste(additional_vars, collapse = " + "))
  # Convert the formula string to a formula object
  model_formula <- as.formula(formula_str)
  # Fit the logistic regression model
  model <- glm.nb(model_formula, data=drugs)
  # Extract the coefficient for 'male'
  race_coefficients[i] <- coef(model)["NEWRACEblack"]
}
# Print the coefficients for 'male'
race_coefficients
#Turn them into odds ratios
race_coefficients = exp(race_coefficients)
#Calculating 95% CI
quantile(race_coefficients, probs = c(0.025, 0.975))

# The plot
# Create a data frame from the 'male' coefficients
race_coefficients_df <- data.frame(coefficient = race_coefficients)
# Create a density plot using ggplot
race_plot = 
  ggplot(race_coefficients_df, aes(x = coefficient)) +
  geom_density(fill = "lightblue", alpha = 0.6, color = "black") +  
  geom_vline(xintercept = 1, color = "grey", linetype = "dashed", size = 0.8) +  
  labs(title = "",
       x = "ratio of considitional prison length black/white offenders",
       y = "") +  
  xlim(c(0.60, 1.40)) +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(
    plot.title = element_blank(),  
    axis.title.x = element_text(family="serif", size = 12),  
    axis.text.y = element_blank(),
    #axis.title.y = element_text(family="serif", size = 12),  
    axis.text = element_text(family="serif", size = 10)  
  )
ggexport(race_plot, width=1400, height=700, res=320,
         filename="blacks.jpeg")


#Model uncertainty##############################################################
#Hispanic offenders
# Initialize a vector to store the 'black' coefficients
race_coefficients <- numeric(nrow(final_combinations))
# Loop through each combination in random_sample
for (i in 1:nrow(final_combinations)) {
  # Extract the non-NA variables from the current row of random_sample
  additional_vars <- na.omit(final_combinations[i, ])
  # Create the formula by adding the additional variables to the benchmark model
  formula_str <- paste("SENT0CAP ~ NEWRACE + MONSEX + NEWEDUC + AGE + NEWCIT + NEWCNVTN +
                           NOCOUNTS + COMBDRG2 +", 
                       paste(additional_vars, collapse = " + "))
  # Convert the formula string to a formula object
  model_formula <- as.formula(formula_str)
  # Fit the logistic regression model
  model <- glm.nb(model_formula, data=drugs)
  # Extract the coefficient for 'male'
  race_coefficients[i] <- coef(model)["NEWRACEHispanic"]
}
# Print the coefficients for 'male'
race_coefficients
#Turn them into odds ratios
race_coefficients = exp(race_coefficients)
#Calculating 95% CI
quantile(race_coefficients, probs = c(0.025, 0.975))

# The plot
# Create a data frame from the 'male' coefficients
race_coefficients_df <- data.frame(coefficient = race_coefficients)
# Create a density plot using ggplot
race_plot = 
  ggplot(race_coefficients_df, aes(x = coefficient)) +
  geom_density(fill = "lightblue", alpha = 0.6, color = "black") +  
  geom_vline(xintercept = 1, color = "grey", linetype = "dashed", size = 0.8) +  
  labs(title = "",
       x = "ratio of considitional prison length Hispanic/white offenders",
       y = "") +  
  xlim(c(0.60, 1.40)) +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(
    plot.title = element_blank(),  
    axis.title.x = element_text(family="serif", size = 12),  
    axis.text.y = element_blank(),
    #axis.title.y = element_text(family="serif", size = 12),  
    axis.text = element_text(family="serif", size = 10)  
  )
ggexport(race_plot, width=1400, height=700, res=320,
         filename="hispanics.jpeg")


