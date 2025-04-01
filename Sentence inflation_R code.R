
################################################################################
###############SENTENCE INFLATION###############################################
################################################################################

library(tseries) 
library(ggplot2)
library(zoo)      #This is to use as.yearmon
library(tidyverse)
library(ggpubr)


#Data ##########################################################################
drugs = read.csv("drugs.csv")
damage = read.csv("damage.csv")
sex = read.csv("sex.csv")
violence = read.csv("violence.csv")
robbery = read.csv("robbery.csv")
theft = read.csv("theft.csv")
weapons = read.csv("weapons.csv")
order = read.csv("order.csv")
society = read.csv("society.csv")
fraud = read.csv("fraud.csv")


#Data cleaning##################################################################

#Setting the date
offence_group = c("damage", "drugs", "sex", "violence", "robbery", "theft", "weapons", 
                  "order", "society", "fraud")
for (name in offence_group) {
  temp_data <- get(name)
  temp_data$date <- as.Date(paste("15-", "06-", temp_data$year, sep = ""), format = "%d-%m-%Y")
  assign(name, temp_data, envir = .GlobalEnv)
}

#Completing custody rate and number of custody sentences
#damage
vars <- c("X56A", "X56B", "X58EJ", "X58D")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  damage[[cp]] <- ifelse(is.na(damage[[cp]]), damage[[c]] / damage[[n]], damage[[cp]])
  damage[[c]] <- ifelse(is.na(damage[[c]]), damage[[n]] * damage[[cp]], damage[[c]])
}

#drugs
vars <- c("X92A", "X92B", "X92C")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  drugs[[cp]] <- ifelse(is.na(drugs[[cp]]), drugs[[c]] / drugs[[n]], drugs[[cp]])
  drugs[[c]] <- ifelse(is.na(drugs[[c]]), drugs[[n]] * drugs[[cp]], drugs[[c]])
}

#robbery
robbery$X34_custp = ifelse(is.na(robbery$X34_custp), robbery$X34_cust / robbery$X34_N, robbery$X34_custp)
robbery$X34_cust = ifelse(is.na(robbery$X34_cust), robbery$X34_custp * robbery$X34_N, robbery$X34_cust)

#fraud
vars <- c("X51", "X52", "X53B", "X53C", "X53D", "X53E", "X53F", "X55")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  fraud[[cp]] <- ifelse(is.na(fraud[[cp]]), fraud[[c]] / fraud[[n]], fraud[[cp]])
  fraud[[c]] <- ifelse(is.na(fraud[[c]]), fraud[[n]] * fraud[[cp]], fraud[[c]])
}

#society
vars <- c("X24", "X27", "X33", "X38", "X54", "X59", "X60", "X61", "X61A", "X67", "X75", "X76", 
          "X78", "X79", "X80", "X82", "X83", "X84", "X85", "X86", "X87", "X89", "X91", 
          "X94", "X95", "X87", "X99", "X802", "X814")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  society[[cp]] <- ifelse(is.na(society[[cp]]), society[[c]] / society[[n]], society[[cp]])
  society[[c]] <- ifelse(is.na(society[[c]]), society[[n]] * society[[cp]], society[[c]])
}

#order
vars <- c("X64", "X65", "X66")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  order[[cp]] <- ifelse(is.na(order[[cp]]), order[[c]] / order[[n]], order[[cp]])
  order[[c]] <- ifelse(is.na(order[[c]]), order[[n]] * order[[cp]], order[[c]])
}

#weapons
vars <- c("X10A", "X10B", "X10C", "X10D", "X81")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  weapons[[cp]] <- ifelse(is.na(weapons[[cp]]), weapons[[c]] / weapons[[n]], weapons[[cp]])
  weapons[[c]] <- ifelse(is.na(weapons[[c]]), weapons[[n]] * weapons[[cp]], weapons[[c]])
}

#theft
vars <- c("X28", "X29", "X30", "X31", "X37", "X45", "X48", "X126", "X39", "X44", 
          "X46", "X35", "X40", "X41", "X42", "X43", "X47", "X49")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  theft[[cp]] <- ifelse(is.na(theft[[cp]]), theft[[c]] / theft[[n]], theft[[cp]])
  theft[[c]] <- ifelse(is.na(theft[[c]]), theft[[n]] * theft[[cp]], theft[[c]])
}

#violence
vars <- c("X1", "X4.4", "X4.6", "X4.8", "X4.9", "X37.1", "X2", "X4.3", "X5A", "X5D", 
          "X5E", "X6", "X7", "X8F", "X8H", "X4.7", "X8G", "X8J", "X8N" , "X3A", "X3B", "X11", 
          "X12", "X13", "X14", "X36", "X104", "X105A", "X105B", "X106", "X8L", "X8M", 
          "X8Q", "X8R", "X8U")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  violence[[cp]] <- ifelse(is.na(violence[[cp]]), violence[[c]] / violence[[n]], violence[[cp]])
  violence[[c]] <- ifelse(is.na(violence[[c]]), violence[[n]] * violence[[cp]], violence[[c]])
}

#sex
vars <- c("X19C", "X19D", "X19E", "X19F", "X19G", "X19H", "X17A", "X17B", "X20A", 
          "X20B", "X21", "X22", "X16", "X22A", "X23", "X25", "X70", "X71", 
          "X72", "X73", "X74", "X88A", "X88B", "X88C", "X88D", "X88E")
for (v in vars) {
  cp <- paste0(v, "_custp"); c <- paste0(v, "_cust"); n <- paste0(v, "_N")
  sex[[cp]] <- ifelse(is.na(sex[[cp]]), sex[[c]] / sex[[n]], sex[[cp]])
  sex[[c]] <- ifelse(is.na(sex[[c]]), sex[[n]] * sex[[cp]], sex[[c]])
}

#Replacing NA acsl values for their offence mean

#sex
sex$X19F_acsl = ifelse(is.na(sex$X19F_acsl)==TRUE, mean(sex$X19F_acsl, na.rm=TRUE), 
                       sex$X19F_acsl)

#fraud
fraud$X52_acsl = ifelse(is.na(fraud$X52_acsl)==TRUE, mean(fraud$X52_acsl, na.rm=TRUE), 
                        fraud$X52_acsl)
fraud$X53B_acsl = ifelse(is.na(fraud$X53B_acsl)==TRUE, mean(fraud$X53B_acsl, na.rm=TRUE), 
                         fraud$X53B_acsl)
fraud$X53D_acsl = ifelse(is.na(fraud$X53D_acsl)==TRUE, mean(fraud$X53D_acsl, na.rm=TRUE), 
                         fraud$X53D_acsl)
fraud$X53E_acsl = ifelse(is.na(fraud$X53E_acsl)==TRUE, mean(fraud$X53E_acsl, na.rm=TRUE), 
                         fraud$X53E_acsl)
fraud$X55_acsl = ifelse(is.na(fraud$X55_acsl)==TRUE, mean(fraud$X55_acsl, na.rm=TRUE), 
                        fraud$X55_acsl)

#order
order$X64_acsl = ifelse(is.na(order$X64_acsl)==TRUE, mean(order$X64_acsl, na.rm=TRUE), 
                        order$X64_acsl)

#society
society$X27_acsl = ifelse(is.na(society$X27_acsl)==TRUE, mean(society$X27_acsl, na.rm=TRUE), 
                          society$X27_acsl)
society$X60_acsl = ifelse(is.na(society$X60_acsl)==TRUE, mean(society$X60_acsl, na.rm=TRUE), 
                          society$X60_acsl)
society$X67_acsl = ifelse(is.na(society$X67_acsl)==TRUE, mean(society$X67_acsl, na.rm=TRUE), 
                          society$X67_acsl)
society$X75_acsl = ifelse(is.na(society$X75_acsl)==TRUE, mean(society$X75_acsl, na.rm=TRUE), 
                          society$X75_acsl)
society$X76_acsl = ifelse(is.na(society$X76_acsl)==TRUE, mean(society$X76_acsl, na.rm=TRUE), 
                          society$X76_acsl)
society$X82_acsl = ifelse(is.na(society$X82_acsl)==TRUE, mean(society$X82_acsl, na.rm=TRUE), 
                          society$X82_acsl)
society$X85_acsl = ifelse(is.na(society$X85_acsl)==TRUE, mean(society$X85_acsl, na.rm=TRUE), 
                          society$X85_acsl)
society$X87_acsl = ifelse(is.na(society$X87_acsl)==TRUE, mean(society$X87_acsl, na.rm=TRUE), 
                          society$X87_acsl)
society$X89_acsl = ifelse(is.na(society$X89_acsl)==TRUE, mean(society$X89_acsl, na.rm=TRUE), 
                          society$X89_acsl)
society$X94_acsl = ifelse(is.na(society$X94_acsl)==TRUE, mean(society$X94_acsl, na.rm=TRUE), 
                          society$X82_acsl)
society$X95_acsl = ifelse(is.na(society$X95_acsl)==TRUE, mean(society$X95_acsl, na.rm=TRUE), 
                          society$X95_acsl)

#theft
theft$X31_acsl = ifelse(is.na(theft$X31_acsl)==TRUE, mean(theft$X31_acsl, na.rm=TRUE), 
                        theft$X31_acsl)
theft$X42_acsl = ifelse(is.na(theft$X42_acsl)==TRUE, mean(theft$X42_acsl, na.rm=TRUE), 
                        theft$X42_acsl)
theft$X43_acsl = ifelse(is.na(theft$X43_acsl)==TRUE, mean(theft$X43_acsl, na.rm=TRUE), 
                        theft$X43_acsl)

#violence
violence$X4.8_acsl = ifelse(is.na(violence$X4.8_acsl)==TRUE, mean(violence$X4.8_acsl, na.rm=TRUE), 
                           violence$X4.8_acsl)
violence$X4.9_acsl = ifelse(is.na(violence$X4.9_acsl)==TRUE, mean(violence$X4.9_acsl, na.rm=TRUE), 
                            violence$X4.9_acsl)
violence$X37.1_acsl = ifelse(is.na(violence$X37.1_acsl)==TRUE, mean(violence$X37.1_acsl, na.rm=TRUE), 
                            violence$X37.1_acsl)
violence$X4.3_acsl = ifelse(is.na(violence$X4.3_acsl)==TRUE, mean(violence$X4.3_acsl, na.rm=TRUE), 
                            violence$X4.3_acsl)
violence$X5D_acsl = ifelse(is.na(violence$X5D_acsl)==TRUE, mean(violence$X5D_acsl, na.rm=TRUE), 
                            violence$X5D_acsl)
violence$X6_acsl = ifelse(is.na(violence$X6_acsl)==TRUE, mean(violence$X6_acsl, na.rm=TRUE), 
                            violence$X6_acsl)
violence$X7_acsl = ifelse(is.na(violence$X7_acsl)==TRUE, mean(violence$X7_acsl, na.rm=TRUE), 
                            violence$X7_acsl)
violence$X8H_acsl = ifelse(is.na(violence$X8H_acsl)==TRUE, mean(violence$X8H_acsl, na.rm=TRUE), 
                            violence$X8H_acsl)
violence$X4.7_acsl = ifelse(is.na(violence$X4.7_acsl)==TRUE, mean(violence$X4.7_acsl, na.rm=TRUE), 
                           violence$X4.7_acsl)
violence$X12_acsl = ifelse(is.na(violence$X12_acsl)==TRUE, mean(violence$X12_acsl, na.rm=TRUE), 
                           violence$X12_acsl)
violence$X14_acsl = ifelse(is.na(violence$X14_acsl)==TRUE, mean(violence$X14_acsl, na.rm=TRUE), 
                           violence$X14_acsl)
violence$X106_acsl = ifelse(is.na(violence$X106_acsl)==TRUE, mean(violence$X106_acsl, na.rm=TRUE), 
                           violence$X106_acsl)
violence$X8R_acsl = ifelse(is.na(violence$X8R_acsl)==TRUE, mean(violence$X8R_acsl, na.rm=TRUE), 
                           violence$X8R_acsl)
violence$X8U_acsl = ifelse(is.na(violence$X8U_acsl)==TRUE, mean(violence$X8U_acsl, na.rm=TRUE), 
                           violence$X8U_acsl)

#weapons
weapons$X81_acsl = ifelse(is.na(weapons$X81_acsl)==TRUE, mean(weapons$X81_acsl, na.rm=TRUE), 
                          weapons$X81_acsl)


#Replacing NaN custp values for 0s
#society
society$X27_custp = ifelse(is.na(society$X27_custp)==TRUE, 0, society$X27_custp)
society$X76_cust = ifelse(is.na(society$X76_cust)==TRUE, 0, society$X76_cust)

#violence
violence$X4.7_custp = ifelse(is.na(violence$X4.7_custp)==TRUE, 0, violence$X4.7_custp)
violence$X4.9_custp = ifelse(is.na(violence$X4.9_custp)==TRUE, 0, violence$X4.9_custp)
violence$X4.3_custp = ifelse(is.na(violence$X4.3_custp)==TRUE, 0, violence$X4.3_custp)
violence$X12_custp = ifelse(is.na(violence$X12_custp)==TRUE, 0, violence$X12_custp)
violence$X106_custp = ifelse(is.na(violence$X106_custp)==TRUE, 0, violence$X106_custp)
violence$X8Q_custp = ifelse(is.na(violence$X8Q_custp)==TRUE, 0, violence$X8Q_custp)



#Calculating sentence severity and offence seriousness##########################
#Offence seriousness
damage$seriousness = (damage$X56A_w*damage$X56A_N + damage$X56B_w*damage$X56B_N + 
                      damage$X58EJ_w*damage$X58EJ_N + damage$X58D_w*damage$X58D_N) /
                     (damage$X56A_N + damage$X56B_N + damage$X58EJ_N + damage$X58D_N)
drugs$seriousness = (drugs$X92A_w*drugs$X92A_N + drugs$X92B_w*drugs$X92B_N + 
                     drugs$X92C_w*drugs$X92C_N) / 
                    (drugs$X92A_N + drugs$X92B_N + drugs$X92C_N)
fraud$seriousness = (fraud$X51_w*fraud$X51_N + fraud$X52_w*fraud$X52_N + fraud$X53B_w*fraud$X53B_N + 
                     fraud$X53C_w*fraud$X53C_N + fraud$X53D_w*fraud$X53D_N + fraud$X53E_w*fraud$X53E_N + 
                     fraud$X53F_w*fraud$X53F_N + fraud$X55_w*fraud$X55_N) /
                    (fraud$X51_N + fraud$X52_N + fraud$X53B_N + fraud$X53C_N + 
                     fraud$X53D_N + fraud$X53E_N + fraud$X53F_N + fraud$X55_N)
order$seriousness = (order$X64_w*order$X64_N + order$X65_w*order$X65_N +
                     order$X66_w*order$X66_N) / 
                    (order$X64_N + order$X65_N + order$X66_N)
robbery$seriousness = (robbery$X34_w*robbery$X34_N) / robbery$X34_N
sex$seriousness = (sex$X19C_w*sex$X19C_N + sex$X19D_w*sex$X19D_N + sex$X19E_w*sex$X19E_N + 
                   sex$X19F_w*sex$X19F_N + sex$X19G_w*sex$X19G_N + sex$X19H_w*sex$X19H_N + 
                   sex$X17A_w*sex$X17A_N + sex$X17B_w*sex$X17B_N + sex$X20A_w*sex$X20A_N + 
                   sex$X20B_w*sex$X20B_N + sex$X21_w*sex$X21_N + sex$X22_w*sex$X22_N + 
                   sex$X16_w*sex$X16_N + sex$X22A_w*sex$X22A_N +
                   sex$X23_w*sex$X23_N + sex$X25_w*sex$X25_N + sex$X70_w*sex$X70_N + 
                   sex$X71_w*sex$X71_N + sex$X72_w*sex$X72_N + sex$X73_w*sex$X73_N + 
                   sex$X74_w*sex$X74_N + sex$X88A_w*sex$X88A_N + sex$X88B_w*sex$X88B_N + 
                   sex$X88C_w*sex$X88C_N + sex$X88D_w*sex$X88D_N + sex$X88E_w*sex$X88E_N) /
                  (sex$X19C_N + sex$X19D_N + sex$X19E_N + sex$X19F_N + sex$X19G_N + 
                   sex$X19H_N + sex$X17A_N + sex$X17B_N + sex$X20A_N + sex$X20B_N + 
                   sex$X21_N + sex$X22_N + sex$X16_N + sex$X22A_N +
                   sex$X23_N + sex$X25_N + sex$X70_N + sex$X71_N + sex$X72_N + sex$X73_N + 
                   sex$X74_N + sex$X88A_N + sex$X88B_N + sex$X88C_N + sex$X88D_N + 
                   sex$X88E_N)
society$seriousness = (society$X24_w*society$X24_N + society$X27_w*society$X27_N + 
                       society$X33_w*society$X33_N + society$X38_w*society$X38_N + 
                       society$X54_w*society$X54_N + society$X59_w*society$X59_N + 
                       society$X60_w*society$X60_N + society$X61_w*society$X61_N + 
                       society$X61A_w*society$X61A_N + society$X67_w*society$X67_N + 
                       society$X75_w*society$X75_N + society$X76_w*society$X76_N + 
                       society$X78_w*society$X78_N + society$X79_w*society$X79_N + 
                       society$X80_w*society$X80_N + society$X82_w*society$X82_N + 
                       society$X83_w*society$X83_N + society$X84_w*society$X84_N + 
                       society$X85_w*society$X85_N + society$X86_w*society$X86_N + 
                       society$X87_w*society$X87_N + society$X89_w*society$X89_N + 
                       society$X91_w*society$X91_N + society$X94_w*society$X94_N + 
                       society$X95_w*society$X95_N + society$X99_w*society$X99_N + 
                       society$X802_w*society$X802_N + society$X814_w*society$X814_N) /
                      (society$X24_N + society$X27_N + society$X33_N + society$X38_N + 
                       society$X54_N + society$X59_N + society$X60_N + society$X61_N + 
                       society$X61A_N + society$X67_N + society$X75_N + society$X76_N + 
                       society$X78_N + society$X79_N + society$X80_N + society$X82_N + 
                       society$X83_N + society$X84_N + society$X85_N + society$X86_N + 
                       society$X87_N + society$X89_N + society$X91_N + society$X94_N + 
                       society$X95_N + society$X99_N + society$X802_N + society$X814_N)  
theft$seriousness = (theft$X28_w*theft$X28_N + theft$X29_w*theft$X29_N + 
                     theft$X30_w*theft$X30_N + theft$X31_w*theft$X31_N + 
                     theft$X37_w*theft$X37_N + theft$X45_w*theft$X45_N + 
                     theft$X48_w*theft$X48_N + theft$X126_w*theft$X126_N + 
                     theft$X39_w*theft$X39_N + theft$X44_w*theft$X44_N + 
                     theft$X46_w*theft$X46_N + theft$X35_w*theft$X35_N + 
                     theft$X40_w*theft$X40_N + theft$X41_w*theft$X41_N + 
                     theft$X42_w*theft$X42_N + theft$X43_w*theft$X43_N + 
                     theft$X47_w*theft$X47_N + theft$X49_w*theft$X49_N) / 
                    (theft$X28_N + theft$X29_N + theft$X30_N + theft$X31_N + 
                     theft$X37_N + theft$X45_N + theft$X48_N + theft$X126_N + 
                     theft$X39_N + theft$X44_N + theft$X46_N + theft$X35_N + 
                     theft$X40_N + theft$X41_N + theft$X42_N + theft$X43_N + 
                     theft$X47_N + theft$X49_N)
violence$seriousness = (violence$X1_w*violence$X1_N + violence$X4.4_w*violence$X4.4_N + 
                        violence$X4.6_w*violence$X4.6_N + violence$X4.8_w*violence$X4.8_N + 
                        violence$X4.9_w*violence$X4.9_N + violence$X37.1_w*violence$X37.1_N + 
                        violence$X2_w*violence$X2_N + violence$X4.3_w*violence$X4.3_N + 
                        violence$X5A_w*violence$X5A_N + violence$X5D_w*violence$X5D_N + 
                        violence$X5E_w*violence$X5E_N + violence$X6_w*violence$X6_N + 
                        violence$X7_w*violence$X7_N + violence$X8F_w*violence$X8F_N + 
                        violence$X8H_w*violence$X8H_N + violence$X4.7_w*violence$X4.7_N + 
                        violence$X8G_w*violence$X8G_N + violence$X8J_w*violence$X8J_N + 
                        violence$X8N_w*violence$X8N_N + violence$X3A_w*violence$X3A_N + 
                        violence$X3B_w*violence$X3B_N + violence$X11_w*violence$X11_N + 
                        violence$X12_w*violence$X12_N + violence$X13_w*violence$X13_N + 
                        violence$X14_w*violence$X14_N + violence$X36_w*violence$X36_N + 
                        violence$X104_w*violence$X104_N + violence$X105A_w*violence$X105A_N + 
                        violence$X105B_w*violence$X105B_N + violence$X106_w*violence$X106_N + 
                        violence$X8L_w*violence$X8L_N + violence$X8M_w*violence$X8M_N + 
                        violence$X8Q_w*violence$X8Q_N + violence$X8R_w*violence$X8R_N + 
                        violence$X8U_w*violence$X8U_N) /   
                       (violence$X1_N + violence$X4.4_N + violence$X4.6_N + violence$X4.8_N + 
                        violence$X4.9_N + violence$X37.1_N + violence$X2_N + violence$X4.3_N + 
                        violence$X5A_N + violence$X5D_N + violence$X5E_N + violence$X6_N + 
                        violence$X7_N + violence$X8F_N + violence$X8H_N + violence$X4.7_N + 
                        violence$X8G_N + violence$X8J_N + violence$X8N_N + violence$X3A_N + 
                        violence$X3B_N + violence$X11_N + violence$X12_N + violence$X13_N + 
                        violence$X14_N + violence$X36_N + violence$X104_N + violence$X105A_N + 
                        violence$X105B_N + violence$X106_N + violence$X8L_N +violence$X8M_N + 
                        violence$X8Q_N + violence$X8R_N + violence$X8U_N)  
weapons$seriousness = (weapons$X10A_w*weapons$X10A_N + weapons$X10B_w*weapons$X10B_N + 
                       weapons$X10C_w*weapons$X10C_N +  weapons$X10D_w*weapons$X10D_N +  
                       weapons$X81_w*weapons$X81_N) /
                      (weapons$X10A_N + weapons$X10B_N + weapons$X10C_N + weapons$X10D_N + 
                       weapons$X81_N)  

#Sentence severity
damage$severity = (damage$X56A_cust*damage$X56A_acsl + damage$X56B_cust*damage$X56B_acsl + 
                   damage$X58EJ_cust*damage$X58EJ_acsl + damage$X58D_cust*damage$X58D_acsl) /
                  (damage$X56A_N + damage$X56B_N + damage$X58EJ_N + damage$X58D_N)  
drugs$severity = (drugs$X92A_cust*drugs$X92A_acsl + drugs$X92B_cust*drugs$X92B_acsl + 
                     drugs$X92C_cust*drugs$X92C_acsl) / 
                    (drugs$X92A_N + drugs$X92B_N + drugs$X92C_N)
fraud$severity = (fraud$X51_cust*fraud$X51_acsl + fraud$X52_cust*fraud$X52_acsl + fraud$X53B_cust*fraud$X53B_acsl + 
                     fraud$X53C_cust*fraud$X53C_acsl + fraud$X53D_cust*fraud$X53D_acsl + fraud$X53E_cust*fraud$X53E_acsl + 
                     fraud$X53F_cust*fraud$X53F_acsl + fraud$X55_cust*fraud$X55_acsl) /
                    (fraud$X51_N + fraud$X52_N + fraud$X53B_N + fraud$X53C_N + 
                     fraud$X53D_N + fraud$X53E_N + fraud$X53F_N + fraud$X55_N)
order$severity = (order$X64_cust*order$X64_acsl + order$X65_cust*order$X65_acsl +
                     order$X66_cust*order$X66_acsl) / 
                    (order$X64_N + order$X65_N + order$X66_N)
robbery$severity = (robbery$X34_cust*robbery$X34_acsl) / robbery$X34_N
sex$severity = (sex$X19C_cust*sex$X19C_acsl + sex$X19D_cust*sex$X19D_acsl + sex$X19E_cust*sex$X19E_acsl + 
                   sex$X19F_cust*sex$X19F_acsl + sex$X19G_cust*sex$X19G_acsl + sex$X19H_cust*sex$X19H_acsl + 
                   sex$X17A_cust*sex$X17A_acsl + sex$X17B_cust*sex$X17B_acsl + sex$X20A_cust*sex$X20A_acsl + 
                   sex$X20B_cust*sex$X20B_acsl + sex$X21_cust*sex$X21_acsl + sex$X22_cust*sex$X22_acsl + 
                   sex$X16_cust*sex$X16_acsl + sex$X22A_cust*sex$X22A_acsl +
                   sex$X23_cust*sex$X23_acsl + sex$X25_cust*sex$X25_acsl + sex$X70_cust*sex$X70_acsl + 
                   sex$X71_cust*sex$X71_acsl + sex$X72_cust*sex$X72_acsl + sex$X73_cust*sex$X73_acsl + 
                   sex$X74_cust*sex$X74_acsl + sex$X88A_cust*sex$X88A_acsl + sex$X88B_cust*sex$X88B_acsl + 
                   sex$X88C_cust*sex$X88C_acsl + sex$X88D_cust*sex$X88D_acsl + sex$X88E_cust*sex$X88E_acsl) /
                  (sex$X19C_N + sex$X19D_N + sex$X19E_N + sex$X19F_N + sex$X19G_N + 
                   sex$X19H_N + sex$X17A_N + sex$X17B_N + sex$X20A_N + sex$X20B_N + 
                   sex$X21_N + sex$X22_N + sex$X16_N + sex$X22A_N +
                   sex$X23_N + sex$X25_N + sex$X70_N + sex$X71_N + sex$X72_N + sex$X73_N + 
                   sex$X74_N + sex$X88A_N + sex$X88B_N + sex$X88C_N + sex$X88D_N + 
                   sex$X88E_N)
society$severity = (society$X24_cust*society$X24_acsl + society$X27_cust*society$X27_acsl + 
                       society$X33_cust*society$X33_acsl + society$X38_cust*society$X38_acsl + 
                       society$X54_cust*society$X54_acsl + society$X59_cust*society$X59_acsl + 
                       society$X60_cust*society$X60_acsl + society$X61_cust*society$X61_acsl + 
                       society$X61A_cust*society$X61A_acsl + society$X67_cust*society$X67_acsl + 
                       society$X75_cust*society$X75_acsl + society$X76_cust*society$X76_acsl + 
                       society$X78_cust*society$X78_acsl + society$X79_cust*society$X79_acsl + 
                       society$X80_cust*society$X80_acsl + society$X82_cust*society$X82_acsl + 
                       society$X83_cust*society$X83_acsl + society$X84_cust*society$X84_acsl + 
                       society$X85_cust*society$X85_acsl + society$X86_cust*society$X86_acsl + 
                       society$X87_cust*society$X87_acsl + society$X89_cust*society$X89_acsl + 
                       society$X91_cust*society$X91_acsl + society$X94_cust*society$X94_acsl + 
                       society$X95_cust*society$X95_acsl + society$X99_cust*society$X99_acsl + 
                       society$X802_cust*society$X802_acsl + society$X814_cust*society$X814_acsl) /
                      (society$X24_N + society$X27_N + society$X33_N + society$X38_N + 
                       society$X54_N + society$X59_N + society$X60_N + society$X61_N + 
                       society$X61A_N + society$X67_N + society$X75_N + society$X76_N + 
                       society$X78_N + society$X79_N + society$X80_N + society$X82_N + 
                       society$X83_N + society$X84_N + society$X85_N + society$X86_N + 
                       society$X87_N + society$X89_N + society$X91_N + society$X94_N + 
                       society$X95_N + society$X99_N + society$X802_N + society$X814_N)  
theft$severity = (theft$X28_cust*theft$X28_acsl + theft$X29_cust*theft$X29_acsl + 
                     theft$X30_cust*theft$X30_acsl + theft$X31_cust*theft$X31_acsl + 
                     theft$X37_cust*theft$X37_acsl + theft$X45_cust*theft$X45_acsl + 
                     theft$X48_cust*theft$X48_acsl + theft$X126_cust*theft$X126_acsl + 
                     theft$X39_cust*theft$X39_acsl + theft$X44_cust*theft$X44_acsl + 
                     theft$X46_cust*theft$X46_acsl + theft$X35_cust*theft$X35_acsl + 
                     theft$X40_cust*theft$X40_acsl + theft$X41_cust*theft$X41_acsl + 
                     theft$X42_cust*theft$X42_acsl + theft$X43_cust*theft$X43_acsl + 
                     theft$X47_cust*theft$X47_acsl + theft$X49_cust*theft$X49_acsl) / 
                    (theft$X28_N + theft$X29_N + theft$X30_N + theft$X31_N + 
                     theft$X37_N + theft$X45_N + theft$X48_N + theft$X126_N + 
                     theft$X39_N + theft$X44_N + theft$X46_N + theft$X35_N + 
                     theft$X40_N + theft$X41_N + theft$X42_N + theft$X43_N + 
                     theft$X47_N + theft$X49_N)
violence$severity = (violence$X1_cust*violence$X1_acsl + violence$X4.4_cust*violence$X4.4_acsl + 
                        violence$X4.6_cust*violence$X4.6_acsl + violence$X4.8_cust*violence$X4.8_acsl + 
                        violence$X4.9_cust*violence$X4.9_acsl + violence$X37.1_cust*violence$X37.1_acsl + 
                        violence$X2_cust*violence$X2_acsl + violence$X4.3_cust*violence$X4.3_acsl + 
                        violence$X5A_cust*violence$X5A_acsl + violence$X5D_cust*violence$X5D_acsl + 
                        violence$X5E_cust*violence$X5E_acsl + violence$X6_cust*violence$X6_acsl + 
                        violence$X7_cust*violence$X7_acsl + violence$X8F_cust*violence$X8F_acsl + 
                        violence$X8H_cust*violence$X8H_acsl + violence$X4.7_cust*violence$X4.7_acsl + 
                        violence$X8G_cust*violence$X8G_acsl + violence$X8J_cust*violence$X8J_acsl + 
                        violence$X8N_cust*violence$X8N_acsl + violence$X3A_cust*violence$X3A_acsl + 
                        violence$X3B_cust*violence$X3B_acsl + violence$X11_cust*violence$X11_acsl + 
                        violence$X12_cust*violence$X12_acsl + violence$X13_cust*violence$X13_acsl + 
                        violence$X14_cust*violence$X14_acsl + violence$X36_cust*violence$X36_acsl + 
                        violence$X104_cust*violence$X104_acsl + violence$X105A_cust*violence$X105A_acsl + 
                        violence$X105B_cust*violence$X105B_acsl + violence$X106_cust*violence$X106_acsl + 
                        violence$X8L_cust*violence$X8L_acsl + violence$X8M_cust*violence$X8M_acsl + 
                        violence$X8Q_cust*violence$X8Q_acsl + violence$X8R_cust*violence$X8R_acsl + 
                        violence$X8U_cust*violence$X8U_acsl) /   
                       (violence$X1_N + violence$X4.4_N + violence$X4.6_N + violence$X4.8_N + 
                        violence$X4.9_N + violence$X37.1_N + violence$X2_N + violence$X4.3_N + 
                        violence$X5A_N + violence$X5D_N + violence$X5E_N + violence$X6_N + 
                        violence$X7_N + violence$X8F_N + violence$X8H_N + violence$X4.7_N + 
                        violence$X8G_N + violence$X8J_N + violence$X8N_N + violence$X3A_N + 
                        violence$X3B_N + violence$X11_N + violence$X12_N + violence$X13_N + 
                        violence$X14_N + violence$X36_N + violence$X104_N + violence$X105A_N + 
                        violence$X105B_N + violence$X106_N + violence$X8L_N + violence$X8M_N + 
                        violence$X8Q_N + violence$X8R_N + violence$X8U_N)  
weapons$severity = (weapons$X10A_cust*weapons$X10A_acsl + weapons$X10B_cust*weapons$X10B_acsl + 
                       weapons$X10C_cust*weapons$X10C_acsl +  weapons$X10D_cust*weapons$X10D_acsl +  
                       weapons$X81_cust*weapons$X81_acsl) /
                      (weapons$X10A_N + weapons$X10B_N + weapons$X10C_N + weapons$X10D_N + 
                       weapons$X81_N)                  


#Dropping the year 2004
damage = damage[-1,]
drugs = drugs[-1,]
fraud = fraud[-1,]
order = order[-1,]
robbery = robbery[-1,]
sex = sex[-1,]
society = society[-1,]
theft = theft[-1,]
violence = violence[-1,]
weapons = weapons[-1,]


#Expressing change in relative terms
#Seriousness
damage$rser = damage$seriousness / damage$seriousness[1]
drugs$rser = drugs$seriousness / drugs$seriousness[1]
fraud$rser = fraud$seriousness / fraud$seriousness[1]
order$rser = order$seriousness / order$seriousness[1]
robbery$rser = robbery$seriousness / robbery$seriousness[1]
sex$rser = sex$seriousness / sex$seriousness[1]
society$rser = society$seriousness / society$seriousness[1]
theft$rser = theft$seriousness / theft$seriousness[1]
violence$rser = violence$seriousness / violence$seriousness[1]
weapons$rser = weapons$seriousness / weapons$seriousness[1]

#Severity
damage$rsev = damage$severity / damage$severity[1]
drugs$rsev = drugs$severity / drugs$severity[1]
fraud$rsev = fraud$severity / fraud$severity[1]
order$rsev = order$severity / order$severity[1]
robbery$rsev = robbery$severity / robbery$severity[1]
sex$rsev = sex$severity / sex$severity[1]
society$rsev = society$severity / society$severity[1]
theft$rsev = theft$severity / theft$severity[1]
violence$rsev = violence$severity / violence$severity[1]
weapons$rsev = weapons$severity / weapons$severity[1]



#Plots##########################################################################

#Criminal Damage
damage_diff = (damage$rsev[20] - damage$rser[20])*100
Damage = ggplot(damage, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Criminal damage") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(damage$date) + 800, 
           y = max(damage$rsev) -1.3, 
           label = round(damage_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(damage$date) + 1150)
ggsave("Damage.tiff", 
       Damage, width = 140, height = 63, units = "mm", dpi = 200)

#Drugs
drugs_diff = (drugs$rsev[20] - drugs$rser[20])*100
Drugs = ggplot(drugs, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Drugs") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(drugs$date) + 800, 
           y = (max(drugs$rser) - (max(drugs$rser) - max(drugs$rsev))/2), 
           label = round(drugs_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(drugs$date) + 1150)
ggsave("Drugs.tiff", 
       Drugs, width = 140, height = 63, units = "mm", dpi = 200)

#Fraud
fraud_diff = (fraud$rsev[20] - fraud$rser[20])*100
Fraud = ggplot(fraud, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Fraud") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(fraud$date) + 800, 
           y = (max(fraud$rsev) - 1.4), 
           label = round(fraud_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(fraud$date) + 1150)
ggsave("Fraud.tiff", 
       Fraud, width = 140, height = 63, units = "mm", dpi = 200)

#Public Order
order_diff = (order$rsev[20] - order$rser[20])*100
Order = ggplot(order, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Public order") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(order$date) + 800, 
           y = max(order$rsev) -.125, 
           label = round(order_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(order$date) + 1150) 
ggsave("Order.tiff", 
       Order, width = 140, height = 63, units = "mm", dpi = 200)

#Robbery
#robbery_diff = (robbery$rsev[20] - robbery$rser[20])*100
Robbery = ggplot(robbery, aes(x=date)) +
  #geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Robbery") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
#  annotate("text", x = max(robbery$date) + 800, 
#           y = max(robbery$rsev) -.125, 
#           label = round(robbery_diff,1), size = 3.5, 
#           fontface = "bold", family="serif") +  
  expand_limits(x = max(robbery$date) + 1150) 
ggsave("Robbery.tiff", 
       Robbery, width = 140, height = 63, units = "mm", dpi = 200)

#Sex
sex_diff = (sex$rsev[20] - sex$rser[20])*100
Sex = ggplot(sex, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Sex") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(sex$date) + 800, 
           y = max(sex$rsev) -.4, 
           label = round(sex_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(sex$date) + 1150) 
ggsave("Sex.tiff", 
       Sex, width = 140, height = 63, units = "mm", dpi = 200)

#Miscellaneous Crimes Against Society
society_diff = (society$rsev[20] - society$rser[20])*100
Society = ggplot(society, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Miscellaneous") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(society$date) + 800, 
           y = max(society$rsev) -.3, 
           label = round(society_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(society$date) + 1150) 
ggsave("Society.tiff", 
       Society, width = 140, height = 63, units = "mm", dpi = 200)

#Theft
theft_diff = (theft$rsev[20] - theft$rser[20])*100
Theft = ggplot(theft, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Theft") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(theft$date) + 800, 
           y = theft$rsev[20] - .175, 
           label = round(theft_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(theft$date) + 1150) 
ggsave("Theft.tiff", 
       Theft, width = 140, height = 63, units = "mm", dpi = 200)

#Violence
violence_diff = (violence$rsev[20] - violence$rser[20])*100
Violence = ggplot(violence, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Violence") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(violence$date) + 800, 
           y = violence$rsev[20] - .4, 
           label = round(violence_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(violence$date) + 1150) 
ggsave("Violence.tiff", 
       Violence, width = 140, height = 63, units = "mm", dpi = 200)

#Weapons
weapons_diff = (weapons$rsev[20] - weapons$rser[20])*100
Weapons = ggplot(weapons, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("Weapons") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(weapons$date) + 800, 
           y = weapons$rsev[20] - .5, 
           label = round(weapons_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(weapons$date) + 1150) 
ggsave("Weapons.tiff", 
       Weapons, width = 140, height = 63, units = "mm", dpi = 200)

#Arranging the 10 plots in 1
inflation = ggarrange(Fraud, Weapons, Violence, Damage, Sex, Society, Theft, Order, 
                      Drugs, Robbery, ncol=2, nrow=5, 
                     heights=c(1,1,1,1), common.legend=TRUE, legend="bottom")

#Exporting the graph in high resolution
ggexport(inflation, width=1500, height=2000, res=200,
         filename="Inflation_jpeg.jpeg")
ggexport(inflation, width=1500, height=2000, res=200,
         filename="Inflation_tiff.tiff")

#Comparison across all sentences################################################

#Creating the dataset
all = data.frame(
  severity = c(1:20),
  seriousness = c(1:20),
  date = theft$date)

#Offence seriousness
all$seriousness = (damage$X56A_w*damage$X56A_N + damage$X56B_w*damage$X56B_N + 
                   damage$X58EJ_w*damage$X58EJ_N + damage$X58D_w*damage$X58D_N) /
                  (damage$X56A_N + damage$X56B_N + damage$X58EJ_N + damage$X58D_N) + 
                  (drugs$X92A_w*drugs$X92A_N + drugs$X92B_w*drugs$X92B_N + 
                   drugs$X92C_w*drugs$X92C_N) / 
                  (drugs$X92A_N + drugs$X92B_N + drugs$X92C_N) +
                  (fraud$X51_w*fraud$X51_N + fraud$X52_w*fraud$X52_N + fraud$X53B_w*fraud$X53B_N + 
                   fraud$X53C_w*fraud$X53C_N + fraud$X53D_w*fraud$X53D_N + fraud$X53E_w*fraud$X53E_N + 
                   fraud$X53F_w*fraud$X53F_N + fraud$X55_w*fraud$X55_N) /
                  (fraud$X51_N + fraud$X52_N + fraud$X53B_N + fraud$X53C_N + 
                   fraud$X53D_N + fraud$X53E_N + fraud$X53F_N + fraud$X55_N) +
                  (order$X64_w*order$X64_N + order$X65_w*order$X65_N +
                   order$X66_w*order$X66_N) / 
                  (order$X64_N + order$X65_N + order$X66_N) +
                  (robbery$X34_w*robbery$X34_N) / robbery$X34_N +
                  (sex$X19C_w*sex$X19C_N + sex$X19D_w*sex$X19D_N + sex$X19E_w*sex$X19E_N + 
                   sex$X19F_w*sex$X19F_N + sex$X19G_w*sex$X19G_N + sex$X19H_w*sex$X19H_N + 
                   sex$X17A_w*sex$X17A_N + sex$X17B_w*sex$X17B_N + sex$X20A_w*sex$X20A_N + 
                   sex$X20B_w*sex$X20B_N + sex$X21_w*sex$X21_N + sex$X22_w*sex$X22_N + 
                   sex$X16_w*sex$X16_N + sex$X22A_w*sex$X22A_N +
                   sex$X23_w*sex$X23_N + sex$X25_w*sex$X25_N + sex$X70_w*sex$X70_N + 
                   sex$X71_w*sex$X71_N + sex$X72_w*sex$X72_N + sex$X73_w*sex$X73_N + 
                   sex$X74_w*sex$X74_N + sex$X88A_w*sex$X88A_N + sex$X88B_w*sex$X88B_N + 
                   sex$X88C_w*sex$X88C_N + sex$X88D_w*sex$X88D_N + sex$X88E_w*sex$X88E_N) /
                  (sex$X19C_N + sex$X19D_N + sex$X19E_N + sex$X19F_N + sex$X19G_N + 
                   sex$X19H_N + sex$X17A_N + sex$X17B_N + sex$X20A_N + sex$X20B_N + 
                   sex$X21_N + sex$X22_N + sex$X16_N + sex$X22A_N +
                   sex$X23_N + sex$X25_N + sex$X70_N + sex$X71_N + sex$X72_N + sex$X73_N + 
                   sex$X74_N + sex$X88A_N + sex$X88B_N + sex$X88C_N + sex$X88D_N + 
                   sex$X88E_N) +
                  (society$X24_w*society$X24_N + society$X27_w*society$X27_N + 
                   society$X33_w*society$X33_N + society$X38_w*society$X38_N + 
                   society$X54_w*society$X54_N + society$X59_w*society$X59_N + 
                   society$X60_w*society$X60_N + society$X61_w*society$X61_N + 
                   society$X61A_w*society$X61A_N + society$X67_w*society$X67_N + 
                   society$X75_w*society$X75_N + society$X76_w*society$X76_N + 
                   society$X78_w*society$X78_N + society$X79_w*society$X79_N + 
                   society$X80_w*society$X80_N + society$X82_w*society$X82_N + 
                   society$X83_w*society$X83_N + society$X84_w*society$X84_N + 
                   society$X85_w*society$X85_N + society$X86_w*society$X86_N + 
                   society$X87_w*society$X87_N + society$X89_w*society$X89_N + 
                   society$X91_w*society$X91_N + society$X94_w*society$X94_N + 
                   society$X95_w*society$X95_N + society$X99_w*society$X99_N + 
                   society$X802_w*society$X802_N + society$X814_w*society$X814_N) /
                  (society$X24_N + society$X27_N + society$X33_N + society$X38_N + 
                   society$X54_N + society$X59_N + society$X60_N + society$X61_N + 
                   society$X61A_N + society$X67_N + society$X75_N + society$X76_N + 
                   society$X78_N + society$X79_N + society$X80_N + society$X82_N + 
                   society$X83_N + society$X84_N + society$X85_N + society$X86_N + 
                   society$X87_N + society$X89_N + society$X91_N + society$X94_N + 
                   society$X95_N + society$X99_N + society$X802_N + society$X814_N) + 
                  (theft$X28_w*theft$X28_N + theft$X29_w*theft$X29_N + 
                   theft$X30_w*theft$X30_N + theft$X31_w*theft$X31_N + 
                   theft$X37_w*theft$X37_N + theft$X45_w*theft$X45_N + 
                   theft$X48_w*theft$X48_N + theft$X126_w*theft$X126_N + 
                   theft$X39_w*theft$X39_N + theft$X44_w*theft$X44_N + 
                   theft$X46_w*theft$X46_N + theft$X35_w*theft$X35_N + 
                   theft$X40_w*theft$X40_N + theft$X41_w*theft$X41_N + 
                   theft$X42_w*theft$X42_N + theft$X43_w*theft$X43_N + 
                   theft$X47_w*theft$X47_N + theft$X49_w*theft$X49_N) / 
                  (theft$X28_N + theft$X29_N + theft$X30_N + theft$X31_N + 
                   theft$X37_N + theft$X45_N + theft$X48_N + theft$X126_N + 
                   theft$X39_N + theft$X44_N + theft$X46_N + theft$X35_N + 
                   theft$X40_N + theft$X41_N + theft$X42_N + theft$X43_N + 
                   theft$X47_N + theft$X49_N) +
                  (violence$X1_w*violence$X1_N + violence$X4.4_w*violence$X4.4_N + 
                   violence$X4.6_w*violence$X4.6_N + violence$X4.8_w*violence$X4.8_N + 
                   violence$X4.9_w*violence$X4.9_N + violence$X37.1_w*violence$X37.1_N + 
                   violence$X2_w*violence$X2_N + violence$X4.3_w*violence$X4.3_N + 
                   violence$X5A_w*violence$X5A_N + violence$X5D_w*violence$X5D_N + 
                   violence$X5E_w*violence$X5E_N + violence$X6_w*violence$X6_N + 
                   violence$X7_w*violence$X7_N + violence$X8F_w*violence$X8F_N + 
                   violence$X8H_w*violence$X8H_N + violence$X4.7_w*violence$X4.7_N + 
                   violence$X8G_w*violence$X8G_N + violence$X8J_w*violence$X8J_N + 
                   violence$X8N_w*violence$X8N_N + violence$X3A_w*violence$X3A_N + 
                   violence$X3B_w*violence$X3B_N + violence$X11_w*violence$X11_N + 
                   violence$X12_w*violence$X12_N + violence$X13_w*violence$X13_N + 
                   violence$X14_w*violence$X14_N + violence$X36_w*violence$X36_N + 
                   violence$X104_w*violence$X104_N + violence$X105A_w*violence$X105A_N + 
                   violence$X105B_w*violence$X105B_N + violence$X106_w*violence$X106_N + 
                   violence$X8L_w*violence$X8L_N + violence$X8M_w*violence$X8M_N + 
                   violence$X8Q_w*violence$X8Q_N + violence$X8R_w*violence$X8R_N + 
                   violence$X8U_w*violence$X8U_N) /   
                  (violence$X1_N + violence$X4.4_N + violence$X4.6_N + violence$X4.8_N + 
                   violence$X4.9_N + violence$X37.1_N + violence$X2_N + violence$X4.3_N + 
                   violence$X5A_N + violence$X5D_N + violence$X5E_N + violence$X6_N + 
                   violence$X7_N + violence$X8F_N + violence$X8H_N + violence$X4.7_N + 
                   violence$X8G_N + violence$X8J_N + violence$X8N_N + violence$X3A_N + 
                   violence$X3B_N + violence$X11_N + violence$X12_N + violence$X13_N + 
                   violence$X14_N + violence$X36_N + violence$X104_N + violence$X105A_N + 
                   violence$X105B_N + violence$X106_N + violence$X8L_N +violence$X8M_N + 
                   violence$X8Q_N + violence$X8R_N + violence$X8U_N) +
                  (weapons$X10A_w*weapons$X10A_N + weapons$X10B_w*weapons$X10B_N + 
                   weapons$X10C_w*weapons$X10C_N +  weapons$X10D_w*weapons$X10D_N +  
                   weapons$X81_w*weapons$X81_N) /
                  (weapons$X10A_N + weapons$X10B_N + weapons$X10C_N + weapons$X10D_N + 
                   weapons$X81_N)  

#Sentence severity
all$severity = (damage$X56A_cust*damage$X56A_acsl + damage$X56B_cust*damage$X56B_acsl + 
                damage$X58EJ_cust*damage$X58EJ_acsl + damage$X58D_cust*damage$X58D_acsl) /
               (damage$X56A_N + damage$X56B_N + damage$X58EJ_N + damage$X58D_N) +   
               (drugs$X92A_cust*drugs$X92A_acsl + drugs$X92B_cust*drugs$X92B_acsl + 
                drugs$X92C_cust*drugs$X92C_acsl) / 
               (drugs$X92A_N + drugs$X92B_N + drugs$X92C_N) + 
               (fraud$X51_cust*fraud$X51_acsl + fraud$X52_cust*fraud$X52_acsl + fraud$X53B_cust*fraud$X53B_acsl + 
                fraud$X53C_cust*fraud$X53C_acsl + fraud$X53D_cust*fraud$X53D_acsl + fraud$X53E_cust*fraud$X53E_acsl + 
                fraud$X53F_cust*fraud$X53F_acsl + fraud$X55_cust*fraud$X55_acsl) /
               (fraud$X51_N + fraud$X52_N + fraud$X53B_N + fraud$X53C_N + 
                fraud$X53D_N + fraud$X53E_N + fraud$X53F_N + fraud$X55_N) + 
               (order$X64_cust*order$X64_acsl + order$X65_cust*order$X65_acsl +
                order$X66_cust*order$X66_acsl) / 
               (order$X64_N + order$X65_N + order$X66_N) + 
               (robbery$X34_cust*robbery$X34_acsl) / robbery$X34_N +
               (sex$X19C_cust*sex$X19C_acsl + sex$X19D_cust*sex$X19D_acsl + sex$X19E_cust*sex$X19E_acsl + 
                sex$X19F_cust*sex$X19F_acsl + sex$X19G_cust*sex$X19G_acsl + sex$X19H_cust*sex$X19H_acsl + 
                sex$X17A_cust*sex$X17A_acsl + sex$X17B_cust*sex$X17B_acsl + sex$X20A_cust*sex$X20A_acsl + 
                sex$X20B_cust*sex$X20B_acsl + sex$X21_cust*sex$X21_acsl + sex$X22_cust*sex$X22_acsl + 
                sex$X16_cust*sex$X16_acsl + sex$X22A_cust*sex$X22A_acsl +
                sex$X23_cust*sex$X23_acsl + sex$X25_cust*sex$X25_acsl + sex$X70_cust*sex$X70_acsl + 
                sex$X71_cust*sex$X71_acsl + sex$X72_cust*sex$X72_acsl + sex$X73_cust*sex$X73_acsl + 
                sex$X74_cust*sex$X74_acsl + sex$X88A_cust*sex$X88A_acsl + sex$X88B_cust*sex$X88B_acsl + 
                sex$X88C_cust*sex$X88C_acsl + sex$X88D_cust*sex$X88D_acsl + sex$X88E_cust*sex$X88E_acsl) /
               (sex$X19C_N + sex$X19D_N + sex$X19E_N + sex$X19F_N + sex$X19G_N + 
                sex$X19H_N + sex$X17A_N + sex$X17B_N + sex$X20A_N + sex$X20B_N + 
                sex$X21_N + sex$X22_N + sex$X16_N + sex$X22A_N +
                sex$X23_N + sex$X25_N + sex$X70_N + sex$X71_N + sex$X72_N + sex$X73_N + 
                sex$X74_N + sex$X88A_N + sex$X88B_N + sex$X88C_N + sex$X88D_N + 
                sex$X88E_N) +
               (society$X24_cust*society$X24_acsl + society$X27_cust*society$X27_acsl + 
                society$X33_cust*society$X33_acsl + society$X38_cust*society$X38_acsl + 
                society$X54_cust*society$X54_acsl + society$X59_cust*society$X59_acsl + 
                society$X60_cust*society$X60_acsl + society$X61_cust*society$X61_acsl + 
                society$X61A_cust*society$X61A_acsl + society$X67_cust*society$X67_acsl + 
                society$X75_cust*society$X75_acsl + society$X76_cust*society$X76_acsl + 
                society$X78_cust*society$X78_acsl + society$X79_cust*society$X79_acsl + 
                society$X80_cust*society$X80_acsl + society$X82_cust*society$X82_acsl + 
                society$X83_cust*society$X83_acsl + society$X84_cust*society$X84_acsl + 
                society$X85_cust*society$X85_acsl + society$X86_cust*society$X86_acsl + 
                society$X87_cust*society$X87_acsl + society$X89_cust*society$X89_acsl + 
                society$X91_cust*society$X91_acsl + society$X94_cust*society$X94_acsl + 
                society$X95_cust*society$X95_acsl + society$X99_cust*society$X99_acsl + 
                society$X802_cust*society$X802_acsl + society$X814_cust*society$X814_acsl) /
               (society$X24_N + society$X27_N + society$X33_N + society$X38_N + 
                society$X54_N + society$X59_N + society$X60_N + society$X61_N + 
                society$X61A_N + society$X67_N + society$X75_N + society$X76_N + 
                society$X78_N + society$X79_N + society$X80_N + society$X82_N + 
                society$X83_N + society$X84_N + society$X85_N + society$X86_N + 
                society$X87_N + society$X89_N + society$X91_N + society$X94_N + 
                society$X95_N + society$X99_N + society$X802_N + society$X814_N) + 
               (theft$X28_cust*theft$X28_acsl + theft$X29_cust*theft$X29_acsl + 
                theft$X30_cust*theft$X30_acsl + theft$X31_cust*theft$X31_acsl + 
                theft$X37_cust*theft$X37_acsl + theft$X45_cust*theft$X45_acsl + 
                theft$X48_cust*theft$X48_acsl + theft$X126_cust*theft$X126_acsl + 
                theft$X39_cust*theft$X39_acsl + theft$X44_cust*theft$X44_acsl + 
                theft$X46_cust*theft$X46_acsl + theft$X35_cust*theft$X35_acsl + 
                theft$X40_cust*theft$X40_acsl + theft$X41_cust*theft$X41_acsl + 
                theft$X42_cust*theft$X42_acsl + theft$X43_cust*theft$X43_acsl + 
                theft$X47_cust*theft$X47_acsl + theft$X49_cust*theft$X49_acsl) / 
               (theft$X28_N + theft$X29_N + theft$X30_N + theft$X31_N + 
                theft$X37_N + theft$X45_N + theft$X48_N + theft$X126_N + 
                theft$X39_N + theft$X44_N + theft$X46_N + theft$X35_N + 
                theft$X40_N + theft$X41_N + theft$X42_N + theft$X43_N + 
                theft$X47_N + theft$X49_N) + 
               (violence$X1_cust*violence$X1_acsl + violence$X4.4_cust*violence$X4.4_acsl + 
                violence$X4.6_cust*violence$X4.6_acsl + violence$X4.8_cust*violence$X4.8_acsl + 
                violence$X4.9_cust*violence$X4.9_acsl + violence$X37.1_cust*violence$X37.1_acsl + 
                violence$X2_cust*violence$X2_acsl + violence$X4.3_cust*violence$X4.3_acsl + 
                violence$X5A_cust*violence$X5A_acsl + violence$X5D_cust*violence$X5D_acsl + 
                violence$X5E_cust*violence$X5E_acsl + violence$X6_cust*violence$X6_acsl + 
                violence$X7_cust*violence$X7_acsl + violence$X8F_cust*violence$X8F_acsl + 
                violence$X8H_cust*violence$X8H_acsl + violence$X4.7_cust*violence$X4.7_acsl + 
                violence$X8G_cust*violence$X8G_acsl + violence$X8J_cust*violence$X8J_acsl + 
                violence$X8N_cust*violence$X8N_acsl + violence$X3A_cust*violence$X3A_acsl + 
                violence$X3B_cust*violence$X3B_acsl + violence$X11_cust*violence$X11_acsl + 
                violence$X12_cust*violence$X12_acsl + violence$X13_cust*violence$X13_acsl + 
                violence$X14_cust*violence$X14_acsl + violence$X36_cust*violence$X36_acsl + 
                violence$X104_cust*violence$X104_acsl + violence$X105A_cust*violence$X105A_acsl + 
                violence$X105B_cust*violence$X105B_acsl + violence$X106_cust*violence$X106_acsl + 
                violence$X8L_cust*violence$X8L_acsl + violence$X8M_cust*violence$X8M_acsl + 
                violence$X8Q_cust*violence$X8Q_acsl + violence$X8R_cust*violence$X8R_acsl + 
                violence$X8U_cust*violence$X8U_acsl) /   
               (violence$X1_N + violence$X4.4_N + violence$X4.6_N + violence$X4.8_N + 
                violence$X4.9_N + violence$X37.1_N + violence$X2_N + violence$X4.3_N + 
                violence$X5A_N + violence$X5D_N + violence$X5E_N + violence$X6_N + 
                violence$X7_N + violence$X8F_N + violence$X8H_N + violence$X4.7_N + 
                violence$X8G_N + violence$X8J_N + violence$X8N_N + violence$X3A_N + 
                violence$X3B_N + violence$X11_N + violence$X12_N + violence$X13_N + 
                violence$X14_N + violence$X36_N + violence$X104_N + violence$X105A_N + 
                violence$X105B_N + violence$X106_N + violence$X8L_N + violence$X8M_N + 
                violence$X8Q_N + violence$X8R_N + violence$X8U_N) + 
               (weapons$X10A_cust*weapons$X10A_acsl + weapons$X10B_cust*weapons$X10B_acsl + 
                weapons$X10C_cust*weapons$X10C_acsl +  weapons$X10D_cust*weapons$X10D_acsl +  
                weapons$X81_cust*weapons$X81_acsl) /
               (weapons$X10A_N + weapons$X10B_N + weapons$X10C_N + weapons$X10D_N + 
                weapons$X81_N)                  

#Expressing change in relative terms
all$rser = all$seriousness / all$seriousness[1]
all$rsev = all$severity / all$severity[1]

#All crime
all_diff = (all$rsev[20] - all$rser[20])*100
All = ggplot(all, aes(x=date)) +
  geom_line(aes(y = rser, linetype = "Crime seriousness"), color = "black", size = 0.7) + 
  geom_line(aes(y = rsev, linetype = "Sentence severity"), color = "black", size = 0.6) + 
  ylab("") +  xlab("") +  labs(linetype = "") +  
  theme_bw() + ggtitle("England & Wales") +
  scale_linetype_manual(
    values = c("Sentence severity" = "solid", "Crime seriousness" = "dashed"), 
    breaks = c("Sentence severity", "Crime seriousness")) + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family="serif", size=12),
        axis.title.x = element_text(family="serif", size=11),
        axis.title.y = element_text(family="serif", size=11),
        axis.text = element_text(size=8),  
        legend.title = element_text(family="serif", size=11),
        legend.text = element_text(family="serif", size=12)) +  
  annotate("text", x = max(all$date) + 800, 
           y = all$rsev[20] - .23, 
           label = round(all_diff,1), size = 3.5, 
           fontface = "bold", family="serif") +  
  expand_limits(x = max(all$date) + 1150) 
ggsave("All_jpeg.jpeg", 
       All, width = 140, height = 63, units = "mm", dpi = 200)
ggsave("All_tiff.tiff", 
       All, width = 140, height = 63, units = "mm", dpi = 200)

