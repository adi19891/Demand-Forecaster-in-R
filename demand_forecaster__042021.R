install.packages("pim")
install.packages("brms")

#################################
### Loading required packages ###
#################################

library(readxl)
library(pim)
library(brms)
library(lubridate)
library(ggplot2)


###############################
### Import data file #########
##############################

canada_travel = read_excel("C:/Users/luke.rathbone/Desktop/Demand Forecaster/Canada_Modelfile_041921.xlsx", sheet="Modelfile")


####################
### Model
##################
# IMPORTANT Notes.
## 1. BRMS package & STAN throw errors when variable names have special characters.


### Prior List.

prior_list<- 
  c(
    set_prior("normal(0,10)", class = "b",nlpar = "bcc",ub=-0.0001),
    set_prior("normal(0,10)", class = "b",nlpar = "bcd",ub=-0.0001),
    set_prior("normal(0,10)", class = "b",nlpar = "bcv",ub=-0.0001),
    set_prior("normal(0,10)", class = "b",nlpar = "bld",lb=-50,ub=-0.001),
    set_prior("normal(0,10)", class = "b",nlpar = "bconf",lb=0.5,ub=10),
    set_prior("normal(0,10)", class = "b",nlpar = "bs",ub=10),
    set_prior("normal(0,10)", class = "b",nlpar = "bvac",lb=0.000001,ub=10)
    )



fit_brms <- brm(
                bf(
                  expedia_si ~ (bcc*covid_cases) + (bcd* covid_deaths) + (bcv * coronavirus_si) + (bld *lockdown) + (bconf * cci) + (bs * seasonality) + (bvac * covid_vaccines),
                  bcc + bcd + bcv + bld + bconf + bs + bvac ~ 1,
                  nl = TRUE
                ),
                data= canada_travel, 
                family = "gaussian",
                prior = prior_list,
                control = list(adapt_delta = 0.97),
                chains = 4,
                warmup = 1000,
                iter = 5000,
                cores = 3
                )

######################
### Fit chart #######
#####################
predicted_values <- as.data.frame(predict(fit_brms , canada_travel))



fit_df <- cbind.data.frame(canada_travel$date , canada_travel$expedia_si , predicted_values$Estimate)

colnames(fit_df)[1] <- "Date"
colnames(fit_df)[2] <- "Actual"
colnames(fit_df)[3] <- "Predicted"
rm(predicted_list)

ggplot(fit_df , aes(x = Date)) + 
  geom_line(aes(y = Actual , color = "Actual")) + 
  geom_line(aes(y = Predicted , color = "Predicted")) + 
  xlab("Date") + 
  ylab("Travel Trends") 



##Coeffs##

fixef(fit_brms)
ranef(fit_brms)

coef(fit_brms)

bayes_R2(fit_brms)

#####################################
#### PREDICTION : V RECOVERY #######
####################################

## Import Data FIle.
v_recov = read_excel("C:/Users/Tushar.Subramaniam/Documents/Demand Forecaster/Canada/Canada_Modelfile_05112020.xlsx", sheet="v-recov")

## Predict.
predicted_vrecov <- as.data.frame(predict(fit_brms , v_recov))

## Plot.
fit_vrecov <- cbind.data.frame(v_recov$date , predicted_vrecov$Estimate)

colnames(fit_vrecov)[1] <- "Date"
colnames(fit_vrecov)[2] <- "Travel"

ggplot(fit_vrecov , aes(x = Date)) + 
  geom_line(aes(y = Travel , color = "V-Recovery Travel Demand")) + 
  xlab("Date") + 
  ylab("V-Recovery Travel Demand") 


#####################################
#### PREDICTION : U RECOVERY #######
####################################

## Import Data FIle.
u_recov = read_excel("C:/Users/luke.rathbone/Desktop/Demand Forecaster/Canada_Modelfile_041921.xlsx", sheet="u-recov")

## Predict.
predicted_urecov <- as.data.frame(predict(fit_brms , u_recov))

## Plot.
fit_urecov <- cbind.data.frame(u_recov$date , predicted_urecov$Estimate)

colnames(fit_urecov)[1] <- "Date"
colnames(fit_urecov)[2] <- "Travel"

ggplot(fit_urecov , aes(x = Date)) + 
  geom_line(aes(y = Travel , color = "U-Recovery Travel Demand")) + 
  xlab("Date") + 
  ylab("U-Recovery Travel Demand") 


print(u_recov)
print(predicted_urecov)




