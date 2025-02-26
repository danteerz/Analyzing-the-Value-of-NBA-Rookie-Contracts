library(dplyr)
library(tidyverse)
library(ggplot2)
library(AER)
library(adaptTest)
library(haven)
library(readxl)
library(plm)
library(mlogit)
library(car)
library(MASS)
library(leaps)
library(knitr)
library(lmtest)
library(readr)

#Read in Data. Note that NBAData has years 2002-2019 
#and years 2020-2023 are read in individually.

NBAData <- read_excel('nbaplayerdata.xlsx')
DraftData <- read_csv('nbaplayersdraft.csv')
DraftData2022 <- read_csv('DraftData2022.txt')
NBAstats2020 <- read_csv('NBA_Stats_2020.txt')
NBAstats2021 <- read_csv('NBA_Stats_2021.txt')
NBAstats2022 <- read_csv('NBA_Stats_2022.txt')
NBAstats2023 <- read_csv('NBA_Stats_2023.txt')
NBAData_Adv2020 <- read_csv('AdvStats2020.txt')
NBAData_Adv2021 <- read_csv('AdvStats2021.txt')
NBAData_Adv2022 <- read_csv('AdvStats2022.txt')
NBAData_Adv2023 <- read_csv('AdvStats2023.txt')
NBAsalary2020 <- read_csv('NBA_Salaries_2020.csv')
NBAsalary2021 <- read_csv('NBA_Salaries_2021.csv')
NBAsalary2022 <- read_csv('NBA_Salaries_2022.csv')
NBAsalary2023 <- read_csv('NBA_Salaries_2023.csv')

#Join the NBA player statistics data and the player 
#salary data together for each year 

NBAData2020 <- left_join(NBAstats2020, NBAData_Adv2020, by = 'Player')
NBAData2020 <- left_join(NBAData2020, NBAsalary2020, by = 'Player')
NBAData2021 <- left_join(NBAstats2021, NBAData_Adv2021, by = 'Player')
NBAData2021 <- left_join(NBAData2021, NBAsalary2021, by = 'Player')
NBAData2022 <- left_join(NBAstats2022, NBAData_Adv2022, by = 'Player')
NBAData2022 <- left_join(NBAData2022, NBAsalary2022, by = 'Player')
NBAData2023 <- left_join(NBAstats2023, NBAData_Adv2023, by = 'Player')
NBAData2023 <- left_join(NBAData2023, NBAsalary2023, by = 'Player')

#Clean the data by specifying column names and deleting unwanted columns

colnames(DraftData2022) <- DraftData2022[1,]
DraftData2022 <- DraftData2022[-1,]
DraftData2022 <- DraftData2022|>
  dplyr::select(Rk, Player, Pk)|>
  rename(player = Player,
         rank = Rk,
         overall_pick = Pk)|>
  mutate(year = 2022)

#Transform rank and overall_pick columns to a double object type

DraftData2022$rank <- as.double(DraftData2022$rank)
DraftData2022$overall_pick <- as.double(DraftData2022$overall_pick)

#Join draft data sets together

DraftData <- full_join(DraftData, DraftData2022)

#Add a new column 'year'

NBAData2020 <- NBAData2020|>
  mutate(year = 2020)
NBAData2021 <- NBAData2021|>
  mutate(year = 2021)
NBAData2022 <- NBAData2022|>
  mutate(year = 2022)
NBAData2023 <- NBAData2023|>
  mutate(year = 2023)

#Join player data together for years 2020, 21, 22, and 23.

NBAData2 <- full_join(NBAData2020, NBAData2021)
NBAData2 <- full_join(NBAData2, NBAData2022)
NBAData2 <- full_join(NBAData2, NBAData2023)

#Manipulate data to remove $ from salary numbers, 
#rename columns, and only keep wanted columns.

NBAData2 <- NBAData2|>
  mutate(Salary = parse_number(Salary))|>
  rename(salary = Salary,
         age = "Age.x",
         player = Player,
         winshares = WS,
         per = PER,
         games = "G.x")|>
  dplyr::select(player, age, salary, winshares, per, year, games)

#Join the two data sets together

NBAData <- full_join(NBAData, NBAData2)

#Delete any unwanted columns from the DraftData data set.

DraftData <- DraftData |>
  dplyr::select(player, overall_pick)

#Delete any unwanted columns from the NBAData data set.

NBAData <- NBAData |>
  dplyr::select(player, year, age, salary, winshares, per, games)

#Join the DraftData data set to the NBAData data set.

NBAData <- left_join(NBAData, DraftData)

#Create a new variable 'cap' (the cap space of an NBA team). 
#Specify the value based on 'year'.

NBAData$cap <-ifelse(NBAData$year == 2005, 49500000,
              ifelse(NBAData$year == 2006, 53135000,
              ifelse(NBAData$year == 2007, 55630000,
              ifelse(NBAData$year == 2008, 58680000,
              ifelse(NBAData$year == 2009, 57700000,
              ifelse(NBAData$year == 2010, 58040000,
              ifelse(NBAData$year == 2011, 58044000,
              ifelse(NBAData$year == 2012, 58044000,
              ifelse(NBAData$year == 2013, 58679000,
              ifelse(NBAData$year == 2014, 63065000,
              ifelse(NBAData$year == 2015, 70000000,
              ifelse(NBAData$year == 2016, 94143000,
              ifelse(NBAData$year == 2017, 99093000,
              ifelse(NBAData$year == 2018, 101869000,
              ifelse(NBAData$year == 2019, 109140000,
              ifelse(NBAData$year == 2020, 109140000,
              ifelse(NBAData$year == 2021, 112414000,
              ifelse(NBAData$year == 2022, 123655000, 136021000
                     ))))))))))))))))))

#Create a new variable 'Experience' (A player's number of years in the NBA) 
#from how many times a player appears in the data. Create another new variable
#'salary_pctcap' (a player's salary as a percentage of the total salary cap).

NBAData <- NBAData |>
  group_by(player) |>
  mutate(experience = row_number(),
         salary_pctcap = salary/cap)

#Remove NA values from the data 

NBAData <- na.omit(NBAData)

#Reove players who played under 19 games in a season 
#to avoid small sample sizes skewing the results.

NBAData <- NBAData |>
  filter(games > 19)

#Create a new data set made up of only players 
#in their first four years in the NBA

NBAData_rookies <- NBAData |>
  filter(experience <= 4,
         salary != 0,
         overall_pick <= 30,
         age <= 26,
         age < 25 | salary < 6000000,
         salary_pctcap < .126)

#Create a new data set of 

NBAData_vets <- NBAData |>
  filter(experience > 4,
         salary != 0,
         overall_pick <= 30)

#Remove an outlier from the NBAData_vets data et.

NBAData_vets <- NBAData_vets[-860, ]

#Remove max salary players from the data set because their salary is capped, 
#so it is not an accurate depiction of their market value.

NBAData_vets <- NBAData_vets|>
  filter(salary_pctcap < .25)

#Create a training data set from the NBAData_vets data set

numtrain_vets <- ceiling(.8 * 2278) 
set.seed(333)
train_ind_vets <- sample(2278, numtrain_vets)
traindata_vets <- NBAData_vets[train_ind_vets, ] 

#Perform a Box-Cox procedure to test determine how to transform the response
#variable, 'salary_pctcap'. The procedure indicated to do a square root 
#transformation.

boxcox(salary_pctcap ~ winshares + per, data = traindata_vets, plotit=TRUE, 
       lambda=seq(-2,3,length=100))

#Create a model to determine the effect of PER and win shares, our chosen
#performance metrics, on player salary. 

vet_mod <- lm(sqrt(salary_pctcap) ~ winshares + per, data = traindata_vets)
summary(vet_mod)
coeftest(vet_mod, vcov = vcovHC(vet_mod, type="HC1"))

#Perform a Shapiro-Wilk test to check whether the model meets the normality
#assumption.

shapiro.test(resid(vet_mod))

#Perform a Breusch-Pagan test for heteroskedasticity.

bptest(vet_mod)

#Check the variance inflation factors of 'winshares' and 'per' to check for
#collinearity.

vif(vet_mod)
plot(vet_mod)

#Create a validation data set to test the predictive ability of the model.

testdata_vets <- NBAData_vets[-train_ind_vets, ]

#Predict the salary for veteran players using the vet_mod model.

predicted_vets <- predict(vet_mod, testdata_vets)

#Undo the square root transformation by squaring the results.

predicted_vets <- predicted_vets^2

#Create a new object, 'actual_vets' to indicate the veteran players' true 
#salary.

actual_vets <- testdata_vets$salary_pctcap

#Create the object 'difference_vets' to indicate the difference between actual
#and predicted salary.

difference_vets <- predicted_vets - actual_vets

#Square the results so all values are positive.

difference_vetssq = difference_vets^2

#Calculate the mean squared prediction error.

MSPE_vets <- mean(difference_vetssq)

#Calculate the mean squared error

summ_vets <- summary(vet_mod)
MSE_vets <- mean(summ_vets$residuals^2)

#Create an object 'e_i_vets' for the vet_mod residuals.

e_i_vets <- resid(vet_mod) 

#Create an object 'h_i_vets' for the leverages.

h_i_vets <- hatvalues(vet_mod) 

#Calculate the sum of squares total.

SST_vets <- sum((anova(vet_mod))$'Sum Sq') 

#Calculate the  predicted residual error sum of squares 

PRESS_vets <- sum( (e_i_vets/(1-h_i_vets))^2 )

#Calculate the predictive R squared.

Pred_Rsq_vets <- 1-PRESS_vets/SST_vets

#Calculate the sum of squares error.

SSE_vets <- sum(vet_mod$residuals^2)

#To ensure only rookie contract players are in the 'rookieData' data set, remove
#all players from before 2004.

rookieData <- NBAData_rookies|>
  filter(year > 2004)

#Predict the market salary of rookie contract players using the 'vet_mod' model.

rookieData$market_salary_sqrt <- predict(vet_mod, newdata = rookieData)
rookieData$market_salary <- rookieData$market_salary_sqrt^2

#Calculate rookie team surplus by subtracting actual salary from market salary.
#Adjust team surplus so that all values are positive and multiply 'team_surplus'
#and 'cap' (salary cap) to create a dollar figure.

rookieData <- rookieData |>
  mutate(team_surplus = market_salary - salary_pctcap,
         team_surplus_adj = team_surplus + .055,
         team_suplus_sal = team_surplus * cap,
         team_surplus_2023 = team_surplus * 136021000)

#Remove outliers.

rookieData <- rookieData[-c(12, 75, 863), ]

#Create a training data set from the rookie data.

numtrain_rooks <- ceiling(.8 * 1605) 
set.seed(333)
train_ind_rooks <- sample(1605, numtrain_rooks)
traindata_rooks <- rookieData[train_ind_rooks, ] 

#Create a new data set 'mean_surplus' that contains the mean team surplus, win
#shares, salary, and surplus as a percentage of the cap for each draft pick.

mean_surplus <- rookieData |>
  group_by(overall_pick) |>
  summarise(mean_surplus = mean(team_suplus_sal),
            mean_winshares = mean(winshares),
            mean_salary = mean(salary),
            mean_surpluspct = mean(team_surplus),
            mean_surplus_2023 = mean_surpluspct * 136021000,
            salary_2023 = mean(salary_pctcap) * 136021000) 

#Create a graph of mean team surplus by draft pick.

mean_surplus |>
  ggplot(aes(x = overall_pick, y = mean_surplus_2023)
  )+
  geom_line() + 
  theme_minimal() +
  labs(x = "Overall Draft Pick",
       y = "",
       subtitle = "Mean Team Surplus (2023 $)"
  )

#Create a plot of mean win shares by draft pick.

mean_surplus |>
  ggplot(aes(x = overall_pick, y = mean_winshares)
  ) +
  geom_line() +
  theme_minimal() +
  labs(x = "Overall Draft Pick",
       y = "",
       subtitle = "Mean Win Shares")

#Create a plot of mean salary by draft pick.

mean_surplus |>
  ggplot(aes(x = overall_pick, y = mean_salary)
  ) +
  geom_line() +
  theme_minimal() +
  labs(x = "Overall Draft Pick",
       y = "",
       subtitle = "Mean Salary ($)")

#Perform a box-cox transformation to check the correct transformation for the 
#team surplus response variable.

boxcox(team_surplus_adj ~ overall_pick + experience, data = rookieData, 
       plotit=TRUE, lambda=seq(-2,3,length=100))

#Create a model of team surplus predicted by overall draft pick with years of 
#experience as a blocking variable. Perform a logarithmic transformation to 
#address model assumptions.

rook_mod <- lm(log(team_surplus_adj) ~ overall_pick + experience, 
               data = traindata_rooks)
coeftest(rook_mod, vcov = vcovHC(rook_mod, type="HC1"))

summary(rook_mod)

#Perform a Shapiro-Wilk test to check whether the model meets the normality
#assumption.

shapiro.test(resid(rook_mod))

#Perform a Breusch-Pagan test for heteroskedasticity.

bptest(rook_mod)

#Check the variance inflation factors of 'winshares' and 'per' to test for
#collinearity.

vif(rook_mod)

plot(rook_mod)

#Separate the test data set.

testdata_rooks <- rookieData[-train_ind_rooks, ]

#Predict the team surplus for rookie scale players using the rook_mod model.

predicted_rooks <- predict(rook_mod, testdata_rooks)

#Adjust for the transformations performed for the model.

predicted_rooks <- (exp(1)^(predicted_rooks)) - .055

#Create an object for the actual rookie scale players' team surplus.

actual_rooks <- testdata_rooks$team_surplus

#Calculate mean squared prediction error.

difference_rooks <- predicted_rooks - actual_rooks
difference_rookssq <- difference_rooks^2
MSPE_rooks <- mean(difference_rookssq)

#Calculate mean squared error.
summ_rooks <- summary(rook_mod)
mse_rooks <- mean(summ_rooks$residuals^2)

#Calculate residuals for rookie scale players.

e_i_rooks <- resid(rook_mod) 

#Calculate leverages for rookie scale players.

h_i_rooks <- hatvalues(rook_mod) 

#Calculate sum of squares total for rookie scale players.

SST_rooks <- sum((anova(rook_mod))$'Sum Sq') 

#Calculate the  predicted residual error sum of squares for rookie scale 
#players.

PRESS_rooks <- sum( (e_i_rooks/(1-h_i_rooks))^2 )

#Calculate the predictive R squared for rookie scale players.


Pred_Rsq_rooks <- 1-PRESS_rooks/SST_rooks

#Calculate the sum of squares error for rookie scale players.

SSE_rooks <- sum(rook_mod$residuals^2)