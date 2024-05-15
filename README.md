# March Madness Project

### Table of Contents

- [Project Overview](#project-overview)
- [Data Source](#data-source)
- [Tools](#tools)
- [Data Cleaning](#data-cleaning)
- [Data Analysis](#data-analysis)
- [Results](#results)
- [Shiny app](#shiny-app)

### Project Overview

This project analyzes historical information during the annual March Madness NCAA basketball tournament to predict the winners. Logistic regression was used as the classification model. In addition, a shiny app was created to visualize the win probabilites for any two matchups in the 2024 tournament.

### Data Source

The files used in this project come from the Machine Learning Mania Mania 2024 Kaggle dataset. [Download here](https://www.kaggle.com/competitions/march-machine-learning-mania-2024). These files contain information about teams such as regular season statistics, coaches, tourney seeds, and conferneces.

### Tools

- Excel
- R - Data cleaning, data analysis, and shiny app

### Data Cleaning

```r
#load in tidyverse and regular season stats
library(tidyverse)
reg_data <- read_csv("C:/Users/mason/OneDrive/Desktop/2024_march_madness/march-machine-learning-mania-2024/MRegularSeasonDetailedResults.csv")

#delineate winning teams and losing teams and fix column names
win_team = reg_data[c(1,3,4,6,9:34)]
lose_team = reg_data[c(1,4,5,6,9:34)]
colnames(win_team) = c("Season" , "TeamID", "Score" ,"oScore", "FGM"   , "FGA" ,  
                       "FGM3",   "FGA3",   "FTM"  ,  "FTA" ,   "OR"  ,  
                       "DR"  ,   "Ast"  ,  "TO"  ,   "Stl"  ,  "Blk",   
                       "PF" , "oFGM"   , "oFGA" ,  
                       "oFGM3",   "oFGA3",   "oFTM"  ,  "oFTA" ,   "oOR"  ,  
                       "oDR"  ,   "oAst"  ,  "oTO"  ,   "oStl"  ,  "oBlk",   
                       "oPF")
colnames(lose_team) = c("Season" ,"oScore","TeamID" ,"Score", "oFGM"   , "oFGA" ,  
                        "oFGM3",   "oFGA3",   "oFTM"  ,  "oFTA" ,   "oOR"  ,  
                        "oDR"  ,   "oAst"  ,  "oTO"  ,   "oStl"  ,  "oBlk",   
                        "oPF" , "FGM"   , "FGA" ,  
                        "FGM3",   "FGA3",   "FTM"  ,  "FTA" ,   "OR"  ,  
                        "DR"  ,   "Ast"  ,  "TO"  ,   "Stl"  ,  "Blk",   
                        "PF")
reg_season = rbind(win_team,lose_team)

#calculate season averages for each team and scale by season averages
reg_s_avg = aggregate(reg_season[3:30],by = list(reg_season$Season,reg_season$TeamID),FUN = mean)
colnames(reg_s_avg)[1] = "Season"
colnames(reg_s_avg)[2] = "TeamID"
reg_s_avg = reg_s_avg %>%
  mutate(Poss = (FGA+.4328*FTA-1.2228*(OR/(OR+oDR))*(FGA-FGM)+TO)+(oFGA+.4328*oFTA-1.2228*(oOR/(oOR+DR))*(oFGA-oFGM)+oTO)) %>%
  mutate(Poss = Poss/2) %>%
  mutate(OER = (Score/Poss)) %>%
  mutate(DER = (oScore/Poss)) %>%
  select(Season,TeamID,OER,DER,Poss)
library(mosaic)
reg_s_avg = reg_s_avg %>%
  group_by(Season) %>%
  mutate(OER = zscore(OER)) %>%
  mutate(DER = zscore(DER)) %>%
  mutate(Poss = zscore(Poss)) %>%
  ungroup()

#load team tournament data, seeds, and conference names
tourney_data <- read_csv("C:/Users/mason/OneDrive/Desktop/2024_march_madness/march-machine-learning-mania-2024/MNCAATourneyDetailedResults.csv")
seeds <- read_csv("C:/Users/mason/OneDrive/Desktop/2024_march_madness/march-machine-learning-mania-2024/MNCAATourneySeeds.csv")
conferences <- read_csv("C:/Users/mason/OneDrive/Desktop/2024_march_madness/march-machine-learning-mania-2024/MTeamConferences.csv")

#fix seed names
for (i in seq_along(seeds$Seed)){
  if (length(seeds$Seed[[i]] >= 4)){
    seeds$Seed[[i]] = substr(seeds$Seed[[i]],2,3)
  }
  
}

for (i in seq_along(seeds$Seed)){
  if (substr(seeds$Seed[[i]],1,1)==0){
    seeds$Seed[[i]] = substr(seeds$Seed[[i]],2,2)
  }
}

seeds = seeds[seeds$Season >= 2003,]

#finding the tourney teams for each season
team1w = data.frame(Season = tourney_data$Season,TeamID = tourney_data$WTeamID, Win = 1 )
team1l = data.frame(Season = tourney_data$Season,TeamID = tourney_data$LTeamID, Win = 0 )
team1 = rbind(team1w,team1l)
team2a = data.frame(Season = tourney_data$Season,TeamID = tourney_data$LTeamID, Win = 0 )
team2b = data.frame(Season = tourney_data$Season,TeamID = tourney_data$WTeamID, Win = 1 )
team2 = rbind(team2a,team2b)

#adding the season stats, seeds, and conference names to each tourney team
team1 = inner_join(team1,seeds,by = c("Season","TeamID"))
team2 = inner_join(team2,seeds,by = c("Season","TeamID"))
team1 = inner_join(team1,reg_s_avg,by = c("Season","TeamID"))
team2 = inner_join(team2,reg_s_avg,by = c("Season","TeamID"))

team1 = inner_join(team1,conferences,by = c("Season","TeamID"))
team2 = inner_join(team2,conferences,by = c("Season","TeamID"))

combined_data = cbind(team1,team2)
combined_data = combined_data[-c(9,10,11)]

combined_data$Seed = as.numeric(combined_data$Seed)
combined_data$Seed.1 = as.numeric(combined_data$Seed.1)
combined_data$Seed_Diff = combined_data$Seed - combined_data$Seed.1
model_data = combined_data[-c(1,2,4,9)]
```

### Data Analysis

```r
#splitting data into training and testing sets
library(caTools)
set.seed(430)
idx = sample.split(Y = model_data$Win,SplitRatio = 0.7)
train = model_data[idx,]
test = model_data[!idx,]

#stepwise model selection
nullmod = glm(Win~1,family = binomial,data = train)
stepmod = step(nullmod, ~ OER*DER*factor(ConfAbbrev)*Poss*OER.1*DER.1*factor(ConfAbbrev.1)*Poss.1*Seed_Diff,direction = "both")

#adding model predictions
preds = predict(stepmod,newdata = test,type = 'response')

#finding model accuracy
compare = test %>%
  mutate(predictions = preds) %>%
  mutate(predwin = ifelse(predictions > 0.5,1,0)) %>%
  mutate(correct = ifelse(predwin == Win,1,0))

accuracy = nrow(compare[compare$correct == 1,])/nrow(compare)
```

### Results

Overall, the model was fairly successful at predicting which team would win, as the testing accuracy was 71.77%. The variables that the variable selection process chose were seed difference, offensive efficiency, defensive efficiency, an interaction term between offensive and defensive efficiency, and an interaction term between the seed difference and offensive efficiency.

### Shiny app

A shiny app that shows the win probabilities for any matchup can be viewed [here](https://masoncb2.shinyapps.io/march_madness_2024/)
