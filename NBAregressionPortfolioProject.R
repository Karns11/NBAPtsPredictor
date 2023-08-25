summary(all_seasons)
pairs(~net_rating + oreb_pct + dreb_pct + usg_pct + ts_pct + ast_pct, data=all_seasons)
firstModel <- lm(pts ~ gp + net_rating + reb + oreb_pct + usg_pct + ts_pct + ast_pct, data=all_seasons)
summary(firstModel)
plot(firstModel)

library(car)
vif(firstModel)

filtered_data <- subset(all_seasons, pts > 0)
library(MASS)
boxcox(pts ~ gp + net_rating + reb + oreb_pct + usg_pct + ts_pct + ast_pct, data=filtered_data)

secondModel <- lm(pts^0.40 ~ gp + net_rating + reb + oreb_pct + usg_pct + ts_pct + ast_pct, data=filtered_data)
summary(secondModel)
plot(secondModel)
vif(secondModel)

selected_columns <- c("gp", "net_rating", "reb", "oreb_pct", "usg_pct", "ts_pct", "ast_pct")
newData <- subset(all_seasons, player_name == 'Gabe Vincent' & season == '2019-20')[, selected_columns]
View(newData)
predict(secondModel, newdata = newData, interval = "prediction")
actualValue <- subset(all_seasons, player_name == 'Gabe Vincent' & season == '2019-20')[, 'pts']
View(actualValue)

secondNewData <- subset(all_seasons, player_name == 'Donovan Mitchell' & season == '2021-22')[, selected_columns]
View(secondNewData)
predict(secondModel, newdata = secondNewData, interval = "prediction")
secondActualValue <- subset(all_seasons, player_name == 'Donovan Mitchell' & season == '2021-22')[, 'pts']
View(secondActualValue)

#------------------------------------------------------------------------------#
#Model to predict next seasons points per game
library(dplyr)
all_seasons <- all_seasons %>%
  arrange(player_name, season)
View(all_seasons)
all_seasons <- all_seasons %>%
  group_by(player_name) %>%
  mutate(next_season_pts = lead(pts)) %>%
  ungroup()
View(all_seasons)
# Subset to exclude rows with NA values for 'next_season_pts'
all_seasons_next_seasons_pts <- all_seasons[!is.na(all_seasons$next_season_pts), ]
View(all_seasons_next_seasons_pts) #almost 10000 rows

thirdModel <- lm(next_season_pts ~ gp + net_rating + usg_pct + ts_pct + pts, data=all_seasons_next_seasons_pts)
summary(thirdModel)
plot(thirdModel)
vif(thirdModel)
onlyPositiveNextSeasonPts <- subset(all_seasons_next_seasons_pts, next_season_pts > 0)
boxcox(next_season_pts ~ gp + net_rating + usg_pct + ts_pct + pts, data=onlyPositiveNextSeasonPts)

fourthModel <- lm(next_season_pts ~ gp + net_rating + usg_pct + ts_pct + pts, data=onlyPositiveNextSeasonPts)
summary(fourthModel)
plot(fourthModel)
vif(fourthModel)
boxcox(next_season_pts ~ gp + net_rating + usg_pct + ts_pct + pts, data=onlyPositiveNextSeasonPts)

fifthModel <- lm(next_season_pts^0.70 ~ gp + net_rating + usg_pct + ts_pct + pts, data=onlyPositiveNextSeasonPts)
summary(fifthModel)
plot(fifthModel)
vif(fifthModel)

fifth_selected_columns <- c("gp", "net_rating", "pts", "usg_pct", "ts_pct")
View(onlyPositiveNextSeasonPts)
fifth_newData <- subset(onlyPositiveNextSeasonPts, player_name == 'Damian Lillard' & season == '2019-20')[, fifth_selected_columns]
View(fifth_newData)
predict(fifthModel, newdata = fifth_newData, interval = "prediction")
fifth_actualValue <- subset(onlyPositiveNextSeasonPts, player_name == 'Damian Lillard' & season == '2019-20')[, 'next_season_pts']
View(fifth_actualValue)

View(all_seasons)


# Using subset function
all_na_pts_2021_22 <- subset(all_seasons, is.na(next_season_pts) & season == '2021-22')
newDataToBeUsed <- all_na_pts_2021_22[, fifth_selected_columns]
View(newDataToBeUsed)
all_na_pts_2021_22$predicted_next_season_pts <- ((predict(fifthModel, newdata = newDataToBeUsed, interval = "prediction"))^(1/0.7))[,"fit"]
View(all_na_pts_2021_22)
finalWithPredicted <- subset(all_na_pts_2021_22, select = c(player_name, predicted_next_season_pts))
View(finalWithPredicted)

install.packages("writexl")
library(writexl)
write_xlsx(finalWithPredicted, "C:/Users/natha/OneDrive/Documents/R/R stuff/2023NBAPTSPREDICTIONS.xlsx")



