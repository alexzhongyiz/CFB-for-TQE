library(dplyr)
library(rlist)
start=Sys.time()

curr_dir = "C:/Users/Zhongyi Zhang/QS Dropbox/Alex Zhongyi Zhang/Alex-summer/CFB/data/16-18/" #getwd()
# curr_dir
# CFB_dir = file.path(curr_dir,"Alex-summer","CFB" ,"data","16-18")
setwd(curr_dir)
list.files(curr_dir)

team_game_2016 = read.csv("TeamGame.2016.csv",header = TRUE, stringsAsFactors = FALSE)

#output_df$StatID = NULL
#output_df[,c("StatID","TeamID", "SeasonType","GameID", "OpponentID","IsGameOver","GlobalOpponentID",
#             "Updated" ,"Created" ,"Games" )] <- list(NULL)

team_game_2017 = read.csv("TeamGame.2017.csv", header = TRUE, stringsAsFactors = FALSE)
team_game_2018 = read.csv("TeamGame.2018.csv", header = TRUE, stringsAsFactors = FALSE)
regular_season_2016 = team_game_2016[1:1670,]
regular_season_2017 = team_game_2017[c(1: 1694),]
regular_season_2018 = team_game_2018[c(1: 1724),]
regular_season_16_to_17 = rbind(regular_season_2016, regular_season_2017)
regular_season_16_to_18 = rbind(regular_season_16_to_17, regular_season_2018)



temp_df_16_to_18 = regular_season_16_to_18



##########NOt right idea to just take difference, but rather consider the offensive and defensive teams respectively
# score_index = which(colnames(temp_df_16_to_18)=="Score")
# first_down_index = which(colnames(temp_df_16_to_18) == "FirstDowns")
# TimeOfPossessionSecond_index = which(colnames(temp_df_16_to_18) == "TimeOfPossessionSeconds")
# FantasyPoint_index = which(colnames(temp_df_16_to_18) == "FantasyPoints")
# FumblesLost_index = ncol(temp_df_16_to_18)
# feature_index_list = c(score_index, first_down_index:TimeOfPossessionSecond_index, FantasyPoint_index:FumblesLost_index)
# length_of_temp = nrow(temp_df_16_to_18)





#####function to update the historical data of a team####
####first get the difference of a team and its opposing team###
# name_list = names(regular_season_16_to_18)
# features_diff_list<-vector()
# #length(name_list)
# #name_list[6:14]
# #class(name_list[1])#character string
# 
# for ( i in feature_index_list) {
#   feature_diff = paste(name_list[i], "_diff", sep = "")
#   features_diff_list <- c(features_diff_list, feature_diff)
# }
# #unlist(features_diff_list[3])
# #class(features_diff_list[3][1])#still a list, thus need to unlist it to a character string
# 
# for (i in c(1:length(features_diff_list))){
#   temp_df_16_to_18[features_diff_list[i] ]<- rep(0, length_of_temp)
# }
# names(temp_df_16_to_18)==



write.csv(temp_df_16_to_18, "temp_16_to_18_first_draft.csv", row.names = FALSE)
temp_df_16_to_18 = read.csv("temp_16_to_18_first_draft.csv", stringsAsFactors = F, row.names =  NULL)
names(temp_df_16_to_18)
sapply(temp_df_16_to_18, class)
temp_df_16_to_18$Score_diff = temp_df_16_to_18$Score - temp_df_16_to_18$OpponentScore
temp_df_16_to_18$ThirdDownConversionRate = temp_df_16_to_18$ThirdDownConversions / temp_df_16_to_18$ThirdDownAttempts
temp_df_16_to_18$TotalTimeOfPossession = temp_df_16_to_18$TimeOfPossessionMinutes + temp_df_16_to_18$TimeOfPossessionSeconds/60
temp_df_16_to_18$YardsPerPlay = (temp_df_16_to_18$RushingYards + temp_df_16_to_18$PassingYards) / (temp_df_16_to_18$RushingAttempts + temp_df_16_to_18$PassingAttempts)
temp_df_16_to_18$CombineHurriesPlusSacks = temp_df_16_to_18$QuarterbackHurries + temp_df_16_to_18$Sacks
temp_df_16_to_18$ForcedTurnovers = temp_df_16_to_18$FumblesLost + temp_df_16_to_18$Interceptions

temp_df_16_to_18 = temp_df_16_to_18[,c("Season","Week","DateTime","Team","HomeOrAway","Opponent", "Score", "Score_diff", "FirstDowns","ThirdDownConversionRate", 
                                         "PenaltyYards", "TotalTimeOfPossession","YardsPerPlay","PassingAttempts","PassingCompletionPercentage",
                                       "PassingYardsPerAttempt",
                                       "RushingAttempts",
                                       "RushingYardsPerAttempt", "CombineHurriesPlusSacks","ForcedTurnovers")]
write.csv(temp_df_16_to_18, "regular_season_temporary_file_with_half_features.csv")

temp_df_16_to_18_with_Fantasy_points =  temp_df_16_to_18[,c("Season","Week","DateTime","Team","HomeOrAway","Opponent", "Score", "Score_diff", "FirstDowns","ThirdDownConversionRate", 
                                                            "PenaltyYards", "TotalTimeOfPossession","YardsPerPlay","PassingAttempts","PassingCompletionPercentage",
                                                            "PassingYardsPerAttempt",
                                                            "RushingAttempts",
                                                            "RushingYardsPerAttempt", "CombineHurriesPlusSacks",
                                                            "ForcedTurnovers", "FantasyPoints")]




which(is.na(as.numeric(temp_df_16_to_18$Score_diff)))#games that didn't play
temp_df_16_to_18 <- temp_df_16_to_18[-c(1399, 1400, 1483, 1484), ]
temp_df_16_to_18_with_Fantasy_points<- temp_df_16_to_18_with_Fantasy_points[complete.cases(temp_df_16_to_18_with_Fantasy_points),]
sum(is.na(temp_df_16_to_18_with_Fantasy_points))

names(temp_df_16_to_18_with_Fantasy_points)
write.csv(temp_df_16_to_18_with_Fantasy_points, "regular_season_16_18_with_Fantasy_Points_first_features.csv", row.names = F)
temp_df_16_to_18_with_Fantasy_points = read.csv("regular_season_16_18_with_Fantasy_Points_first_features.csv", stringsAsFactors =F)
offensive_features = vector()
for (i in c(7, 9:18)){
 offensive_features<- c(offensive_features, names(temp_df_16_to_18_with_Fantasy_points)[i])
  
}
offensive_features_with_fantasy = c(offensive_features, "FantasyPoints")
new_defensive_features = vector()
for (i in c(1: 11)){
  new_defense_feat = paste(offensive_features[i], "_allowed", sep = "")
  new_defensive_features<- c(new_defensive_features, new_defense_feat)
}
new_defensive_features_with_Fantasy = c(new_defensive_features, "OpponentFantasyPoints")

for(defense_feat in new_defensive_features){
  temp_df_16_to_18[defense_feat] = rep(0.0, nrow(temp_df_16_to_18))
}

for(defense_feat in new_defensive_features_with_Fantasy){
  temp_df_16_to_18_with_Fantasy_points[defense_feat] = rep(0.0, nrow(temp_df_16_to_18_with_Fantasy_points))
}

temp_df_16_to_18["Hurriesandsacks_allowed"] = rep(0.0, nrow(temp_df_16_to_18))
temp_df_16_to_18["turnovers"] = rep(0.0, nrow(temp_df_16_to_18))
temp_df_16_to_18_with_Fantasy_points["Hurriesandsacks_allowed"] = rep(0.0, nrow(temp_df_16_to_18_with_Fantasy_points))
temp_df_16_to_18_with_Fantasy_points["turnovers"] = rep(0.0, nrow(temp_df_16_to_18_with_Fantasy_points))
temp_df_16_to_18<- temp_df_16_to_18[, c(1: 18, 32, 33, 19:31)]
temp_df_16_to_18<- temp_df_16_to_18[,c(1: 20, 23: 33,21, 22 )]
temp_df_16_to_18_with_Fantasy_points<- temp_df_16_to_18_with_Fantasy_points[, c(1:18,34: 35,21, 22:32,19,20, 33)]
write.csv(temp_df_16_to_18, "regular_season_16_to_18_with_all_features.csv", row.names = F)
temp_df_16_to_18 = read.csv("regular_season_16_to_18_with_all_features.csv", stringsAsFactors = F )
write.csv(temp_df_16_to_18_with_Fantasy_points, "regular_season_with_fantasy_points.csv", row.names = F)
temp_df_16_to_18_with_Fantasy_points = read.csv("regular_season_with_fantasy_points.csv", stringsAsFactors = F)


####now update the features of each team according to the games
for (i in (1: (nrow(temp_df_16_to_18)/2))){
  temp_df_16_to_18[2*i - 1, c(21:31)] = temp_df_16_to_18[2*i, c(7, 9:18)]
  temp_df_16_to_18[2*i, c(21: 31)] = temp_df_16_to_18[2*i -1 , c(7, 9:18)]
  
  temp_df_16_to_18[2*i - 1, c(19:20)] = temp_df_16_to_18[2*i, c(32: 33)]
  temp_df_16_to_18[2*i, c(19:20)] = temp_df_16_to_18[2*i - 1, c(32: 33)]
  
}

for (i in (1: (nrow(temp_df_16_to_18_with_Fantasy_points)/2))){
  temp_df_16_to_18_with_Fantasy_points[2*i - 1, c(22:32, 35)] = temp_df_16_to_18_with_Fantasy_points[2*i, c(7, 9:18, 21)]
  temp_df_16_to_18_with_Fantasy_points[2*i, c(22: 32, 35)] = temp_df_16_to_18_with_Fantasy_points[2*i -1 , c(7, 9:18, 21)]
  
  temp_df_16_to_18_with_Fantasy_points[2*i - 1, c(19:20)] = temp_df_16_to_18_with_Fantasy_points[2*i, c(33: 34)]
  temp_df_16_to_18_with_Fantasy_points[2*i, c(19:20)] = temp_df_16_to_18_with_Fantasy_points[2*i - 1, c(33: 34)]
  
}

temp_df_16_to_18<- temp_df_16_to_18[complete.cases(temp_df_16_to_18),]


write.csv(temp_df_16_to_18, "regular_season_16_to_18_with_all_features_filled.csv", row.names = F)
temp_df_16_to_18 = read.csv("regular_season_16_to_18_with_all_features_filled.csv", stringsAsFactors = F)
write.csv(temp_df_16_to_18_with_Fantasy_points, "regular_season_with_Fantasy_filled.csv", row.names = F)
temp_df_16_to_18_with_Fantasy_points = read.csv("regular_season_with_Fantasy_filled.csv", stringsAsFactors = F)


#methods to remedy the fact that thirddownconverstionrate, totaltimeofpocession and yardsperplay are not integer



####now fill in the data of regular season using historical data, starting from week 2
##here for each feature, we take arithmetic mean of previous games played by this team
##and the prediction of the difference, we just take the difference of those two features
##use a list(equv to a dict in python) to keep track of the games played  by each team

#first given this data, calculate the difference of features of the game itself
# 
# First_Feature_difference_index = which(colnames(temp_df_16_to_18)=="Score_diff") #index of column with name ...
# difference_of_FirstDowns_indices_diff_to_FirstDowns = First_Feature_difference_index + 1 - first_down_index#69
# differnece_of_2nd_group_of_indices = ncol(temp_df_16_to_18) - ncol(regular_season_16_to_18)
# for (i in 1: ( nrow(temp_df_16_to_18)/2 )){
#   temp_df_16_to_18$Score_diff[2*i -1] = temp_df_16_to_18$Score[2*i - 1] - temp_df_16_to_18$Score[2*i]
#   temp_df_16_to_18$Score_diff[2*i] = temp_df_16_to_18$Score[2*i] - temp_df_16_to_18$Score[2*i-1]
#   for (j in c(first_down_index : TimeOfPossessionSecond_index)){#difference of col indeces are 68 #now 69
#     temp_df_16_to_18[2*i - 1, j + difference_of_FirstDowns_indices_diff_to_FirstDowns] = temp_df_16_to_18[2*i -1, j] - temp_df_16_to_18[2*i, j]
#     temp_df_16_to_18[2*i, j + difference_of_FirstDowns_indices_diff_to_FirstDowns] = temp_df_16_to_18[2*i, j] - temp_df_16_to_18[2*i - 1, j]
#   }
#   for (k in c(FantasyPoint_index: FumblesLost_index)){#difference of col indices are 63 
#     temp_df_16_to_18[2*i - 1, k+ differnece_of_2nd_group_of_indices] = temp_df_16_to_18[2*i-1, k] - temp_df_16_to_18[2*i, k]
#     temp_df_16_to_18[2*i, k + differnece_of_2nd_group_of_indices] = temp_df_16_to_18[2*i, k] - temp_df_16_to_18[2*i-1, k]
#   }
#   
# }
# 
# fix(temp_df_16_to_18)




regular_season_by_team_16_18 = arrange(temp_df_16_to_18,Team)
regular_season_by_team_with_Fantasy = arrange(temp_df_16_to_18_with_Fantasy_points, Team)


feature_pred_list = list()
for (i in c(7:33)){
  feature_pred = paste(names(regular_season_by_team_16_18)[i], "_pred", sep ="")
  feature_pred_list <- c(feature_pred_list, feature_pred)
}
for (i in c(1:27)){
  regular_season_by_team_16_18[unlist(feature_pred_list[i] )]<- rep(0.0, nrow(regular_season_by_team_16_18))
  
}
####Update the feature lists
feature_pred_list = list()
opponent_pred_list = list()
for (i in c(7:35)){
  feature_pred = paste(names(regular_season_by_team_with_Fantasy)[i], "_pred", sep ="")
  opponent_pred = paste(names(regular_season_by_team_with_Fantasy)[i], "_oppo_pred", sep ="")
  feature_pred_list <- c(feature_pred_list, feature_pred)
  opponent_pred_list<- c(opponent_pred_list, opponent_pred)
}

for (i in c(1:29)){
  regular_season_by_team_with_Fantasy[unlist(feature_pred_list[i] )]<- rep(0.0, nrow(regular_season_by_team_with_Fantasy))
}
for(i in c(1:29)){
  regular_season_by_team_with_Fantasy[unlist(opponent_pred_list[i])]<- rep(0.0, nrow(regular_season_by_team_with_Fantasy))
}

write.csv(regular_season_by_team_16_18, "regular_season_by_team_16_18_with_empty_predictions.csv", row.names = F)
regular_season_by_team_16_18 = read.csv("regular_season_by_team_16_18_with_empty_predictions.csv", stringsAsFactors = F)
write.csv(regular_season_by_team_with_Fantasy, "regular_season_by_team_with_Fantasy.csv", row.names = F)
regular_season_by_team_with_Fantasy = read.csv( "regular_season_by_team_with_Fantasy.csv", stringsAsFactors = F)

Team_list = as.character(regular_season_by_team_16_18$Team)
class(Team_list)


#####################UPDATE the feature predictions, multiple ways to do so
##1st way is arithmetic mean, not very good

teams_visited = vector()
number_of_previous_games_by_team = 1
team_sum = t(rep(0, 27))
for (i in (1:nrow(regular_season_by_team_16_18)) ){
  print(i)
  if (Team_list[i] %in% teams_visited){
    
    # for (j in c(73:135)){#using arithmetic mean to update predictions, can be different, eg. weighted sum
    #   team_sum[i,j] = team_sum[i,j] + regular_season_by_team_16_18[i-1,j]
    #   regular_season_by_team_16_18[i, j] = team_sum[i, j]/number_of_previous_games_by_team
    # }
    # 
    team_sum = team_sum + regular_season_by_team_16_18[i - 1, 7:33]
    regular_season_by_team_16_18[i,34:60] = team_sum/ number_of_previous_games_by_team
    
    number_of_previous_games_by_team <- 1 + number_of_previous_games_by_team
  }
  else{
    teams_visited <- c(teams_visited, Team_list[i])
    team_sum = t(rep(0.0, 27))
    number_of_previous_games_by_team = 1
    
    
  }
}
View(regular_season_by_team_16_18)
write.csv(regular_season_by_team_16_18, "regular_season_by_team_16_18_with_predictions_filled_simple_arithmetic_mean.csv", row.names = F)
regular_season_by_team_16_18 = read.csv("regular_season_by_team_16_18_with_predictions_filled_simple_arithmetic_mean.csv", stringsAsFactors = F)

######2nd way of updating the feature predictions, weighted sum, namely geometric series
######keep in mind that the predictions is the difference between stats of two teams, thus need a dictionary to keep track of team stats

teams_stats_list = list()
#teams_visited = vector()
games_team_played_in_season = 1
team_stats = t(rep(0.0, 27))
tem_stats = t(rep(0.0, 29))#with Fantasy Points
weight_this_game = 0.5
weight_this_season = 0.7
season = 2016

feature_list = names(regular_season_by_team_16_18)[5,7:33]#now includes HomeOrAway
regular_season_by_team_16_18$HomeOrAway [which(regular_season_by_team_16_18$HomeOrAway == "AWAY")] = 1
regular_season_by_team_16_18$HomeOrAway [which(regular_season_by_team_16_18$HomeOrAway == "HOME")] = 0


update_opponent <- function(opponent, weight,teams_stats_list){
  
  
  if (!(opponent %in% names(teams_stats_list))){
    opponent_current_stats_numeric = as.numeric(regular_season_by_team_16_18[i, c(21,8, 22: 33, 7, 9: 20)])
    opponent_current_stats_numeric[2] <- opponent_current_stats_numeric[2] * -1 #changing sign of score_diff
    #opponent_current_stats_numeric[1] = 1- opponent_current_stats_numeric[1] #changing HomeorAway for opponent

    teams_stats_list[opponent] = list(opponent_current_stats_numeric)
  }
  else{
    opponent_current_stats_numeric = regular_season_by_team_16_18[i, c(21,8, 22: 33, 7, 9: 20)]
    opponent_current_stats_numeric[2] <- opponent_current_stats_numeric[2] * -1
    #opponent_current_stats_numeric[1] = 1- opponent_current_stats_numeric[1]
    teams_stats_list[opponent] =list( as.numeric(unlist(teams_stats_list[opponent] ) ) * (1- weight) + opponent_current_stats_numeric* weight )
  }
  return(teams_stats_list)
}

teams_stats_list = list()
for(i in (1: nrow(regular_season_by_team_16_18))){
  print(i)
  team = regular_season_by_team_16_18$Team[i]
  opponent = regular_season_by_team_16_18$Opponent[i]
  
  if(!(team %in% names(teams_stats_list)) ){
    #teams_visited <- c(teams_visited, Team_list[i])
    season = regular_season_by_team_16_18$Season[i]
    teams_stats_list[team] = list(regular_season_by_team_16_18[i,c( 7:33) ] )
    teams_stats_list <- update_opponent(opponent, weight_this_game,teams_stats_list)
    
    
    #games_team_played_in_season = 1
    }
  else{#team already in teams_stats_list
    if(regular_season_by_team_16_18$Season[i] != season){#first game in season
      season = regular_season_by_team_16_18$Season[i]
      #games_team_played_in_season = 1
      teams_stats_list[team] = list(regular_season_by_team_16_18[i, c(7:33)]*weight_this_season + as.numeric(unlist(teams_stats_list[team] ) )* (1- weight_this_season) )
      
      teams_stats_list<- update_opponent(opponent, weight_this_season, teams_stats_list)
    }
    else{ #not first game in season
      
      teams_stats_list[team] =list( weight_this_game * as.numeric( regular_season_by_team_16_18[i - 1, c(7:33)]) + as.numeric(unlist(teams_stats_list[team]) )* (1 - weight_this_game) )
      
      #games_team_played_in_season = games_team_played_in_season + 1
      teams_stats_list<- update_opponent(opponent, weight_this_game, teams_stats_list)
      
      #finally update the predictions
      
      regular_season_by_team_16_18[i, 34:60] = as.numeric(unlist(teams_stats_list[team]))- as.numeric(unlist(teams_stats_list[opponent] )[c(15, 2,16:27, 1, 3:14)] )
      
      
    }

  }
}

###############################  update with only home games---The Correct Way#####################################


score_pred_index = which(colnames(regular_season_by_team_with_Fantasy) == 'Score_pred')
OpponentFantasy_point_index = which(colnames(regular_season_by_team_with_Fantasy) == "OpponentFantasyPoints_oppo_pred")
for (i in c(score_pred_index: OpponentFantasy_point_index)){
  regular_season_by_team_with_Fantasy[,i]= rep(0, nrow(regular_season_by_team_with_Fantasy))

}

regular_season_16_18 = arrange(regular_season_by_team_with_Fantasy, Season, Week)
regular_season_16_18$DateTime =as.POSIXct(regular_season_16_18$DateTime,format="%m/%d/%Y", tz = Sys.timezone())
#regular_season_16_to_18 = arrange(regular_season_16_18, DateTime)
regular_season_16_18 = regular_season_16_18[order(as.Date(regular_season_16_18$DateTime, format="%d/%m/%Y")),]
home_regular_season_16_18= regular_season_16_18[which(regular_season_16_18$HomeOrAway == "HOME"),]
sum(is.na(home_regular_season_16_18))
division_1_teams = unique(home_regular_season_16_18$Team)

##########################################code just to figure out the division1 and division2 teams##########
teams_appearance_count = list()
for (team in unique(home_regular_season_16_18$Team)){
  teams_appearance_count[team] = 0
}
for (i in (1: nrow(home_regular_season_16_18))) {
  if (i%%200 == 0){print(i)}
  team = home_regular_season_16_18$Team[i]
  teams_appearance_count[team] = as.numeric(unlist(teams_appearance_count[team] )) + 1
}

division1_teams =names( teams_appearance_count[which(teams_appearance_count>=5)] )

Away_team_list = unique(home_regular_season_16_18$Opponent)
Division2_teams = Away_team_list[!(Away_team_list %in% division1_teams)]
#############################################################################################################
#Update using weights and geometric series

#for naive updating, just make weight of this game/season to be 1
weight_this_game = 1
weight_this_season = 1

teams_stats_list = list()#only for d1 against d1 team
di_against_d2 = list()
d2_stats = rep(0, 30)
season = 2016


update_opponent <- function(i, weight,teams_stats_list){
  opponent = home_regular_season_16_18$Opponent[i]
  season = home_regular_season_16_18$Season[i]
  if (!(opponent %in% names(teams_stats_list))){
    opponent_current_stats_numeric = as.numeric(home_regular_season_16_18[i,  c(22,8, 23: 35, 7, 9: 21)])
    opponent_current_stats_numeric[2] <- opponent_current_stats_numeric[2] * -1 #changing sign of score_diff
    #opponent_current_stats_numeric[1] = 1- opponent_current_stats_numeric[1] #changing HomeorAway for opponent
    
    teams_stats_list[opponent] = list(c(opponent_current_stats_numeric, season))
  }
  else{
    opponent_current_stats_numeric = as.numeric(home_regular_season_16_18[i, c(22,8, 23: 35, 7, 9: 21)])
    opponent_current_stats_numeric[2] <- opponent_current_stats_numeric[2] * -1
    #opponent_current_stats_numeric[1] = 1- opponent_current_stats_numeric[1]
    #home_regular_season_16_18[i, 65: 93] = as.numeric(unlist(teams_stats_list[opponent]))[1:29]
    pure_stats =  as.numeric(unlist(teams_stats_list[opponent] ) )[1:29] * (1- weight) + opponent_current_stats_numeric* weight 
    teams_stats_list[opponent] =list( c(pure_stats, season) )
  }
  return(teams_stats_list)
}

update_hometeam <- function(i, weight, teams_stats_list){
  team = home_regular_season_16_18$Team[i]
  season = home_regular_season_16_18$Season[i]
  if(! (team %in% names(teams_stats_list))){
    team_stats = as.numeric(home_regular_season_16_18[i, c(7:35)])
    teams_stats_list[team] = list(c(team_stats, season))
  }
  else{
    #home_regular_season_16_18[i, 36:64] = as.numeric(unlist(teams_stats_list[team]))[1:29]
    pure_stats = weight * as.numeric(home_regular_season_16_18[i,c(7:35)]) + as.numeric(unlist(teams_stats_list[team]))[1:29] * (1- weight)
    teams_stats_list[team] = list(c(pure_stats, season))
  }
  return (teams_stats_list)
}

invalid_game_index_list_due_to_lack_of_data = vector()
teams_stats_list = list()

for(i in (1:nrow(home_regular_season_16_18))){
  #print(c(i,home_regular_season_16_18$Week[i]))
  
  
  team = home_regular_season_16_18$Team[i]
  opponent = home_regular_season_16_18$Opponent[i]
  season = home_regular_season_16_18$Season[i]
  if ((opponent %in% Division2_teams)| (team %in% Division2_teams)){next}
  
  if (team == "AIRF" | opponent == "AIRF"){
    print(c(home_regular_season_16_18$DateTime[i], season, as.numeric(unlist(teams_stats_list['AIRF']))[30]))
    
    print(teams_stats_list['AIRF'])
  }

  if(!(team %in% names(teams_stats_list)) ){
    #teams_visited <- c(teams_visited, Team_list[i])
    #season = home_regular_season_16_18$Season[i]
    #teams_stats_list[team] = list(home_regular_season_16_18[i,c(7:35, 1)] )
    teams_stats_list <- update_hometeam(i, weight_this_game, teams_stats_list)
    teams_stats_list <- update_opponent(i, weight_this_game,teams_stats_list)
  }
  else{#team already in teams_stats_list
    
    if(season != as.numeric(unlist(teams_stats_list[team]))[30] ){    #first game in season, also update, but will not use in train

      
      if (! (opponent %in% names(teams_stats_list))){
        invalid_game_index_list_due_to_lack_of_data = c(invalid_game_index_list_due_to_lack_of_data, i)
        
       teams_stats_list <- update_hometeam(i, weight_this_season, teams_stats_list)
      
       teams_stats_list<- update_opponent(i, weight_this_season, teams_stats_list)
       
      } 
      
      else{
        
        home_regular_season_16_18[i, 36:64] = as.numeric(unlist(teams_stats_list[team]))[1:29]
        home_regular_season_16_18[i, 65: 93] = as.numeric(unlist(teams_stats_list[opponent]))[1:29]
        #we have updated the feature of first game of 2017 using the data of 2016 completely
        teams_stats_list <- update_hometeam(i, weight_this_season, teams_stats_list)
        
        if(season!= as.numeric(unlist(teams_stats_list[opponent]))[30]){
          teams_stats_list <- update_opponent(i, weight_this_season, teams_stats_list)
        }
        else{
          teams_stats_list <- update_opponent(i, weight_this_game, teams_stats_list)
        }
        
      }
    }
    else{ #not first game in season, finally update predictions, note give predictions first then update the data!!!!
      #if (team == "AIRF" | opponent == "AIRF"){     
        #print("not fist game in season")}
      if (! (opponent %in% names(teams_stats_list))){
        invalid_game_index_list_due_to_lack_of_data = c(invalid_game_index_list_due_to_lack_of_data, i)
        teams_stats_list <- update_hometeam(i, weight_this_game, teams_stats_list)
        teams_stats_list<- update_opponent(i, weight_this_game, teams_stats_list)
      }
      
      
      else{
          home_regular_season_16_18[i, 36:64] = as.numeric(unlist(teams_stats_list[team]))[1:29]
          home_regular_season_16_18[i, 65: 93] = as.numeric(unlist(teams_stats_list[opponent]))[1:29]
          
          teams_stats_list <- update_hometeam(i, weight_this_game, teams_stats_list)
          
          if(season!= as.numeric(unlist(teams_stats_list[opponent]))[30]){
            teams_stats_list <- update_opponent(i, weight_this_season, teams_stats_list)
          }
          else{
            teams_stats_list <- update_opponent(i, weight_this_game, teams_stats_list)
          }
      }

    }
  }
}

D1_game_df = home_regular_season_16_18[which( (home_regular_season_16_18$Team %in% division_1_teams)&(home_regular_season_16_18$Opponent %in%division_1_teams) ), ]
D1_viewing = D1_game_df[, c("DateTime", "Team", 'Opponent', "Score","Score_allowed", "Score_pred", "Score_oppo_pred")]

write.csv(D1_game_df, 'Division1_games_wts_0.3&0.7.csv', row.names = F)
D1_game_df = read.csv('Division1_games_wts_0.3&0.7.csv', stringsAsFactors = F)


write.csv(home_regular_season_16_18, 'home_game_with_game_wt_0.3&season_wt_0.7.csv', row.names = F)
home_regular_season_16_18 = read.csv('home_game_with_game_wt_0.3&season_wt_0.7.csv', stringsAsFactors = F)

write.csv(home_regular_season_16_18,'home_game_regular_season_with_weights_0.5&0.7_filled.csv', row.names = F )
home_regular_season_16_18 = read.csv('home_game_regular_season_with_weights_0.5&0.7_filled.csv', stringsAsFactors = F)
home_regular_season_by_team = arrange(home_regular_season_16_18, Team)


#######################################################################################################################
#update using simple average of current season games

update_stats_simple_average<- function(i, teams_stats_list){

    team = home_regular_season_16_18$Team[i]
    season = home_regular_season_16_18$Season[i]
    if(! (team %in% names(teams_stats_list)) | (season!= as.numeric(unlist(teams_stats_list[team]))[30] )){
      team_stats = as.numeric(home_regular_season_16_18[i, c(7:35)])
      teams_stats_list[team] = list(c(team_stats, season, 1))
    }
    else{
      #home_regular_season_16_18[i, 36:64] = as.numeric(unlist(teams_stats_list[team]))[1:29]
      games_team_played_in_season= as.numeric(unlist(teams_stats_list[team]))[31]
      pure_stats = as.numeric(home_regular_season_16_18[i,c(7:35)]) + 
        as.numeric(unlist(teams_stats_list[team]))[1:29] * games_team_played_in_season
      games_team_played_in_season <- games_team_played_in_season + 1
      
      teams_stats_list[team] = list(c(pure_stats/games_team_played_in_season, season, games_team_played_in_season))
    }
    
    opponent = home_regular_season_16_18$Opponent[i]
    
    if(! (opponent %in% names(teams_stats_list)) | (season!= as.numeric(unlist(teams_stats_list[opponent]))[30] )){
      opponent_current_stats_numeric = as.numeric(home_regular_season_16_18[i,  c(22,8, 23: 35, 7, 9: 21)])
      opponent_current_stats_numeric[2] <- opponent_current_stats_numeric[2] * -1
      teams_stats_list[opponent] = list(c(opponent_current_stats_numeric, season, 1))
    }
    else{
      #home_regular_season_16_18[i, 36:64] = as.numeric(unlist(teams_stats_list[team]))[1:29]
      games_oppo_played_in_season= as.numeric(unlist(teams_stats_list[opponent]))[31]
      opponent_current_stats_numeric = as.numeric(home_regular_season_16_18[i,  c(22,8, 23: 35, 7, 9: 21)])
      opponent_current_stats_numeric[2] = opponent_current_stats_numeric[2]*(-1)
      pure_stats = opponent_current_stats_numeric + 
        as.numeric(unlist(teams_stats_list[opponent]))[1:29] * games_oppo_played_in_season
      games_oppo_played_in_season <- games_oppo_played_in_season + 1
      
      teams_stats_list[opponent] = list(c(pure_stats/games_oppo_played_in_season, season, games_oppo_played_in_season))
    }
    
    return (teams_stats_list)
  }

invalid_game_index_list_due_to_lack_of_data = vector()
teams_stats_list = list()

for(i in (1:nrow(home_regular_season_16_18))){
  #print(c(i,home_regular_season_16_18$Week[i]))
  #print(i)
  
  team = home_regular_season_16_18$Team[i]
  opponent = home_regular_season_16_18$Opponent[i]
  if ((opponent %in% Division2_teams)| (team %in% Division2_teams)){next}
  if (! (team %in% names(teams_stats_list)) | !(opponent %in% names(teams_stats_list))){
    teams_stats_list<- update_stats_simple_average(i, teams_stats_list)
    if (team == "AIRF" | opponent == "AIRF"){print(teams_stats_list['AIRF'])
  }

  }
  else{
    
  home_regular_season_16_18[i, 36:64] = as.numeric(unlist(teams_stats_list[team]))[1:29]
  home_regular_season_16_18[i, 65: 93] = as.numeric(unlist(teams_stats_list[opponent]))[1:29]
  teams_stats_list <- update_stats_simple_average(i, teams_stats_list)
  if (team == "AIRF" | opponent == "AIRF"){print(teams_stats_list['AIRF'])
  }
}

}

D1_game_df = home_regular_season_16_18[which( (home_regular_season_16_18$Team %in% division_1_teams)&(home_regular_season_16_18$Opponent %in%division_1_teams) ), ]
D1_viewing = D1_game_df[, c("DateTime", "Team", 'Opponent', "Score","Score_allowed", "Score_pred", "Score_oppo_pred")]

games_to_be_retained_simple_average = vector()
Games_played_by_team_in_season_list = list()
for (team in division1_teams){
  Games_played_by_team_in_season_list[team] = 0
}
current_season = 2016

for (i in (1: nrow(home_regular_season_16_18))){
  team = home_regular_season_16_18$Team[i]
  opponent = home_regular_season_16_18$Opponent[i]
  season = home_regular_season_16_18$Season[i]
  if (! (team %in% division_1_teams) | !(opponent %in% division1_teams)){next}
  if (season != current_season){
    for (team in division1_teams){
      Games_played_by_team_in_season_list[team] = 0
    }
    current_season = season
  }
  
  if (unlist(Games_played_by_team_in_season_list[team]) <= first_n_games | unlist(Games_played_by_team_in_season_list[opponent])<= first_n_games){
    Games_played_by_team_in_season_list[team] <- unlist(Games_played_by_team_in_season_list[team] )+ 1
    Games_played_by_team_in_season_list[opponent]<- unlist(Games_played_by_team_in_season_list[opponent] )+ 1
  }
  else{
    games_to_be_retained_simple_average <- c(games_to_be_retained_simple_average, i)
    Games_played_by_team_in_season_list[team] <- unlist(Games_played_by_team_in_season_list[team]) + 1
    Games_played_by_team_in_season_list[opponent]<-unlist( Games_played_by_team_in_season_list[opponent])+ 1
  }
}



#############################################################
#from the correlation of all different updating scheme, we realize that no matter how we update, the correlation of actual feature 
#and updated features is at 30% at best, therefore I need to differentiate the teams into strong/median/week categories:mean of score_diff/ standard deviation

saveRDS(division_1_teams, file = "division1_ teams")
saveRDS(Division2_teams, file ='division2_teams')
Score_diff_list = list()
for (team in division_1_teams){
  Score_diff_list[team] =( sum(home_regular_season_16_18$Score_diff[which(home_regular_season_16_18$Team == team) ]) 
                           - sum(home_regular_season_16_18$Score_diff[which(home_regular_season_16_18$Opponent == team)]) )/ length(which(home_regular_season_16_18$Team == team |
                                                                                                                                            home_regular_season_16_18$Opponent == team))
}
Score_diff_list <- Score_diff_list[order(names(Score_diff_list))]

home_regular_season_16_17 = home_regular_season_16_18[which(home_regular_season_16_18$Season == 2016| home_regular_season_16_18$Season == 2017),]
home_regular_season_16= home_regular_season_16_18[which(home_regular_season_16_18$Season == 2016),]
home_regular_season_17 = home_regular_season_16_18[which(home_regular_season_16_18$Season == 2017),]
home_regular_season_18 = home_regular_season_16_18[which(home_regular_season_16_18$Season == 2018),]

Score_diff_list_16_17 = list()
for (team in division_1_teams){
  Score_diff_list_16_17[team] =( sum(home_regular_season_16_17$Score_diff[which(home_regular_season_16_17$Team == team) ]) 
                           - sum(home_regular_season_16_17$Score_diff[which(home_regular_season_16_17$Opponent == team)]) )/
                                        length(which(home_regular_season_16_17$Team == team | home_regular_season_16_17$Opponent == team))
}


Score_diff_list_16_17 <- Score_diff_list_16_17[order(-unlist(Score_diff_list_16_17))]
saveRDS(Score_diff_list_16_17, file = "Score_difference_list_2016to2017")

team_rank_list_16_17 = list()
for (team in division_1_teams){
  team_rank_list_16_17[team] = which(names(Score_diff_list_16_17) == team)
}
team_rank_list_16_17 = team_rank_list[order(unlist(team_rank_list))]
team_rank_list_16_17['D2_team'] = 132


Score_diff_list_16 = list()
for (team in division_1_teams){
  Score_diff_list_16[team] = (sum(home_regular_season_16$Score_diff[which(home_regular_season_16$Team == team)]) - 
                                sum(home_regular_season_16$Score_diff[which(home_regular_season_16$Opponent == team)])) /
    length(which(home_regular_season_16$Team == team| home_regular_season_16$Opponent == team))
}
Score_diff_list_16 <- Score_diff_list_16[order(-unlist(Score_diff_list_16))]
team_rank_list_16 = list()
for (team in division_1_teams){
  team_rank_list_16[team] = which(names(Score_diff_list_16) == team)
}
team_rank_list_16['D2_team'] = 132



saveRDS(Score_diff_list_16, file = '2016_team_score_diff_list')
saveRDS(team_rank_list_16, file = '2016_team_rank_list')


Score_diff_list_17 = list()
for (team in division_1_teams){
  Score_diff_list_17[team] = (sum(home_regular_season_17$Score_diff[which(home_regular_season_17$Team == team)]) - 
                                sum(home_regular_season_17$Score_diff[which(home_regular_season_17$Opponent == team)])) /
    length(which(home_regular_season_17$Team == team| home_regular_season_17$Opponent == team))
}
Score_diff_list_17 <- Score_diff_list_17[order(-unlist(Score_diff_list_17))]
team_rank_list_17 = list()
for (team in division_1_teams){
  team_rank_list_17[team] = which(names(Score_diff_list_17) == team)
}
team_rank_list_17['D2_team'] = 132
saveRDS(Score_diff_list_17, file = '2017_team_score_diff_list')
saveRDS(team_rank_list_17, file = '2017_team_rank_list')



Score_diff_list_18 = list()
for (team in division_1_teams){
  Score_diff_list_18[team] = (sum(home_regular_season_18$Score_diff[which(home_regular_season_18$Team == team)]) - 
                                sum(home_regular_season_18$Score_diff[which(home_regular_season_18$Opponent == team)])) /
    length(which(home_regular_season_18$Team == team| home_regular_season_18$Opponent == team))
}
Score_diff_list_18 <- Score_diff_list_18[order(-unlist(Score_diff_list_18))]
team_rank_list_18 = list()
for (team in division_1_teams){
  team_rank_list_18[team] = which(names(Score_diff_list_18) == team)
}
team_rank_list_18['D2_team'] = 132
saveRDS(Score_diff_list_18, file = '2017_team_score_diff_list')
saveRDS(team_rank_list_18, file = 
          '2017_team_rank_list')



for ( i in (1:nrow(home_regular_season_16_17) )){
  if (home_regular_season_16_17$Team[i] %in% Division2_teams){
    home_regular_season_16_17[i, 'Team'] <- 'D2_team'
  }
  if(home_regular_season_16_17$Opponent[i] %in% Division2_teams){
    home_regular_season_16_17[i, 'Opponent'] <- 'D2_team'
  }
}

D2_team.score_diff = -home_regular_season_16_17$Score_diff[which(home_regular_season_16_17$Opponent == 'D2_team')]
summary(D2_team.score_diff)
Score_diff_list_16_17 ['D2_team'] = mean(D2_team.score_diff)


team_rank_list = list()
for (team in division_1_teams){
  team_rank_list[team] = which(names(score_diff_list_by_value) == team)
}
team_rank_list = team_rank_list[order(unlist(team_rank_list))]
team_rank_list['D2_team'] = 132



current_season = 2018
#want to see the second half season's predictions, use the first half of this season and the second half of last season
Last_season = current_season - 1
score_diff_list_previous_two_half_seasons = list()
for (team in division_1_teams){
  home_team_indices = union(which(home_regular_season_16_18$Team == team & home_regular_season_16_18$Season == Last_season & home_regular_season_16_18$Week >= 8),
        which(home_regular_season_16_18$Team == team & home_regular_season_16_18$Season == current_season & home_regular_season_16_18$Week <= 7))
  away_team_indices = union(which(home_regular_season_16_18$Opponent == team & home_regular_season_16_18$Season == Last_season & home_regular_season_16_18$Week >= 8),
                     which(home_regular_season_16_18$Opponent == team & home_regular_season_16_18$Season == current_season & home_regular_season_16_18$Week <= 7) )     
  score_diff_list_previous_two_half_seasons[team] = (sum(home_regular_season_16_18$Score_diff[home_team_indices]) -
                                                       sum(home_regular_season_16_18$Score_diff[away_team_indices]) ) /
    (length(home_team_indices) + length(away_team_indices))
}
score_diff_list_previous_two_half_seasons <- score_diff_list_previous_two_half_seasons[order(-unlist(score_diff_list_previous_two_half_seasons))]
team.rank.list.previous.two.half.seasons = list()
for (team in division_1_teams){
  team.rank.list.previous.two.half.seasons[team] = which(names(score_diff_list_previous_two_half_seasons) == team)
}
saveRDS(team.rank.list.previous.two.half.seasons, file = 'team.rank.second.half.16&first.half.17')
saveRDS(team.rank.list.previous.two.half.seasons, file = 'team.rank.second.half.17&first.half.18')
team.rank.list.16_second.half.and.17.first.half = readRDS('team.rank.second.half.16&first.half.17')

################for first half of season
second_to_last_season = current_season - 2

score_diff_list_previous_3_half_seasons = list()
for(team in division_1_teams){
  home_team_indices = intersect(union(which(home_regular_season_16_18$Season == Last_season), 
                            which(home_regular_season_16_18$Season == second_to_last_season & home_regular_season_16_18$Week >= 8) ),
                            which(home_regular_season_16_18$Team == team))
  
  away_team_indices= intersect(union(which(home_regular_season_16_18$Season == Last_season),
                           which(home_regular_season_16_18$Season == second_to_last_season & home_regular_season_16_18$Week >=8) ),
                          which(home_regular_season_16_18$Opponent == team))
  score_diff_list_previous_3_half_seasons[team] = (sum (home_regular_season_16_18$Score_diff[home_team_indices]) - 
                                                     sum(home_regular_season_16_18$Score_diff[away_team_indices]))/
    (length(home_team_indices) + length(away_team_indices))
}
score_diff_list_previous_3_half_seasons <- score_diff_list_previous_3_half_seasons[order(-unlist(score_diff_list_previous_3_half_seasons))]
saveRDS(score_diff_list_previous_3_half_seasons, file = 'score_diff_list second half 16& whole 17')
team.rank.list.prev.3.half.seasons = list()
for (team in division_1_teams){
  team.rank.list.prev.3.half.seasons[team] = which(names(score_diff_list_previous_3_half_seasons) == team)
}
saveRDS(team.rank.list.prev.3.half.seasons, file = 'team.rank.16.second.half.season&whole.17.season')

plot(-unlist(Score_diff_list_16_17), unlist(team_rank_list))
x <- seq(-30, 20, 0.1)
y4 = 135*sigmoid( (x+ 1.6)/6) -1
lines(x, y4, col = "red", lwd = 2)


inverse_affine <- function (y){
  return ((y + 1)/135)
}
inverse_sigmoid <- function(z){
  6* log(inverse_affine(z)/ (1- inverse_affine(z)) ) - 1.6
}

score_diff_projection_by_prev_2_seasons = list()
for (team in names(Score_diff_list_16_17)){
  score_diff_projection_by_prev_2_seasons[team] = -inverse_sigmoid(which(names(Score_diff_list_16_17) == team))
}
score_diff_projection_by_prev_2_seasons['D2_team']= -28.29


plot(unlist(team_rank_list), -unlist(score_diff_projection_by_prev_2_seasons))
lines(unlist(team_rank_list), -unlist(Score_diff_list_16_17), col = 'red', lwd = 2)



Standard_deviation_list = list()
for (team in division1_teams){
  home_game_score_diff = home_regular_season_16_18$Score_diff[which(home_regular_season_16_18$Team == team)]
  away_game_score_diff = home_regular_season_16_18$Score_diff[which(home_regular_season_16_18$Opponent == team)]* -1
  Standard_deviation_list[team] = sd(c(home_game_score_diff, away_game_score_diff))
}

summary(unlist(Standard_deviation_list))
Standard_deviation_list <- Standard_deviation_list[order(names(Standard_deviation_list))]
mean_over_sd_list = list()
for (team in division_1_teams){
  mean_over_sd_list[team] = unlist(Score_diff_list[team]) / unlist(Standard_deviation_list[team])
}
summary(unlist(mean_over_sd_list))
mean_over_sd_list <- mean_over_sd_list[order(names(mean_over_sd_list))]

saveRDS(mean_over_sd_list, file =  "Mean_over_standard_deviation_list")
mean_over_sd_list = readRDS("Mean_over_standard_deviation_list")

mean_over_sd_rank_List = list()
for(team in division_1_teams){
  mean_over_sd_rank_List[team] = which(names(mean_over_sd_list) == team)
}
mean_over_sd_rank_List = mean_over_sd_rank_List[order(unlist(mean_over_sd_rank_List))]
unlist(mean_over_sd_rank_List[division1_teams])- unlist(team_rank_list[division_1_teams])


ClassA_teams = vector()
for (team in division1_teams){
  if (unlist(mean_over_sd_list[team])  >= summary(unlist(mean_over_sd_list))[5] ){
    ClassA_teams <- c(ClassA_teams, team)
  }
}
ClassA_teams<- sort(ClassA_teams)

ClassB_teams = vector()
for (team in division_1_teams){
  if ( (unlist(mean_over_sd_list[team]) <= 0.3887 )
       & (unlist(mean_over_sd_list[team]) > 0.081 )){
    ClassB_teams <- c(ClassB_teams, team)
  }
}
ClassB_teams <- sort(ClassB_teams)
ClassC_teams = vector()
for (team in division_1_teams){
  if ( (unlist(mean_over_sd_list[team]) <= 0.081) & (unlist(mean_over_sd_list[team]) >= -0.14559 ) ){
    ClassC_teams <- c(ClassC_teams, team)
  }
}
ClassC_teams = sort(ClassC_teams)
ClassD_teams = vector()
for (team in division1_teams){
  if (unlist(mean_over_sd_list[team]) <  -0.14559){
    ClassD_teams <- c(ClassD_teams, team)
  }
}
ClassD_teams = sort(ClassD_teams)


# ClassA_teams_by_score = names(team_rank_list)[which(unlist(team_rank_list) <= 33)]
# ClassB_teams_by_score = names(team_rank_list)[which(unlist(team_rank_list) > 33 & unlist(team_rank_list)<= 66 )]
# ClassC_teams_by_score = names(team_rank_list)[which(unlist(team_rank_list) > 67 & unlist(team_rank_list)<= 99 )]

saveRDS(score_diff_list_by_value, file = "score_difference")

plot(-unlist(score_diff_list_by_value), unlist(team_rank_list))#this plot shows that score_difference and team rank should be a sigmoid function
library(sigmoid)
x <- seq(-30, 20, 0.1)
y3 = 128*(sigmoid(x/4.2))+ 9
y4 = 135*sigmoid( (x+ 1.8)/6)-1 
lines(x, y4, col = "red", lwd = 2)


score_diff_sigmoid = vector()
for ( y in c(1: 131)){
  score_diff_sigmoid <- c(score_diff_sigmoid, 6 * log( inverse_affine(y)/ (1- inverse_affine(y)) ) - 1.6)
}

YardsPerPlay_diff_list = list()
for( team in division_1_teams){
  YardsPerPlay_diff_list[team] = (sum(home_regular_season_16_18$YardsPerPlay[which(home_regular_season_16_18$Team == team)]) 
                                  + sum(home_regular_season_16_18$YardsPerPlay_allowed[which(home_regular_season_16_18$Opponent == team)] ) - 
                                    sum(home_regular_season_16_18$YardsPerPlay_allowed[which(home_regular_season_16_18$Team == team)]) - 
                                    sum(home_regular_season_16_18$YardsPerPlay[which(home_regular_season_16_18$Opponent == team)]))/
                                  (length(which(home_regular_season_16_18$Team == team | home_regular_season_16_18$Opponent == team)))
}
YardsPerPlay_diff_list = YardsPerPlay_diff_list[order(-unlist(YardsPerPlay_diff_list))]
YPP_rank_list = list()
for (team in names(YardsPerPlay_diff_list)){
  YPP_rank_list[team] = which(names(YardsPerPlay_diff_list) == team)
}
plot(-unlist(YardsPerPlay_diff_list), unlist(YPP_rank_list))#also look like a sigmoid function
saveRDS(YardsPerPlay_diff_list, file = 'YardsPerPlay_difference_list')

saveRDS(ClassA_teams, file = 'ClassA_teams')
saveRDS(ClassB_teams, file = 'ClassB_teams')
saveRDS(ClassC_teams, file = 'ClassC_teams')
saveRDS(ClassD_teams, file = 'ClassD_teams')
ClassA_teams = readRDS('ClassA_teams')
ClassB_teams = readRDS('ClassB_teams')
ClassC_teams = readRDS('ClassC_teams')
ClassD_teams = readRDS('ClassD_teams')

Actual_games_data$YardsPerPlay_diff = Actual_games_data$YardsPerPlay - Actual_games_data$YardsPerPlay_allowed


YardsPerPlay_list = list()
for(team in division1_teams){
  YardsPerPlay_list[team] = (sum(home_regular_season_16_18$YardsPerPlay[which(home_regular_season_16_18$Team == team)]) +
    sum(home_regular_season_16_18$YardsPerPlay[which(home_regular_season_16_18$Opponent == team)]) ) /
    (length(which(home_regular_season_16_18$Team == team | home_regular_season_16_18$Opponent == team)))
}
summary(unlist(YardsPerPlay_list))

YardsPP_diff_list = list()
for (team in division1_teams){
  YardsPP_diff_list[team] = (sum(Actual_games_data$YardsPerPlay_diff[which(Actual_games_data$Team == team)]) -
                               sum(Actual_games_data$YardsPerPlay_diff[which(Actual_games_data$Opponent == team)]))/ 
    (length(which(home_regular_season_16_18$Team == team | home_regular_season_16_18$Opponent == team)))
}
summary(unlist(YardsPP_diff_list))
YardsPP_diff_list <- YardsPP_diff_list[order(unlist(YardsPP_diff_list))]
ClassA_YPP = vector()
for (team in division_1_teams){
  if (unlist(YardsPP_diff_list[team]) >= 0.98 ){
    ClassA_YPP <- c(ClassA_YPP, team)
  }
}
ClassA_YPP<- sort(ClassA_YPP)

classB_YYP = vector()
for (team in division1_teams){
  if ((unlist(YardsPP_diff_list[team]) < 0.98) & 
    (unlist(YardsPP_diff_list[team]) >= YardsPP_diff_list["VTECH"]) ){
      classB_YYP = c(classB_YYP, team)
    }
}
classB_YYP <- sort(classB_YYP)
YYP_diff_jump = vector()
for (i in c(1:130)){
  YYP_diff_jump <- c( YYP_diff_jump ,unlist(YardsPP_diff_list)[i + 1] - unlist(YardsPP_diff_list)[i])
}


classC_YYP = vector()
for (team in division_1_teams){
  if ((unlist(YardsPP_diff_list[team]) < YardsPP_diff_list['VTECH']) &
      unlist(YardsPP_diff_list[team]) >= YardsPP_diff_list["TXTECH"]){
        classC_YYP <- c(classC_YYP, team)
      }
}
classC_YYP <- sort(classC_YYP)

classD_YYP = vector()
for (team in division_1_teams){
  if ((unlist(YardsPP_diff_list[team]) < YardsPP_diff_list['TXTECH']) &
      unlist(YardsPP_diff_list[team]) >= YardsPP_diff_list["DUKE"]){
    classD_YYP <- c(classD_YYP, team)
  }
}
classD_YYP <- sort(classD_YYP)

classE_YYP = vector()
for (team in division_1_teams){
  if (unlist(YardsPP_diff_list[team]) < YardsPP_diff_list['DUKE']){
    classE_YYP <- c(classE_YYP,team)
  }
}
classE_YYP <- sort(classE_YYP)


Score_jump = vector()
for (i in (1:130)){
  Score_jump <- c(Score_jump, (unlist(score_diff_list_by_value)[i + 1] - unlist(score_diff_list_by_value)[i]) *10)
}
Score_jump
saveRDS(ClassA_YPP, file = 'ClassA_YardsPP')
saveRDS(classB_YYP, file = 'ClassB_YardsPP')
saveRDS(classC_YYP, file = 'ClassC_YardsPP')
saveRDS(classD_YYP, file = 'ClassD_YardsPP')
saveRDS(classE_YYP, file = 'ClassE_YardsPP')
############################################################################################################################
####update using Team dimension, namely we use the 4 classes in division1 team and take the 5th class to be division2 teams
class_of_team <- function(team){
  if (team %in% Division2_teams){ 5}
  else if (team %in% ClassA_teams){ 1}
  else if (team %in% ClassB_teams){ 2}
  else if (team %in% ClassC_teams){3}
  else{ 4}
}

class_of_team_YPP<- function(team){
  if (team %in%Division2_teams){6}
  else if (team %in% ClassA_YPP){1}
  else if (team %in% classB_YYP){2}
  else if (team %in% classC_YYP){3}
  else if (team %in% classD_YYP){4}
  else if (team %in% classE_YYP){5}
}
#now I am going to use simple average in season for both classes and team stats. the difference between two teams in same class is calculated using team diff

#declare a 4 by 5 by 31 array with rows home team and columns away team to store the previous game stats

#team_stats_table = data.frame(matrix(0, nrow = 4, ncol =5))
#colnames(team_stats_table) <- c('awayA', 'awayB', 'awayC', 'awayD', 'division2')


team_stats_table = array(rep(0, 4*5*4), dim = c(4,5,4))
class_games_played = 0


update_stats_team_class <- function(i, team_stats_table){
  team = home_regular_season_16_18$Team[i]
  opponent = home_regular_season_16_18$Opponent[i]
  season = home_regular_season_16_18$Season[i]
  home_class = class_of_team(team)
  opponent_class = class_of_team(opponent)
  if (home_class == 5){ team_stats_table }#only 10 games, can ignore
  
  # else if (class_of_team(opponent) == 5){ #opponent is a division2 team, don't care about season, just update, and don't update teams_stats_list
  #  
  #  
  #  class_games_played = team_stats_table[home_class, 5,4]
  #  pure_stats = (as.numeric(home_regular_season_16_18[i, c(7, 22)]) + class_games_played * team_stats_table[home_class, 5, 1:2])
  #  
  #  class_games_played <- class_games_played + 1
  #  team_stats_table[home_class, 5, ] = c(pure_stats/class_games_played, season, class_games_played)
  # }  
  # 
  # else{
    
  else if(home_class != opponent_class){
      class_stats_game = as.numeric(home_regular_season_16_18[i, c(7, 22)])
      class_played_in_season = team_stats_table[home_class, opponent_class, 4]
      pure_stats = (class_stats_game + class_played_in_season * team_stats_table[home_class, opponent_class, c(1:2)])
      team_stats_table[home_class, opponent_class, ] = c(pure_stats/ (class_played_in_season + 1), season, class_played_in_season + 1)
      
      #don't need to update team_stats_table[opponent_class, home_class,] because those are when opponent_class is home team
      # class_stats_game_oppo = as.numeric(home_regular_season_16_18[i,c(22,8, 23: 35, 7, 9: 21)])
      # class_stats_game_oppo[2] = class_stats_game_oppo[2]* (-1)
      # class_played_in_season_oppo = team_stats_table[opponent_class, home_class, 31]
      # pure_stats = (class_stats_game_oppo + class)
    }
    
    
   (team_stats_table)
}

YPP_table = array(rep(0, 5*6* 4), dim = c(5, 6, 4))
YPP_table[, ,3] = rep(2015, 5 * 6)

update_YPP_table <- function (i, YPP_table){
  team = home_regular_season_16_18$Team[i]
  opponent = home_regular_season_16_18$Opponent[i]
  season = home_regular_season_16_18$Season[i]
  home_class_YPP = class_of_team_YPP(team)
  opponent_class_YPP = class_of_team_YPP(opponent)
  
  if (home_class_YPP == 6){YPP_table}
  
  else if (home_class_YPP != opponent_class_YPP){
    games_played_by_class_YPP = YPP_table[home_class_YPP, opponent_class_YPP, 4]
    previous_stats_YPP = YPP_table[home_class_YPP,opponent_class_YPP, c(1:2)]
    YPP_class_this_game = as.numeric(home_regular_season_16_18[i, c(13,27)])
    pure_stats_YPP = (YPP_class_this_game + games_played_by_class_YPP * previous_stats_YPP)
    YPP_table[home_class_YPP, opponent_class_YPP,] = c( pure_stats_YPP/(games_played_by_class_YPP + 1), season, games_played_by_class_YPP + 1)
    
  }
  YPP_table
}


teams_stats_list = list()
valid_games_for_class_updation = vector()
team_stats_table = array(rep(0, 4*5*4), dim = c(4,5,4))
team_stats_table[,,3] = rep(2015, 4*5)
YPP_table = array(rep(0, 5*6* 4), dim = c(5, 6, 4))
YPP_table[, ,3] = rep(2015, 5 * 6)
games_involving_same_class_valid = vector()


for (i in (1: nrow(home_regular_season_16_18))){
  
team = home_regular_season_16_18$Team[i]
opponent = home_regular_season_16_18$Opponent[i]
home_class = class_of_team(team)
opponent_class = class_of_team(opponent)
home_class_YPP = class_of_team_YPP(team)
opponent_class_YPP = class_of_team_YPP(opponent)
  if (i %% 200 == 0){print(i)}
  if (home_class == 5){next}
  if (!(team %in% names(teams_stats_list) ) |!(opponent %in% names(teams_stats_list)) ){
    if ( (home_class!= opponent_class) & (team_stats_table[home_class, opponent_class, 4] != 0)){
      if ((home_class_YPP != opponent_class_YPP) & (YPP_table[home_class_YPP, opponent_class_YPP, 4]!= 0)){
        valid_games_for_class_updation <- c(valid_games_for_class_updation, i)
        home_regular_season_16_18[i, c(36,51)] = team_stats_table[home_class, opponent_class, c(1:2)]
        home_regular_season_16_18$YardsPerPlay_pred[i] = YPP_table[home_class_YPP, opponent_class_YPP,1]
        home_regular_season_16_18$YardsPerPlay_allowed_pred[i] = YPP_table[home_class_YPP, opponent_class_YPP, 2]
      }
    }
  }

  else{# both are in team_stats_list
    season = home_regular_season_16_18$Season[i]
    if (home_class == opponent_class | home_class_YPP == opponent_class_YPP){ #any of two classification in the same class
      if (season == as.numeric(unlist(teams_stats_list[team]))[30] & season == as.numeric(unlist(teams_stats_list[opponent]))[30] ){
        valid_games_for_class_updation <- c(valid_games_for_class_updation, i)
      
        if (home_class == opponent_class){
          home_regular_season_16_18[i, c(36,51)] = as.numeric( unlist(teams_stats_list[team]))[c(1, 16)]
          home_regular_season_16_18[i, c(65,80)] = as.numeric( unlist(teams_stats_list[opponent]))[c(1, 16)]
          
        
          if (home_class_YPP == opponent_class_YPP){
            home_regular_season_16_18[i, c(42, 56)] = as.numeric(unlist(teams_stats_list[team]))[c(7, 21)]
            home_regular_season_16_18[i, c(71,85)] = as.numeric(unlist(teams_stats_list[opponent]))[c(7, 21)]
          }
          else{
            home_regular_season_16_18$YardsPerPlay_pred[i] = YPP_table[home_class_YPP, opponent_class_YPP,1]
            home_regular_season_16_18$YardsPerPlay_allowed_pred[i] = YPP_table[home_class_YPP, opponent_class_YPP, 2]
          }
        
        }
        else{#now only possibility is home_class_YPP same but score_class_different
          
          home_regular_season_16_18[i, c(42, 56)] = as.numeric(unlist(teams_stats_list[team]))[c(7, 21)]
          home_regular_season_16_18[i, c(71,85)] = as.numeric(unlist(teams_stats_list[opponent]))[c(7, 21)]
          home_regular_season_16_18[i, c(36,51)] = team_stats_table[home_class, opponent_class, c(1:2)]
        }
        
        
      }
      
    }
    else {#both classification end up in different class
      if ((team_stats_table[home_class, opponent_class, 4]!= 0) & (YPP_table[home_class_YPP, opponent_class_YPP,4] != 0)){
        valid_games_for_class_updation <- c(valid_games_for_class_updation, i)
        home_regular_season_16_18[i, c(36,51)] = team_stats_table[home_class, opponent_class, c(1:2)]
        home_regular_season_16_18$YardsPerPlay_pred[i] = YPP_table[home_class_YPP, opponent_class_YPP,1]
        home_regular_season_16_18$YardsPerPlay_allowed_pred[i] = YPP_table[home_class_YPP, opponent_class_YPP, 2]
      }
      
    }
    
  }

  teams_stats_list<- update_stats_simple_average(i, teams_stats_list)
  team_stats_table <- update_stats_team_class(i, team_stats_table)
  YPP_table<- update_YPP_table(i, YPP_table)
}
games_for_training_and_testing = home_regular_season_16_18[valid_games_for_class_updation,]
games_for_training_and_testing$Score_diff_pred = games_for_training_and_testing$Score_pred - games_for_training_and_testing$Score_allowed_pred
cor(games_for_training_and_testing$Score_diff, games_for_training_and_testing$Score_diff_pred)
cor(games_for_training_and_testing$Score, games_for_training_and_testing$Score_pred)
cor(games_for_training_and_testing$Score_allowed, games_for_training_and_testing$Score_allowed_pred)
cor(games_for_training_and_testing$YardsPerPlay, games_for_training_and_testing$YardsPerPlay_pred)
cor(games_for_training_and_testing$YardsPerPlay_allowed, games_for_training_and_testing$YardsPerPlay_allowed_pred)
games_for_training_and_testing$YardsPerPlay_diff = games_for_training_and_testing$YardsPerPlay - games_for_training_and_testing$YardsPerPlay_allowed
games_for_training_and_testing$YardsPerPlay_diff_pred = games_for_training_and_testing$YardsPerPlay_pred - 
  games_for_training_and_testing$YardsPerPlay_allowed_pred
cor(games_for_training_and_testing$Score_diff, games_for_training_and_testing$YardsPerPlay_diff)
cor(games_for_training_and_testing$YardsPerPlay_diff, games_for_training_and_testing$YardsPerPlay_diff_pred)
cor(games_for_training_and_testing$Score_diff, games_for_training_and_testing$YardsPerPlay_diff_pred)
write.csv(games_for_training_and_testing, file = "update_with_score_and_YPP.csv", row.names = F)

games_score_diff_and_YPP_diff = games_for_training_and_testing[, c(1, 4,6, 7:8, 13, 22, 27, 36,37, 42, 51, 56, 95)]
train = which(games_score_diff_and_YPP_diff$Season == 2016 | games_score_diff_and_YPP_diff$Season == 2017)
linear.reg.with.score.and.YPP = lm(Score_diff~ Score_pred+ Score_allowed_pred + YardsPerPlay_pred + YardsPerPlay_allowed_pred,
                                   data = games_score_diff_and_YPP_diff[train,])
linear.reg.with.score.and.YPP = lm(Score_diff~ Score_diff_pred + YardsPerPlay_diff_pred, data = games_score_diff_and_YPP_diff[train,])
summary(linear.reg.with.score.and.YPP)
linear.score.and.ypp.train = predict (linear.reg.with.score.and.YPP, newdata = games_score_diff_and_YPP_diff[train,])
plot(linear.score.and.ypp.train, games_score_diff_and_YPP_diff$Score_diff[train])
mean((linear.score.and.ypp.train - games_score_diff_and_YPP_diff$Score_diff[train])^2)
linear.score.and.ypp.test = predict(linear.reg.with.score.and.YPP, newdata = games_score_diff_and_YPP_diff[-train,])
plot(linear.score.and.ypp.test, games_score_diff_and_YPP_diff$Score_diff[-train])
mean((linear.score.and.ypp.test - games_score_diff_and_YPP_diff$Score_diff[-train])^2)
# if (!(team %in% names(teams_stats_list)) | !(opponent %in% names(teams_stats_list))){
#   if (home_class!= opponent_class){
#    # print(c(team, home_class, opponent, opponent_class))
#     if  (team_stats_table[home_class, opponent_class, 31] != 0) {
#      
#       home_regular_season_16_18[i, 36:64] = team_stats_table[home_class, opponent_class, c(1:29)]
#       inverse_stats = team_stats_table[home_class, opponent_class, c(16, 2, 17:29, 1, 3:15)]
#       inverse_stats[2] = inverse_stats[2]*(-1)
#       home_regular_season_16_18[i, 65:93] = inverse_stats
#       valid_games_for_class_updation <- c(valid_games_for_class_updation, i)
#       team_stats_table <- update_stats_team_class(i,team_stats_table)
#     }
#     else{
#     team_stats_table <- update_stats_team_class(i,team_stats_table)
#   }
#   
#   }
#   teams_stats_list <- update_stats_simple_average(i, teams_stats_list)
# }
# 
# 
# else{ #both team and opponent in teams_stats_list
#   season = home_regular_season_16_18$Season[i]
#   if (home_class == opponent_class){
#     if (season == as.numeric(unlist(teams_stats_list[team]))[30] & season == as.numeric(unlist(teams_stats_list[opponent]))[30] ){  
#       #print(c(i, home_regular_season_16_18$Team[i], home_regular_season_16_18$Opponent[i], home_class))
#       games_involving_same_class_valid <- c(games_involving_same_class_valid, i)
#       home_regular_season_16_18[i, 36:64] = as.numeric(unlist(teams_stats_list[team]))[1:29]
#       home_regular_season_16_18[i, 65: 93] = as.numeric(unlist(teams_stats_list[opponent]))[1:29]
#       teams_stats_list<- update_stats_simple_average(i, teams_stats_list)
#       valid_games_for_class_updation <- c(valid_games_for_class_updation, i)
#     }
#     else{
#       teams_stats_list<- update_stats_simple_average(i, teams_stats_list)
#     }
#   }
# 
#   
#   else{#home_class different from opponent_class
#     if (team_stats_table[home_class, opponent_class, 31] != 0){
#     home_regular_season_16_18[i, 36:64] = team_stats_table[home_class, opponent_class, c(1:29)]
#     inverse_stats = team_stats_table[home_class, opponent_class, c(16, 2, 17:29, 1, 3:15)]
#     inverse_stats[2] = inverse_stats[2]*(-1)
#     home_regular_season_16_18[i, 65:93] = inverse_stats
#     valid_games_for_class_updation <- c(valid_games_for_class_updation, i)
#     team_stats_table = update_stats_team_class(i,team_stats_table)
#     teams_stats_list <- update_stats_simple_average(i, teams_stats_list)
#     }
#     else{
#       team_stats_table = update_stats_team_class(i,team_stats_table)
#       teams_stats_list <- update_stats_simple_average(i, teams_stats_list)
#     }
#   }
#  
# }
#print(i)
#}


################################UPdate with Fantasy Points#################################################################



# regular_season_by_team_with_Fantasy = regular_season_by_team_with_Fantasy[complete.cases(regular_season_by_team_with_Fantasy),]
# 
# update_opponent <- function(opponent, weight,teams_stats_list){
#   
#   
#   if (!(opponent %in% names(teams_stats_list))){
#     opponent_current_stats_numeric = as.numeric(regular_season_by_team_with_Fantasy[i, c(22,8, 23: 35, 7, 9: 21)])
#     opponent_current_stats_numeric[2] <- opponent_current_stats_numeric[2] * -1 #changing sign of score_diff
#     #opponent_current_stats_numeric[1] = 1- opponent_current_stats_numeric[1] #changing HomeorAway for opponent
#     
#     teams_stats_list[opponent] = list(opponent_current_stats_numeric)
#   }
#   else{
#     opponent_current_stats_numeric = regular_season_by_team_with_Fantasy[i, c(22,8, 23: 35, 7, 9: 21)]
#     opponent_current_stats_numeric[2] <- opponent_current_stats_numeric[2] * -1
#     #opponent_current_stats_numeric[1] = 1- opponent_current_stats_numeric[1]
#     teams_stats_list[opponent] =list( as.numeric(unlist(teams_stats_list[opponent] ) ) * (1- weight) + opponent_current_stats_numeric* weight )
#   }
#   return(teams_stats_list)
# }
# teams_stats_list = list()
# 
# for(i in (1: nrow(regular_season_by_team_with_Fantasy))){
#   if (i %%100 == 0){
#     print(i)
#   }
#   team = regular_season_by_team_with_Fantasy$Team[i]
#   opponent = regular_season_by_team_with_Fantasy$Opponent[i]
#   
#   if(!(team %in% names(teams_stats_list)) ){
#     #teams_visited <- c(teams_visited, Team_list[i])
#     season = regular_season_by_team_with_Fantasy$Season[i]
#     teams_stats_list[team] = list(regular_season_by_team_with_Fantasy[i,c( 7:35) ] )
#     teams_stats_list <- update_opponent(opponent, weight_this_game,teams_stats_list)
#     
#     
#     #games_team_played_in_season = 1
#   }
#   else{#team already in teams_stats_list
#     if(regular_season_by_team_with_Fantasy$Season[i] != season){#first game in season
#       season = regular_season_by_team_with_Fantasy$Season[i]
#       #games_team_played_in_season = 1
#       teams_stats_list[team] = list(regular_season_by_team_with_Fantasy[i, c(7:35)]*weight_this_season + as.numeric(unlist(teams_stats_list[team] ) )* (1- weight_this_season) )
#       
#       teams_stats_list<- update_opponent(opponent, weight_this_season, teams_stats_list)
#       #note we don't have a prediction for the first game of each season
#     }
#     else{ #not first game in season
#       
#       teams_stats_list[team] =list( weight_this_game * as.numeric( regular_season_by_team_with_Fantasy[i - 1, c(7:35)]) + as.numeric(unlist(teams_stats_list[team]) )* (1 - weight_this_game) )
#       
#       #games_team_played_in_season = games_team_played_in_season + 1
#       teams_stats_list<- update_opponent(opponent, weight_this_game, teams_stats_list)
#       
#       #finally update the predictions
#       
#       regular_season_by_team_with_Fantasy[i, 36:64] = as.numeric(unlist(teams_stats_list[team]) )
#       regular_season_by_team_with_Fantasy[i, 65: 93]= as.numeric(unlist(teams_stats_list[opponent]))
#       # - as.numeric(unlist(teams_stats_list[opponent])[c(16, 2, 17:29, 1, 3: 15)] )
#       
#       
#     }
#     
#   }
# }
# 
# 


#############Finish updating

write.csv(regular_season_by_team_16_18, "regular_season_by_team_16_to_18_with_weights_0.5&0.3_preds_filled.csv", row.names = F)
regular_season_by_team_16_18 = read.csv("regular_season_by_team_16_to_18_with_weights_0.5&0.3_preds_filled.csv", stringsAsFactors = F)
regular_season_by_team_16_18 = read.csv('division1_regular_season_16_18_with_vegas_numeric.csv', stringsAsFactors = F)
home_with_vegas = read.csv('division1_regular_season_16_18_with_vegas_numeric_correct_update.csv', stringsAsFactors = F)
home_with_vegas = read.csv('division1_regular_season_16_18_with_vegas_0.3&0.7.csv', stringsAsFactors = F)
home_with_vegas = read.csv("division1_update_only_with_vegas_0.3&0.7.csv", stringsAsFactors = F)
home_with_vegas = read.csv("updating_team_dimension_with_vegas.csv", stringsAsFactors = F)
home_with_vegas = read.csv("update_score_YPP_with_vegas.csv", stringsAsFactors = F)

home_with_vegas<- home_with_vegas[,-1]


sum(is.na(regular_season_by_team_16_18))

write.csv(regular_season_by_team_with_Fantasy, "regular_season_by_team_with_Fantasy_weights_0.5&0.3_preds_filled.csv", row.names = F)
regular_season_by_team_with_Fantasy = read.csv("regular_season_by_team_with_Fantasy_weights_0.5&0.3_preds_filled.csv", stringsAsFactors = F)
sum(is.na(regular_season_by_team_with_Fantasy))
######Now with the features given, we are set to do ML models to do REGRESSION first since we don't have the Vegas line yet####
##for each team, we get rid of the first two games of the season and retain every other set as the train set
#note we are trying to predict the score_diff with the predicted features.
Team_list = (regular_season_by_team_16_18$Team)
first_n_games = 2
games_to_be_retained = vector()
teams_visited = vector()
games_team_played_in_season = 1
season = 2016 #season and week are of class integer, DateTime is of class Char
for (i in (1: nrow(regular_season_by_team_16_18))){
  if(i%%100 ==0){
    print(i)
  }
  
  if (!(Team_list[i] %in% teams_visited)){
    teams_visited<- c(teams_visited, Team_list[i])
    season = regular_season_by_team_16_18$Season[i]
    games_team_played_in_season = 1
  }
  else{
    if (regular_season_by_team_16_18$Season[i] != season){
      season = regular_season_by_team_16_18$Season[i]
      games_team_played_in_season = 1
    }
    else{
      if (games_team_played_in_season < first_n_games){
        games_team_played_in_season = games_team_played_in_season + 1
      }
      else{
        games_team_played_in_season = games_team_played_in_season + 1
        games_to_be_retained = c(games_to_be_retained, i)
      }
    }
  }
}
saveRDS(games_to_be_retained, file= "games_that_are_not_the_first_2_games_in_season.RData")
games_to_be_retained = readRDS("games_that_are_not_the_first_2_games_in_season.RData")

saveRDS(games_to_be_retained, file = "division1_games_that_are_not_the_first_2_games_in_season.RData")
games_to_be_retained = readRDS("division1_games_that_are_not_the_first_2_games_in_season.RData")

Home_team_list = home_with_vegas$Team
Away_team_list = home_with_vegas$Opponent


Games_played_by_team_in_season_list = list()
current_season = 2016
games_to_be_retained = vector()


for (team in unique(Away_team_list)){
  Games_played_by_team_in_season_list[team] = 0
}


for(i in (1: nrow(home_with_vegas))){
  home_team = home_with_vegas$Team[i]
  away_team= home_with_vegas$Opponent[i]
  
  if (home_with_vegas$Season[i] != current_season){
    current_season = home_with_vegas$Season[i]
    for (team in division_1_teams){
      Games_played_by_team_in_season_list[team] = 0
    }
    Games_played_by_team_in_season_list[home_team] = unlist(Games_played_by_team_in_season_list[home_team]) + 1
    Games_played_by_team_in_season_list[away_team] = unlist(Games_played_by_team_in_season_list[away_team]) + 1
  }
  
  else{
      if ((Games_played_by_team_in_season_list[home_team] >= first_n_games) & (Games_played_by_team_in_season_list[away_team]>= first_n_games)){
        games_to_be_retained <- c(games_to_be_retained, i)
        Games_played_by_team_in_season_list[home_team] = unlist(Games_played_by_team_in_season_list[home_team]) + 1
        Games_played_by_team_in_season_list[away_team] = unlist(Games_played_by_team_in_season_list[away_team]) + 1
      }
      else{
        Games_played_by_team_in_season_list[home_team] = unlist(Games_played_by_team_in_season_list[home_team]) + 1
        Games_played_by_team_in_season_list[away_team] = unlist(Games_played_by_team_in_season_list[away_team]) + 1
      }
  }
  
}

saveRDS(games_to_be_retained, file = "home_division1_games_that_are_not_the_first_2_games_in_season.RData")
games_to_be_retained = readRDS("home_division1_games_that_are_not_the_first_2_games_in_season.RData")


games_for_training_and_testing = home_with_vegas[games_to_be_retained,]




######Using the actual game features, we realized that the simple linear regression does pretty well. 
######therefore the problem lies in the way we update our data. 


#training_df$HomeOrAway <- as.factor( training_df$HomeOrAway)
training_df_with_only_Y_and_X_preds = training_df[, c(5,8,34: 60)]

#training_df_without_score_diff_preds = training_df[,c(5,8, 34,36: 60)]#now without score_diff_pred, which is highly collinear
CFB_16_18_df = training_df_with_Fantasy[, c(5,8, 36, 38:49, 51:63, 65, 67:78, 80:92)]

#data with Vegas info
training_df = training_df[, -1]
home_team_games = training_df[which(training_df$HomeOrAway =="HOME"),]
away_team_games = training_df[which(training_df$HomeOrAway == 'AWAY'),]
CFB_16_18_df = home_team_games[, c(8, 36, 38:49, 51:63, 65, 67:78, 80:92,94)]#no homeor away
CFB_16_18_df = away_team_games[, c(8, 36, 38:49, 51:63, 65, 67:78, 80:92,94)]
CFB_16_18_df = games_for_training_and_testing[, c(1,8, 36, 38:49, 51:63, 65, 67:78, 80:92,94)]
CFB_16_18_df = CFB_16_18_df[complete.cases(CFB_16_18_df),]
CFB_16_18_df = training_df[, c(5,8, 36, 38:49, 51:63, 65, 67:78, 80:92,94)]
CFB_16_18_df = home_with_vegas[, c("Season",'Week','Team','Opponent',
                                   'Score','Score_diff', 'Score_allowed',
                                   "Score_pred", 'Score_diff_pred','Score_allowed_pred',
                                    'YardsPerPlay', 'YardsPerPlay_allowed', "YardsPerPlay_diff",
                                   "YardsPerPlay_pred","YardsPerPlay_allowed", "YardsPerPlay_diff_pred",
                                   'vegas_sprd' )]
CFB_16_18_df<- CFB_16_18_df[complete.cases(CFB_16_18_df),]
Actual_games_data = games_for_training_and_testing[,c(1,4, 6, 7:20, 22:34, 94)]


CFB_16_18_df$home_team_score_diff_proj = rep(0, nrow(CFB_16_18_df))
CFB_16_18_df$opponent_score_diff_proj = rep(0, nrow(CFB_16_18_df))
for (i in (1:nrow(CFB_16_18_df))){
  if (CFB_16_18_df$Opponent[i] %in% Division2_teams){
    CFB_16_18_df$Opponent[i]<- 'D2_team'
  }
}
for(i in c(1:nrow(CFB_16_18_df))){
  CFB_16_18_df[i, "home_team_score_diff_proj"] = as.numeric(unlist(score_diff_projection_by_prev_2_seasons[CFB_16_18_df$Team[i]]))
  CFB_16_18_df[i, "opponent_score_diff_proj"] = as.numeric(unlist(score_diff_projection_by_prev_2_seasons[CFB_16_18_df$Opponent[i]]))
}

division_1_games = CFB_16_18_df[which(CFB_16_18_df$Opponent %in% division_1_teams),]

cor(division_1_games$Score_diff, unlist(division_1_games$opponent_score_diff_proj))
cor(division_1_games$Score_diff, unlist(division_1_games$home_team_score_diff_proj))
cor(division_1_games$Score_diff, unlist(division_1_games$home_team_score_diff_proj) - unlist(division_1_games$opponent_score_diff_proj))
cor(division_1_games$Score_diff, division_1_games$vegas_sprd)

home_team_rank_vect = vector()
opponent_team_rank_vect = vector()
for(i in (1: nrow(CFB_16_18_df))){
  home_team_rank_vect<- c(home_team_rank_vect, unlist(team_rank_list_16_17[CFB_16_18_df$Team[i]]))
  opponent_team_rank_vect <- c(opponent_team_rank_vect, unlist(team_rank_list_16_17[CFB_16_18_df$Opponent[i]]))
}
CFB_16_18_df$home_team_rank = as.numeric(home_team_rank_vect)
CFB_16_18_df$opponent_team_rank = as.numeric(opponent_team_rank_vect)


division_1_18_games = division_1_games[which(division_1_games$Season == 2018),]
projection_18 = unlist(division_1_18_games$home_team_score_diff_proj)- unlist(division_1_18_games$opponent_score_diff_proj)
plot(division_1_18_games$Score_diff, projection_18)
plot(division_1_18_games$Score_diff, division_1_18_games$vegas_sprd)
plot(division_1_18_games$Score_diff, division_1_18_games$home_team_score_diff_proj)
train = which(division_1_games$Season == 2016 | division_1_games$Season == 2017)
linear.reg.team.rank = lm(Score_diff~ vegas_sprd + home_team_rank + opponent_team_rank + Score_pred + Score_allowed_pred,
                        data = division_1_games[train,])
summary(linear.reg.team.rank)
linear.reg.team.rank.train = predict(linear.reg.team.rank, newdata = division_1_games[train,])
team.rank.train.correction = sign((linear.reg.team.rank.train + division_1_games$vegas_sprd[train]) * 
                                    (division_1_games$ Score_diff + division_1_games$vegas_sprd)[train])
length(which(team.rank.train.correction == 1))/ length(team.rank.train.correction)

linear.reg.team.rank.test = predict(linear.reg.team.rank, newdata = division_1_18_games)
team.rank.test.correction = sign( (linear.reg.team.rank.test + division_1_18_games$vegas_sprd) *
                                    (division_1_18_games$Score_diff + division_1_18_games$vegas_sprd))

length(team.rank.test.correction)
length(which(team.rank.test.correction == 1))


#######################################################SVM for division 1 teams only#############################################################


home_team_rank_vect = vector()
opponent_team_rank_vect = vector()
for(i in (1: nrow(CFB_16_18_df))){
  home_team_rank_vect<- c(home_team_rank_vect, unlist(team_rank_list_16_17[CFB_16_18_df$Team[i]]))
  opponent_team_rank_vect <- c(opponent_team_rank_vect, unlist(team_rank_list_16_17[CFB_16_18_df$Opponent[i]]))
}
CFB_16_18_df$home_team_rank = as.numeric(home_team_rank_vect)
CFB_16_18_df$opponent_team_rank = as.numeric(opponent_team_rank_vect)
division_1_games = CFB_16_18_df[which(CFB_16_18_df$Opponent %in% division_1_teams),]

division_1_18_games = division_1_games[which(division_1_games$Season == 2018),]


train = which( (division_1_games$Season == Last_season & division_1_games$Week >= 8)  | (division_1_games$Season == current_season & division_1_games$Week <= 7) )
test = which(division_1_games$Season == current_season & division_1_games$Week >= 8)
game_df_sprd_for_2nd_half = division_1_games[c(train, test),]

home_team_rank_vect.previous.two.half.season = vector()
away_team_rank_vect.previous.two.half.season = vector()
for (i in (1:nrow(game_df_sprd_for_2nd_half))){
  home_team_rank_vect.previous.two.half.season <- c(home_team_rank_vect.previous.two.half.season,
                                                    unlist(team.rank.list.previous.two.half.seasons[game_df_sprd_for_2nd_half$Team[i]]))
  away_team_rank_vect.previous.two.half.season<- c(away_team_rank_vect.previous.two.half.season,
                                                   unlist(team.rank.list.previous.two.half.seasons[game_df_sprd_for_2nd_half$Opponent[i]]))
}
game_df_sprd_for_2nd_half$home_team_rank = home_team_rank_vect.previous.two.half.season
game_df_sprd_for_2nd_half$opponent_team_rank = away_team_rank_vect.previous.two.half.season

svm.team.rank = svm(Score_diff~ vegas_sprd + home_team_rank + opponent_team_rank,
                    data = game_df_sprd_for_2nd_half[c(1:length(train)),])

summary(svm.team.rank)

svm.team.rank.train = predict(svm.team.rank, newdata = game_df_sprd_for_2nd_half[c(1:length(train)),])
svm.team.rank.train.correction = sign((svm.team.rank.train + division_1_games$vegas_sprd[train])*
                                  (division_1_games$vegas_sprd + division_1_games$Score_diff)[train])
length(which(svm.team.rank.train.correction == 1))/ length(svm.team.rank.train.correction)

svm.team.rank.predict = predict(svm.team.rank, newdata = game_df_sprd_for_2nd_half[c(length(train) + 1: 
                                                                                                        nrow(game_df_sprd_for_2nd_half)),])
svm.team.rank.correction = sign( (svm.team.rank.predict + division_1_games$Score_diff[test]) *
                                   (division_1_games$vegas_sprd + division_1_games$Score_diff)[test])
length(which(svm.team.rank.correction == 1))
length(which(svm.team.rank.correction == 1))/ length(svm.team.rank.correction)

####use 3 previous half seasons to predict the first half season
train = which( division_1_games$Season == Last_season | (division_1_games$Season == second_to_last_season & division_1_games$Week>=8))
test = which(division_1_games$Season == current_season & division_1_games$Week<=7)
team.rank.games.1st.half.season = division_1_games[c(train,test),]
home.rank.vect.prev.3.half.seasons = vector()
away.rank.vect.prev.3.half.seasons = vector()
for (i in (1: nrow(team.rank.games.1st.half.season))){
  home.rank.vect.prev.3.half.seasons <- c(home.rank.vect.prev.3.half.seasons, unlist(team.rank.list.prev.3.half.seasons[team.rank.games.1st.half.season$Team[i]]))
  away.rank.vect.prev.3.half.seasons<- c(away.rank.vect.prev.3.half.seasons, unlist(team.rank.list.prev.3.half.seasons[team.rank.games.1st.half.season$Opponent[i]]))
}
team.rank.games.1st.half.season$home_team_rank = home.rank.vect.prev.3.half.seasons
team.rank.games.1st.half.season$opponent_team_rank = away.rank.vect.prev.3.half.seasons

svm.team.rank.3.half.season = svm(Score_diff~ vegas_sprd + home_team_rank + opponent_team_rank,
                                  data =team.rank.games.1st.half.season[c(1: length(train)), ] )
svm.team.rank.3.half.season.train = predict(svm.team.rank.3.half.season, newdata =team.rank.games.1st.half.season[c(1: length(train)), ] )
svm.3.half.season.train.correction = sign((svm.team.rank.3.half.season.train + division_1_games$vegas_sprd[train])*
                                            (division_1_games$vegas_sprd + division_1_games$Score_diff)[train])
length(which(svm.3.half.season.train.correction == 1))
length(svm.3.half.season.train.correction)

svm.team.rank.3.half.season.test = predict(svm.team.rank.3.half.season, newdata = team.rank.games.1st.half.season[c(length(train) + 1: 
                                                                                                                      nrow(team.rank.games.1st.half.season)),])
svm.3.half.season.test.correction = sign((svm.team.rank.3.half.season.test + division_1_games$vegas_sprd[test]) *
                                           (division_1_games$Score_diff + division_1_games$vegas_sprd)[test])
length(which(svm.3.half.season.test.correction == 1))
length(svm.3.half.season.test.correction)

#################now test if the rank of previous season makes sense

division1_16_games = division_1_games[which(division_1_games$Season == 2016),]
division1_17_games = division_1_games[which(division_1_games$Season == 2017),]
home_team_rank_vect_16 = vector()
opponent_team_rank_vect_16 = vector()
for(i in (1: nrow(division1_16_games))){
  home_team_rank_vect_16<- c(home_team_rank_vect_16, unlist(team_rank_list_16[division1_16_games$Team[i]]))
  opponent_team_rank_vect_16 <- c(opponent_team_rank_vect_16, unlist(team_rank_list_16[division1_16_games$Opponent[i]]))
}
division1_16_games$home_team_rank = as.numeric(home_team_rank_vect_16)
division1_16_games$opponent_team_rank = as.numeric(opponent_team_rank_vect_16)

home_team_rank_vect_16 = vector()
opponent_team_rank_vect_16 = vector()
for(i in (1: nrow(division1_17_games))){
  home_team_rank_vect_16<- c(home_team_rank_vect_16, unlist(team_rank_list_16[division1_17_games$Team[i]]))
  opponent_team_rank_vect_16 <- c(opponent_team_rank_vect_16, unlist(team_rank_list_16[division1_17_games$Opponent[i]]))
}
division1_17_games$home_team_rank = as.numeric(home_team_rank_vect_16)
division1_17_games$opponent_team_rank = as.numeric(opponent_team_rank_vect_16)



svm.16.team.rank = svm(Score_diff~ home_team_rank + opponent_team_rank + vegas_sprd, data = division1_16_games)
svm.16.team.rank.train = predict(svm.16.team.rank, newdata = division1_16_games)
svm.16.team.rank.correction = sign((svm.16.team.rank.train + division1_16_games$vegas_sprd)*
                                     (division1_16_games$vegas_sprd + division1_16_games$Score_diff))
length(svm.16.team.rank.correction)
length(which(svm.16.team.rank.correction == 1))

division1_17_games_without_two_teams = division1_17_games[-which(division1_17_games$Team %in% c('UAB', 'COAST') | 
                                                                   division1_17_games$Opponent %in% c('UAB', 'COAST')),]
svm.16.team.rank.test = predict(svm.16.team.rank, newdata = division1_17_games_without_two_teams)
svm.16.team.rank.test.correction = sign((svm.16.team.rank.test+ division1_17_games_without_two_teams$vegas_sprd) *
                                          (division1_17_games_without_two_teams$Score_diff + division1_17_games_without_two_teams$vegas_sprd))
length(which(svm.16.team.rank.test.correction == 1))
length(svm.16.team.rank.test.correction)


#now from 17 to 18

home_team_rank_vect_17 = vector()
opponent_team_rank_vect_17 = vector()
for (i in (1: nrow(division1_17_games))){
  home_team_rank_vect_17 = c(home_team_rank_vect_17, unlist(team_rank_list_17[division1_17_games$Team[i]]))
  opponent_team_rank_vect_17 = c(opponent_team_rank_vect_17, unlist(team_rank_list_17[division1_17_games$Team[i]]))
}
division1_17_games$home_team_rank = home_team_rank_vect_17
division1_17_games$opponent_team_rank = opponent_team_rank_vect_17

home_team_rank_vect_18_from_17 = vector()
opponent_team_rank_vect_18_from_17 = vector()

for (i in (1: nrow(division_1_18_games))){
  home_team_rank_vect_18_from_17 <- c(home_team_rank_vect_18_from_17, unlist(team_rank_list_17[division_1_18_games$Team[i]]))
  opponent_team_rank_vect_18_from_17<- c(opponent_team_rank_vect_18_from_17, unlist(team_rank_list_17[division_1_18_games$Opponent[i]]))
}
division_1_18_games$home_team_rank = home_team_rank_vect_18_from_17
division_1_18_games$opponent_team_rank = opponent_team_rank_vect_18_from_17

svm.17.to.18.team.rank = svm(Score_diff~ home_team_rank + opponent_team_rank + vegas_sprd, data = division1_17_games)
svm.17.to.18.train = predict(svm.17.to.18.team.rank, newdata = division1_17_games)
svm.17.to.18.train.correction = sign( (svm.17.to.18.train + division1_17_games$vegas_sprd) *
                                        (division1_17_games$Score_diff + division1_17_games$vegas_sprd) )
length(which(svm.17.to.18.train.correction == 1))
length(svm.17.to.18.train.correction)

svm.17.to.18.test = predict(svm.17.to.18.team.rank, newdata = division_1_18_games)
svm.17.to.18.test.correction = sign( (svm.17.to.18.test + division_1_18_games$vegas_sprd) *
                                       (division_1_18_games$vegas_sprd + division_1_18_games$Score_diff))
length(svm.17.to.18.test.correction)
length(which(svm.17.to.18.test.correction == 1))

######################################################################################################


####use SVM on team rank with all games(including division2)
CFB_train = which(CFB_16_18_df$Season == 2016| CFB_16_18_df$Seaso == 2017)
svm.all_regular.team.rank = svm(Score_diff~ home_team_rank + opponent_team_rank+ vegas_sprd, data = CFB_16_18_df[CFB_train,])

svm.all_regular.team.rank.train = predict(svm.all_regular.team.rank, newdata = CFB_16_18_df[CFB_train,])
svm.all_regular.team.rank.train.correction = sign((svm.all_regular.team.rank.train + CFB_16_18_df$vegas_sprd[CFB_train]) *
                                              (CFB_16_18_df$vegas_sprd + CFB_16_18_df$Score_diff)[CFB_train])
length(which(svm.all_regular.team.rank.train.correction == 1))/ length(svm.all_regular.team.rank.train)

svm.all_regular.team.rank.pred = predict(svm.all_regular.team.rank, newdata = CFB_16_18_df[-CFB_train,])
svm.all_regular.team.rank.correction = sign((svm.all_regular.team.rank.pred + CFB_16_18_df$vegas_sprd[-CFB_train])*
                                              (CFB_16_18_df$Score_diff + CFB_16_18_df$vegas_sprd)[-CFB_train])

length(svm.all_regular.team.rank.correction)
length(which(svm.all_regular.team.rank.correction == 1))

svm.d1.team.rank.pred = predict(svm.all_regular.team.rank, newdata = division_1_18_games)
svm.d1.team.rank.correction = sign((svm.d1.team.rank.pred + division_1_18_games$vegas_sprd)*
                                     (division_1_18_games$vegas_sprd + division_1_18_games$Score_diff))
length(which(svm.d1.team.rank.correction == 1))

#sigmoid_correction = sign (( unlist(division_1_18_games$home_team_score_diff_proj) + division_1_18_games$vegas_sprd) * (division_1_18_games$Score_diff + division_1_18_games$vegas_sprd))
#length(which(sigmoid_correction == 1))/ length(sigmoid_correction)


Actual_games_data_with_vegas = CFB_16_18_df
############################################use correlation between projected feature and actual feature for feature selection
games_for_training_and_testing = home_regular_season_16_18[games_to_be_retained_simple_average,] #this is just for testing naive update and other updating schemes
games_for_training_and_testing = home_regular_season_16_18[valid_games_for_class_updation,]
write.csv(games_for_training_and_testing, "update_with_team_dimension_simple_average.csv", row.names = F)
for (i in c(7, 9:18)){
  feature = names(games_for_training_and_testing)[i]
  feature_pred = paste(feature, "_pred", sep = "")
  feature_allowed_oppo_pred = paste(feature, "_allowed_oppo_pred", sep = "")
  
  correlation1 = cor(games_for_training_and_testing[feature], games_for_training_and_testing[feature_pred])
  
  correlation2 = cor(games_for_training_and_testing[feature], games_for_training_and_testing[feature_allowed_oppo_pred])
  #correlation_diff = cor(games_for_training_and_testing[feature], games_for_training_and_testing[feature_pred] 
  #                       - games_for_training_and_testing[feature_allowed_oppo_pred])
  print(c(feature, correlation1, correlation2))
}

cor(games_for_training_and_testing$Hurriesandsacks_allowed, games_for_training_and_testing$Hurriesandsacks_allowed_pred)
cor(games_for_training_and_testing$turnovers, games_for_training_and_testing$turnovers_pred)

games_for_training_and_testing[c(100: 200), c("Score", 'Score_pred', 'Score_allowed', 'Score_allowed_pred')]
games_for_training_and_testing[c(100: 140), c("Score_diff", 'Score_diff_pred')]
cor(games_for_training_and_testing[, 'Score_allowed'], games_for_training_and_testing[, 'Score_allowed_pred'])
cor(games_for_training_and_testing$Score_oppo_pred, games_for_training_and_testing$Score_allowed)
cor(games_for_training_and_testing$Score, games_for_training_and_testing$Score_allowed_oppo_pred)
cor(games_for_training_and_testing$Score_diff, games_for_training_and_testing$Score_diff_pred)
cor(games_for_training_and_testing$Score_diff, games_for_training_and_testing$Score_diff_oppo_pred)


###############################################
#Train with actual game features and see how good is the result 
#############################################
train_actual = which(Actual_games_data_with_vegas$Season == 2016 | Actual_games_data_with_vegas$Season == 2017)
Actual_game_linear_reg = lm(Score_diff~. - Season - Score_allowed -Score - vegas_sprd, data = Actual_games_data_with_vegas[train,])
Actual_game_linear_reg = lm(Score_diff~ Score_diff_pred + YardsPerPlay_diff_pred+ vegas_sprd, data = Actual_games_data_with_vegas[train,])

summary(Actual_game_linear_reg)
actual_train_preds = predict(Actual_game_linear_reg, newdata = Actual_games_data_with_vegas[train,])
plot(actual_train_preds, Actual_games_data_with_vegas$Score_diff[train] )
actual_train_correction = sign((actual_train_preds + Actual_games_data_with_vegas$vegas_sprd[train]) * 
                                 (Actual_games_data_with_vegas$Score_diff[train] + Actual_games_data_with_vegas$vegas_sprd[train]) )
length(which(actual_train_correction == 1))/length(actual_train_correction)
actual_preds = predict(Actual_game_linear_reg, newdata = Actual_games_data_with_vegas[-train, ])
plot(actual_preds, Actual_games_data_with_vegas$Score_diff[-train])
actual_correction = sign((actual_preds + Actual_games_data_with_vegas$vegas_sprd[-train]) * 
                           (Actual_games_data_with_vegas$Score_diff[-train] + Actual_games_data_with_vegas$vegas_sprd[-train]) )
pred_coverage = actual_preds + Actual_games_data_with_vegas$vegas_sprd[-train]
vegas_coverage = Actual_games_data_with_vegas$Score_diff[-train] + Actual_games_data_with_vegas$vegas_sprd[-train]
length(which(actual_correction == 1))/length(actual_correction)

train_actual = which(Actual_games_data$Season == 2016 | Actual_games_data$Season == 2017)


boost.CFB = gbm(Score_diff~.-Season- Score- Score_allowed, data=Actual_games_data, distribution=
                  "gaussian",n.trees =5000, interaction.depth =3, shrinkage = 0.003)
summary(boost.CFB)

for( feature in names(Actual_games_data)[c(4:15, 17: 28)]){
  print(c(feature, cor(Actual_games_data$Score_diff, Actual_games_data[feature])))
}
####################### training with all the features including score_diff_pred, large VIF, thus high colinerality
simple_linear_regression = lm(Score_diff~., data = training_df_with_only_Y_and_X_preds)
summary(simple_linear_regression)
preds = predict(simple_linear_regression)
plot( preds, training_df_with_only_Y_and_X_preds$Score_diff, xlab = "predicts", ylab = "actual score differences")
abline(a=0, b= 1, col = "red", lwd = 2)
par(mfrow= c(2,2))
plot(simple_linear_regression)
library(car)
vif(simple_linear_regression)

set.seed(3)
train = sample(1: nrow(CFB_16_18_df), 3000)

train = which(CFB_16_18_df$Season ==2016 | CFB_16_18_df$Season ==2017)
test = which(CFB_16_18_df$Season == 2018)


##############after class_updating, only score_diff_pred and vegas_sprd seem to be the most important feature:
linear_reg_after_class = lm(Score_diff~ Score_diff_pred + vegas_sprd, data = CFB_16_18_df[train,])
summary(linear_reg_after_class)
linear_reg_after_class_train_preds = predict(linear_reg_after_class, data = CFB_16_18_df[train,])
plot(linear_reg_after_class_train_preds, CFB_16_18_df$Score_diff[train])
abline(0, 1, col = 'red', lwd = 2)
mean((linear_reg_after_class_train_preds - CFB_16_18_df$Score_diff[train])^2)
mean((CFB_16_18_df$vegas_sprd[train] + CFB_16_18_df$Score_diff[train])^2)
train_correction_vector = sign((linear_reg_after_class_train_preds + CFB_16_18_df[train, 'vegas_sprd'])* 
                                 (CFB_16_18_df$Score_diff[train] + CFB_16_18_df$vegas_sprd[train] ))
length(which(train_correction_vector == 1))/length(train)

linear_reg_after_class_preds = predict(linear_reg_after_class, newdata = CFB_16_18_df[-train,])
plot(linear_reg_after_class_preds, CFB_16_18_df$Score_diff[-train])
mean((linear_reg_after_class_preds - CFB_16_18_df$Score_diff[-train])^2)
mean((CFB_16_18_df$Score_diff[-train] + CFB_16_18_df$vegas_sprd[-train])^2)
correction_vector = sign((linear_reg_after_class_preds + CFB_16_18_df$vegas_sprd[-train]) *
  (CFB_16_18_df$vegas_sprd[-train] + CFB_16_18_df$Score_diff[-train]))
length(which(correction_vector == 1))/length(correction_vector)


same_class = which(CFB_16_18_df$Score_allowed_pred != CFB_16_18_df$Score_oppo_pred)
same_class_train = intersect(train, same_class)
same_class_test = setdiff(same_class, same_class_train)
same_class_test_preds = predict(linear_reg_after_class, newdata = CFB_16_18_df[same_class_test,])
correction_same_class = sign((same_class_test_preds + CFB_16_18_df$vegas_sprd[same_class_test])* 
                               (CFB_16_18_df$vegas_sprd[same_class_test] + CFB_16_18_df$Score_diff[same_class_test]))
length(which(correction_same_class ==1)) 

different_class_preds = predict(linear_reg_after_class, newdata = CFB_16_18_df[setdiff(test, same_class_test), ])
different_class_test = setdiff(test, same_class_test)
correction_different_class = sign ((different_class_preds + CFB_16_18_df$vegas_sprd[different_class_test] )*
                                     (CFB_16_18_df$vegas_sprd[different_class_test] + CFB_16_18_df$Score_diff[different_class_test]))

length(which(correction_different_class == 1))/ length(different_class_test)
as.numeric(unlist((different_class_preds + CFB_16_18_df$vegas_sprd[different_class_test])[1:20]))
as.numeric(unlist( (CFB_16_18_df$vegas_sprd[different_class_test] + CFB_16_18_df$Score_diff[different_class_test])[1:20] ))
#linear regression with only 1 single feature, namely score_diff_pred
linear_reg_only_score_diff = lm(Score_diff~Score_diff_pred, data = CFB_16_18_df[train,])
summary(linear_reg_only_score_diff)

linear_reg_only_score_diff_train = predict(linear_reg_only_score_diff, newdata =  CFB_16_18_df[train,])
mean((linear_reg_only_score_diff_train - CFB_16_18_df$Score_diff[train])^2)
plot(linear_reg_only_score_diff_train, CFB_16_18_df$Score_diff[train])
correction_only_score_diff_train = sign((CFB_16_18_df$vegas_sprd[train] + linear_reg_only_score_diff_train) *
                                    (CFB_16_18_df$vegas_sprd + CFB_16_18_df$Score_diff)[train])

length(which(correction_only_score_diff_train == 1))/length(train)
linear_reg_only_score_diff_test = predict(linear_reg_only_score_diff, newdata = CFB_16_18_df[test,])
correction_only_score_diff_test = sign((CFB_16_18_df$vegas_sprd[test] + linear_reg_only_score_diff_test) *
                                         (CFB_16_18_df$vegas_sprd + CFB_16_18_df$Score_diff)[test])
plot(linear_reg_only_score_diff_test, CFB_16_18_df$Score_diff[test])
length(which(correction_only_score_diff_test == 1))/length(test)
###SVM with class update

svm.class.model = svm(Score_diff~ vegas_sprd + Score_diff_pred, data = CFB_16_18_df[train,])
summary(svm.class.model)
svm.train.preds = predict(svm.class.model, newdata = CFB_16_18_df[train,])
plot(svm.train.preds, CFB_16_18_df$Score_diff[train])
mean((svm.train.preds - CFB_16_18_df$Score_diff[train])^2 )
correction.svm.train = sign((svm.train.preds + CFB_16_18_df$vegas_sprd[train]) *
                              (CFB_16_18_df$vegas_sprd + CFB_16_18_df$Score_diff)[train])
length(which(correction.svm.train == 1))/length(train)
svm.preds = predict(svm.class.model, newdata =  CFB_16_18_df[test,])
plot(svm.preds, CFB_16_18_df$Score_diff[test])
correction.svm.test = sign((CFB_16_18_df$vegas_sprd[test] + svm.preds) *
                             (CFB_16_18_df$vegas_sprd + CFB_16_18_df$Score_diff)[test])
length(which(correction.svm.test == 1))/length(test)

for (cos in 2^(0: 3)){
  for (eps in seq(0.01,0.3, 0.05)){
    for (gam in seq(0.1, 1, 0.3)){
    svm_model = svm(Score_diff~ Score_diff_pred + vegas_sprd, data = CFB_16_18_df[train,], epsilon = eps, cost = cos, gamma =  gam)
    
    svm_preds = predict(svm_model, newdata = CFB_16_18_df[-train,])
    mse = mean((svm_preds- CFB_16_18_df$Score_diff[-train])^2)
    correction_vector = sign((svm_preds + CFB_16_18_df$vegas_sprd[-train])* (CFB_16_18_df$Score_diff[-train] + CFB_16_18_df$vegas_sprd[-train]))
    acc_rate = length(which(correction_vector == 1))/ length(correction_vector)
    
    svm_train_preds = predict(svm_model, newdata = CFB_16_18_df[train,])
    train_mse = mean((svm_train_preds- CFB_16_18_df$Score_diff[train])^2)
    train_correction_vector = sign((svm_train_preds + CFB_16_18_df$vegas_sprd[train])* (CFB_16_18_df$Score_diff[train] + CFB_16_18_df$vegas_sprd[train]))
    train_acc_rate = length(which(train_correction_vector == 1) )/ length(train)
    
    print(c(cos, eps,gam, train_mse, train_acc_rate, mse, acc_rate))
    }
  }
}


#now linear regression without score_diff_pred
linear_reg_with_score_only = lm(Score_diff~, data = CFB_16_18_df[train,])

linear_reg_without_score_diff_pred = lm(Score_diff~.-Season- vegas_sprd, data = CFB_16_18_df[train,])
linear_reg_train= predict(linear_reg_without_score_diff_pred, newdata = CFB_16_18_df[train,])

vif(linear_reg_without_score_diff_pred)
summary(linear_reg_without_score_diff_pred)
test = CFB_16_18_df[-train,]

linear_reg_pred = predict(linear_reg_without_score_diff_pred, newdata = test)
y = test$Score_diff
plot(y, linear_reg_pred)
abline(0, 1, col ="red", lwd = 2)
mean((CFB_16_18_df$Score_diff[-train] - linear_reg_pred)^2)
mean((CFB_16_18_df$Score_diff[-train] + CFB_16_18_df$vegas_sprd[-train])^2)
mean((CFB_16_18_df$Score_diff[train] + CFB_16_18_df$vegas_sprd[train])^2)

library(leaps)
# regfit.full = regsubsets(Score_diff~., CFB_16_18_df[train,], nvmax = 20, really.big = T)
# reg.full.summary = summary(regfit.full)
# reg.full.summary$adjr2
# which.max(reg.full.summary$adjr2)#12 features is maximal here 
# coef(regfit.full, 12)

correction_vector = sign((linear_reg_pred + test[ "vegas_sprd"]) *
       (test$Score_diff + test$vegas_sprd))
length(which(correction_vector == 1))
linear_reg_train = predict(linear_reg_without_score_diff_pred, newdata = CFB_16_18_df[train,])
train_correction_vector = sign((linear_reg_train + CFB_16_18_df[train, 'vegas_sprd'])* 
                                 (CFB_16_18_df$Score_diff[train] + CFB_16_18_df$vegas_sprd[train] ))
length(which(train_correction_vector == 1))/length(train)

same_class = which(CFB_16_18_df$Score_allowed_pred != CFB_16_18_df$Score_oppo_pred)
same_class_train = intersect(train, same_class)
same_class_test = setdiff(same_class, same_class_train)
####Now use Ridge/Lasso on the dataframe without score_diff_pred
x = model.matrix(Score_diff~.-Season- vegas_sprd, CFB_16_18_df)[, -1]
y = CFB_16_18_df$Score_diff
library(glmnet)
grid =  10^seq(10, -2, length = 100)
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, alpha =0, s = bestlam, newx = x[-train,])
mean((ridge.pred - y[-train])^2)


predict(glmnet(x, y, alpha =0), s = bestlam, type = "coefficients")

plot(y[-train],ridge.pred)
abline(0, 1, col = 'red', lwd = 2)
correction_ridge = sign((ridge.pred + CFB_16_18_df[-train, 'vegas_sprd']) * (CFB_16_18_df$Score_diff[-train] + CFB_16_18_df$vegas_sprd[-train]))
length(which(correction_ridge == 1))

####LASSO
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda =grid)
plot(lasso.mod)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
bestlam_lasso = cv.out$lambda.min
bestlam_lasso
lasso_coeffs = predict(glmnet(x, y, alpha = 1), s = bestlam_lasso, type = "coefficients")
lasso_coeffs
lasso_coeffs = lasso_coeffs[lasso_coeffs!= 0]
lasso_coeffs
lasso.pred = predict(lasso.mod,alpha = 1, s = bestlam_lasso, newx = x[-train,])
mean((y[-train]- lasso.pred)^2)
plot(lasso.pred, y[-train])
abline(0, 1, col = 'red', lwd = 2)
correction.lasso = sign((lasso.pred + CFB_16_18_df[-train, 'vegas_sprd']) * (CFB_16_18_df$Score_diff[-train] + CFB_16_18_df$vegas_sprd[-train]))
length(which(correction.lasso == 1))
#PCA

library(pls)
set.seed(2)
pcr.fit = pcr(Score_diff~., data =CFB_16_18_df[train,], scale = T, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit ,val.type="MSEP")

##############################
#now go for classification. Since the we wants to bet on whether the team covers or not, the response variable changes
#y = sign(score_diff + vegas_sprd), when this is positive, we have team A covers the spread.
cover_sprd_numeric = training_df$Score_diff + training_df$vegas_sprd
cover_sprd_sign = sign(cover_sprd_numeric)



##############################

#Tree based regression, see how this might work.

library(tree)
CFB_16_18_df$HomeOrAway [which(CFB_16_18_df$HomeOrAway == "AWAY")] = 1
CFB_16_18_df$HomeOrAway [which(CFB_16_18_df$HomeOrAway == "HOME")] = 0
CFB_16_18_df$HomeOrAway <- as.numeric(CFB_16_18_df$HomeOrAway)

tree.CFB = tree(Score_diff~., data = CFB_16_18_df[train,])
summary(tree.CFB)
plot(tree.CFB)
text(tree.CFB)

cv.CFB= cv.tree(tree.CFB)
plot(cv.CFB$size, cv.CFB$dev, type = "b")
yhat = predict(tree.CFB, newdata = CFB_16_18_df[-train,])
plot(yhat, CFB_16_18_df$Score_diff[-train])
abline(a = 0, b =1 )
mean((yhat - y[-train])^2)

#bagging and RandomForest
library(randomForest)
bag.CFB = randomForest(Score_diff~.-Season , data = CFB_16_18_df[train,], mtry = 53, ntree = 50, importance = T)
bag.CFB
yhat = predict(bag.CFB, newdata =  CFB_16_18_df[-train,])
mean((yhat - y[-train])^2)

set.seed(1)
rf.CFB = randomForest(Score_diff~., data = CFB_16_18_df[train,], mtry = 8, importance = T)

yhat.rf = predict(rf.CFB, newdata =CFB_16_18_df[-train,])
mean((yhat.rf-y[-train])^2)
importance(rf.CFB)
varImpPlot(rf.CFB)

#boosting:
library(gbm)
boost.CFB = gbm(Score_diff~.-Season - vegas_sprd, data=CFB_16_18_df[train,], distribution=
                  "gaussian",n.trees =5000, interaction.depth =3, shrinkage = 0.003)

summary(boost.CFB)
yhat.boost = predict(boost.CFB, newdata = CFB_16_18_df[-train,], n.trees = 5000)
mean((yhat.boost - y[-train])^2)
plot(yhat.boost, y[-train])
abline(0, 1, lwd = 2, col = 'red')
correction.boost = sign((yhat.boost + CFB_16_18_df[-train, 'vegas_sprd']) * (CFB_16_18_df$Score_diff[-train] + CFB_16_18_df$vegas_sprd[-train]))
length(which(correction.boost == 1) )

#################################################SVR
library(e1071)
tuneResult <- tune(svm, Score_diff~.- Season,  data = CFB_16_18_df[train,], 
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9) ) )
print(tuneResult)
plot(tuneResult)
svm_model <- svm(Score_diff~vegas_sprd + log(Score_pred) + log(Score_allowed_pred) + log(Score_allowed_oppo_pred) + log(Score_oppo_pred) + 
                   log(RushingYardsPerAttempt_allowed_oppo_pred) + log(FirstDowns_oppo_pred) + log(YardsPerPlay_allowed_oppo_pred) + 
                   log(YardsPerPlay_pred )+ log(PassingCompletionPercentage_pred) + log(CombineHurriesPlusSacks_pred )+ 
                   log( PassingYardsPerAttempt_allowed_pred)
                  +log(YardsPerPlay_oppo_pred) + log(turnovers_oppo_pred) , data = CFB_16_18_df[train,])

svm_train_preds = predict(svm_model, newdata = CFB_16_18_df[train,])
svm_preds = predict(svm_model, newdata = CFB_16_18_df[-train,])
plot(svm_train_preds, CFB_16_18_df$Score_diff[train])

plot(svm_preds, CFB_16_18_df$Score_diff[-train])
abline(0,1, col ='red', lwd = 2)
mean((svm_train_preds - CFB_16_18_df$Score_diff[train])^2)
train_svm_correction_vector = sign((CFB_16_18_df$Score_diff[train] + CFB_16_18_df$vegas_sprd[train]) * (CFB_16_18_df$vegas_sprd[train] + svm_train_preds))
length(which(train_svm_correction_vector == 1))
svm_correction_vector = sign((CFB_16_18_df$Score_diff[-train] + CFB_16_18_df$vegas_sprd[-train])* (CFB_16_18_df$vegas_sprd[-train] + svm_preds))
length(which(svm_correction_vector == 1))

#################################################################experimenting using log of features

feature_names = names(CFB_16_18_df)[3:54]
names_log = vector()
for (name in feature_names){
  name_with_log = paste(name,"_log", sep = "")
  names_log = c(names_log, name_with_log)
}
for (i in (1:52)){
  name = feature_names[i]
  name_with_log = names_log[i]
  CFB_16_18_df[name_with_log] = log(CFB_16_18_df[name])
}

CFB_16_18_df$signed_score_diff_log = rep(0, nrow(CFB_16_18_df))

take_signed_log <- function(datalist  ){
  log_signed_datalist = vector()
  if (i%%200 == 0){print(i)}
  for (i in c(1:length(datalist))){
    
    if (datalist[i] == 0) {
      log_signed_datalist = c(log_signed_datalist, 0)
    }
    else{
      log_signed_datalist = c(log_signed_datalist, sign(datalist[i])* log(abs(datalist[i])) )
    }
  }
  #print(log_signed_datalist)
  return (log_signed_datalist)
}

score_diff_vector = CFB_16_18_df$Score_diff
v= vector()
signed_log_score_diff = take_signed_log(score_diff_vector )
CFB_16_18_df$signed_score_diff_log = signed_log_score_diff

signed_vegas_sprd = take_signed_log(CFB_16_18_df$vegas_sprd)
CFB_16_18_df$signed_vegas_sprd_log = signed_vegas_sprd

CFB_log = CFB_16_18_df[, c(1,2,55: 107)]
CFB_log = CFB_log[complete.cases(CFB_log),]
CFB_log <- CFB_log[is.finite(rowSums(CFB_log)),]
train = which(CFB_log$Season == 2016 | CFB_log$Season == 2017) 



for (cos in 2^(0: 3)){
  for (eps in seq(0.05,1, 0.1)){
    svm_model = svm(Score_diff~.-Season- vegas_sprd, data = CFB_log[train,], epsilon = eps, cost = cos )
    
    svm_preds = predict(svm_model, newdata = CFB_log[-train,])
    mse = mean((svm_preds- CFB_log$Score_diff[-train])^2)
    correction_vector = sign((svm_preds + CFB_log$vegas_sprd[-train])* (CFB_log$Score_diff[-train] + CFB_log$vegas_sprd[-train]))
    acc_rate = length(which(correction_vector == 1))/ length(correction_vector)
    
    svm_train_preds = predict(svm_model, newdata = CFB_log[train,])
    train_mse = mean((svm_train_preds- CFB_log$Score_diff[train])^2)
    train_correction_vector = sign((svm_train_preds + CFB_log$vegas_sprd[train])* (CFB_log$Score_diff[train] + CFB_log$vegas_sprd[train]))
    train_acc_rate = length(which(train_correction_vector == 1) )/ length(train)
    
    print(c(eps, cos, train_mse, train_acc_rate, mse, acc_rate, svm_preds[1]))
  }
}

svm_model = svm(Score_diff~.-Season - vegas_sprd, data = CFB_log[train,])
svm_train_preds = predict(svm_model, newdata = CFB_log[train,])
plot(svm_train_preds, CFB_log$Score_diff[train])
mean((svm_train_preds- CFB_log$Score_diff[train])^2)
train_correction_vector = sign((svm_train_preds + CFB_log$vegas_sprd[train])* (CFB_log$Score_diff[train] + CFB_log$vegas_sprd[train]))
length(which(train_correction_vector == 1) )/ length(train)
svm_log_preds = predict(svm_model, newdata = CFB_log[-train,])
plot(svm_log_preds, CFB_log$Score_diff[-train])
mean((svm_log_preds- CFB_log$Score_diff[-train])^2)
correction_vector = sign((svm_log_preds + CFB_log$vegas_sprd[-train])* (CFB_log$Score_diff[-train] + CFB_log$vegas_sprd[-train]))
length(which(correction_vector == 1))/ length(correction_vector)





#Linear Regression using log data


linear_reg_log = lm(signed_score_diff_log ~.-Season, data = CFB_log[train,] )
summary(linear_reg_log)
linear_log_train_pred = predict(linear_reg_log, newdata = CFB_log[train,])
plot(CFB_log$signed_score_diff_log[train], linear_log_train_pred)
abline(0,1, col= "red", lwd = 2)
mean((linear_log_train_pred - CFB_log$signed_score_diff_log[train])^2)
linear_reg_log_pred = predict(linear_reg_log, newdata = CFB_log[-train,])
plot(linear_reg_log_pred, CFB_log$signed_score_diff_log[-train])
after_exponential = exp(linear_reg_log_pred)


x = model.matrix(signed_score_diff_log~.-Season, CFB_log)[, -1]
y = CFB_log$signed_score_diff_log
library(glmnet)
grid =  10^seq(10, -2, length = 100)
ridge.log.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.log.pred = predict(ridge.log.mod, alpha =0, s = bestlam, newx = x[-train,])
mean((ridge.log.pred - y[-train])^2)
plot(ridge.log.pred, CFB_log$signed_score_diff_log[-train])
ridge.after.exponential = exp(ridge.log.pred)
ridge.after.exponential[1:20]


#####################SVM with log of score_diff and vegas_sprd, not good

svm.log.model = svm(signed_score_diff_log~.-Season, data = CFB_log[train,])
svm.log.train.pred = predict(svm.log.model, newdata = CFB_log[train,])
mean((svm.log.train.pred- CFB_log$signed_score_diff_log[train])^2)
svm.train.after.exponential = exp(svm.log.train.pred)
svm.log.pred = predict(svm.log.pred, newdata = CFB_log[-train,])












#######################################################################################################################################

#Final code for predicting spread:
library(dplyr)
library(rlist)
start=Sys.time()

curr_dir = "C:/Users/Zhongyi Zhang/QS Dropbox/Alex Zhongyi Zhang/Alex-summer/CFB/data/16-18/" #getwd()

setwd(curr_dir)
list.files(curr_dir)

regular_season_16_18 = read.csv("home_all_valid_regular_season_with_vegas_info.csv", stringsAsFactors = F)
regular_season_16_to_18<- regular_season_16_18[,-1]

#regular_season_16_18$DateTime =as.POSIXct(regular_season_16_18$DateTime,format="%m/%d/%Y", tz = Sys.timezone())

#regular_season_16_18 = regular_season_16_18[order(as.Date(regular_season_16_18$DateTime, format="%d/%m/%Y")),]
home_regular_season_16_18= regular_season_16_18[which(regular_season_16_18$HomeOrAway == "HOME"),]
home_regular_season_16_18$YPP_diff = home_regular_season_16_18$YardsPerPlay - home_regular_season_16_18$YardsPerPlay_allowed
division1_teams = readRDS('division1_ teams')
# division_1_teams = division1_teams



################for first half of season###not very successful
second_to_last_season = current_season - 2

score_diff_list_previous_3_half_seasons = list()
for(team in division_1_teams){
  home_team_indices = intersect(union(which(home_regular_season_16_18$Season == Last_season), 
                                      which(home_regular_season_16_18$Season == second_to_last_season & home_regular_season_16_18$Week >= 8) ),
                                which(home_regular_season_16_18$Team == team))
  
  away_team_indices= intersect(union(which(home_regular_season_16_18$Season == Last_season),
                                     which(home_regular_season_16_18$Season == second_to_last_season & home_regular_season_16_18$Week >=8) ),
                               which(home_regular_season_16_18$Opponent == team))
  score_diff_list_previous_3_half_seasons[team] = (sum (home_regular_season_16_18$Score_diff[home_team_indices]) - 
                                                     sum(home_regular_season_16_18$Score_diff[away_team_indices]))/
    (length(home_team_indices) + length(away_team_indices))
}
score_diff_list_previous_3_half_seasons <- score_diff_list_previous_3_half_seasons[order(-unlist(score_diff_list_previous_3_half_seasons))]
saveRDS(score_diff_list_previous_3_half_seasons, file = 'score_diff_list second half 16& whole 17')
team.rank.list.prev.3.half.seasons = list()
for (team in division_1_teams){
  team.rank.list.prev.3.half.seasons[team] = which(names(score_diff_list_previous_3_half_seasons) == team)
}
saveRDS(team.rank.list.prev.3.half.seasons, file = 'team.rank.16.second.half.season&whole.17.season')


##################################################after merging with vegas_sprd and vegas line#######################################

home_with_vegas = read.csv("home_all_valid_regular_season_with_vegas_info.csv", stringsAsFactors = F)

home_with_vegas<- home_with_vegas[,-1]

# CFB_16_18_df = home_with_vegas[, c("Season",'Week','Team','Opponent',
#                                    'Score','Score_diff', 'Score_allowed',
#                                    "Score_pred", 'Score_diff_pred','Score_allowed_pred',
#                                    'YardsPerPlay', 'YardsPerPlay_allowed', "YardsPerPlay_diff",
#                                    "YardsPerPlay_pred","YardsPerPlay_allowed", "YardsPerPlay_diff_pred",
#                                    'vegas_sprd' )]

#CFB_16_18_df = home_with_vegas[, c("Season",'Week','Team','Opponent','Score','Score_diff', 'Score_allowed',
#                                  'YardsPerPlay', 'YardsPerPlay_allowed', 'vegas_sprd' )]

CFB_16_18_df = home_with_vegas[,c(1:35, 94, 95)]
division_1_games.with.vegas.sprd = CFB_16_18_df[which(CFB_16_18_df$Opponent %in% division1_teams & CFB_16_18_df$Team %in% division1_teams),]
# division_1_games.with.vegas.sprd$total_Yards = division_1_games.with.vegas.sprd$PassingAttempts * division_1_games.with.vegas.sprd$PassingYardsPerAttempt +
#   division_1_games.with.vegas.sprd$RushingAttempts * division_1_games.with.vegas.sprd$RushingYardsPerAttempt
# 
# division_1_games.with.vegas.sprd$total_Yards_allowed = division_1_games.with.vegas.sprd$PassingAttempts_allowed * division_1_games.with.vegas.sprd$PassingYardsPerAttempt_allowed+
#                                            division_1_games.with.vegas.sprd$RushingAttempts_allowed * division_1_games.with.vegas.sprd$RushingYardsPerAttempt_allowed
# division_1_games.with.vegas.sprd$YPP_diff = division_1_games.with.vegas.sprd$YardsPerPlay - division_1_games.with.vegas.sprd$YardsPerPlay_allowed


#Now add momentum feature of division1 teams:
Adding_momentum <- function(division_1_games.with.vegas.sprd, division1_teams){
division_1_games.with.vegas.sprd$home_recent_4_game_mean = rep(0, nrow(division_1_games.with.vegas.sprd))
division_1_games.with.vegas.sprd$away_recent_4_game_mean = rep(0, nrow(division_1_games.with.vegas.sprd))
# division_1_games.with.vegas.sprd$YPP_home_recent_4_game_mean =  rep(0, nrow(division_1_games.with.vegas.sprd))
# division_1_games.with.vegas.sprd$YPP_away_recent_4_game_mean =  rep(0, nrow(division_1_games.with.vegas.sprd))
division_1_games.with.vegas.sprd$not_first_4_games = rep(0, nrow(division_1_games.with.vegas.sprd))
# division_1_games.with.vegas.sprd$home_wins_last_4_games = rep(0, nrow(division_1_games.with.vegas.sprd))
# division_1_games.with.vegas.sprd$away_wins_last_4_games = rep(0, nrow(division_1_games.with.vegas.sprd))
Seasons = unique(division_1_games.with.vegas.sprd$Season)
not_first_4_games = vector()

for (team in division1_teams){
  
  for (season in Seasons){
    team_df = division_1_games.with.vegas.sprd[which( (division_1_games.with.vegas.sprd$Team == team | division_1_games.with.vegas.sprd$Opponent == team) &
                                                        (division_1_games.with.vegas.sprd$Season == season) ),]
    recent_4_games_score_diff = vector()
    #recent_4_games_score_diff_mean_by_team = vector()
    #recent_4_games_YPP_diff = vector()
    recen_4_game_wins = vector()
    if( nrow(team_df) != 0){
      for (i in (1: nrow(team_df))){
        
        if (team_df$Opponent[i] == team){
          current_game_team_score_diff = team_df$Score_diff[i] * -1
          #current_game_YPP_diff = team_df$YPP_diff[i]*-1
        }
        else{
          current_game_team_score_diff = team_df$Score_diff[i] - 4.3
          #current_game_YPP_diff = team_df$YPP_diff[i] - 0.4
        }
       # team_wins_current_game <- if (current_game_team_score_diff > 0) 1 else 0
        
        
        if (i <= 4){
          #recent_4_games_score_diff_mean_by_team <- c(recent_4_games_score_diff_mean_by_team, mean(recent_4_games_score_diff))
          score_diff_previous_4_games_mean = mean(recent_4_games_score_diff)
          recent_4_games_score_diff <- c(recent_4_games_score_diff, current_game_team_score_diff)
         # YPP_diff_prev_4_games_mean = mean(recent_4_games_YPP_diff)
          #recent_4_games_YPP_diff<- c(recent_4_games_YPP_diff, current_game_YPP_diff)
          #number_of_wins_previous_4_games = sum(recen_4_game_wins)
         # recen_4_game_wins = c(recen_4_game_wins, team_wins_current_game)
          
          j = which((division_1_games.with.vegas.sprd$Team == team | division_1_games.with.vegas.sprd$Opponent == team) & division_1_games.with.vegas.sprd$DateTime == team_df$DateTime[i])
          if (division_1_games.with.vegas.sprd$Team[j] == team){
            division_1_games.with.vegas.sprd$home_recent_4_game_mean[j] = score_diff_previous_4_games_mean + 4.3
            #division_1_games.with.vegas.sprd$YPP_home_recent_4_game_mean[j] = YPP_diff_prev_4_games_mean
            #division_1_games.with.vegas.sprd$home_wins_last_4_games[j] = number_of_wins_previous_4_games
          }else{
            division_1_games.with.vegas.sprd$away_recent_4_game_mean[j] = score_diff_previous_4_games_mean
           # division_1_games.with.vegas.sprd$away_wins_last_4_games[j] = number_of_wins_previous_4_games
           # division_1_games.with.vegas.sprd$YPP_away_recent_4_game_mean[j] = YPP_diff_prev_4_games_mean
          }
          
          }
        else{
          score_diff_previous_4_games_mean = mean(recent_4_games_score_diff)
          #recent_4_games_score_diff_mean_by_team <- c(recent_4_games_score_diff_mean_by_team, score_diff_previous_4_games_mean)
          recent_4_games_score_diff<-c(recent_4_games_score_diff[2:4], current_game_team_score_diff)
         # YPP_diff_prev_4_games_mean = mean(recent_4_games_YPP_diff)
         # recent_4_games_YPP_diff <- c(recent_4_games_YPP_diff[2:4], current_game_YPP_diff)
         # number_of_wins = number_of_wins_previous_4_games = sum(recen_4_game_wins)
        #  recen_4_game_wins = c(recen_4_game_wins[2:4], team_wins_current_game)
          
          j = which((division_1_games.with.vegas.sprd$Team == team | division_1_games.with.vegas.sprd$Opponent == team) & division_1_games.with.vegas.sprd$DateTime == team_df$DateTime[i])
          not_first_4_games <- c(not_first_4_games, j)
          if (division_1_games.with.vegas.sprd$Team[j] == team){
            division_1_games.with.vegas.sprd$home_recent_4_game_mean[j] = score_diff_previous_4_games_mean  + 4.3
            #division_1_games.with.vegas.sprd$YPP_home_recent_4_game_mean[j] = YPP_diff_prev_4_games_mean
            #division_1_games.with.vegas.sprd$home_wins_last_4_games[j] = number_of_wins_previous_4_games
          }else{
            division_1_games.with.vegas.sprd$away_recent_4_game_mean[j] = score_diff_previous_4_games_mean
            #division_1_games.with.vegas.sprd$YPP_away_recent_4_game_mean[j] = YPP_diff_prev_4_games_mean
            #division_1_games.with.vegas.sprd$away_wins_last_4_games[j] = number_of_wins_previous_4_games
            }
        }
      }
    }
  }
}
division_1_games.with.vegas.sprd$not_first_4_games[not_first_4_games[duplicated(not_first_4_games)]] = 1

division_1_games.with.vegas.sprd
}

division_1_games.with.vegas.sprd<- Adding_momentum(division_1_games.with.vegas.sprd, division1_teams)




get_games_not_first_4_in_season <- function(division_1_games.with.vegas.sprd){
not_first_4_games_division_1 = which(division_1_games.with.vegas.sprd$not_first_4_games == 1)
division_1_games.with.vegas.sprd.valid = division_1_games.with.vegas.sprd[not_first_4_games_division_1,]
division_1_games.with.vegas.sprd.valid <- division_1_games.with.vegas.sprd.valid[complete.cases(division_1_games.with.vegas.sprd.valid),]
division_1_games.with.vegas.sprd.valid$recent_4_games_diff = division_1_games.with.vegas.sprd.valid$home_recent_4_game_mean - 
  division_1_games.with.vegas.sprd.valid$away_recent_4_game_mean 
  
# division_1_games.with.vegas.sprd$recent_4_YPP_diff = division_1_games.with.vegas.sprd$YPP_home_recent_4_game_mean - division_1_games.with.vegas.sprd$YPP_away_recent_4_game_mean +
#   rep(0.4,nrow(division_1_games.with.vegas.sprd))

division_1_games.with.vegas.sprd.valid
}
#division_1_games.with.vegas.sprd <- subset(division_1_games.with.vegas.sprd, select = -recent_4_game_mean)
#division_1_games.with.vegas.sprd.valid = get_games_not_first_4_in_season(division_1_games.with.vegas.sprd)

# division_1_games.with.vegas.sprd$vegas_coverage = sign(division_1_games.with.vegas.sprd$Score_diff + division_1_games.with.vegas.sprd$vegas_sprd)
# vegas_coverage_0_1_vec = pmax(rep(0, nrow(division_1_games.with.vegas.sprd)), division_1_games.with.vegas.sprd$vegas_coverage)
# division_1_games.with.vegas.sprd$vegas_coverage_0_1= vegas_coverage_0_1_vec
# write.csv(division_1_games.with.vegas.sprd, 'division_1_game_info_with_vegas.csv',row.names = F )
# write.csv(game_df_sprd_for_2nd_half, "games_later_half_16_and_whole_17.csv", row.names = F)



boost.cfb = gbm(Score_diff~.-Score-Score_allowed-Season-Week-DateTime-Team-Opponent-HomeOrAway-FantasyPoints - OpponentFantasyPoints, data = division_1_games.with.vegas.sprd, distribution = 'laplace', n.trees = 1000, interaction.depth = 3, shrinkage = 0.005)
summary(boost.cfb)
#####################################################################################################################################
#updating based on the previous weeks' performance
#input division 1 games with vegas spread info and momentum
#output is same dataframe with added simple average of previous games in season and rankings each week starting from week 5
#also give predictions of spread based on momentum and accumulated simple average difference
update_games_with_simple_average_of_previous_games_in_season <- function(division_1_games.with.vegas.sprd, division1_teams){
  
  division_1_games.with.vegas.sprd$home_score_diff_pred_on_prev_weeks = rep(0, nrow(division_1_games.with.vegas.sprd))
  division_1_games.with.vegas.sprd$away_score_diff_pred_on_prev_weeks = rep(0, nrow(division_1_games.with.vegas.sprd))
  division_1_games.with.vegas.sprd$home_score_diff_rank_on_prev_weeks = rep(0, nrow(division_1_games.with.vegas.sprd))
  division_1_games.with.vegas.sprd$away_score_diff_rank_on_prev_weeks = rep(0, nrow(division_1_games.with.vegas.sprd))
  
  Seasons = unique(division_1_games.with.vegas.sprd$Season)
  for (current_season in Seasons) {
  #want to see the second half season's predictions, use the first half of this season and the second half of last season
      Last_season = current_season - 1
      
      # score_diff_list_previous_two_half_seasons = list()
      # YPP_diff_List_first_half_season = list()
      score_diff_list_first_i_weeks = list()
      
      Score_diff_by_week = data.frame(matrix(0, nrow = 131, ncol = 14))
      Score_diff_rank_by_week = data.frame((matrix(0, nrow = 131, ncol = 14)))
      row.names(Score_diff_by_week) <- division_1_teams
      row.names(Score_diff_rank_by_week) <- division_1_teams
      for (week in (5:14)){
        for (team in division1_teams){
          #home_team_indices = union(which(home_regular_season_16_18$Team == team & home_regular_season_16_18$Season == Last_season & home_regular_season_16_18$Week >= 8),
          #                          which(home_regular_season_16_18$Team == team & home_regular_season_16_18$Season == current_season & home_regular_season_16_18$Week <= 7))
          #away_team_indices = union(which(home_regular_season_16_18$Opponent == team & home_regular_season_16_18$Season == Last_season & home_regular_season_16_18$Week >= 8),
          #                         which(home_regular_season_16_18$Opponent == team & home_regular_season_16_18$Season == current_season & home_regular_season_16_18$Week <= 7) )
          # home_team_indices = which(division_1_games.with.vegas.sprd$Team == team & division_1_games.with.vegas.sprd$Season == current_season & division_1_games.with.vegas.sprd$Week < week)
          # away_team_indices = which(division_1_games.with.vegas.sprd$Opponent == team &division_1_games.with.vegas.sprd$Season == current_season & division_1_games.with.vegas.sprd$Week < week)
          home_team_df = filter(division_1_games.with.vegas.sprd, (Team == team) & (Season == current_season) & (Week < week))
          away_team_df = filter(division_1_games.with.vegas.sprd, (Opponent == team) & (Season == current_season) & (Week < week))
          # score_diff_list_previous_two_half_seasons[team] = (sum(home_regular_season_16_18$Score_diff[home_team_indices] - rep(4.3, length(home_team_indices))) -
          #                                                      sum(home_regular_season_16_18$Score_diff[away_team_indices]) ) /
          #   (length(home_team_indices) + length(away_team_indices) )
          # YPP_diff_List_first_half_season [team] = (sum(home_regular_season_16_18$YPP_diff[home_team_indices] - rep(0.4, length( home_team_indices))) -
          #                                             sum(home_regular_season_16_18$YPP_diff[away_team_indices]) )/
          #   (length(home_team_indices) + length(away_team_indices) )
      
          score_diff_list_first_i_weeks[team] = (sum(home_team_df$Score_diff) - 4.3* nrow(home_team_df) -
                                                               sum(away_team_df$Score_diff ) ) /
            (nrow(home_team_df) + nrow(away_team_df) )
          Score_diff_by_week[team, week] = score_diff_list_first_i_weeks[team]
        }
        score_diff_list_first_i_weeks <- score_diff_list_first_i_weeks[order(-unlist(score_diff_list_first_i_weeks))]
        #score_diff_list_previous_two_half_seasons <- score_diff_list_previous_two_half_seasons[order(-unlist(score_diff_list_previous_two_half_seasons))]
        #YPP_diff_List_first_half_season <- YPP_diff_List_first_half_season[order(-unlist(YPP_diff_List_first_half_season))]
        
        
        team.rank.list.first.i.weeks = list()
        for (team in division_1_teams){
          team.rank.list.first.i.weeks[team] = which(names(score_diff_list_first_i_weeks) == team)
        }
        Score_diff_rank_by_week[, week] = unlist(team.rank.list.first.i.weeks)
        
        # team.rank.list.previous.two.half.seasons = list()
        # for (team in division_1_teams){
        #   team.rank.list.previous.two.half.seasons[team] = which(names(score_diff_list_previous_two_half_seasons) == team)
        # }
        # team.rank.list.previous.two.half.seasons<- team.rank.list.previous.two.half.seasons[order(unlist(team.rank.list.previous.two.half.seasons))]
        # 
      }
      
      #game_df_sprd_for_2nd_half = filter(division_1_games.with.vegas.sprd, Season == current_season)

      for (week in c(5:14)){
        for (team in division1_teams){
          index = which((division_1_games.with.vegas.sprd$Team == team) & (division_1_games.with.vegas.sprd$Week == week) &
                          (division_1_games.with.vegas.sprd$Season == current_season) )
          if (length(index) == 1){
            division_1_games.with.vegas.sprd$home_score_diff_pred_on_prev_weeks[index] = Score_diff_by_week[team, week] + 4.3
            division_1_games.with.vegas.sprd$home_score_diff_rank_on_prev_weeks[index] = Score_diff_rank_by_week[team, week]
          }
          away_index = which(division_1_games.with.vegas.sprd$Opponent == team & division_1_games.with.vegas.sprd$Week == week
                             & division_1_games.with.vegas.sprd$Season == current_season)
          if (length(away_index == 1)){
            division_1_games.with.vegas.sprd$away_score_diff_pred_on_prev_weeks[away_index] = Score_diff_by_week[team, week]
            division_1_games.with.vegas.sprd$away_score_diff_rank_on_prev_weeks[away_index] = Score_diff_rank_by_week[team, week]
          }
        }
        
      }
  
  }
  division_1_games.with.vegas.sprd$score_diff_pred_on_prev_weeks = division_1_games.with.vegas.sprd$home_score_diff_pred_on_prev_weeks -
    division_1_games.with.vegas.sprd$away_score_diff_pred_on_prev_weeks
  division_1_games.with.vegas.sprd$recent_4_games_diff = division_1_games.with.vegas.sprd$home_recent_4_game_mean -
    division_1_games.with.vegas.sprd$away_recent_4_game_mean
  division_1_games.with.vegas.sprd
}

division_1_games.with.vegas.sprd <- update_games_with_simple_average_of_previous_games_in_season(division_1_games.with.vegas.sprd, division1_teams )


division_1_games.with.vegas.sprd.valid <- filter(division_1_games.with.vegas.sprd, not_first_4_games == 1)
division_1_games.with.vegas.sprd.valid <- get_games_not_first_4_in_season(division_1_games.with.vegas.sprd)

# saveRDS(team.rank.list.previous.two.half.seasons, file = 'team.rank.second.half.16&first.half.17')
# saveRDS(team.rank.list.previous.two.half.seasons, file = 'team.rank.second.half.17&first.half.18')
# team.rank.list.16_second.half.and.17.first.half = readRDS('team.rank.second.half.16&first.half.17')



#game_df_sprd_for_2nd_half = filter(division_1_games.with.vegas.sprd, (Season == current_season)|(Season == Last_season & Week >= 8))
#game_df_sprd_for_2nd_half = filter(division_1_games.with.vegas.sprd, Season == current_season)
  
Momentum_and_accumulate_on_different_side_of_vegas = 
  which((division_1_games.with.vegas.sprd.valid$score_diff_pred_on_prev_weeks + division_1_games.with.vegas.sprd.valid$vegas_sprd)* 
     (division_1_games.with.vegas.sprd.valid$vegas_sprd + division_1_games.with.vegas.sprd.valid$recent_4_games_diff) >=0 )

length(which(abs(division_1_games.with.vegas.sprd.valid$recent_4_games_diff - division_1_games.with.vegas.sprd.valid$score_diff_pred_on_prev_weeks) <= 3) )
summary(abs(division_1_games.with.vegas.sprd.valid$recent_4_games_diff + division_1_games.with.vegas.sprd.valid$vegas_sprd))
summary(abs(division_1_games.with.vegas.sprd.valid$score_diff_pred_on_prev_weeks + division_1_games.with.vegas.sprd.valid$vegas_sprd))
summary(abs(division_1_games.with.vegas.sprd.valid$recent_4_games_diff - division_1_games.with.vegas.sprd.valid$Score_diff))
summary(abs(division_1_games.with.vegas.sprd.valid$score_diff_pred_on_prev_weeks - division_1_games.with.vegas.sprd.valid$Score_diff))
summary(abs(division_1_games.with.vegas.sprd.valid$weighted_score_diff_pred + division_1_games.with.vegas.sprd.valid$vegas_sprd))

for(alpha in seq(0.1, 0.3, 0.02)){
  score_diff_alpha = alpha *division_1_games.with.vegas.sprd.valid$recent_4_games_diff + (1-alpha)* division_1_games.with.vegas.sprd.valid$score_diff_pred_on_prev_weeks
  correction_vector_alpha = (division_1_games.with.vegas.sprd.valid$vegas_sprd + division_1_games.with.vegas.sprd.valid$Score_diff)*
    (division_1_games.with.vegas.sprd.valid$vegas_sprd + 
      score_diff_alpha )
  print(c(alpha, length(which(correction_vector_alpha >= 0)), mean(abs(division_1_games.with.vegas.sprd.valid$Score_diff - score_diff_alpha)), 
          mean(abs(division_1_games.with.vegas.sprd.valid$vegas_sprd + score_diff_alpha)) ))
}

#give a weighted sum of momentum(last 4 games) and simple average of previous games in the current season: weighted_score_diff_pred
#the weight of simple average is 0.8 while the momentum is 0.2
#also calculate the absolute value of the difference between vegas spread and our weighted sum prediction:
#the name is absolute_diff_between_vegas_and_average
Add_weighted_average <- function(division_1_games.with.vegas.sprd.valid){
division_1_games.with.vegas.sprd.valid$weighted_score_diff_pred = 
  0.2 * division_1_games.with.vegas.sprd.valid$recent_4_games_diff + 0.8 * division_1_games.with.vegas.sprd.valid$score_diff_pred_on_prev_weeks

division_1_games.with.vegas.sprd.valid$diff_between_vegas_and_average = 
  division_1_games.with.vegas.sprd.valid$vegas_sprd + 
  (division_1_games.with.vegas.sprd.valid$score_diff_pred_on_prev_weeks * 0.8 + division_1_games.with.vegas.sprd.valid$recent_4_games_diff * 0.2)

division_1_games.with.vegas.sprd.valid$absolute_diff_between_vegas_and_average = abs(division_1_games.with.vegas.sprd.valid$diff_between_vegas_and_average)
division_1_games.with.vegas.sprd.valid$absolute_diff_between_vegas_and_momentum = 
  abs(division_1_games.with.vegas.sprd.valid$recent_4_games_diff + division_1_games.with.vegas.sprd.valid$vegas_sprd)

division_1_games.with.vegas.sprd.valid
}
division_1_games.with.vegas.sprd.valid <- Add_weighted_average(division_1_games.with.vegas.sprd.valid)

boosting.laplace.on.prev.average = gbm(Score_diff~ vegas_sprd + recent_4_games_diff + absolute_diff_between_vegas_and_average, 
                                       data = filter(division_1_games.with.vegas.sprd.valid, Season == 2016 | Season == 2017), n.trees = 5000, distribution = 'laplace', interaction.depth = 3,
                                       shrinkage = 0.003,  cv.folds = 5,n.cores = 1)
summary(boosting.laplace.on.prev.average)
games_2017.valid = filter(division_1_games.with.vegas.sprd.valid, Season == 2017)
games_2018.valid = filter(division_1_games.with.vegas.sprd.valid, Season == 2018)
get_correction(games_2018.valid, boosting.laplace.on.prev.average, -games_2018.valid$vegas_sprd, games_2018.valid$Score_diff, 1)



get_3rd_and_4th_game <- function(division_1_games.with.vegas.sprd, division1_teams){
  game_indices_involves_third_or_4th_game_of_team = vector()
  game_indices_4th_game_of_team = vector()
  Seasons = unique(division_1_games.with.vegas.sprd$Season)
  for (current_season in Seasons){
    
    for (team in division1_teams){
      team_game_indices = which((division_1_games.with.vegas.sprd$Team == team | division_1_games.with.vegas.sprd$Opponent == team) &
                                  division_1_games.with.vegas.sprd$Season ==current_season)
      team_game_indices = sort(team_game_indices)
      game_indices_involves_third_or_4th_game_of_team <- c(game_indices_involves_third_or_4th_game_of_team, team_game_indices[3:4])
      game_indices_4th_game_of_team <- c(game_indices_4th_game_of_team, team_game_indices[4])
    }
  }
  game_indices_involves_third_or_4th_game_of_team_for_both = game_indices_involves_third_or_4th_game_of_team[duplicated(game_indices_involves_third_or_4th_game_of_team)]
  game_indices_involves_third_or_4th_for_single_while_5th_for_the_other = setdiff(unique(game_indices_4th_game_of_team), game_indices_involves_third_or_4th_game_of_team_for_both)
  division_1_games.with.vegas.sprd.3rd.or.4th = division_1_games.with.vegas.sprd[union(game_indices_involves_third_or_4th_game_of_team_for_both,game_indices_involves_third_or_4th_for_single_while_5th_for_the_other),]
  division_1_games.with.vegas.sprd.3rd.or.4th <- division_1_games.with.vegas.sprd.3rd.or.4th[complete.cases(division_1_games.with.vegas.sprd.3rd.or.4th),]
  division_1_games.with.vegas.sprd.3rd.or.4th$absolute_diff_between_vegas_and_average = 
    abs(division_1_games.with.vegas.sprd.3rd.or.4th$vegas_sprd + division_1_games.with.vegas.sprd.3rd.or.4th$recent_4_games_diff)
  division_1_games.with.vegas.sprd.3rd.or.4th
}
division_1_games.with.vegas.sprd.3rd.or.4th <- get_3rd_and_4th_game(division_1_games.with.vegas.sprd, division1_teams)

games_2018.3rd_or_4th = filter(division_1_games.with.vegas.sprd.3rd.or.4th, Season == 2018)

boosting.laplace.on.3rd.or.4th = gbm(Score_diff~ vegas_sprd + absolute_diff_between_vegas_and_average+ recent_4_games_diff, 
                                     data = filter(division_1_games.with.vegas.sprd.3rd.or.4th, Season == 2016 | Season == 2017), n.trees = 5000,
                                     distribution = 'laplace', interaction.depth = 3,
                                    shrinkage = 0.003,  cv.folds = 5,n.cores = 1)


get_correction(division_1_games.with.vegas.sprd.3rd.or.4th, boosting.laplace.on.prev.average, -division_1_games.with.vegas.sprd.3rd.or.4th$vegas_sprd, division_1_games.with.vegas.sprd.3rd.or.4th$Score_diff, 1)
get_correction(games_2018.3rd_or_4th, boosting.laplace.on.prev.average, -games_2018.3rd_or_4th$vegas_sprd, games_2018.3rd_or_4th$Score_diff,1)


get_1st_and_2nd_game <- function(division_1_games.with.vegas.sprd, division1_teams){
  team_game_indices_for_first_2_games = vector()
  Seasons = unique(division_1_games.with.vegas.sprd$Season)
  for (current_season in Seasons){
    for (team in division1_teams){
      team_game_indices = which((division_1_games.with.vegas.sprd$Team == team | division_1_games.with.vegas.sprd$Opponent == team) & 
                                  division_1_games.with.vegas.sprd$Season == current_season)
      team_game_indices <- sort(team_game_indices)
      team_game_indices_for_first_2_games = c( team_game_indices_for_first_2_games, team_game_indices[1:2] )
    }
  }
  team_game_indices_for_first_2_games_unique = sort(unique(team_game_indices_for_first_2_games))
  division_1_games.with.vegas.sprd.first.2.games = division_1_games.with.vegas.sprd[team_game_indices_for_first_2_games_unique,]
  
  
  division_1_games.with.vegas.sprd.first.2.games
}

division_1_games.with.vegas.sprd.first.2.games <- get_1st_and_2nd_game(division_1_games.with.vegas.sprd , division1_teams)
# train_larger = which( ((game_df_sprd_for_2nd_half$Season == Last_season & game_df_sprd_for_2nd_half$Week >= 8)  |
#               (game_df_sprd_for_2nd_half$Season == current_season & game_df_sprd_for_2nd_half$Week <= 7)) &
#                 abs(game_df_sprd_for_2nd_half$vegas_sprd) >= 10)
# train_small = which( ((game_df_sprd_for_2nd_half$Season == Last_season & game_df_sprd_for_2nd_half$Week >= 8)  |
#                          (game_df_sprd_for_2nd_half$Season == current_season & game_df_sprd_for_2nd_half$Week <= 7)) &
#                         abs(game_df_sprd_for_2nd_half$vegas_sprd) < 10)
# #train = which((game_df_sprd_for_2nd_half$Season == current_season & game_df_sprd_for_2nd_half$Week <= 7))
# test_larger = which(game_df_sprd_for_2nd_half$Season == current_season & game_df_sprd_for_2nd_half$Week >= 8 & abs(game_df_sprd_for_2nd_half$vegas_sprd) >= 10 )
# test_small = which(game_df_sprd_for_2nd_half$Season == current_season & game_df_sprd_for_2nd_half$Week >= 8 & abs(game_df_sprd_for_2nd_half$vegas_sprd) < 10)

#home_team_rank_vect.previous.two.half.season = vector()
#away_team_rank_vect.previous.two.half.season = vector()
# home_team_score_diff_1st_half_season = vector()
# away_team_score_diff_1st_half_season = vector()
# home_team_YPP_diff_1st_half_season = vector()
# away_team_YPP_diff_1st_half_season = vector()
# for (i in (1:nrow(game_df_sprd_for_2nd_half))){
#   # home_team_rank_vect.previous.two.half.season <- c(home_team_rank_vect.previous.two.half.season,
#   #                                                   unlist(team.rank.list.previous.two.half.seasons[game_df_sprd_for_2nd_half$Team[i]]))
#   # away_team_rank_vect.previous.two.half.season<- c(away_team_rank_vect.previous.two.half.season,
#   #                                                  unlist(team.rank.list.previous.two.half.seasons[game_df_sprd_for_2nd_half$Opponent[i]]))
#   home_team_score_diff_1st_half_season <- c(home_team_score_diff_1st_half_season, 
#                                             unlist(score_diff_list_previous_two_half_seasons[game_df_sprd_for_2nd_half$Team[i]]))
#   away_team_score_diff_1st_half_season <- c(away_team_score_diff_1st_half_season,
#                                             unlist(score_diff_list_previous_two_half_seasons[game_df_sprd_for_2nd_half$Opponent[i]]))
#   home_team_YPP_diff_1st_half_season <-c(home_team_YPP_diff_1st_half_season,
#                                         unlist(YPP_diff_List_first_half_season[game_df_sprd_for_2nd_half$Team[i]]))
#   away_team_YPP_diff_1st_half_season <- c(away_team_YPP_diff_1st_half_season,
#                                           unlist(YPP_diff_List_first_half_season[game_df_sprd_for_2nd_half$Opponent[i]]))
#   
# }
# game_df_sprd_for_2nd_half$home_team_rank = home_team_rank_vect.previous.two.half.season
# game_df_sprd_for_2nd_half$opponent_team_rank = away_team_rank_vect.previous.two.half.season
# game_df_sprd_for_2nd_half$home_team_score_diff_pred_based_on_1st_half = home_team_score_diff_1st_half_season
# game_df_sprd_for_2nd_half$away_team_score_diff_pred_based_on_1st_half = away_team_score_diff_1st_half_season
# game_df_sprd_for_2nd_half$home_team_YPP_diff_pred_based_on_1st_half = home_team_YPP_diff_1st_half_season
# game_df_sprd_for_2nd_half$away_team_YPP_diff_pred_based_on_1st_half = away_team_YPP_diff_1st_half_season
# #game_df_sprd_for_2nd_half$rank_difference = game_df_sprd_for_2nd_half$home_team_rank - game_df_sprd_for_2nd_half$opponent_team_rank




games_2016_with_pred_by_week = game_df_sprd_for_2nd_half
games_2017_with_pred_by_week = game_df_sprd_for_2nd_half
games_2018_with_pred_by_week = game_df_sprd_for_2nd_half
games_2016_2018_with_pred_by_week =rbind( rbind(games_2016_with_pred_by_week, games_2017_with_pred_by_week), games_2018_with_pred_by_week)
games_2016_2018_with_pred_by_week<- games_2016_2018_with_pred_by_week[complete.cases(games_2016_2018_with_pred_by_week),]
games_2016_2018_with_pred_by_week.valid = filter(games_2016_2018_with_pred_by_week, not_first_4_games == 1)
write.csv(games_2016_2018_with_pred_by_week, file = "games_from_2016_2018_with_week_by_week_updates.csv", row.names = F)

games_2016_with_pred_by_week$score_diff_pred_on_prev_weeks = games_2016_with_pred_by_week$home_score_diff_pred_on_prev_weeks - 
  games_2016_with_pred_by_week$away_score_diff_pred_on_prev_weeks


viewing_df = games_2016_2018_with_pred_by_week[, c(2,8, 36,42, 48:54)]
viewing_df_valid = filter(viewing_df, Week >= 5 & Week <= 14)
viewing_df_valid = viewing_df_valid[complete.cases(viewing_df_valid),]
par(mfrow= c(2,2))
plot(viewing_df_valid$Score_diff, viewing_df_valid$score_diff_pred_on_prev_weeks)
plot(viewing_df_valid$Score_diff, viewing_df_valid$recent_4_score_diff)
plot(viewing_df_valid$Score_diff, viewing_df_valid$vegas_sprd)
mean(abs(viewing_df_valid$score_diff_pred_on_prev_weeks + viewing_df_valid$vegas_sprd))
mean(abs(viewing_df_valid$score_diff_pred_on_prev_weeks - viewing_df_valid$Score_diff))
mean(abs(viewing_df_valid$score_diff + viewing_df_valid$vegas_sprd))
naive_correction_vector = (viewing_df_valid$score_diff_pred_on_prev_weeks + viewing_df_valid$vegas_sprd) *(viewing_df_valid$Score_diff + viewing_df_valid$vegas_sprd)
length(which(naive_correction_vector >0))
recent_4_correction_vector = (viewing_df_valid$recent_4_score_diff + viewing_df_valid$vegas_sprd)*
  (viewing_df_valid$vegas_sprd + viewing_df_valid$Score_diff)
length(which(recent_4_correction_vector >0))
boosting.previous.weeks = gbm(Score_diff~vegas_sprd + home_score_diff_pred_on_prev_weeks + away_score_diff_pred_on_prev_weeks, 
                              data = filter(games_2016_2018_with_pred_by_week.valid, (Season == 2016)|(Season == 2017) ),
                              distribution = 'laplace', n.trees = 5000, interaction.depth = 2, shrinkage = 0.003)
summary(boosting.previous.weeks)
get_correction(filter(games_2016_2018_with_pred_by_week.valid, Season == 2018),boosting.previous.weeks, 
               -games_2016_2018_with_pred_by_week.valid$vegas_sprd[which(games_2016_2018_with_pred_by_week.valid$Season == 2018)],
               games_2016_2018_with_pred_by_week.valid$Score_diff[which(games_2016_2018_with_pred_by_week.valid$Season == 2018)], 1)



svm.sprd = svm(Score_diff~ home_team_rank+ opponent_team_rank + vegas_sprd, data = game_df_sprd_for_2nd_half[train_larger,])
get_correction(game_df_sprd_for_2nd_half[train_larger,], svm.sprd, game_df_sprd_for_2nd_half$vegas_sprd[train_larger]* -1, game_df_sprd_for_2nd_half$Score_diff[train_larger],0 )
get_correction(game_df_sprd_for_2nd_half[test_larger,], svm.sprd, game_df_sprd_for_2nd_half$vegas_sprd[test_larger]*-1, game_df_sprd_for_2nd_half$Score_diff[test_larger], 0)

svm.sprd = svm(Score_diff~ home_team_rank + opponent_team_rank + vegas_sprd, data = game_df_sprd_for_2nd_half[train_small,])
get_correction(game_df_sprd_for_2nd_half[train_small,], svm.sprd, game_df_sprd_for_2nd_half$vegas_sprd[train_small]* -1, game_df_sprd_for_2nd_half$Score_diff[train_small], 0)
get_correction(game_df_sprd_for_2nd_half[test_small,], svm.sprd, game_df_sprd_for_2nd_half$vegas_sprd[test_small]*-1, game_df_sprd_for_2nd_half$Score_diff[test_small], 0)


adaboosting.2nd.half.season = gbm(vegas_coverage_0_1~ home_team_rank + opponent_team_rank + vegas_sprd, data = game_df_sprd_for_2nd_half[train_larger,],
                                  distribution = 'adaboost',
                                  n.trees = 500, interaction.depth =3, shrinkage = 0.05 )
adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[train_larger,], n.trees = 5000)
length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[train_larger]))/ length(adaboosting.pred) 
adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[test_larger,], n.trees = 5000)
length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[test_larger]))/ length(adaboosting.pred)


adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[train_small,], n.trees = 5000)
length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[train_small]))/ length(adaboosting.pred) 
adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[test_small,], n.trees = 5000)
length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[test_small]))/ length(adaboosting.pred)


for (stump in (1:3)){ 
  for (shrink  in seq(0.03, 0.06, 0.002)){
    adaboosting.2nd.half.season = gbm(vegas_coverage_0_1~ home_team_rank + opponent_team_rank + vegas_sprd, data = game_df_sprd_for_2nd_half[train_larger,],
                                      distribution = 'adaboost',
                                      n.trees = 1000, interaction.depth =stump, shrinkage = shrink )
    print(c("stump:", stump, "shinkage:", shrink))
    # adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[train_small,], n.trees = 500)
    # print(c("training acc:",length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[train_small]))/ length(adaboosting.pred)))
    adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[test_small,], n.trees = 1000)
    correct_length = length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[test_larger]))
    print(c("larger testing acc:",correct_length/ length(adaboosting.pred), "correct len:", correct_length ))
    
 }
}#Looks like interation depth = 3 and shrinkage = 0.005 is best

for (stump in (1:3)){ 
  for (shrink  in seq(0.01, 0.3, 0.01)){
    adaboosting.2nd.half.season = gbm(vegas_coverage_0_1~ home_team_rank + opponent_team_rank + vegas_sprd, data = game_df_sprd_for_2nd_half[train_small,],
                                      distribution = 'adaboost',
                                      n.trees = 100, interaction.depth =stump, shrinkage = shrink )
    print(c("stump:", stump, "shinkage:", shrink))
    # adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[train_small,], n.trees = 500)
    # print(c("training acc:",length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[train_small]))/ length(adaboosting.pred)))
    adaboosting.pred = predict(adaboosting.2nd.half.season, newdata = game_df_sprd_for_2nd_half[test_small,], n.trees = 100)
    correct_length = length(which(sign(adaboosting.pred) == game_df_sprd_for_2nd_half$vegas_coverage[test_small]))
    print(c("larger testing acc:",correct_length/ length(adaboosting.pred), "correct len:", correct_length ))
    
  }
}#Looks like interation depth = 3 and shrinkage = 0.005 is best

length(which( abs(game_df_sprd_for_2nd_half$home_team_rank - game_df_sprd_for_2nd_half$opponent_team_rank)>= 40))


vegas_coverage =  sign((game_df_sprd_for_2nd_half$Score_diff + game_df_sprd_for_2nd_half$vegas_sprd) )
length(which(vegas_coverage >0 ))
pred_coverage = ((predict(svm.sprd, newdata = game_df_sprd_for_2nd_half[test,]) + game_df_sprd_for_2nd_half$vegas_sprd[test]))
length(pred_coverage)
length(which(pred_coverage > 0))
mean(abs(vegas_coverage))
mean(abs(pred_coverage))
pred_actual_coverage = predict(svm.sprd, newdata = game_df_sprd_for_2nd_half[test,]) - game_df_sprd_for_2nd_half$Score_diff[test]
mean(abs(pred_actual_coverage))
#######################################################################################################################################################################
#below is multiple ranking: with offense and defense rankings by score
Score_by_team_for_sprd_2nd_half = list()
Score_allowed_by_team_for_sprd_2nd_half = list()
YPP_by_team_for_sprd_2nd_half = list()
YPP_allowed_by_team_for_sprd_2nd_half = list()
for(team in division1_teams){
  home_team_df_train = filter(game_df_sprd_for_2nd_half,Team == team & (Season == Last_season | (Season == current_season & Week <= 7)))
  away_team_df_train = filter(game_df_sprd_for_2nd_half,Opponent == team & (Season == Last_season | (Season == current_season & Week <= 7)))
  Score_by_team_for_sprd_2nd_half[team] = (sum(home_team_df_train$Score) + sum(away_team_df_train$Score_allowed))/ (nrow(home_team_df_train) + nrow(away_team_df_train))
  Score_allowed_by_team_for_sprd_2nd_half[team] = (sum(home_team_df_train$Score_allowed) + sum(away_team_df_train$Score))/  (nrow(home_team_df_train) + nrow(away_team_df_train))
  YPP_by_team_for_sprd_2nd_half[team] = (sum(home_team_df_train$YardsPerPlay) + sum(away_team_df_train$YardsPerPlay_allowed))/(nrow(home_team_df_train) + nrow(away_team_df_train))
  YPP_allowed_by_team_for_sprd_2nd_half[team] = (sum(home_team_df_train$YardsPerPlay_allowed)+ sum(away_team_df_train$YardsPerPlay))/(nrow(home_team_df_train) + nrow(away_team_df_train))
}
Score_by_team_for_sprd_2nd_half<-Score_by_team_for_sprd_2nd_half[order(-unlist(Score_by_team_for_sprd_2nd_half))]
Score_allowed_by_team_for_sprd_2nd_half<- Score_allowed_by_team_for_sprd_2nd_half[order(unlist(Score_allowed_by_team_for_sprd_2nd_half))]
YPP_by_team_for_sprd_2nd_half <- YPP_by_team_for_sprd_2nd_half[order(-unlist(YPP_by_team_for_sprd_2nd_half))]
YPP_allowed_by_team_for_sprd_2nd_half<-YPP_allowed_by_team_for_sprd_2nd_half[order(unlist(YPP_allowed_by_team_for_sprd_2nd_half))]

Team_rank_list_by_score_for_sprd = list()
Team_rank_list_by_score__allowed_for_sprd = list()
Team_rank_list_by_YPP_for_sprd = list()
Team_rank_list_by_YPP_allowed_for_sprd = list()
for(team in division_1_teams){
  Team_rank_list_by_score_for_sprd[team] = which(names(Score_by_team_for_sprd_2nd_half) == team)
  Team_rank_list_by_score__allowed_for_sprd [team]= which(names(Score_allowed_by_team_for_sprd_2nd_half) == team)
  Team_rank_list_by_YPP_for_sprd[team] = which(names(YPP_by_team_for_sprd_2nd_half) == team)
  Team_rank_list_by_YPP_allowed_for_sprd [team]= which(names(YPP_allowed_by_team_for_sprd_2nd_half) == team)
}

team_rank_df = data.frame(
  Team = division_1_teams,
  Score = unlist(Team_rank_list_by_score_for_sprd),
  Score_allowed = unlist(Team_rank_list_by_score__allowed_for_sprd),
  YPP = unlist(Team_rank_list_by_YPP_for_sprd),
  YPP_allowed = unlist(Team_rank_list_by_YPP_allowed_for_sprd)
)
team_rank_df = arrange(team_rank_df, Team)


home_team_score_pred_vect = vector()
away_team_score_pred_vect = vector()
home_team_score_allowed_pred_vect = vector()
away_team_score_allowed_pred_vect = vector()
home_team_score_rank_vect = vector()
away_team_score_rank_vect = vector()
home_team_score_allowed_rank_vect = vector()
away_team_score_allowed_rank_vect = vector()
home_team_YPP_rank_vect = vector()
away_team_YPP_rank_vect = vector()
home_team_YPP_allowed_rank_vect = vector()
away_team_YPP_allowed_rank_vect = vector()
for (i in (1: nrow(game_df_sprd_for_2nd_half))){
  home_team_score_pred_vect <- c(home_team_score_pred_vect, unlist(Score_by_team_for_sprd_2nd_half[game_df_sprd_for_2nd_half$Team[i]]))
  away_team_score_pred_vect <- c(away_team_score_pred_vect, unlist(Score_by_team_for_sprd_2nd_half[game_df_sprd_for_2nd_half$Opponent[i]]))
  
  home_team_score_allowed_pred_vect <- c(home_team_score_allowed_pred_vect, unlist(Score_allowed_by_team_for_sprd_2nd_half[game_df_sprd_for_2nd_half$Team[i]]))
  away_team_score_allowed_pred_vect <- c(away_team_score_allowed_pred_vect, unlist(Score_allowed_by_team_for_sprd_2nd_half[game_df_sprd_for_2nd_half$Opponent[i]]))
    
  home_team_score_rank_vect <- c(home_team_score_rank_vect, unlist(Team_rank_list_by_score_for_sprd[game_df_sprd_for_2nd_half$Team[i]]))
  away_team_score_rank_vect <- c(away_team_score_rank_vect, unlist(Team_rank_list_by_score_for_sprd[game_df_sprd_for_2nd_half$Opponent[i]]))
  
  home_team_score_allowed_rank_vect<- c(home_team_score_allowed_rank_vect, unlist(Team_rank_list_by_score__allowed_for_sprd[game_df_sprd_for_2nd_half$Team[i]]))
  home_team_YPP_rank_vect<- c(home_team_YPP_rank_vect, unlist(Team_rank_list_by_YPP_for_sprd[game_df_sprd_for_2nd_half$Team[i]]))
  home_team_YPP_allowed_rank_vect <- c(home_team_YPP_allowed_rank_vect, unlist(Team_rank_list_by_YPP_allowed_for_sprd[game_df_sprd_for_2nd_half$Team[i]]))
  
  away_team_score_allowed_rank_vect <- c(away_team_score_allowed_rank_vect, unlist(Team_rank_list_by_score__allowed_for_sprd[game_df_sprd_for_2nd_half$Opponent[i]]))
  away_team_YPP_rank_vect <- c(away_team_YPP_rank_vect, unlist(Team_rank_list_by_YPP_for_sprd[game_df_sprd_for_2nd_half$Opponent[i]]))
  away_team_YPP_allowed_rank_vect<- c(away_team_YPP_allowed_rank_vect, unlist(Team_rank_list_by_YPP_allowed_for_sprd[game_df_sprd_for_2nd_half$Opponent[i]]))
}
game_df_sprd_for_2nd_half$home_score_pred = home_team_score_pred_vect
game_df_sprd_for_2nd_half$away_score_pred = away_team_score_pred_vect
game_df_sprd_for_2nd_half$home_score_allowed_pred = home_team_score_allowed_pred_vect
game_df_sprd_for_2nd_half$away_score_allowed_pred = away_team_score_allowed_pred_vect

game_df_sprd_for_2nd_half$home_score_rank = home_team_score_rank_vect
game_df_sprd_for_2nd_half$away_score_rank = away_team_score_rank_vect
game_df_sprd_for_2nd_half$home_score_allowed_rank = home_team_score_allowed_rank_vect
game_df_sprd_for_2nd_half$away_score_allowed_rank = away_team_score_allowed_rank_vect


game_df_sprd_for_2nd_half<-game_df_sprd_for_2nd_half[complete.cases(game_df_sprd_for_2nd_half),]
write.csv(game_df_sprd_for_2nd_half, "games_later_half_17&whole_18.csv", row.names = F)
write.csv(game_df_sprd_for_2nd_half, "games_whole_2016.csv", row.names = F )
write.csv(game_df_sprd_for_2nd_half, "games_whole_2017.csv", row.names = F)
write.csv(game_df_sprd_for_2nd_half, 'games_whole_2018.csv', row.names = F)

write.csv(games_2016, "games_whole_2016_YPP.csv", row.names = F)
write.csv(games_2017, "games_whole_2017_YPP.csv", row.names = F)
write.csv(games_2018, "games_whole_2018_YPP.csv", row.names = F)
games_2016 = read.csv("games_whole_2016.csv", stringsAsFactors = F)
games_2016$vegas_coverage = games_2016$vegas_sprd + games_2016$Score_diff
train_inlier = which(abs(games_2016$vegas_coverage) <= 2 * sd(games_2016$vegas_coverage))
train_df = games_2016[intersect(which(games_2016$Week >7 & games_2016$not_first_4_games == 1), train_inlier),]
games_2017 = read.csv("games_whole_2017.csv", stringsAsFactors = F)
games_2018 = read.csv("games_whole_2018.csv", stringsAsFactors = F)
test_df = games_2017[which(games_2017$Week >7 & games_2017$not_first_4_games == 1),]
test_df<- test_df[complete.cases(test_df),]
boosting.2nd.half = gbm(Score_diff~ vegas_sprd + home_recent_4_game_mean + away_recent_4_game_mean + home_wins_last_4_games + away_wins_last_4_games
                        +home_team_rank + opponent_team_rank,
                        data = train_df, distribution = 'laplace',n.trees = 1000, interaction.depth = 3, shrinkage = 0.005)

summary(boosting.2nd.half)
games_2016$recent_4_score_diff = games_2016$home_recent_4_game_mean - games_2016$away_recent_4_game_mean + rep(4.3, nrow(games_2016))
games_2017$recent_4_score_diff = games_2017$home_recent_4_game_mean - games_2017$away_recent_4_game_mean + rep(4.3, nrow(games_2017))
games_2018$recent_4_score_diff = games_2018$home_recent_4_game_mean - games_2018$away_recent_4_game_mean + rep(4.3,nrow(games_2018))
games_2016$score_diff_pred_based_on_1st_half = games_2016$home_team_score_diff_pred_based_on_1st_half - games_2016$away_team_score_diff_pred_based_on_1st_half +
  rep(4.3,nrow(games_2016) )
games_2017$score_diff_pred_based_on_1st_half = games_2017$home_team_score_diff_pred_based_on_1st_half - games_2017$away_team_score_diff_pred_based_on_1st_half +
  rep(4.3,nrow(games_2017) )
games_2018$score_diff_pred_based_on_1st_half = games_2018$home_team_score_diff_pred_based_on_1st_half - games_2018$away_team_score_diff_pred_based_on_1st_half +
  rep(4.3,nrow(games_2018) )

games_2016$YPP_diff_pred_based_on_1st_half = games_2016$home_team_YPP_diff_pred_based_on_1st_half - games_2016$away_team_YPP_diff_pred_based_on_1st_half +
  rep(0.4,nrow(games_2016) )
games_2017$YPP_diff_pred_based_on_1st_half = games_2017$home_team_YPP_diff_pred_based_on_1st_half - games_2017$away_team_YPP_diff_pred_based_on_1st_half +
  rep(0.4,nrow(games_2017) )
games_2018$YPP_diff_pred_based_on_1st_half = games_2018$home_team_YPP_diff_pred_based_on_1st_half - games_2018$away_team_YPP_diff_pred_based_on_1st_half +
  rep(0.4,nrow(games_2018) )


games_2016_valid = games_2016[which(games_2016$not_first_4_games == 1),]
games_2017_valid = games_2017[which(games_2017$not_first_4_games == 1),]
games_2018_valid = games_2018[which(games_2018$not_first_4_games == 1),]
games_2016_2017_valid = rbind(games_2016_valid, games_2017_valid)
games_2016_2017_second_half = games_2016_2017_valid[which(games_2016_2017_valid$Week >= 8),]
games_2016_2018_valid = rbind(games_2016_2017_valid, games_2018_valid)
games_2016_2018_valid <- games_2016_2018_valid[complete.cases(games_2016_2018_valid),]
games_2016_2018_second_half = games_2016_2018_valid[which(games_2016_2018_valid$Week >=8),]
games_2018_second_half = games_2018_valid[which(games_2018_valid$Week >=8),]
par(mfrow= c(2,2))
pls.fit = plsr(Score_diff~ vegas_sprd + recent_4_score_diff + recent_4_YPP_diff + score_diff_pred_based_on_1st_half + YPP_diff_pred_based_on_1st_half,
               data = games_2016_2017_second_half,
               scale = T, validation = 'CV')
summary(pls.fit)
pls.pred = predict(pls.fit, games_2018_second_half ,ncomp = 3)
length(which((games_2018_second_half$vegas_sprd + games_2018_second_half$vegas_sprd) * 
               (games_2018_second_half$Score_diff + games_2018_second_half$vegas_sprd) >0 ))
games_2016_2018_second_half$vegas_coverage = games_2016_2018_second_half$vegas_sprd + games_2016_2018_second_half$Score_diff
summary(games_2016_2018_second_half$vegas_coverage)

names(games_2016_2017_second_half)
accumulate_and_momentum_same_side_of_vegas = which((games_2016_2018_second_half$score_diff_pred_based_on_1st_half + games_2016_2018_second_half$vegas_sprd) *
                                                 (games_2016_2018_second_half$recent_4_score_diff+ games_2016_2018_second_half$vegas_sprd) >=0)
mom_acc_diff_side_of_vegas = setdiff(c(1:nrow(games_2016_2018_second_half)), accumulate_and_momentum_same_side_of_vegas)
length(intersect(mom_acc_diff_side_of_vegas, momentum_accurate))
length(intersect(mom_acc_diff_side_of_vegas, momentum_wrong))

for (alpha in seq(0, 1, 0.05)){
  score_diff_alpha = alpha* games_2016_2018_second_half$recent_4_score_diff + (1-alpha)*games_2016_2018_second_half$score_diff_pred_based_on_1st_half
  #diff_with_actual_score = sqrt(mean((score_diff_alpha - games_2016_2018_second_half$Score_diff)^2))
  diff_with_actual_score = mean(abs(score_diff_alpha - games_2016_2018_second_half$Score_diff))
  diff_with_vegas = mean(abs(score_diff_alpha + games_2016_2018_second_half$vegas_sprd))
  correlation_with_actual_score = cor(games_2016_2018_second_half$Score_diff, score_diff_alpha)
  correction_vector = (games_2016_2018_second_half$vegas_sprd + games_2016_2018_second_half$Score_diff) * (games_2016_2018_second_half$vegas_sprd + score_diff_alpha)
  accuracy_alpha = length(which(correction_vector >0))
  print(c( alpha, "diff w/ actual:", diff_with_actual_score, correlation_with_actual_score,diff_with_vegas, "Numbers correct:", accuracy_alpha))
}


momentum_accurate = which ((games_2016_2018_second_half$vegas_sprd + games_2016_2018_second_half$Score_diff) *
(games_2016_2018_second_half$vegas_sprd + games_2016_2018_second_half$recent_4_score_diff) > 0 )
momentum_wrong = setdiff(c(1:nrow(games_2016_2018_second_half)), momentum_accurate)
accumulate_accurate = which ((games_2016_2018_second_half$vegas_sprd + games_2016_2018_second_half$Score_diff) *
                               (games_2016_2018_second_half$vegas_sprd + games_2016_2018_second_half$score_diff_pred_based_on_1st_half) >= 0 )
accumulate_wrong = setdiff(c(1:nrow(games_2016_2018_second_half)), accumulate_accurate)
both_accurate = intersect(momentum_accurate, accumulate_accurate)

outlier_vegas_coverage = which(abs(games_2016_2018_second_half$vegas_coverage) > 2* sd(games_2016_2018_second_half$vegas_coverage))
length(outlier_vegas_coverage)
games_2016_2018_second_half_no_outlier = games_2016_2018_second_half[-outlier_vegas_coverage,]



train = sample(nrow(games_2016_2018_second_half), size = 700)
test = setdiff( c(1: nrow(games_2016_2018_second_half)), train)
length(which(games_2016_2018_valid[test,]$Week >=8))
boosting.2nd.half = gbm(Score_diff~  recent_4_score_diff + score_diff_pred_based_on_1st_half, 
                        data = games_2016_2018_second_half[train,], distribution = 'laplace', n.trees = 5000, interaction.depth = 2, shrinkage = 0.001)
#games_2017_valid$vegas_coverage = games_2017_valid$Score_diff+ games_2017_valid$vegas_sprd

summary(boosting.2nd.half)
#get_correction(games_2016_2018_valid[test,], boosting.2nd.half, games_2016_2018_valid$vegas_sprd[test]*-1, games_2016_2018_valid$Score_diff[test], 1)

get_correction(games_2016_2018_second_half[test,], boosting.2nd.half, games_2016_2018_second_half$vegas_sprd[test]*-1, games_2016_2018_second_half$Score_diff[test], 1)


get_correction(games_2016_valid, boosting.2nd.half, games_2016_valid$vegas_sprd*-1, games_2016_valid$Score_diff, 1)

games_2017 <- games_2017[complete.cases(games_2017),]

second_half_2017 = which(games_2017_valid$Week>= 8)
games_2017_valid_2nd_half = games_2017_valid[second_half_2017,]
games_2017_valid_2nd_half<- games_2017_valid_2nd_half[complete.cases(games_2017_valid_2nd_half),]
get_correction(games_2017_valid_2nd_half, boosting.2nd.half, games_2017_valid_2nd_half$vegas_sprd*-1, games_2017_valid_2nd_half$Score_diff,1)

second_half_2018 = which(games_2018_valid$Week >= 8)
games_2018_valid_2nd_half = games_2018_valid[second_half_2018,]
get_correction(games_2018_valid_2nd_half, boosting.2nd.half, games_2018_valid_2nd_half$vegas_sprd*-1, games_2018_valid_2nd_half$Score_diff, 1)

plot(games_2017$vegas_sprd[not_first_4_games] *-1, games_2017$recent_4_score_diff[not_first_4_games])
games_2017_valid = games_2017[which(games_2017$not_first_4_games == 1),]
larger_diff_between_vegas_recent_4_games = which(abs(games_2017_valid$vegas_sprd + games_2017_valid$recent_4_score_diff) >
                                                   median(abs(games_2017_valid$vegas_sprd + games_2017_valid$recent_4_score_diff)))
naive_correction_vector = (games_2017_valid$vegas_sprd[larger_diff_between_vegas_recent_4_games] + games_2017_valid$Score_diff[larger_diff_between_vegas_recent_4_games])*
                          (games_2017_valid$vegas_sprd[larger_diff_between_vegas_recent_4_games] + games_2017_valid$recent_4_score_diff[larger_diff_between_vegas_recent_4_games])

length(which(naive_correction_vector > 0))
length(naive_correction_vector)
small_diff_between_vegas_recent_4_games = which(abs(games_2017_valid$vegas_sprd + games_2017_valid$recent_4_score_diff) <=
                                                   median(abs(games_2017_valid$vegas_sprd + games_2017_valid$recent_4_score_diff)))
naive_correction_vector = 
library(e1071)
svm.team.rank = svm(Score_diff~  home_score_rank + away_score_rank + home_score_allowed_rank + away_score_allowed_rank + vegas_sprd,
                    data = game_df_sprd_for_2nd_half[train,])

#summary(svm.team.rank)
get_correction(game_df_sprd_for_2nd_half[train,], svm.team.rank, 
               game_df_sprd_for_2nd_half$vegas_sprd[train] * -1,
               game_df_sprd_for_2nd_half$Score_diff[train], is.boosting = 0)

get_correction(game_df_sprd_for_2nd_half[test,], svm.team.rank, 
               game_df_sprd_for_2nd_half$vegas_sprd[test] * -1,
               game_df_sprd_for_2nd_half$Score_diff[test], is.boosting = 0)
# svm.team.rank.train = predict(svm.team.rank, newdata = game_df_sprd_for_2nd_half[c(1:length(train)),])
# svm.team.rank.train.correction = sign((svm.team.rank.train + game_df_sprd_for_2nd_half$vegas_sprd[train])*
#                                         (game_df_sprd_for_2nd_half$vegas_sprd + game_df_sprd_for_2nd_half$Score_diff)[train])
# length(which(svm.team.rank.train.correction == 1))/ length(svm.team.rank.train.correction)
# 
# svm.team.rank.predict = predict(svm.team.rank, newdata = game_df_sprd_for_2nd_half[c(length(train) + 1: 
#                                                                                                         nrow(game_df_sprd_for_2nd_half)),])
# svm.team.rank.correction = sign( (svm.team.rank.predict + game_df_sprd_for_2nd_half$vegas_sprd[test]) *
#                     (game_df_sprd_for_2nd_half$vegas_sprd + game_df_sprd_for_2nd_half$Score_diff)[test])
# length(which(svm.team.rank.correction == 1))
# length(which(svm.team.rank.correction == 1))/ length(svm.team.rank.correction)
library(xgboost)
xgb.2nd.half = xgboost()

library(gbm)
boosting.team.rank = gbm(Score_diff~  home_score_rank + away_score_rank + home_score_allowed_rank + away_score_allowed_rank + vegas_sprd, 
                         data = game_df_sprd_for_2nd_half[c(1: length(train)),],
                         distribution = 'laplace',
                         n.trees = 5000, interaction.depth =3, shrinkage = 0.003)
summary(boosting.team.rank)
get_correction(game_df_sprd_for_2nd_half[c(1: length(train_larger)),], boosting.team.rank, 
               game_df_sprd_for_2nd_half$vegas_sprd[c(1: length(train_larger))]* -1, 
               game_df_sprd_for_2nd_half$Score_diff[c(1: length(train_larger))], is.boosting = 1)

get_correction(game_df_sprd_for_2nd_half[test,], boosting.team.rank, 
               game_df_sprd_for_2nd_half$vegas_sprd[test] * -1,
               game_df_sprd_for_2nd_half$Score_diff[test], 1)
####use 3 previous half seasons to predict the first half season####not very good
train = which( division_1_games$Season == Last_season | (division_1_games$Season == second_to_last_season & division_1_games$Week>=8))
test = which(division_1_games$Season == current_season & division_1_games$Week<=7)
team.rank.games.1st.half.season = division_1_games[c(train,test),]
home.rank.vect.prev.3.half.seasons = vector()
away.rank.vect.prev.3.half.seasons = vector()
for (i in (1: nrow(team.rank.games.1st.half.season))){
  home.rank.vect.prev.3.half.seasons <- c(home.rank.vect.prev.3.half.seasons, unlist(team.rank.list.prev.3.half.seasons[team.rank.games.1st.half.season$Team[i]]))
  away.rank.vect.prev.3.half.seasons<- c(away.rank.vect.prev.3.half.seasons, unlist(team.rank.list.prev.3.half.seasons[team.rank.games.1st.half.season$Opponent[i]]))
}
team.rank.games.1st.half.season$home_team_rank = home.rank.vect.prev.3.half.seasons
team.rank.games.1st.half.season$opponent_team_rank = away.rank.vect.prev.3.half.seasons

svm.team.rank.3.half.season = svm(Score_diff~ vegas_sprd + home_team_rank + opponent_team_rank,
                                  data =team.rank.games.1st.half.season[c(1: length(train)), ] )
svm.team.rank.3.half.season.train = predict(svm.team.rank.3.half.season, newdata =team.rank.games.1st.half.season[c(1: length(train)), ] )
svm.3.half.season.train.correction = sign((svm.team.rank.3.half.season.train + division_1_games$vegas_sprd[train])*
                                            (division_1_games$vegas_sprd + division_1_games$Score_diff)[train])
length(which(svm.3.half.season.train.correction == 1))
length(svm.3.half.season.train.correction)

svm.team.rank.3.half.season.test = predict(svm.team.rank.3.half.season, newdata = team.rank.games.1st.half.season[c(length(train) + 1: 
                                                                                                                      nrow(team.rank.games.1st.half.season)),])
svm.3.half.season.test.correction = sign((svm.team.rank.3.half.season.test + division_1_games$vegas_sprd[test]) *
                                           (division_1_games$Score_diff + division_1_games$vegas_sprd)[test])
length(which(svm.3.half.season.test.correction == 1))
length(svm.3.half.season.test.correction)



#################################################################################################################################################

#################################################################################################################################################


# OU                        



#################################################################################################################################################

#################################################################################################################################################


library(dplyr)
library(rlist)
start=Sys.time()

curr_dir = "C:/Users/Zhongyi Zhang/QS Dropbox/Alex Zhongyi Zhang/Alex-summer/CFB/data/16-18/" #getwd()
# curr_dir
# CFB_dir = file.path(curr_dir,"Alex-summer","CFB" ,"data","16-18")
setwd(curr_dir)
list.files(curr_dir)
home_with_vegas = read.csv("home_all_valid_regular_season_with_vegas_info.csv", stringsAsFactors = F)
#home_with_vegas = read.csv('division1_regular_season_16_18_with_vegas_numeric_correct_update.csv', stringsAsFactors = F)
#home_with_vegas = read.csv("update_score_YPP_with_vegas.csv", stringsAsFactors = F)

home_with_vegas<- home_with_vegas[,-1]
home_with_vegas = filter(home_with_vegas, Score_pred >0 | Score_allowed_pred >0)





home.games.with.vegas = home_with_vegas[, c(1:20, 22:34, 94, 95) ]

home.with.vegas.ou = home.games.with.vegas[-which(is.na(home.games.with.vegas$vegas_ou)),]
home.with.vegas.ou$Score_total = home.with.vegas.ou$Score + home.with.vegas.ou$Score_allowed

division1_teams = readRDS("division1_ teams")
division1.games = home.games.with.vegas[which(home.games.with.vegas$Opponent %in% division1_teams),]
division1.games.with.vegas.ou = home.with.vegas.ou[which(home.with.vegas.ou$Opponent %in% division1_teams & 
                                                           home.with.vegas.ou$Team %in% division1_teams),]
division1.games.with.vegas.ou <- division1.games.with.vegas.ou[ -which(division1.games.with.vegas.ou$vegas_ou == 0),]

mean((division1.games.with.vegas.ou$Score_total - division1.games.with.vegas.ou$vegas_ou)^2)
plot(division1.games.with.vegas.ou$Score_total, division1.games.with.vegas.ou$vegas_ou)
plot(division1.games.with.vegas.ou$Score_diff, division1.games.with.vegas.ou$vegas_sprd)

###################experiment different ML models  with actual team data with score_total

games_data_without_score= subset(division1.games.with.vegas.ou, select = - c(Season,HomeOrAway, Week, Team, Opponent, Score, Score_diff, Score_allowed))

ou.actual.game = lm(Score_total~.-vegas_sprd- DateTime, data = games_data_without_score)
summary(ou.actual.game)

train = which(games_data_without_score$DateTime <= '2018-01-01')

games_data_without_score_or_sprd = subset(games_data_without_score, select = -vegas_sprd)

train_no_sprd = which(games_data_without_score_or_sprd$DateTime <= "2018-01-01")
library(gbm)
boost.CFB = gbm(Score_total~.- DateTime, data=games_data_without_score[train,], distribution=
                  "gaussian",n.trees =5000, interaction.depth =3, shrinkage = 0.003)

summary(boost.CFB)
##the summary tells us that YardPerPlay and FirstDowns are the most important features.

###this is the function for testing the accuracy of our model
###with input test dataframe, the model, vegas_line( in case of vegas sprd, need to be negative of that), actual game line, and if the model uses boosting
###prints the overal accuracy, the quantiles of absolute difference from the vegas line and how the accuracy is according to which quantile it lies in
get_correction <- function(my_data, my_model, vegas_line, actual_line, is.boosting){
  
  if (is.boosting){
  model.preds = predict(my_model, newdata = my_data, n.tree = 5000)
  }
  else{model.preds = predict(my_model, newdata = my_data)}
  #model.preds = predict(my_model, newdata = my_data)
  correction.vector = sign ((model.preds - vegas_line) * (actual_line - vegas_line))
  #print(length(model.preds))
  #print(model.preds[1:30])
  #print(vegas_line[1:30])
  #print(length(vegas_line))
  print(c("length of data set:", length(correction.vector), "length of correct side:", length(which(correction.vector == 1)), 
          'accuracy:', length(which(correction.vector == 1))/ length(correction.vector) ))
  print(c( "MSE between predictions and actual data:", mean(abs(model.preds - actual_line)) ))
  print(c('MSE between predictions and vegas_line:', mean(abs(model.preds - vegas_line) ) ) )
  median_diff_between_two_preds = median(abs(model.preds - vegas_line))
  Quantile_1st = summary(abs(model.preds - vegas_line))[2]
  Quantile_3rd = summary(abs(model.preds - vegas_line))[5]
  #larger_diff = which(abs(model.preds - vegas_line) > median_diff_between_two_preds)
  print("summary of abs diff between vegas line and model.preds")
  print(summary(abs(vegas_line - model.preds)) )
  larger_diff = which(abs(model.preds - vegas_line)>= Quantile_3rd)
  between_median_and_3rd_quantile = which(abs(model.preds - vegas_line) >= median_diff_between_two_preds  & (abs(model.preds - vegas_line) < Quantile_3rd))
                                             
  between_1st_and_median = which(abs(model.preds - vegas_line) >= Quantile_1st &
                                     (abs(model.preds - vegas_line)< median_diff_between_two_preds ))
  small_diff = which(abs(model.preds - vegas_line)< Quantile_1st)
  
  larger_diff_correction = correction.vector[larger_diff]
  print(c( "larger than 3rd Quant:", length(larger_diff_correction), "length of correct:", length(which(larger_diff_correction == 1)) ,
    "acc:", length(which(larger_diff_correction == 1)) /length(larger_diff_correction)  ))
  
  between_med_3rd_correction = correction.vector[between_median_and_3rd_quantile]
  print(c("between median and 3rd:", length(between_med_3rd_correction), "length of correct:", length(which(between_med_3rd_correction == 1)),
                           "acc:", length(which(between_med_3rd_correction == 1))/ length(between_med_3rd_correction)     ))             
  
  between_1st_quan_median_correction = correction.vector[between_1st_and_median]
  print(c("between 1st and median :", length(between_1st_quan_median_correction), "length of correct:", length(which(between_1st_quan_median_correction == 1)),
          "acc:", length(which(between_1st_quan_median_correction == 1))/ length(between_1st_quan_median_correction) ))
  
  small_diff_correction = correction.vector[small_diff]
  print(c("less than 1st:", length(small_diff_correction), "length of correct:", length(which(small_diff_correction == 1)), 
         "acc:", length(which(small_diff_correction == 1))/ length(small_diff_correction) ))
  
  # par(mfrow= c(2,2))
  # plot (actual_line, model.preds)
  # abline(0, 1, col= 'red', lwd = 2)
  # plot(actual_line, vegas_line)
  # abline(0, 1, col= 'red', lwd = 2)
  # plot(model.preds, vegas_line)
  # abline(0,1,col = 'red', lwd = 2)
}

# boost.preds = predict(boost.CFB, newdata = games_data_without_score[-train,], n.trees = 5000)
# plot(boost.preds, games_data_without_score$Score_total[-train])
# mean((boost.preds - games_data_without_score$Score_total[-train])^2)
# 
# Laplaceboost.CFB = gbm(Score_total~.-DateTime, data=games_data_without_score[-train,], distribution=
#                   "laplace",n.trees =5000, interaction.depth =3, shrinkage = 0.003)
# 
# summary(Laplaceboost.CFB)
# laplaceboost.preds = predict(Laplaceboost.CFB, newdata = games_data_without_score[-train,], n.trees = 5000)
# mean((laplaceboost.preds - games_data_without_score$Score_total[-train])^2)
# library(e1071)
# svm.ou = svm(Score_total~ YardsPerPlay + YardsPerPlay_allowed +home_total_Yards +away_total_Yards + vegas_ou,
#                    data=games_data_without_score[train,])
# 
# 
# get_correction(games_data_without_score[train,], svm.ou, games_data_without_score$vegas_ou[train], 
#                games_data_without_score$Score_total[train], 1)
# get_correction(games_data_without_score[-train,], svm.ou, games_data_without_score$vegas_ou[-train], 
#                games_data_without_score$Score_total[-train],1)
# 
# svm.preds = predict(svm.ou, newdata =  games_data_without_score[-train,])
# mean((svm.preds - games_data_without_score$Score_total[-train])^2)
# cor(games_data_without_score$Score_total, games_data_without_score$FirstDowns)
# cor(games_data_without_score$Score_total, games_data_without_score$YardsPerPlay)
# cor(games_data_without_score$Score_total, games_data_without_score$YardsPerPlay_allowed)
# cor(games_data_without_score$Score_total, games_data_without_score$FirstDowns_allowed)
# cor(games_data_without_score$Score_total, games_data_without_score$vegas_ou)

###this is the function for updating the second half of a season using simple average of previous 2 half seasons (can be changed to 1 or 3)
###input is the dataframe of only home games with both sides division1 teams with vegas ou, current_season and the division 1 teams
###rank team by average score and score allowed respectively
###for each game, add the score and score_allowed predictions of both home and away teams (4 columns), also the rank of the teams in those 4 categories
###add a column:test_for_2nd_half, 1 means it is a test set, namely games that in the 2nd half of the season
###return the dataframe that is only relevant to the 2nd half of current season, namely later half of previous year and whole current season

game_df_update_ou_2nd_half_with_score_ranking <- function(division1.games.with.vegas.ou, current_season, division1_teams){
#current_season = 2017
Last_season = current_season - 1
Score_by_team_list_previous_2_half_seasons = list()
score_allowed_by_team_prev_2_half_seasons = list()
# YPP_by_team_list_previous_2_half_seasons = list()
# YPP_allowed_by_team_prev_2_half_seasons = list()
# total_yards_by_team_prev_2_half_seasons = list()
# total_yards_allowed_by_team_prev_2_half_seasons = list()
for(team in division1_teams){
  # home_team_df = filter(division1.games.with.vegas.ou, ((Season == current_season & Week <=7) | (Season = Last_season )) & Team == team)
  # away_team_df = filter(division1.games.with.vegas.ou, ((Season == current_season & Week <=7) | (Season = Last_season )) & Opponent == team)
  home_team_df = filter(division1.games.with.vegas.ou, ((Season == current_season & Week <=7) | (Season = Last_season & Week >=8)) & Team == team)
  away_team_df = filter(division1.games.with.vegas.ou, ((Season == current_season & Week <=7) | (Season = Last_season & Week >=8)) & Opponent == team)
  # #home_team_df = filter(division1.games.with.vegas.ou, ((Season == current_season & Week <=7)  & Team == team))
  #away_team_df = filter(division1.games.with.vegas.ou, ((Season == current_season & Week <=7) & Opponent == team))
  
  Score_by_team_list_previous_2_half_seasons[team] = (sum(home_team_df$Score) + sum(away_team_df$Score_allowed) )/ (nrow(home_team_df) + nrow(away_team_df))
  score_allowed_by_team_prev_2_half_seasons[team] = (sum(home_team_df$Score_allowed) + sum(away_team_df$Score))/ (nrow(home_team_df) + nrow(away_team_df))
  # YPP_by_team_list_previous_2_half_seasons[team] = (sum(home_team_df$YardsPerPlay) + sum(away_team_df$YardsPerPlay_allowed))/
  #   (nrow(home_team_df) + nrow(away_team_df))
  # YPP_allowed_by_team_prev_2_half_seasons[team] = (sum(home_team_df$YardsPerPlay_allowed) + sum(away_team_df$YardsPerPlay))/
  #   (nrow(home_team_df) + nrow(away_team_df))
  # total_yards_by_team_prev_2_half_seasons[team] = (sum(home_team_df$home_total_Yards) + sum(away_team_df$away_total_Yards))/
  #   (nrow(home_team_df) + nrow(away_team_df))
  # total_yards_allowed_by_team_prev_2_half_seasons[team] = (sum(home_team_df$away_total_Yards) + sum(away_team_df$home_total_Yards) )/
  #   (nrow(home_team_df) + nrow(away_team_df))
}
Score_by_team_list_previous_2_half_seasons<- Score_by_team_list_previous_2_half_seasons[order(-unlist(Score_by_team_list_previous_2_half_seasons))]
score_allowed_by_team_prev_2_half_seasons <- score_allowed_by_team_prev_2_half_seasons[order(unlist(score_allowed_by_team_prev_2_half_seasons))]
# YPP_by_team_list_previous_2_half_seasons <- YPP_by_team_list_previous_2_half_seasons[order(-unlist(YPP_by_team_list_previous_2_half_seasons))]
# YPP_allowed_by_team_prev_2_half_seasons <- YPP_allowed_by_team_prev_2_half_seasons[order(unlist(YPP_allowed_by_team_prev_2_half_seasons))]
# total_yards_by_team_prev_2_half_seasons <- total_yards_by_team_prev_2_half_seasons[order(-unlist(total_yards_by_team_prev_2_half_seasons))]
# total_yards_allowed_by_team_prev_2_half_seasons<- total_yards_allowed_by_team_prev_2_half_seasons[order(unlist(total_yards_allowed_by_team_prev_2_half_seasons))]

team_rank_list_by_score_previous_2_half_seasons = list()
team_rank_by_score_allowed_prev_2_half_seasons = list()
# team_rank_list_by_YPP_previous_2_half_seasons = list()
# team_rank_by_YPP_allowed_prev_2_half_seasons = list()
# team_rank_by_total_Yards_prev_2_half_seasons = list()
# team_rank_by_total_Yards_allowed_prev_2_half_seasons = list()
for(team in division1_teams){
  team_rank_list_by_score_previous_2_half_seasons[team] = which(names(Score_by_team_list_previous_2_half_seasons) == team)
  team_rank_by_score_allowed_prev_2_half_seasons[team] = which(names(score_allowed_by_team_prev_2_half_seasons) == team)
  # team_rank_list_by_YPP_previous_2_half_seasons[team] = which(names(YPP_by_team_list_previous_2_half_seasons) == team)
  # team_rank_by_YPP_allowed_prev_2_half_seasons[team] = which(names(YPP_allowed_by_team_prev_2_half_seasons) == team)
  # team_rank_by_total_Yards_prev_2_half_seasons[team] = which(names(total_yards_by_team_prev_2_half_seasons) == team)
  # team_rank_by_total_Yards_allowed_prev_2_half_seasons[team] = which(names(total_yards_allowed_by_team_prev_2_half_seasons) == team)
}

# team_rank_df = data.frame(
#  Team = division1_teams,
#  Score = unlist(team_rank_list_by_score_previous_2_half_seasons), 
#  Score_allowed = unlist(team_rank_by_score_allowed_prev_2_half_seasons),
#  # total_yards = unlist(team_rank_by_total_Yards_prev_2_half_seasons),
#  # total_Yards_allowed = unlist(team_rank_by_total_Yards_allowed_prev_2_half_seasons),
#  # YPP = unlist(team_rank_list_by_YPP_previous_2_half_seasons),
#  # YPP_allowed = unlist(team_rank_by_YPP_allowed_prev_2_half_seasons)
#   
# )
# team_rank_df = arrange(team_rank_df, Team)

team_rank_list_by_score_previous_2_half_seasons <- team_rank_list_by_score_previous_2_half_seasons[order(unlist(team_rank_list_by_score_previous_2_half_seasons))]
#team_rank_list_by_YPP_previous_2_half_seasons <- team_rank_list_by_YPP_previous_2_half_seasons[order(unlist(team_rank_list_by_YPP_previous_2_half_seasons))]
team_rank_by_score_allowed_prev_2_half_seasons <- team_rank_by_score_allowed_prev_2_half_seasons[order(unlist(team_rank_by_score_allowed_prev_2_half_seasons))]
#team_rank_by_YPP_allowed_prev_2_half_seasons <- team_rank_by_YPP_allowed_prev_2_half_seasons[order(unlist(team_rank_by_YPP_allowed_prev_2_half_seasons))]
#team_rank_by_total_Yards_allowed_prev_2_half_seasons<- team_rank_by_total_Yards_allowed_prev_2_half_seasons[order(unlist(team_rank_by_total_Yards_allowed_prev_2_half_seasons))]
#team_rank_by_total_Yards_prev_2_half_seasons <- team_rank_by_total_Yards_prev_2_half_seasons[order(unlist(team_rank_by_total_Yards_prev_2_half_seasons))]


#game_df_for_2nd_half = filter(division1.games.with.vegas.ou, (Season == current_season) | (Season == Last_season ))
game_df_for_2nd_half = filter(division1.games.with.vegas.ou, (Season == current_season) | (Season == Last_season & Week >= 8))
#game_df_for_2nd_half = filter(division1.games.with.vegas.ou, Season == current_season)
#game_df_cross_seasons = fileter(division1.games.with.vegas.ou, (Season == current_season & Week<=7 ) | (season == Last_season & Week >= 8))
#game_df_current_season_2nd_half = filter(division1.games.with.vegas.ou, (Season == current_season & Week >= 8))
#division1.games.with.vegas.ou = subset(division1.games.with.vegas.ou, select = -vegas_sprd)
test_for_2nd_half = which(game_df_for_2nd_half$Season == current_season & game_df_for_2nd_half$Week >= 8)
game_df_for_2nd_half$test_for_2nd_half = rep(0, nrow(game_df_for_2nd_half))
game_df_for_2nd_half$test_for_2nd_half[test_for_2nd_half] = 1

home_team_score_pred_vect = vector()
away_team_score_pred_vect = vector()
home_team_score_allowed_pred_vect = vector()
away_team_score_allowed_pred_vect = vector()
home_team_score_rank_vect = vector()
away_team_score_rank_vect = vector()
home_team_score_allowed_rank_vect = vector()
away_team_score_allowed_rank_vect = vector()
# home_team_YPP_rank_vect = vector()
# away_team_YPP_rank_vect = vector()
# home_team_YPP_allowed_rank_vect = vector()
# away_team_YPP_allowed_rank_vect = vector()
# home_team_total_Yards_rank_vect = vector()
# away_team_total_Yards_rank_vect = vector()
# home_team_total_Yards_allowed_rank_vect = vector()
# away_team_total_Yards_allowed_rank_vect= vector()

for (i in (1: nrow(game_df_for_2nd_half))){
home_team_score_pred_vect <- c(home_team_score_pred_vect, unlist(Score_by_team_list_previous_2_half_seasons[game_df_for_2nd_half$Team[i]]))
away_team_score_pred_vect <- c(away_team_score_pred_vect, unlist(Score_by_team_list_previous_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
home_team_score_rank_vect <- c(home_team_score_rank_vect, unlist(team_rank_list_by_score_previous_2_half_seasons[game_df_for_2nd_half$Team[i]]))
away_team_score_rank_vect <- c(away_team_score_rank_vect, unlist(team_rank_list_by_score_previous_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
home_team_score_allowed_rank_vect<- c(home_team_score_allowed_rank_vect, unlist(team_rank_by_score_allowed_prev_2_half_seasons[game_df_for_2nd_half$Team[i]]))
home_team_score_allowed_pred_vect <- c(home_team_score_allowed_pred_vect, unlist(Score_by_team_list_previous_2_half_seasons[game_df_for_2nd_half$Team[i]]))
away_team_score_allowed_pred_vect <- c(away_team_score_allowed_pred_vect, unlist(score_allowed_by_team_prev_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
#home_team_YPP_rank_vect<- c(home_team_YPP_rank_vect, unlist(team_rank_list_by_YPP_previous_2_half_seasons[game_df_for_2nd_half$Team[i]]))
#home_team_YPP_allowed_rank_vect <- c(home_team_YPP_allowed_rank_vect, unlist(team_rank_by_YPP_allowed_prev_2_half_seasons[game_df_for_2nd_half$Team[i]]))
away_team_score_allowed_rank_vect <- c(away_team_score_allowed_rank_vect, unlist(team_rank_by_score_allowed_prev_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
# away_team_YPP_rank_vect <- c(away_team_YPP_rank_vect, unlist(team_rank_list_by_YPP_previous_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
# away_team_YPP_allowed_rank_vect<- c(away_team_YPP_allowed_rank_vect, unlist(team_rank_by_YPP_allowed_prev_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
# home_team_total_Yards_rank_vect <- c(home_team_total_Yards_rank_vect, unlist(team_rank_by_total_Yards_prev_2_half_seasons[game_df_for_2nd_half$Team[i]]))
# away_team_total_Yards_rank_vect <- c(away_team_total_Yards_rank_vect, unlist(team_rank_by_total_Yards_prev_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
# home_team_total_Yards_allowed_rank_vect<- c(home_team_total_Yards_allowed_rank_vect, unlist(team_rank_by_total_Yards_allowed_prev_2_half_seasons[game_df_for_2nd_half$Team[i]]))
# away_team_total_Yards_allowed_rank_vect <- c(away_team_total_Yards_allowed_rank_vect, unlist(team_rank_by_total_Yards_allowed_prev_2_half_seasons[game_df_for_2nd_half$Opponent[i]]))
}
game_df_for_2nd_half$home_score_pred = home_team_score_pred_vect
game_df_for_2nd_half$away_score_pred = away_team_score_pred_vect
game_df_for_2nd_half$home_score_allowed_pred = home_team_score_allowed_pred_vect
game_df_for_2nd_half$away_score_allowed_pred = away_team_score_allowed_pred_vect

game_df_for_2nd_half$home_score_rank = home_team_score_rank_vect
game_df_for_2nd_half$away_score_rank = away_team_score_rank_vect
game_df_for_2nd_half$home_score_allowed_rank = home_team_score_allowed_rank_vect
game_df_for_2nd_half$away_score_allowed_rank = away_team_score_allowed_rank_vect
# game_df_for_2nd_half$home_YPP_rank = home_team_YPP_rank_vect
# game_df_for_2nd_half$away_YPP_rank = away_team_YPP_rank_vect
# game_df_for_2nd_half$home_YPP_allowed_rank = home_team_YPP_allowed_rank_vect
# game_df_for_2nd_half$away_YPP_allowed_rank= away_team_YPP_allowed_rank_vect
# game_df_for_2nd_half$home_total_Yards_rank = home_team_total_Yards_rank_vect
# game_df_for_2nd_half$home_total_Yards_allowed_rank = home_team_total_Yards_allowed_rank_vect
# game_df_for_2nd_half$away_total_Yards_rank = away_team_total_Yards_rank_vect
# game_df_for_2nd_half$away_total_Yards_allowed_rank = away_team_total_Yards_allowed_rank_vect

# game_df_for_2nd_half$home_score_rank =  game_df_for_2nd_half$home_score_rank - 66
# game_df_for_2nd_half$away_score_rank = game_df_for_2nd_half$away_score_rank - 66
# game_df_for_2nd_half$home_score_allowed_rank = game_df_for_2nd_half$home_score_allowed_rank - 66
# game_df_for_2nd_half$away_score_allowed_rank = game_df_for_2nd_half$away_score_allowed_rank - 66

game_df_for_2nd_half

}


###function for h
game_df_for_2nd_half <- (game_df_update_ou_2nd_half_with_score_ranking(division1.games.with.vegas.ou, 2017, division1_teams))
# svm.ou = svm(Score_total~home_score_rank + away_score_rank + home_score_allowed_rank + away_score_allowed_rank+ vegas_ou
#             ,
#              data = game_df_for_2nd_half[which(game_df_for_2nd_half$test_for_2nd_half == 0),])
# 
# get_correction(game_df_for_2nd_half[-test_for_2nd_half,], svm.ou, game_df_for_2nd_half$vegas_ou[-test_for_2nd_half], 
#                game_df_for_2nd_half$Score_total[-test_for_2nd_half],1)
# 
# get_correction(game_df_for_2nd_half[which(game_df_for_2nd_half$test_for_2nd_half == 1),], svm.ou, 
#                game_df_for_2nd_half$vegas_ou[which(game_df_for_2nd_half$test_for_2nd_half == 1)], 
#                game_df_for_2nd_half$Score_total[which(game_df_for_2nd_half$test_for_2nd_half == 1)],0)



library(gbm)

LaplaceBoost.prev_2_seasons = gbm(Score_total~ home_score_rank + away_score_rank + home_score_allowed_rank + away_score_allowed_rank+ vegas_ou,
                                  data=game_df_for_2nd_half[which(game_df_for_2nd_half$test_for_2nd_half == 0),], distribution=
                                    "laplace",n.trees =5000, interaction.depth =2, shrinkage = 0.003)


# LaplaceBoost.prev_2_seasons = gbm(Score_total~ home_score_pred + away_score_pred + home_score_allowed_pred + away_score_allowed_pred+ vegas_ou,
#                                   data=game_df_for_2nd_half[which(game_df_for_2nd_half$test_for_2nd_half == 0),], distribution=
#                                     "laplace",n.trees =5000, interaction.depth =2, shrinkage = 0.003)
# get_correction(game_df_for_2nd_half[which(game_df_for_2nd_half$test_for_2nd_half == 0),], LaplaceBoost.prev_2_seasons, 
#                game_df_for_2nd_half$vegas_ou[which(game_df_for_2nd_half$test_for_2nd_half == 0)], 
#                game_df_for_2nd_half$Score_total[which(game_df_for_2nd_half$test_for_2nd_half == 0)],1)
# 

get_correction(game_df_for_2nd_half[which(game_df_for_2nd_half$test_for_2nd_half == 1),], LaplaceBoost.prev_2_seasons, 
               game_df_for_2nd_half$vegas_ou[which(game_df_for_2nd_half$test_for_2nd_half == 1)], 
               game_df_for_2nd_half$Score_total[which(game_df_for_2nd_half$test_for_2nd_half == 1)],1)




##########################################################################################################################
#for first half of season, update similar to that used in updating with 2nd half
#here uses previous season's score and score_allowed
#outputs a dataframe with relevant games


game_df_update_1st_half_with_score_ranking <- function(division1.games.with.vegas.ou, current_season, division1_teams){
#current_season = 2018
Last_season = current_season - 1
second_to_last_season = current_season - 2

Score_by_team_list_previous_3_half_seasons = list()
score_allowed_by_team_prev_3_half_seasons = list()


for(team in division1_teams){
   home_team_df = filter(division1.games.with.vegas.ou, ((Season == second_to_last_season & Week > 7) | (Season = Last_season)) & Team == team)
   away_team_df = filter(division1.games.with.vegas.ou, ((Season == second_to_last_season & Week > 7) | (Season = Last_season)) & Opponent == team)

  # home_team_df = filter(division1.games.with.vegas.ou, Season == Last_season & Team == team)
  # away_team_df = filter(division1.games.with.vegas.ou, Season == Last_season & Opponent == team)
  Score_by_team_list_previous_3_half_seasons[team] = (sum(home_team_df$Score) + sum(away_team_df$Score_allowed) )/ (nrow(home_team_df) + nrow(away_team_df))
  score_allowed_by_team_prev_3_half_seasons[team] = (sum(home_team_df$Score_allowed) + sum(away_team_df$Score))/ (nrow(home_team_df) + nrow(away_team_df))
  
}
Score_by_team_list_previous_3_half_seasons<- Score_by_team_list_previous_3_half_seasons[order(-unlist(Score_by_team_list_previous_3_half_seasons))]
score_allowed_by_team_prev_3_half_seasons <- score_allowed_by_team_prev_3_half_seasons[order(unlist(score_allowed_by_team_prev_3_half_seasons))]


Score_by_team_list_previous_3_half_seasons<- Score_by_team_list_previous_3_half_seasons[order(-unlist(Score_by_team_list_previous_3_half_seasons))]
score_allowed_by_team_prev_3_half_seasons <- score_allowed_by_team_prev_3_half_seasons[order(unlist(score_allowed_by_team_prev_3_half_seasons))]

team_rank_list_by_score_previous_3_half_seasons = list()
team_rank_by_score_allowed_prev_3_half_seasons = list()
for(team in division1_teams){
  team_rank_list_by_score_previous_3_half_seasons[team] = which(names(Score_by_team_list_previous_3_half_seasons) == team)
  team_rank_by_score_allowed_prev_3_half_seasons[team] = which(names(score_allowed_by_team_prev_3_half_seasons) == team)
}

game_df_for_1st_half =  filter(division1.games.with.vegas.ou, (Season == current_season & Week <= 7) | (Season == second_to_last_season & Week >= 8)|
                             (Season == Last_season))

# game_df_for_1st_half =  filter(division1.games.with.vegas.ou, (Season == current_season & Week <= 7) |
#                                  (Season == Last_season))
test_for_1st_half = which(game_df_for_1st_half$Season == current_season)
game_df_for_1st_half$test_for_1st_half = rep(0, nrow(game_df_for_1st_half))
game_df_for_1st_half$test_for_1st_half[test_for_1st_half] = 1

home_team_score_rank_vect = vector()
away_team_score_rank_vect = vector()
home_team_score_allowed_rank_vect = vector()
away_team_score_allowed_rank_vect = vector()

home_score_pred_vect = vector()
away_score_pred_vect = vector()
home_score_allowed_pred_vect = vector()
away_score_allowed_pred_vect = vector()
for (i in (1: nrow(game_df_for_1st_half))){
  home_team_score_rank_vect <- c(home_team_score_rank_vect, unlist(team_rank_list_by_score_previous_3_half_seasons[game_df_for_1st_half$Team[i]]))
  away_team_score_rank_vect <- c(away_team_score_rank_vect, unlist(team_rank_list_by_score_previous_3_half_seasons[game_df_for_1st_half$Opponent[i]]))
  home_team_score_allowed_rank_vect<- c(home_team_score_allowed_rank_vect, unlist(team_rank_by_score_allowed_prev_3_half_seasons[game_df_for_1st_half$Team[i]]))
  away_team_score_allowed_rank_vect<- c(away_team_score_allowed_rank_vect, unlist(team_rank_by_score_allowed_prev_3_half_seasons[game_df_for_1st_half$Opponent[i]]))
  
  home_score_pred_vect <- c(home_score_pred_vect, unlist(Score_by_team_list_previous_3_half_seasons[game_df_for_1st_half$Team[i]]))
  away_score_pred_vect <- c(away_score_pred_vect, unlist(Score_by_team_list_previous_3_half_seasons[game_df_for_1st_half$Opponent[i]]))
  home_score_allowed_pred_vect <- c(home_score_allowed_pred_vect,unlist(Score_by_team_list_previous_3_half_seasons[game_df_for_1st_half$Team[i]]))
  away_score_allowed_pred_vect <- c(away_score_allowed_pred_vect, unlist(Score_by_team_list_previous_3_half_seasons[game_df_for_1st_half$Opponent[i]]))
}

game_df_for_1st_half$home_score_rank = home_team_score_rank_vect
game_df_for_1st_half$away_score_rank = away_team_score_rank_vect
game_df_for_1st_half$home_score_allowed_rank = home_team_score_allowed_rank_vect
game_df_for_1st_half$away_score_allowed_rank = away_team_score_allowed_rank_vect

game_df_for_1st_half$home_score_pred = home_score_pred_vect
game_df_for_1st_half$away_score_pred = away_score_pred_vect
game_df_for_1st_half$home_score_allowed_pred = home_score_allowed_pred_vect
game_df_for_1st_half$away_score_allowed_pred = away_score_allowed_pred_vect

game_df_for_1st_half
}

game_df_for_1st_half = game_df_update_1st_half_with_score_ranking(division1.games.with.vegas.ou, 2018, division1_teams)
test_for_1st_half = which(game_df_for_1st_half$test_for_1st_half == 1)
LaplaceBoost.prev_3_seasons = gbm(Score_total~ home_score_rank + away_score_rank + home_score_allowed_rank + away_score_allowed_rank+ vegas_ou,
                                  data=game_df_for_1st_half[-test_for_1st_half,], distribution=
                                    "laplace",n.trees =5000, interaction.depth =2, shrinkage = 0.003)
# LaplaceBoost.prev_3_seasons = gbm(Score_total~ home_score_pred + away_score_pred + home_score_allowed_pred + away_score_allowed_pred+ vegas_ou,
#                                   data=game_df_for_1st_half[-test_for_1st_half,], distribution=
#                                     "laplace",n.trees =5000, interaction.depth =3, shrinkage = 0.003)

# get_correction(game_df_for_1st_half[-test_for_1st_half,], LaplaceBoost.prev_3_seasons, game_df_for_1st_half$vegas_ou[-test_for_1st_half], 
#                game_df_for_1st_half$Score_total[-test_for_1st_half],1)


get_correction(game_df_for_1st_half[test_for_1st_half,], LaplaceBoost.prev_3_seasons, game_df_for_1st_half$vegas_ou[test_for_1st_half], 
               game_df_for_1st_half$Score_total[test_for_1st_half],1)


###below shows we can't use naive score_pred of home and away teams to predict the score_diff, both correlation and correction vectors are unsatisfactory
# cor(game_df_for_2nd_half$vegas_ou, game_df_for_2nd_half$home_score_pred + game_df_for_2nd_half$away_score_pred)
# game_df_for_2nd_half$score_total_pred = game_df_for_2nd_half$home_score_pred + game_df_for_2nd_half$away_score_pred
# naive_correction_vector = sign((game_df_for_2nd_half$score_total_pred - game_df_for_2nd_half$vegas_ou) *
#                                  (game_df_for_2nd_half$Score_total - game_df_for_2nd_half$vegas_ou))
# length(which(naive_correction_vector == 1))
# length(naive_correction_vector)
# plot(unlist(Score_by_team_list_previous_2_half_seasons))
# plot(unlist(YPP_by_team_list_previous_2_half_seasons))
#regular_season_raw = read.csv("regular_season.csv", stringsAsFactors = F)
division1.games.with.vegas.ou$playsPG_weighted = division1.games.with.vegas.ou$PassingAttempts + division1.games.with.vegas.ou$RushingAttempts * 0.6
division1.games.with.vegas.ou$playsPG_weighted_allowed = division1.games.with.vegas.ou$PassingAttempts_allowed + division1.games.with.vegas.ou$RushingAttempts_allowed* 0.6
cor(division1.games.with.vegas.ou$Score_total, division1.games.with.vegas.ou$playsPG_weighted + division1.games.with.vegas.ou$playsPG_weighted_allowed)

division1.games.with.vegas.ou$home_total_Yards = division1.games.with.vegas.ou$RushingAttempts*division1.games.with.vegas.ou$RushingYardsPerAttempt +
                                                division1.games.with.vegas.ou$PassingAttempts * division1.games.with.vegas.ou$PassingYardsPerAttempt

division1.games.with.vegas.ou$away_total_Yards = division1.games.with.vegas.ou$RushingAttempts_allowed * division1.games.with.vegas.ou$RushingYardsPerAttempt_allowed +
  division1.games.with.vegas.ou$PassingAttempts_allowed * division1.games.with.vegas.ou$PassingYardsPerAttempt_allowed
cor(division1.games.with.vegas.ou$Score_total, division1.games.with.vegas.ou$home_total_Yards + division1.games.with.vegas.ou$away_total_Yards)
