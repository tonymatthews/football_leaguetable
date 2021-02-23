

#Create list with all teams
teams <- sort(unique(na.omit(rawdata$home)))
  
team_results <- function(i) {
  rawdata %>% 
    
  #Keep only the teams results
  filter(home==i | visitor==i) %>%

  mutate(team=i) %>%
  
  #Order by season then date 
  arrange(Season, Date) %>%

  #Everything below is done witnin each season
  group_by(Season) %>%

  #Create games played
  mutate(total_GP = (order_by(Date, cumsum(
                      ifelse(!is.na(Season), 1, 0
        ))))) %>%

  #Change the result variable to be 1 for win, 0 for draw, and -1 for loss
  mutate(result = ifelse(home==i & result=="H", 1,
                  ifelse(home!=i & result=="A", 1,
                  ifelse(home==i & result=="A", -1,
                  ifelse(home!=i & result=="H", -1 ,
                  ifelse(result=="D", 0, NA
        )))))) %>%   

  #Create total won, drew, and lost
  mutate(total_Wins = order_by(Date, cumsum(
                                      ifelse(result==1, 1, 0
        )))) %>%

 
  mutate(total_Draws = order_by(Date, cumsum(
                                        ifelse(result==0, 1, 0
        )))) %>%

  mutate(total_Loss = order_by(Date, cumsum(
                                      ifelse(result==-1, 1, 0
        )))) %>%

  #Create total goals for and total goals against
  mutate(total_GF = order_by(Date, cumsum(
                                    ifelse(home==i, hgoal,
                                    ifelse(home!=i, vgoal, NA
        ))))) %>%

  mutate(total_GA = order_by(Date, cumsum(
                                    ifelse(home!=i, hgoal,
                                    ifelse(home==i, vgoal, NA
        ))))) %>%

  #Create the running total goal difference after each game (data are coded to make original gd for home team)
  mutate(total_GD = order_by(Date, cumsum(
                                      ifelse(home==i, goaldif,
                                      ifelse(home!=i, -goaldif, NA
        ))))) %>%

  
  #Give 3 points for a win, 1 for a draw and 0 for a loss
  mutate(points = 
              ifelse(result==1 & Season>=1981, 3,
              ifelse(result==1 & Season<1981, 2,
              ifelse(result==-1 ,0,
              ifelse(result==0, 1, NA
        ))))) %>%

  #Create the total number of points after each game within that season
  mutate(total_Points = order_by(Date, cumsum(points))) %>%


  #Rename Date and Season
  rename(date=Date) %>%
  rename(season=Season) %>%

  #Drop variables not needed
  select(-c(FT,totgoal,goaldif, hgoal, vgoal, division, points)) %>%
  select(c(team, tier, date, result, home, visitor, everything()))
}


all_teams <- bind_rows(lapply(teams, team_results))
rm(team_results)

    