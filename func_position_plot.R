#Author - Anthony Matthews
#Date started - 26/07/2019
#Purpose - function to return plot of a teams progress over a season at a certain date
#Returned - Plot of progress
#Known problems 

    
    #Team and date to values
    team_current <- "Sunderland"
    date_current <- as.Date("31/07/2019",'%d/%m/%Y')

    
    #Find the season (if month is before August, then season year is the one before)
    if (as.numeric(format(date_current, '%m')) < 8) {
                season_current <- as.numeric(format(date_current, '%Y')) -1
    } else {
                season_current <- as.numeric(format(date_current, '%Y'))
    }
     
    
    #Identify the tier
    season_tier <- all_teams %>%
                    filter(team==team_current & season==season_current)
                    
    tier_current <- all_teams$tier[1]
    rm(season_tier)    
    
    #Get the date of all of that teams game during that season, and put them into a list
    team_games_current_season <- all_teams %>%
                                  filter(team==team_current & season==season_current & date<=date_current)

    game_dates <- team_games_current_season$date
    rm(team_games_current_season)
    
    #Create the league table at each date of games, and keep the position, and no of games played
    game <- 0
    
    for (i in game_dates) {
        league_table("Sunderland", i)
    }
    