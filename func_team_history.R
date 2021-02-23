#Author - Anthony Matthews
#Date - 25/07/2019
#Purpose - To generate a teams historical end of season positions and statistics for every year they have played
#Returned - Printed history, and history in data frame
#Known problems - doesn't take point deductions into account
#               - Only returning first season

team_history <- function(team) {

    #Set the team to a value
    team_current <- team
  
    #Load all_teams data
    teamhist <- all_teams %>% 
            filter(team==team_current) %>% 
            group_by(season) %>% 
            filter(total_GP==min(total_GP)) %>% 
            select(c(season))
    
    #Create a vector with all seasons the team has played
    all_seasons <- c(teamhist$season[])

    #Create an empty data frame that will hold the teams history at the end of season
    history <- data.frame()
    
    #Loop through every season that the team has played in, and get final position and statistics
    for (i in all_seasons) {
          #Update this function so that the final table is put in a data frame, then this can be used
          league_table(team_current, paste('31/07/',i+1,sep=""))

          total_teams <- max(table_current$Pos)
          
          table_current_team <- table_current %>% 
                                filter(team==team_current)  %>% 
                                mutate(Season=i)  %>%
                                mutate(Tier=tier_current) %>%
                                mutate(LeagueSize=total_teams) %>%
                                select(c(Season, Tier, Pos,LeagueSize, everything())) %>% 
                                select(-c(team))
          
          history <- rbind(history, table_current_team)
          rm(table_current_team, total_teams)
    } #end for (i in all_seasons)
    
    team_history <<- history
    return(kable(history, caption = paste('End of season history of',team_current), format='pandoc'))

} #end function(team_history)                       
      
   