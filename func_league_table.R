#Author - Anthony Matthews
#Date - 25/07/2019
#Purpose - function to return league table for any team at any date
#Returned - Printed league table, league table in data frame, and tier in value
#Known problems - doesn't take point deductions into account
#               - position not working
#               - funny output in console


league_table <- function(team, date = NULL, gp = NULL, season = NULL) {

    team_current <- team
    
    if(!is.null(date)) {
        #convert the team and date to strings
        date_current <- as.Date(date,'%d/%m/%Y')
        
        #Find the season (if month is before August, then season year is the one before)
        if (as.numeric(format(date_current, '%m')) < 8) {
            season_current <- as.numeric(format(date_current, '%Y')) -1
        } else {
            season_current <- as.numeric(format(date_current, '%Y'))
        }
    }
    
    if(!is.null(gp) & !is.null(season)) {
        season_current <- as.numeric(format(season))
        date_find <- all_teams %>% 
            filter(team == team_current, total_GP == gp, season == season_current)
        date_current <- as.Date(date_find$date[1])
        rm(date_find)
    }
        
    #Identify the tier
    season_tier <- all_teams %>%
        filter(team==team_current & season==season_current)
    
    tier_current <- season_tier$tier[1]
    rm(season_tier)
    
    
    #Get all the teams matched within that season and that tier
    all_teams_specific <- all_teams %>%
        filter(tier==tier_current & season==season_current) %>%
        arrange(date) %>%
        select(-c(result, home, visitor, season)) %>%
        select(c(team, date, everything()))
    
    table_current <- all_teams_specific %>%
        group_by(team) %>%
        mutate(daysfromgame = date_current-as.Date(date)) %>%
        filter(daysfromgame>=0) %>%
        arrange(desc(as.numeric(daysfromgame))) %>%
        slice(n=n()) %>%
        ungroup() %>% 
        arrange(desc(total_Points), desc(total_GD), desc(total_GF)) %>%
        select(-c(date, daysfromgame)) %>%
        rename_all(~ str_remove(.x, "total_")) 
    
    return(print(table_current, n=Inf))
    rm(tier_current, table_current)

} #End of function


