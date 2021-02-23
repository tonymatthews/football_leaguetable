
# load packages (if needed)
library(devtools)
library(here)
library(engsoccerdata)
library(tidyverse)
library(knitr)
library(printr)
library(kableExtra)
library(R.utils)
library(here)
library(shiny)
library(htmltools)


#### DATA PREP ######

rawdata <- rbind(as_tibble(england), as_tibble(england_current()))

#Create a dataset that includes the results for each team each season (with # points for win correct irt year), gd, etc. 
source("scripts/cr_all_teams.R")



#### CREATE FUNCTIONS #####

#LEAGUE TABLE FUNCTION
#Create a function that prints league table, and stores the table as a data frame, at any date for a team
source(here("scripts", "func_league_table.R"))

#test
league_table("Sunderland", date = "07/02/2021")

league_table(team = "Sunderland", gp = "33", season = "2001")

lapply(c("07/02/2021", "07/02/2020", "07/02/2019"),league_table, team="Sunderland")

#TEAM HISTORY FUNCTION
#function that returns all the final positions from all seasons for a certain team
source("scripts/func_team_history.R")
#test
team_history("Sunderland")
#POSITION PLOT FUNCTION
source("scripts/func_position_plot")


