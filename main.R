# Loading the libraries
source('R/libraries.R')

#########################
#### 1- Season analysis ######
# Loading data
source('R/get_season.R')
league_stats_2019 <- get_league_teams_stats(league_name = "Bundesliga", year = 2019)
season_19 <- get_season(league_stats_2019)
bundesliga_all <- rbind(season_19)

# Platonic table
source('R/benchmarks.R')
platonictable_ger <- build_platonictable(bundesliga_all)

# Ranking lollipop
source('R/platonictable_lollipop.R', print.eval=TRUE)
lollipoprank(platonictable_ger)

# Win/lose/draw piechart
source('R/platonictable_piechart.R', print.eval=TRUE)
winlosedraw(platonictable_ger)

# Goal difference
source('R/platonictable_pyramid.R', print.eval=TRUE)
pyramid(platonictable_ger)

# Goals for minus xG
source('R/platonictable_gxg.R', print.eval=TRUE)
gxg(platonictable_ger)

# Attitude and work
source('R/platonictable_work.R', print.eval=TRUE)
workrate(platonictable_ger)

########################################################################################################################
# match data
source('R/loading_data.R')
getData(updateFile=TRUE)

matchIDs <- read.csv('data/matchIds.csv', stringsAsFactors = FALSE)
matchIDs %>%
  group_by(league_name, year) %>%
  summarise(n())

dataShots <- read.csv('data/matchShots.csv', stringsAsFactors = FALSE)
dataShots$team <- ifelse(dataShots$h_a=='h',dataShots$h_team,dataShots$a_team)
dataShots <- left_join(dataShots, select(dataMatchIDs, match_id,year), by='match_id')
ggplot(dataShots[dataShots$team=='RasenBallsport Leipzig' & dataShots$year=='2019',]) +
  annotate_pitch() + 
  theme_pitch() +
  aes(x=X*100, y=Y*100, size=xG) +
  geom_point(alpha=0.3)

