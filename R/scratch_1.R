###############################################################################################################

dataShots <- read.csv('data/matchShots.csv', stringsAsFactors = FALSE)
unique(dataShots$h_team)

dataShots[dataShots$h_team=='RasenBallsport Leipzig',]
dataShots_RBL <- dataShots[dataShots$h_team=='RasenBallsport Leipzig' | dataShots$a_team=='RasenBallsport Leipzig',]

understatr::get_player_matches_stats(player_id=65)

team_players_stats <- get_team_players_stats(team_name = "RasenBallsport Leipzig", year = 2019)

team_players_stats$player_id

dataMatchIDs <- read.csv('data/matchIds.csv', stringsAsFactors = FALSE)

View(dataShots)

dataShots$team <- ifelse(dataShots$h_a=='h',dataShots$h_team,dataShots$a_team)
dataShots <- left_join(dataShots, select(dataMatchIDs, match_id,year), by='match_id')

unique(left_join(dataShots, select(dataMatchIDs, match_id,year), by='match_id')$year)

View(get_team_players_stats(team_name='Aston Villa', year=2018))

View(get_team_players_stats(team_name='Aston Villa', year=2018))

get_player_seasons_stats(player_id = 65)


library(ggsoccer)
ggplot(dataShots[dataShots$team=='RasenBallsport Leipzig' & dataShots$year=='2019',]) +
  annotate_pitch() + 
  theme_pitch() +
  aes(x=X*100, y=Y*100, size=xG) +
  geom_point(alpha=0.3)


setdiff(c(1:20000),unique(dataShots$match_id))

source('R/loading_data.R')
getData(updateFile=TRUE)

matchIDs <- read.csv('data/matchIds.csv', stringsAsFactors = FALSE)

View(matchIDs %>%
  group_by(league_name, year) %>%
  summarise(n()))

#dataShots <- dataShots[!dataShots$year=='NA',]
dataShots <- dataShots[!is.na(dataShots$year),] # add to 'main'
rbLeigpzie <- dataShots[dataShots$team=='RasenBallsport Leipzig' & dataShots$year=='2019',]
rbLeigpzie$X <- rbLeigpzie$X*100
rbLeigpzie$Y <- rbLeigpzie$Y*100

rbLeigpzie$side <- ifelse(rbLeigpzie$Y > 50,'L','R')
table(rbLeigpzie[rbLeigpzie$situation!='Penalty','side'])

ggplot(rbLeigpzie[rbLeigpzie$side=='L' & rbLeigpzie$situation!='Penalty',]) +
  annotate_pitch() + 
  theme_pitch() +
  aes(x=X, y=Y, size=xG) +
  geom_point(alpha=0.3)

ggplot(rbLeigpzie[rbLeigpzie$side=='R' & rbLeigpzie$situation!='Penalty',]) +
  annotate_pitch() + 
  theme_pitch() +
  aes(x=X, y=Y, size=xG) +
  geom_point(alpha=0.3)

sum(rbLeigpzie[rbLeigpzie$situation!='Penalty' & rbLeigpzie$side=='R','xG'])
sum(rbLeigpzie[rbLeigpzie$situation!='Penalty' & rbLeigpzie$side=='L','xG'])

rbLeigpzie %>% 
  group_by(match_id) %>%
  summarise(xG90=sum(xG),
            goals=n_distinct(id[result=='Goal'])) %>%
  left_join(matchIDs[,c('match_id','date')], by='match_id') %>%
  ggplot() +
  aes(x=as.Date(date)) +
  geom_line(aes(y=goals), color='red') +
  geom_line(aes(y=xG90), color='steelblue')

#####################################################################################
# whole bundesliga 2019
bundesliga_2019 <- dataShots[dataShots$match_id %in% matchIDs[matchIDs$league_name=='Bundesliga' & matchIDs$year==2019,'match_id'],]
bundesliga_2019

bundesliga_2019[bundesliga_2019$h_team=='Bayern Munich' | bundesliga_2019$a_team=='Bayern Munich',] %>%
  group_by(match_id) %>%
  summarise(goals=n_distinct(id[result=='Goal'&team==h_team]))

rbind(
bundesliga_2019 %>%
  group_by(match_id) %>%
  summarise(goalsfor=n_distinct(id[result=='Goal' & team==h_team]),
            xG90for=sum(xG[team==h_team]),
            goalsagainst=n_distinct(id[result=='Goal' & team==a_team]),
            xG90against=sum(xG[team==a_team]),
            team=unique(h_team)),
bundesliga_2019 %>%
  group_by(match_id) %>%
  summarise(goalsfor=n_distinct(id[result=='Goal' & team==a_team]),
            xG90for=sum(xG[team==a_team]),
            goalsagainst=n_distinct(id[result=='Goal' & team==h_team]),
            xG90against=sum(xG[team==h_team]),
            team=unique(a_team))
) %>%
  left_join(matchIDs[,c('match_id','date')], by='match_id') %>%
  ggplot() +
  aes(x=as.Date(date)) +
  geom_line(aes(y=goalsfor), color='red') +
  geom_line(aes(y=xG90for), color='steelblue') +
  geom_line(aes(y=goalsagainst), color='yellow') +
  geom_line(aes(y=xG90against), color='green') +
  facet_wrap(vars(team))
