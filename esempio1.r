library(ggplot2)
library(StatsBombR)


Comp <- FreeCompetitions()

Matches <- FreeMatches(Comp) %>%
  group_by(season.season_id)
  
SplitSeasons <- group_split(Matches)


#Pulling down Lionel Messi minutes in all seasons, then we sort the dataframe with ascendant order, based on season
MessiMinutesSeason <- data.frame()

for (i in (1:length(SplitSeasons))) {
  Events <- free_allevents(MatchesDF = SplitSeasons[[i]], Parallel = T)
  Events <- formatelapsedtime(Events)
  MessiMinutesPlayed <- get.minutesplayed(Events) %>%
    filter(player.id == 5503) %>%
    summarise(player.id = 5503,
              season = unique(SplitSeasons[[i]]$season.season_name),
              tot.minutes = sum(MinutesPlayed),
              matches = n(),
              avg.minutes = tot.minutes / matches)
  if (MessiMinutesPlayed$matches > 0) {
    MessiMinutesSeason <- rbind(MessiMinutesSeason, MessiMinutesPlayed)
  }
}

MessiMinutesSeason <- MessiMinutesSeason %>%
  arrange(season)

view(MessiMinutesSeason)


#Now we can finally plot results
ggplot(MessiMinutesSeason, mapping=aes(x=season, y=avg.minutes, group=1)) +
  geom_line(color="blue") +
  geom_point() +
  geom_text(aes(x=season, y=avg.minutes, label=matches),size = 2.5, nudge_y = 2, nudge_x = -0.05) +
  ylab('Media minuti per partita') + 
  xlab('Stagione calcistica')