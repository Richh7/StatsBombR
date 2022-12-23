library(StatsBombR)

Comp <- FreeCompetitions() %>%
  filter(competition_id==2)

Matches <- FreeMatches(Comp)

Events <- free_allevents(MatchesDF = Matches, Parallel = T)
  
Events <- allclean(Events) %>%
  filter(player.name != "NA") %>%
  group_by(player.name)

Shots <- Events %>%
  filter(shot.type.id != "NA") %>%
  count(player.name)
colnames(Shots)[2] ="number.shots"

Passes <- Events %>%
  filter(pass.type.id != "NA") %>%
  count(player.name)
colnames(Passes)[2] ="number.passes"

Dribblings <- Events %>%
  filter(dribble.outcome.id != "NA") %>%
  count(player.name)
colnames(Dribblings)[2] ="number.dribblings"

Merge <- data.frame(unique(Events$player.name))
colnames(Merge)[1] ="player.name"

Merge <- Merge %>% 
  left_join(Shots, by="player.name") %>%
  left_join(Passes, by="player.name") %>%
  left_join(Dribblings, by="player.name") %>%
  filter(number.shots != "NA" & number.dribblings != "NA" & number.passes != "NA")

tot <- data.frame()
for (i in 1:nrow(Merge)) {
  tmp <- Merge[i, 2] + Merge[i, 3] + Merge[i, 4]
  tot <- rbind(tot, tmp)
}

Merge <- cbind(Merge, tot)
