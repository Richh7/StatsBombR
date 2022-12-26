library(StatsBombR)
library(ggtern)

Comp <- FreeCompetitions() %>%
  filter(competition_id==11 & season_id==90)

Matches <- FreeMatches(Comp)

Events <- free_allevents(MatchesDF = Matches, Parallel = T)
  
Events <- allclean(Events) %>%
  filter(player.name != "NA") %>%
  group_by(player.name, team.name)

Shots <- Events %>%
  filter(shot.type.id != "NA") %>%
  count(player.name)
colnames(Shots)[3] ="n.shots"

Passes <- Events %>%
  filter(pass.type.id != "NA") %>%
  count(player.name)
colnames(Passes)[3] ="n.passes"

Dribblings <- Events %>%
  filter(dribble.outcome.id != "NA") %>%
  count(player.name)
colnames(Dribblings)[3] ="n.dribblings"

Results <- data.frame(unique(Events$player.name))
colnames(Results)[1] ="player.name"

Results <- Results %>%
  left_join(Shots, by="player.name") %>%
  left_join(Passes, by="player.name") %>%
  left_join(Dribblings, by="player.name") %>%
  filter(n.shots != "NA" & n.dribblings != "NA" & n.passes != "NA")

Percent <- data.frame()
for (i in 1:nrow(Results)) {
  Tmp <- Results[i, 3] + Results[i, 5] + Results[i, 7]
  PercS <- (Results[i, 3] / Tmp) * 100
  PercP <- (Results[i, 5] / Tmp) * 100
  PercD <- (Results[i, 7] / Tmp) * 100
  SumPerc <- PercS + PercP + PercD
  Values <- c(Tmp, PercS, PercP, PercD, SumPerc)
  Percent <- rbind(Percent, Values)
}

colnames(Percent)[1] ="tot.sum"
colnames(Percent)[2] ="p.shots"
colnames(Percent)[3] ="p.passes"
colnames(Percent)[4] ="p.dribblings"
colnames(Percent)[5] ="tot.p.sum"

Results <- cbind(Results, Percent)

ggtern(data = Results, aes(x = p.passes, y = p.dribblings, z = p.shots)) + 
  geom_point(size = 2, 
             shape = 21, 
             color = "black",
             fill="black")

playerHightlight <- Results %>%
  filter(player.name == "Lionel Messi")

ggtern() + 
  geom_point(data = Results, aes(x = p.passes, y = p.dribblings, z = p.shots),
             size = 2, 
             shape = 21, 
             color = "grey",
             fill= "grey") +
  geom_point(data = playerHightlight, aes(x = perc.passes, y = perc.dribblings, z = perc.shots), 
             size = 2, 
             shape = 21, 
             color = "black",
             fill= "red")

#aggiungere colonna squadra
ClubSelect <- Results %>%
  filter(player.name == "Barcelona")

ggtern() + 
  geom_point(data = Results, aes(x = perc.passes, y = perc.dribblings, z = perc.shots),
             size = 2, 
             shape = 21, 
             color = "grey",
             fill= "grey") + 
  geom_point(data = ClubHightlight, aes(x = perc.passes, y = perc.dribblings, z = perc.shots), 
             size = 2, 
             shape = 21, 
             color = "black",
             fill= "red")

ResultsKmean <- Results[6:8]

# use the kmeans() function to perform the analysis, we chose to devide the data into 6 'clusters' 
kmeans <- kmeans(ResultsKmean, 3)

# We want to bind the kMeans Clusters to the original dataframe 
Results$Cluster <- as.character(kmeans$cluster)

ggtern(data = Results, aes(x = perc.passes, y = perc.dribblings, z = perc.shots)) + 
  geom_point(aes(fill = Cluster), size = 2, 
             shape = 21, 
             color = "black")
