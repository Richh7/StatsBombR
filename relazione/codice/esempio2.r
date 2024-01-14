library(ggtern)
library(StatsBombR)


#Filtering season by competition (Spain - La Liga) and season_id (2020/21)
Comp <- FreeCompetitions() %>%
  filter(competition_id == 11 & season_id == 90)

Matches <- FreeMatches(Comp)


#Computing separately shots, passes and dribblings data
Events <- free_allevents(MatchesDF = Matches, Parallel = T) %>%
  allclean() %>%
  filter(player.name != "NA") %>%
  group_by(player.name, team.name)

Shots <- Events %>%
  filter(shot.type.id != "NA") %>%
  count(player.name)
colnames(Shots)[3] = "n.shots"

Passes <- Events %>%
  filter(pass.type.id != "NA") %>%
  count(player.name)
colnames(Passes)[3] = "n.passes"

Dribblings <- Events %>%
  filter(dribble.outcome.id != "NA") %>%
  count(player.name)
colnames(Dribblings)[3] = "n.dribblings"

Results <- data.frame(unique(Events$player.name))
colnames(Results)[1] = "player.name"

#Merging Shots, Dribblings and Passes in Results, keeping players who played in the same team the whole year
Results <- Results %>%
  left_join(Shots, by = "player.name") %>%
  left_join(Passes, by = "player.name") %>%
  left_join(Dribblings, by = "player.name") %>%
  na.omit() %>%
  filter(team.name.x == team.name.y & team.name.y == team.name & team.name == team.name.x) %>%
  select(-team.name.y, -team.name)

#Computing percentages of shots, dribblings and passes and then we merge them again in Results 
Percent <- data.frame()
for (i in 1:nrow(Results)) {
  Tmp <- Results[i, 3] + Results[i, 4] + Results[i, 5]
  PercS <- (Results[i, 3] / Tmp) * 100
  PercP <- (Results[i, 4] / Tmp) * 100
  PercD <- (Results[i, 5] / Tmp) * 100
  SumPerc <- PercS + PercP + PercD
  Values <- c(Tmp, PercS, PercP, PercD, SumPerc)
  Percent <- rbind(Percent, Values)
}

Results <- cbind(Results, Percent)

colnames(Results) <- c("player.name", "team.name", "n.shots", "n.passes", "n.dribblings", 
                      "tot.sum", "p.shots", "p.passes", "p.dribblings", "tot.p.sum")

head(Results)


#We are ready to plot results
ggtern(data = Results, aes(x = p.passes, y = p.dribblings, z = p.shots)) + 
  geom_point(size = 2, 
             shape = 21, 
             color = "black",
             fill = "black") +
  theme_rgbg() +
  theme_showarrows()


#Let's highlight the point which represents the so called player "Lionel Messi" and "Marc-André ter Stegen"
playerHightlight <- Results %>%
  filter(player.name == "Lionel Andrés Messi Cuccittini" |
         player.name == "Marc-André ter Stegen")

ggtern() + 
  geom_point(data = Results, aes(x = p.passes, y = p.dribblings, z = p.shots),
             size = 2, 
             shape = 21, 
             color = "grey",
             fill = "grey") +
  geom_point(data = playerHightlight, aes(x = p.passes, y = p.dribblings, z = p.shots), 
             size = 2, 
             shape = 21, 
             color = "black",
             fill = "red") +
  theme_rgbg() +
  theme_showarrows()


#"Let's highlight all the players of Barcelona football club
ClubHightlight <- Results %>%
  filter(team.name == "Barcelona")

ggtern() + 
  geom_point(data = Results, aes(x = p.passes, y = p.dribblings, z = p.shots),
             size = 2, 
             shape = 21, 
             color = "grey",
             fill = "grey") + 
  geom_point(data = ClubHightlight, aes(x = p.passes, y = p.dribblings, z = p.shots), 
             size = 2, 
             shape = 21, 
             color = "black",
             fill = "red") +
  theme_rgbg() +
  theme_showarrows()


#We now apply the k-means procedure, in order to group the points in clusters and then we plot the result
kmeans <- kmeans(Results[7:9], 5)

Results$cluster <- as.character(kmeans$cluster)

ggtern(data = Results, aes(x = p.passes, y = p.dribblings, z = p.shots)) + 
  geom_point(aes(fill = cluster), size = 2, 
             shape = 21, 
             color = "black") +
  theme_rgbg() +
  theme_showarrows()

ReplacePlayer <- Results %>%
  filter(cluster == "4") %>%
  arrange(-p.passes) %>%
  slice(1:10)

view(ReplacePlayer)