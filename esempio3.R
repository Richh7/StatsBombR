library(ggrepel)
library(ggplot2)
library(ggsoccer)
library(StatsBombR)


#Considering all the seasons available in the English championship (Premier League)
Comp <- FreeCompetitions() %>%
  filter(competition_id == 2)

Matches <- FreeMatches(Comp)

Events <- free_allevents(MatchesDF = Matches, Parallel = T) %>% 
  allclean()


#We first need player minutes stats, then we wanna know who does the player.id relate to
Minutes <- get.minutesplayed(Events)

Players <- Events %>% 
  distinct(player.id, player.name, team.name) %>% 
  left_join(Minutes, by = "player.id") %>% 
  na.omit()

Mins <- Players %>%
  group_by(player.name) %>% 
  summarise(total_mins = sum(MinutesPlayed),
            matches = n()) %>%
  arrange(-matches)


#Preparing shots/xG/goals/matches for some data manipulation
Summary <- Events %>% 
  mutate(is_shot = ifelse(type.name == "Shot", 1, 0),
         is_goal = ifelse(shot.outcome.name == "Goal", 1, 0)) %>% 
  filter(is_shot == 1) %>% 
  group_by(player.name) %>% 
  summarise(shots = sum(is_shot),
            goals = sum(is_goal),
            npxg = sum(shot.statsbomb_xg)) %>% 
  left_join(Mins, by = "player.name") %>%
  mutate(shots_p90 = shots/matches,
         goals_p90 = goals/matches,
         npxg_p90 = npxg/matches) %>%
  filter(matches>5) %>% 
  arrange(-npxg_p90)

view(Summary)


#Plotting goals/shots per match with a scatterplot
ggplot(data = Summary,
       aes(x = goals_p90,
           y = shots_p90))+
  geom_point() +
  geom_hline(yintercept = 1, 
             colour = "red", 
             alpha = 0.7, 
             linetype = "dotted")+
  geom_vline(xintercept = 0.2, 
             colour = "red", 
             alpha = 0.7, 
             linetype = "dotted")+
  geom_text_repel(data = Summary %>% 
                    filter(shots_p90 >= 1 | goals_p90 >= 0.2),
                  aes(label = player.name))+
  theme_minimal() +
  xlab("Goals P90") +
  ylab("Shots P90")


#We will consider the top3 players by NpxG
Summary<-Summary %>% 
  slice(1:3)


#filter location data by top 3 players - shots only
shot_data <- Events %>% 
  filter(player.name %in% unique(Summary$player.name),
         type.name == "Shot") %>% 
  mutate(is_goal = ifelse(shot.outcome.name=="Goal", 1, 0) %>% 
           as.factor()) %>% 
  select(player.name, location.x, location.y, is_goal, shot.outcome.name, shot.statsbomb_xg, team.name)


ggplot()+
  annotate_pitch(dimensions = pitch_statsbomb)+
  theme_pitch()+
  coord_flip(xlim = c(60,120),
             ylim = c(0,80)) +
  geom_point(data = shot_data, 
             aes(x = location.x, 
                 y = location.y, 
                 colour = is_goal, 
                 size = shot.statsbomb_xg)) +
  geom_point(data = shot_data %>% 
               filter(is_goal==1),
             aes(x = location.x,
                 y = location.y,
                 size = shot.statsbomb_xg),
             shape = 21,
             fill = "#66C2A5",
             stroke = 0.6,
             colour = "black")+
  scale_colour_manual(values = c("#FC8D62", "#66C2A5"), 
                      name = "Shot Outcome", 
                      labels = c("No-Goal", "Goal"))+
  facet_wrap(~player.name) +
  geom_text(data = Summary,
            aes(x=80,
                y=15,
                label = paste("Shots P90:", round(shots_p90, digits = 2))))+
  geom_text(data = Summary,
            aes(x=74,
                y=15,
                label = paste("Goals P90:", round(goals_p90, digits = 2))))+
  geom_text(data = Summary,
            aes(x=68,
                y=15,
                label = paste("NPxG P90:", round(npxg_p90, digits = 2))))+
  theme(strip.text = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.caption = element_text(size = 10),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "top",
        strip.background = element_blank())
