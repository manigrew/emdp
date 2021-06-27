library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/manish.grewal/git-emdp/emdp/scrum")
dat <- read.csv("Scrum.csv")

dat$Sprint <- sub("^20", "", dat$Sprint) # 2020-01 to 20-01
#dat$Sprint
#str(dat)

dat <- dat %>% 
  filter(Team != "Gladiators", PI != "19-4")

#######################################################################
# by sprint by team + PI + PI avg delivered - take 2
#######################################################################

by_pi_team = group_by(dat, PI, Team)
summ <- summarise(by_pi_team, mDelivered = mean(Delivered), mDelpct = mean(Delpct))
dat <- left_join(dat, summ, by = c("PI", "Team"))

my_colors = c("Delpct" = "Green", "Committed" = "Blue", "Delivered" = "Red")
my_linetype = c("Avg. Delpct" = "dashed", "Avg. Delivered" = "longdash")
outdir = "storypoints-charts"

ggplot(dat) +
  geom_line(aes(Sprint, Delivered, group = Team, color="Delivered")) + 
  geom_point(aes(Sprint, Delivered, color="Delivered")) +
  geom_line(aes(Sprint, Committed, group = Team, color="Committed")) +
  geom_point(aes(Sprint, Committed, color="Committed")) +
  geom_line(aes(Sprint, Delpct, group = Team, color="Delpct")) +
  geom_point(aes(Sprint, Delpct, color="Delpct")) +
  scale_color_manual(name = "", values = my_colors) + # legend

  # PI background
  geom_rect(xmin = 1, ymin = 0, xmax = 5.5, ymax = 100, fill = "#00000001") +
  #geom_rect(xmin = 5.5, ymin = 0, xmax = 10.5, ymax = 100, fill = "#33cccc01") +
  geom_rect(xmin = 10.5, ymin = 0, xmax = 15.5, ymax = 100, fill = "#00000001") +
  
  geom_line(aes(Sprint, mDelivered, group = Team, linetype = "Avg. Delivered"), color = "Black", size = 0.5) + # Average by PI
  geom_line(aes(Sprint, mDelpct, group = Team, linetype = "Avg. Delpct"), color = "Black", size = 0.5) + # Average by PI
  scale_linetype_manual(name = "", values = my_linetype) + # legend
  
  facet_wrap(~Team) + 
  theme_bw() +
  theme(legend.position = "top") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Committed, Delivered, Delpct(%)")

ggsave("all-teams.png", path = outdir)
#######################################################################
# Individual teams
#######################################################################

for (team in unique(dat$Team)) {
  curr <- filter(dat, Team == team)
  ggplot(curr) +
    geom_line(aes(Sprint, Delivered, group = Team, color="Delivered")) + 
    geom_point(aes(Sprint, Delivered, color="Delivered")) +
    geom_line(aes(Sprint, Committed, group = Team, color="Committed")) +
    geom_point(aes(Sprint, Committed, color="Committed")) +
    geom_line(aes(Sprint, Delpct, group = Team, color="Delpct")) +
    geom_point(aes(Sprint, Delpct, color="Delpct")) +
    scale_color_manual(name = "", values = my_colors) + # legend
    
    # PI background
    geom_rect(xmin = 1, ymin = 0, xmax = 5.5, ymax = 100, fill = "#00000001") +
    #geom_rect(xmin = 5.5, ymin = 0, xmax = 10.5, ymax = 100, fill = "#33cccc01") +
    geom_rect(xmin = 10.5, ymin = 0, xmax = 15.5, ymax = 100, fill = "#00000001") +
    
    geom_line(aes(Sprint, mDelivered, group = Team, linetype = "Avg. Delivered"), color = "Black", size = 0.5) + # Average by PI
    geom_line(aes(Sprint, mDelpct, group = Team, linetype = "Avg. Delpct"), color = "Black", size = 0.5) + # Average by PI
    scale_linetype_manual(name = "", values = my_linetype) + # legend
    
    #facet_wrap(~Team) + 
    theme_bw() +
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
    ggtitle(team) +
    scale_y_continuous(breaks = seq(0, 100, 10))# +
    #theme(axis.text.x = element_text(angle = 90)) 
  
  ggsave(paste0(team, ".png"), path = outdir)
    #readline()
}

