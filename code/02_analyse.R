library(tidyverse)
library(ggplot2)
library(ggimage)



chart_lasted_season<-
alone_data %>%
  filter(season != 4) %>%
  group_by(season)%>%
  mutate(contestant_id = row_number()) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(x = contestant_id,
                y = days_lasted,
                colour = season),
            size = 8, alpha = 1)+
  scale_colour_brewer(palette = "YlOrRd")+
  scale_x_continuous(breaks = c(1:10),
                     trans = "reverse") +
  theme(legend.spacing.x = unit(0, 'cm'))+
  guides(col = guide_legend(override.aes = list(alpha = 1)))+
  theme_black()+
  labs(title = "Days lasted by Season",
       x = "Contestant position #",
       y = "Days lasted",
       colour = "Season")




chart_lasted_age<-
  alone_data %>%
  group_by(season)%>%
  mutate(contestant_id = row_number()) %>%
  group_by(age)%>%
  ungroup() %>%
    select(age,days_lasted,winner)%>%
    unique() %>%
    mutate(img = if_else(winner == TRUE, paste0(getwd(),"/man/trophy.png"),paste0(getwd(),"/man/skull.png"))) %>%
  ggplot(aes(x = age, y = days_lasted))+
  geom_image(aes(image = img))+
  scale_colour_manual(values = c("#ffeda0","#f03b20"))+
  theme_black()+
  labs(title = "Days lasted by Age",
       x = "Age",
       y = "Days lasted")


ggsave("output/chart_lasted_season.png", chart_lasted_season)
ggsave("output/chart_lasted_age.png", chart_lasted_age)
