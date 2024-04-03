#############################################
##Day 1
#############################################
# creating this so my dir aligns with leaners

library(tidyverse)
library(ggplot2)

gapminder_data <- read.csv("data/gapminder_data.csv")

#gapminder_data <- read_csv("~/Desktop/un-report/gapminder_data.csv")

ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5) + # we made our points slightly transparent, because it makes it easier to see overlapping points
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color= "Continent", size="Population (in millions)")+
  theme_classic()
ggsave("gdp_percap.pdf")


