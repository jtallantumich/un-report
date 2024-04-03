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


######
## day 2
#####

gapminder_data <- read_csv("data/gapminder_data.csv")

#Calculate something
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(avg=mean(lifeExp), 
            min = min(lifeExp), 
            max = max(lifeExp))

#adding columns to dataset using mutate
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)

#mutate to create col for popInMillions
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap, popInMillions = pop / 1000000)

#filter to select rows
#select to select cols
gapminder_data %>% 
  select(pop, year)

#ends in p
gapminder_data %>% select(ends_with("p"))


#save a dataframe that contains only the Americas in 2007

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

glimpse(gapminder_data_2007)

#cleaning messy data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country","year", "series","value", "footnotes","source"))

co2_emissions_dirty

#select the country, year, series, and values columns
co2_emissions <- co2_emissions_dirty %>% select(country,year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)


#join two dataframes in tidyverse
inner_join(gapminder_data, co2_emissions, by ="country")

anti_join(gapminder_data, co2_emissions, by="country")


read_csv("data/co2-un-data.csv", skip = 2, 
         col_names = c("region", "country","year", "series","value", "footnotes","source")) %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year) %>% 
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))

#address Puerto Rico
gapminder_data <- gapminder_data %>% mutate(country = recode(country,"Puerto Rico" = "United States"))


gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")
glimpse(gapminder_co2)  

gapminder_co2 %>% 
  group_by(continent) %>% 
  summarize(avgLifeExp = mean(lifeExp))

gapminder_co2 %>% 
  filter(continent == "Americas") %>% 
  mutate(region = ifelse(country == "United States" | country == "Canada" | country =="Mexico", "north", "south")) %>% 
  view()


#I want the Americas in 2007
gapminder_Americas_2007 <- gapminder_co2 %>% 
  filter(continent == "Americas" & year == "2007") %>% 
  mutate(region = ifelse(country == "United States" | country == "Canada" | country =="Mexico", "north", "south")) #%>%

gapminder_Americas_2007
