#Section 4 <- Gapminder

library(ggplot2)
library(dslabs)
data("gapminder")
head(gapminder)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(GGally)
library(ggridges)
library(gridExtra)
?cat

gapminder %>% 
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>% 
  select(country, infant_mortality)

ds_theme_set()
six2 <- filter(gapminder, year==1962) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

#Faceting
twenty12 <- filter(gapminder, year==2012) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

grid.arrange(six2, twenty12, ncol = 2)

#facet_grid() <- take a 

filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent~year)

filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(.~year, vars(rows = length(levels(c(1962, 2012)))))
?facet_grid
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")

filter(gapminder, year %in% years & continent %in% continents) %>% 
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)
#facet_wrap keeps scale across all graphs

gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c("South Korea", "Germany")
gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year,fertility)) +
  geom_line()
#Produces squiggly line because we haven't grouped yet


countries <- c("South Korea", "Germany")
gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year,fertility, group = country)) +
  geom_line() 

gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year,fertility,  col = country)) +
  geom_line() 
  
labels <- data.frame(country = countries, x= c(1975, 1965), y = c(60, 72))
gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

#Transformations

gapminder <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")
#this chart has local modes


gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(log10(population))) +
  geom_histogram( color = "black")

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")


#Stratify and boxplot

length(levels(gapminder$region))

length(levels(heights$sex))
#length(levels()) <- count distinct
#levels(data$colum) <- returns distinct values
p <- gapminder  %>% 
  filter(year==past_year & !is.na(gdp)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent)) 
p  
p + geom_boxplot()
#can't read labels

p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#reorder function

fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)
value <- c(10, 11, 12, 6, 4)

fac <- reorder(fac, value, FUN = mean)
levels(fac)

p <- gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") 
p #with fill

d <- gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, color = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("")
d #with color

p <- gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2")
p #with log scae

p + geom_point(show.legend = FALSE)
p + geom_point(aes(col = fertility),show.legend = FALSE)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  mutate(group = ifelse(region %in% west, "West","Developing")) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(.~group)

present_year <- 2010
gapminder %>% 
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>% 
  mutate(group = ifelse(region %in% west, "West","Developing")) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)

country_list_1 <- gapminder %>% 
  filter(year==past_year & !is.na(dollars_per_day)) %>% 
  .$country
country_list_1

country_list_2 <- gapminder %>% 
  filter(year==present_year & !is.na(dollars_per_day)) %>% 
  .$country
country_list_2

country_list <- intersect(country_list_1, country_list_2)
country_list

gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)


p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

aes(x = dollars_per_day, y = ..count..)
 #adding .. before and after count is to access the count variable in aes

p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(x = dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.6) +
  facet_grid( year ~ .)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))
head(gapminder)  

gapminder <- gapminder %>% 
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
head(gapminder)

# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>% 
  mutate(weight = population/sum(population)*2) %>% 
  ungroup() %>% 
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) + 
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)
p

library(tidyverse)
library(dslabs)
data(gapminder)
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income <- gapminder %>% 
  filter(year %in% present_year & !is.na(gdp) & ! is.na(infant_mortality) & !is.na(group)) %>% 
  group_by(group) %>% 
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1- sum(infant_mortality/1000*population)/sum(population))
surv_income

surv_income %>% arrange(income)

surv_income %>% 
  ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(.025, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, 0.9981),
                     breaks = c(0.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE)

#logistic proportion log(p/1-p) <- shows the odds
              

#Assessment
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
## fill out the missing parts in filter and aes
gapminder %>% filter(continent == "Africa" & year == 2012) %>%
  ggplot(aes(x = fertility, y = life_expectancy, color = region)) +
  geom_point()

head(gapminder)


df <- gapminder %>% 
  filter(continent == "Africa" & year == 2012 & fertility <= 3 & life_expectancy >= 70) %>% 
  select(country, region)
  df
  
seqyears <- seq(1960:2010)  
tab <- gapminder %>% 
  filter(country %in% c("Vietnam", "United States") & year >= 1960 & year <= 2010) %>% 
  select(country, year, infant_mortality, life_expectancy, fertility, population, gdp, continent, region)
tab  

p <- tab %>% 
  ggplot(aes(x = year, y = life_expectancy, color = country)) +
  geom_line()
p  


p <- gapminder %>% 
  filter(country == "Cambodia" & year >= 1960 & year <= 2010) %>% 
  ggplot(aes(x = year, y = life_expectancy)) +
  geom_line()
p

daydollars <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365 )) %>% 
  filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day))
daydollars

daydollars %>% 
  filter( year >= 1960 & year <= 2010))
  ggplot(aes(x = dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2")
  
daydollars <- gapminder %>% 
    mutate(dollars_per_day = gdp/population/365) %>% 
    filter(continent == "Africa" & year %in% c(1970,  2010) & !is.na(dollars_per_day))
daydollars

p <- daydollars %>% 
  ggplot(aes(x = dollars_per_day, fill = region)) +
  geom_density(bw = 0.5, position = "stack") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)
p


gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365 ) %>% 
  filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day))
gapminder_Africa_2010

gapminder_Africa_2010 %>% 
  ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region)) + 
  geom_point()

gapminder_Africa_2010 %>% 
  ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region)) + 
  geom_point() +
  scale_x_continuous(trans = "log2")

gapminder_Africa_2010 %>% 
  ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region, label = country)) + 
  geom_point() +
  scale_x_continuous(trans = "log2") + 
  geom_text(aes(label = country))

gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365 ) %>% 
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day) & !is.na(gdp) & !is.na(infant_mortality))
gapminder_Africa_2010

gapminder_Africa_2010 %>% 
  ggplot(aes(x = dollars_per_day, y = infant_mortality, color = region, label = country)) + 
  geom_point() +
  scale_x_continuous(trans = "log2") + 
  geom_text(aes(label = country)) +
  facet_grid(year~.)



testing123 <- 123
