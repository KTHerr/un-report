setwd("C:/Users/Katie/Desktop/DataCarpentryWorkshop/un-report")
install.packages(tidyverse)
path.package("tidyverse")
library(tidyverse)
library(ggplot2)
gapminder_1997 <- read_csv("gapminder_1997.csv")
View(gapminder_1997)
ggplot(data=gapminder_1997)

t<- ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, shape = continent, size = pop/1000000) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy",
       title = "Do people in wealthy countries live longer?", size = "Population (in millions)")
t
gapminder_data<-read_csv("gapminder_data.csv")
view(gapminder_data)
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)
str(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_boxplot()

#color violin outline pink
ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_violin(color="pink")+
  geom_jitter(aes(size=pop))

#make whole violine pink
ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_violin(fill="pink")+
  geom_jitter(aes(size=pop))

#both
ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_violin(color="pink",fill="cornflowerblue")+
  geom_jitter(aes(size=pop))
#corrected:
ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_jitter(aes(size=pop)+
  geom_violin(color="pink",fill="cornflowerblue"))

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp, color = continent) +
  geom_jitter(aes(size=pop))+
  geom_violin()

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_jitter() +
  geom_violin()

#Histogram
ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram()

##Facets
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point()

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows=vars(continent))


ggsave("plot1.tiff", device="tiff", width = 6, height =4, units="cm")

###Day two of workshop:

library(tidyverse)
library(ggplot2)
library(readr)
gapminder_1997 <- read_csv("gapminder_1997.csv")
gapminder_data <- read_csv("un-report/gapminder_data.csv")
summarise(gapminder_data)
summarize(gapminder_data, averageLifeExp=mean(lifeExp))
#learn to pipe
  # %>%
gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))
gapminder_summary<- gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))
#filter data (select specific rows)
gapminder_data %>% 
  filter(year = 2007) #doesnt work huh? Need to double ==
gapminder_data %>% 
  filter(year == 2007)
gapminder_data %>% 
  filter(year != 2007)

#now pipe
gapminder_data %>% 
  filter(year == 2007)%>%
  summarise(average = mean(lifeExp))
gapminder_data %>% 
  summarise(year == min(year))

gapminder_data %>% 
  filter(year == min(year))

gapminder_data %>% 
  filter(year == min(year))%>%
  summarise(Average_GDP == mean(gdpPercap))

#groupBy
gapminder_data %>%
  group_by(year) %>%
  summarize(average=mean(lifeExp))#Groups and summarizes data by year

  #groupby multiple variables
gapminder_data %>%
  group_by(year,continent) %>%
  summarize(average=mean(lifeExp))#Groups and summarizes data by year and continent
gapminder_data %>%
  group_by(year,continent) %>%
  summarize(average=mean(lifeExp), error=sd(lifeExp))


#What about expanding our data instead of filtering? Mutate
?mutate
  #gapminder_data %>%
  # mutate(newcolumn = (math to make that column))
gapminder_data %>%
  mutate(gdp = pop * gdpPercap)
gapminder_data %>%
  mutate(gdp = pop * gdpPercap, Popinmillions = pop / 1000000)

#how to select for specific columns
gapminder_data %>%
  select(pop,year)
#how to select against specific columns
gapminder_data %>%
  select(-continent, -pop)

#How to reshape/Re-format your data with tidyverse and dyplr (pivot_wider and pivot_longer)
  # change from long format:
gapminder_data %>%
  select(country, continent, year, lifeExp)
  # to wide format
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp )
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp )%>%
  View()



#Working with messy data

read_csv("un-report/co2-un-data.csv")
