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
read_csv("un-report/co2-un-data.csv", skip = 1) # read but skip the first column (1)
  # But our column names are messed up! ("..2"). Lets skip two columns and assign names to the rest.
read_csv("un-report/co2-un-data.csv", skip = 2,
         col_names = c("region","country", "year","series","value","footnotes","source"))
co2_emissions_dirty <- read_csv("un-report/co2-un-data.csv", skip=2,
                                col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))
co2_emissions_dirty
#Lets replace the number values in the emissions column with descriptions based on those emissions using recode 
?recode
co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission"))
?pivot_wider
co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>%
  pivot_wider(names_from=series, values_from=value)

# First filter for just 2005, then get rid of year column
co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year)

  # assign to a variable
co2_emissions <- co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year)

#Join Data Frames: Bringing in 2007 Population Data
#(want to combine co2 emissions data with population data per country for each year)
read_csv("un-report/gapminder_data.csv") %>%
  filter(year == 2007) %>%
  select(country, pop, lifeExp, gdpPercap)

  # assign as an object "gapminder_data_2007"
gapminder_data_2007<- read_csv("un-report/gapminder_data.csv") %>%
  filter(year == 2007) %>%
  select(country, pop, lifeExp, gdpPercap)

 # alternative way to achieve the same thing (by subtracting year and continent column)
gapminder_data_2007 <- read_csv("un-report/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

#Join two tables with a similar column, but rest of columns differ.
?inner_join
inner_join(co2_emissions, gapminder_data_2007)
inner_join(co2_emissions, gapminder_data_2007, by = "country") #only kept rows with data for each one
 
   # which countries do we not have data from? Use anti_join
?anti_join
anti_join(co2_emissions, gapminder_data_2007, by = "country")# What data is not shared between these files?

  # full_join (includes NAs, where as inner would not include NAs)
?full_join
full_join(co2_emissions, gapminder_data_2007)


###After Lunch break
##Goal: Find the relationship between CO2 and GDP
joined_co2_pop <- inner_join(co2_emissions, gapminder_data_2007)

#writing to CSV
write_csv(joined_co2_pop, file = "/cloud/project/un-report/joined_co2_pop.csv")
  # Read that csv file
joined_co2_pop<-read_csv("joined_co2_pop.csv")


#plot it!
  # Create a histogram for both gpdPercap and lifeExp (separately), to explore those variables distributions
ggplot(data = joined_co2_pop)+
  aes(x = gdpPercap)+
  geom_histogram()

ggplot(joined_co2_pop)+
  aes(x=lifeExp)+
  geom_histogram()

# make a scatterplot
ggplot(joined_co2_pop, aes(x=gdpPercap, y=per_capita_emission)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  )
install.packages("ggpubr")
plot<- ggplot(joined_co2_pop, aes(x=gdpPercap, y=per_capita_emission)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces") + 
    theme_classic()+
    ggpubr::stat_regline_equation(aes(label= ..rr.label..))#deprecated error means it is no longer being updated in that package
 
 # save that plot
ggsave(plot, filename = "./awesomeplot.png",
       height = 4, width =6, units = "in",
       dpi = 300) #dpi = dotsPerInch (resolution)
