library(tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")

summarize(gapminder_data, averagelifeExp=mean(lifeExp)) # summary stats
# creating a new variable for the mean - it's not saved in a df though

# %>% = pipe symbol, Ctrl+Shift+M
# it connects functions, piping arg=first arg for a certain function

gapminder_data %>% summarise(averagelifeExp=mean(lifeExp)) # you could continue piping and stack functions here

gapminder_data_summarized <- gapminder_data %>% summarise(averagelifeExp=mean(lifeExp))
# creating an additional stand-alone variable

gapminder_data %>% summarize(recent_year=max(year))

# subsetting dataframe using the function=filter

gapminder_data %>% filter(year==2007) # get only rows with year==2007

gapminder_data %>% filter(year==2007) %>% summarize(averagelifeExp=mean(lifeExp))

## What is the avg GDP per capita for the first year in a dataset?
gapminder_data %>% filter(year==min(year)) %>% summarize(averagegdpPercap = mean(gdpPercap)) # 3725.

# in two steps:
first_year <- gapminder_data %>% summarize(first_year=min(year)) # 1952
first_year
gapminder_data %>% filter(year==1952) %>% summarize(averagegdpPercap = mean(gdpPercap)) # 3725.


## group_by() to group values from a column
gapminder_data %>% group_by(year) %>% summarize(averagelifeExp=mean(lifeExp)) #mean lifeExp for every year

# calculate average life expectancy by continent
gapminder_data %>% group_by(continent) %>% summarise(averagelifeExp=mean(lifeExp))

# get two stats at the same time
gapminder_data %>% group_by(continent) %>%  summarize(averagelifeExp=mean(lifeExp), minlifeExp=min(lifeExp))

# create a new/change a column with some statistics = mutate()

gapminder_data %>% mutate(gdp = pop * gdpPercap)

# make a new column popInMillions
gapminder_data = gapminder_data %>% mutate(popInMillions = pop/1000000) 
# need re-assigning if we want to alter the original df

## subsetting df by columns = select()
gapminder_data %>% select(pop, year)
gapminder_data %>% select(-continent) # all but the continent column

# create a dataframe with only country, continent, year, and lifeExp columns
gapminder_data %>% select(country, continent, year, lifeExp)
gapminder_data %>% select(-pop, -gdpPercap, -popInMillions)


## transforming our dataframe from a long to a wide format
# pivot_wider(), pivot_longer()

gapminder_data %>% 
  select(continent, year, lifeExp, country) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

gapminder_data %>% 
  select(continent, year, lifeExp, country) %>% 
  pivot_wider(names_from = country, values_from = lifeExp)
# gets unique combinations continent+year, then creates country columns and populate them with lifeExp where avail

# subsetting the data to the year 2007 and drop the year and continent cols
gapminder_data %>% filter(year==2007) %>% select(-year, -continent)

# two filters together
gapminder_data_2007 <- gapminder_data %>% 
  filter(year==2007 & continent=="Americas") %>% 
  select(-year, -continent)

# is CO2 emissions related to GDP
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2, # will skip first two rows
         col_names=c("region", "country", "year", "series", 
                     "value", "footnotes", "source"))

co2_emissions_dirty %>% select(country, year, series, value) %>% 
  mutate(series=recode(series,"Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                       "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))# over-writing the series column

# transform it to a wider format

co2_emissions_dirty %>% select(country, year, series, value) %>% 
  mutate(series=recode(series,"Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                       "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% # over-writing the series column
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year) # get how many observations per year

# subset by year and save
co2_emissions <- co2_emissions_dirty %>% select(country, year, series, value) %>% 
  mutate(series=recode(series,"Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                       "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% # over-writing the series column
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year==2005) %>% 
  select(-year)

# combine with another dataset
# use a common field = "country" and do inner_join (take only common for both rows)

inner_join(gapminder_data_2007, co2_emissions)
anti_join(gapminder_data_2007, co2_emissions) # shows what rows are going to be dropped

# clean the file later after we explored the anti_join issues

co2_emissions <- co2_emissions_dirty %>% select(country, year, series, value) %>% 
  mutate(series=recode(series,"Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                       "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% # over-writing the series column
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year==2005) %>% 
  select(-year) %>% 
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)"="Bolivia",
                        "United States of America"="United States",
                        "Venezuela (Boliv. Rep. of)"="Venezuela"))

anti_join(gapminder_data_2007, co2_emissions) 

# let's combine Puerto Rico and US data since PR is not an its own country

gapminder_data_2007 <- gapminder_data %>% 
  filter(year==2007 & continent=="Americas") %>% 
  select(-year, -continent) %>% 
  mutate(country=recode(country, 
                        "Puerto Rico"="United States"))

gapminder_data_2007 <- gapminder_data %>% 
  filter(year==2007 & continent=="Americas") %>% 
  select(-year, -continent) %>% 
  mutate(country=recode(country, 
                        "Puerto Rico"="United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp=sum(lifeExp*pop)/sum(pop), # weighted average
            gdpPercap=sum(gdpPercap*pop)/sum(pop), # weighted average
            pop=sum(pop),
            popInMillions=sum(pop)/1000000)
            
anti_join(gapminder_data_2007, co2_emissions) # now is empty

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions)

# Is CO2 emissions related to GDP?
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita_emissions)) +
  geom_point() +
  geom_smooth(method="lm") + # adding trend line, using OLS as a method
  labs(x="GDP per capita", y="CO2 emitted per capita")

# saving the data
write_csv(gapminder_co2, "data/gapminder_co2.csv")

