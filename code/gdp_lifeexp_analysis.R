library(tidyverse)

gapminder_1997 <- read_csv("gapminder_1997.csv")
# Rows: 142 Columns: 5
str(gapminder_1997) # what's the structure

# Alt + "-" for Insert Assignment Operator

?read_csv
getwd() # get path to the working directory

sum(5,6)
5+6
round(1.45,1) # default is zero digits

library(readxl)
# execl_file <- readxl::read_excel("file_name")

# read_excel() from tidyverse

ggplot(data=gapminder_1997) + # empty plot
  aes(x=gdpPercap, y=lifeExp) +
  labs(x="GDP per Capita", y="Life Expectancy") + # labels
  geom_point() + # type of a graph, geom_point=scatter plot
  labs(title="Do people in wealthier countries live longer?") + #title
  aes(color=continent) + # separate data by colored continents
  scale_color_brewer(palette="Set3") + # different colors available, choose palette
  aes(size = pop/1000000) + # aes function adds variation to the graph, adding population component
  labs(size="Population (in millions)")

# same as above but written together
ggplot(data=gapminder_1997)+
  aes(x=gdpPercap, y=lifeExp, color=continent, size=pop/1000000)+
  geom_point()+
  scale_color_brewer(palette="Set1")+
  labs(x="GDP per Capita", y="Life Expectancy",
       title="Do people in wealthier countries live longer?", 
       size="Population (in millions)")


# load in a larger dataset

gapminder_data <- read_csv("gapminder_data.csv")
# Rows: 1704 Columns: 6
dim(gapminder_data)

ggplot(data=gapminder_data)+
  aes(x=year, y=lifeExp, 
      group=country)+
  aes(color=continent)+
  geom_line()

# Discrete plots

ggplot(data=gapminder_1997)+
  aes(x=continent, y=lifeExp)+
  geom_boxplot()

ggplot(data=gapminder_1997)+
  aes(x=continent, y=lifeExp)+
  geom_jitter()+ # similar to geom_point
  geom_violin()

# Master aesthetics

ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp))+
  geom_violin()+
  geom_jitter(aes(size=pop), color="pink") # assign mapping to the jitter function only

ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp))+
  geom_violin(fill="yellow")

ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp))+
  geom_violin(aes(fill=continent)) # colors are different for each continent

ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp))+
  geom_violin(fill="springgreen") # remove aes if we want all violins be green, aes is responsible for mapping

# Univariate plots

ggplot(gapminder_1997) + 
  aes(x=lifeExp)+
  geom_histogram(bins=20) # distribution

# Plot themes

ggplot(data=gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(bins=20)+
  theme_classic()+ # general theme
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) # change text sidewise

# facet wrap

plot1 <- ggplot(data=gapminder_1997)+
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_wrap(vars(continent)) # separate graph by continent - creating subgraphs

# facet grid

plot2 <- ggplot(data=gapminder_1997)+
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_grid(rows = vars(continent)) # separate graph by rows

# saving plot

ggsave("plot2.jpg", width=6, height=4) # saving the last plot, using the current WDir
ggsave(plot1, file="plot1.jpg", width=6, height=4) # saving the plot we specified, using the current WDir

# practice - violin plot

violin_plot <- ggplot(data=gapminder_1997)+
  aes(x=continent, y=lifeExp)+
  #aes(color=continent)+ # removes black border line for violins
  labs(x="Continent", y="Life Expectancy", color="Continent")+ #labs(color = "XYZ") changes the title of a legend
  geom_violin(aes(color=continent))+ # color or fill
  theme_bw()

violin_plot

ggsave(violin_plot, file="awesome_violin_plot.jpg", width=4, height=3)
