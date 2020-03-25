library(tidyverse)
library(maps)
library(plotly)
library(gifski)
library(gganimate)
library(googlesheets4)


covid <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1avGWWl1J19O_Zm0NGTGy2E-fOG05i4ljRfjl87P7FiA/edit?ts=5e5e9222#gid=0"))

head(covid)
str(covid)
nrow(covid)
colnames(covid)
# clean column names
covid <- covid %>% rename(country = Country_Region,
                          province_state = Province_State,
                          test_date = Date,
                          case_type = Case_Type,
                          cases = Cases,
                          long = Long,
                          lat = Lat,
                          difference = Difference,
                          last_update = Latest_Date)
colnames(covid)

# Explore different types of cases
table(covid$case_type)

# Generate world map to plot over
world <- map_data("world") %>% filter(region != "Antarctica")
world_map <- ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5)

# Filter data for confirmed cases
covid_confirmed <- covid %>%
  filter(case_type == "Confirmed")


# plot covid cases
plot <- world_map + geom_point(data=covid_confirmed, aes(x=long, y=lat, size=cases, color=country), alpha=.5) +
  scale_size(range = c(1, 30), name="Number of Cases") +
  transition_time(test_date) +
  guides(color=FALSE) +
  #+geom_text(data=filter(covid_confirmed, cases > 1000), aes(long, lat, label=country)) + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ggtitle("Spread of COVID-19") +
  labs(title = 'Number of Confirmed COVID-19 cases on: {frame_time}')

# animate
animate(plot, width=1125, height=675, end_pause=30)


# make animated bar plot of top 10 countries by deaths

# create df that ranks countries daily by death toll
covid_deaths <- covid %>% 
  filter(case_type == "Deaths", test_date > "2020-02-20") %>%
  group_by(country, test_date) %>%
  summarize(cases=sum(cases)) %>%
  group_by(test_date) %>%
  mutate(rank=rank(-cases, ties.method = 'first')) %>%
  filter(rank <= 10) %>%
  ungroup()

# create static plot to iterate over
staticplot = ggplot(covid_deaths, aes(rank, group = country, 
                                       fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = cases/2,
                height = cases,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=cases,label = as.integer(cases), hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

# animate plot by adding date transition_state
anim = staticplot + transition_states(test_date, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'COVID-19 Deathtoll on: {closest_state}',  
       subtitle  =  "Top 10 Countries")

animate(anim, 200, fps = 10,  width = 1200, height = 1000)


# could do chart of cases/deaths relative to country population
