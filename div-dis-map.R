## Catherine Hulshof
## Academic Diversity Disparity Map
## The following code uses data from the National Center for Education Statistics to create a map demonstrating the disparity between percent minority faculty and percent minority students across universities and colleges in the United States

#### Install and load packages

#install.packages(c("cowplot",...))

library(sf)
library(ggthemes)
library(tidyverse)
library(urbnmapr)

#devtools::install_github("UrbanInstitute/urbnmapr")
#https://urbaninstitute.github.io/urbnmapr/index.html

#### Load Data

inst <- read_csv('data_raw/HD2018.csv')

ak <- inst %>% 
  filter(STABBR == "AK") %>% 
  mutate(LONGITUD = LONGITUD+40, LATITUDE = LATITUDE -40)

hi <- inst %>% 
  filter(STABBR == "HI") %>% 
  mutate(LONGITUD = LONGITUD+40, LATITUDE = LATITUDE + 4)

inst <- inst %>% 
  filter(STABBR != "AK") %>% 
  filter(STABBR != "HI") %>% 
  bind_rows(hi,ak)

sites <- inst %>% 
  select(LONGITUD, LATITUDE)

#### Use state maps from urbnmapr
states_sf <- get_urbn_map("states", sf = TRUE)
states_sf <- st_transform(states_sf, 4326) #change CRS to WGS84

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = sites, 
             aes(x = LONGITUD, y = LATITUDE), 
             size = .5, fill = "darkblue")+
  coord_sf(xlim = c(-125, -60), ylim = c(15, 50), expand = FALSE)

## Still need to work on AK, HI, PR

