## Catherine Hulshof
## Academic Diversity Disparity Map
## The following code uses data from the National Center for Education Statistics to create a map demonstrating the disparity between percent minority faculty and percent minority students across universities and colleges in the United States

#### Install and load packages ####

#install.packages(c("",...))

library(sf)
library(ggthemes)
library(tidyverse)
library(urbnmapr)
library(rnaturalearth)

#devtools::install_github("UrbanInstitute/urbnmapr")
#devtools::install_github("ropensci/rnaturalearth")
#https://urbaninstitute.github.io/urbnmapr/index.html

#### Load and Select Data ####

inst <- read_csv('data_raw/HD2018.csv')

ak <- inst %>% 
  filter(STABBR == "AK", ICLEVEL==1,HLOFFER>=5) %>% 
  mutate(LONGITUD = LONGITUD+33, LATITUDE = LATITUDE -36)

hi <- inst %>% 
  filter(STABBR == "HI", ICLEVEL==1,HLOFFER>=5) %>% 
  mutate(LONGITUD = LONGITUD+49, LATITUDE = LATITUDE + 5.5)

inst <- inst %>% 
  filter(STABBR != "AK") %>% 
  filter(STABBR != "HI") %>% 
  bind_rows(hi,ak)

sites <- inst %>% 
  filter(ICLEVEL==1,HLOFFER>=5) %>% 
  select(LONGITUD, LATITUDE) 

#### State maps from urbnmapr ####
states_sf <- get_urbn_map("states", sf = TRUE)
states_sf <- st_transform(states_sf, 4326) #change CRS to WGS84


## Still need to work on AK, HI, PR

#### Puerto Rico map from rnaturalearthhires ####

pr_basemap <- ne_states(country="puerto rico", 
                        returnclass = "sf")
pr_basemap <- st_transform(pr_basemap, 4326) #change CRS to WGS84
pr_basemap <- pr_basemap %>% 
  select(fips, iso_a2,name,geometry) %>% 
  rename(state_fips = fips, state_abbv=iso_a2, 
         state_name = name)

#### Combine maps ####
states_sf_pr=rbind(states_sf, pr_basemap)

#### Plot institutions ####
#### 4-year institutions (ICLEVEL==1)
#### Offers Bachelor's degree or higher (HLOFFER>=5)

states_sf_pr %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = sites, 
             aes(x = LONGITUD, y = LATITUDE), 
             size = .5, fill = "darkblue")+
  coord_sf(xlim = c(-125, -60), ylim = c(15, 50), expand = FALSE)