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

# ICLEVEL: Level of Institution. Four or more years.
# CARNEGIE: Carnegie classification: Remove faith-based 
# HDEGOFR1: Highest Degree Offered. Bachelors to Doctorate degrees.
# C18SZSET: Institution Classification. At least four years.
sites <- inst %>% 
  filter(ICLEVEL==1,HLOFFER>=5,
         C18SZSET>=6,
         CARNEGIE!=51) 

#HBCU <- sites %>% 
#  filter(HBCU ==1)

#TRIBAL <- sites %>% 
#  filter(TRIBAL ==1)

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

#### Map institutions ####
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

#### Merge student data from DRVEF2018 ####

#PctEnrWh white
#PctEnrBK black
#PctEnrHS hispanic
#PctEnrAP asian pacific islander
#PctEnrAN america indian
#PctEnrUn unknown
#PctEnrNr nonresident 
#PCTENRW women
#PCTFT1ST first generation
#ENRTOT total enrollment

axes.student <-
  c('UNITID', 'PCTENRWH','PCTENRBK', 'PCTENRHS',
    'PCTENRAP', 'PCTENRAN', 'PCTENRUN', 'PCTENRNR',
    'PCTENRW', 'PCTFT1ST', 'ENRTOT')

perc.student <- read_csv('data_raw/DRVEF2018.csv') %>% 
  select(all_of(axes.student)) %>% 
  right_join(sites,by = "UNITID",keep=F)

## sample map with size by percent hispanic, some unis with no data...will need to remove.
states_sf_pr %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = perc.student, 
             aes(x = LONGITUD, y = LATITUDE,size = PCTENRHS))+
  coord_sf(xlim = c(-125, -60), ylim = c(15, 50), expand = FALSE)

#### Merge faculty data from S2018_IS ####
#### HRTOTLT, HRAIANT, HRAIANM, HRAIANW, HRASIAT,HRASIAM, HRASIAW, HRBKAAT, HRBKAAM, HRBKAAW, HRHISPT, HRHISPM, HRHISPW, HRNHPIT, HRNHPIM, HRNHPIW, HRWHITT, HRWHITM, HRWHITW, HR2MORT, HR2MORM, HR2MORW

axes.faculty <-
  c('UNITID', 'HRTOTLT', 'HRAIANT', 'HRAIANM', 'HRAIANW', 'HRASIAT','HRASIAM', 'HRASIAW', 'HRBKAAT', 'HRBKAAM', 'HRBKAAW', 'HRHISPT', 'HRHISPM', 'HRHISPW', 'HRNHPIT', 'HRNHPIM', 'HRNHPIW', 'HRWHITT', 'HRWHITM', 'HRWHITW', 'HR2MORT', 'HR2MORM', 'HR2MORW')

perc.faculty <- read_csv('data_raw/S2018_IS.csv') %>% 
  select(all_of(axes.faculty)) %>% 
  right_join(perc.student,by = "UNITID",keep=F)

perc.faculty$disp_hs <- perc.faculty$PCTENRHS-
  (perc.faculty$HRHISPT/perc.faculty$HRTOTLT*100)

states_sf_pr %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = perc.faculty, 
             aes(x = LONGITUD, y = LATITUDE,color = disp_hs))+
  scale_color_viridis_c()+
  coord_sf(xlim = c(-125, -60), ylim = c(15, 50), expand = FALSE)

### Initial plotting thoughts: need to winnow down list of institutions: Big10, Top 100 USNews, remove points with no values, could this be construed wrong?