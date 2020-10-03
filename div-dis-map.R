## Catherine Hulshof, PhD
## Biodiversity Data Science
## Virginia Commonwealth University
## Richmond, VA 23284
## 
## Academic Diversity Disparity Map
## 
## The following code uses data from the National Center for Education Statistics to create a map demonstrating the disparity between percent minority faculty and percent minority students across universities and colleges in the United States

#### Install and load packages ####

#install.packages(c("",...))

library(sf)
library(ggthemes)
library(tidyverse)
library(urbnmapr)
library(rnaturalearth)
library(plotly)

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
# HLOFFER: Highest Degree Offered. Bachelors to Doctorate
# C18SZSET: Institution Classification. At least four years.
sites <- inst %>% 
  filter(ICLEVEL==1,HLOFFER>=5,
         C18SZSET>=15,
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
  right_join(sites,by = "UNITID")


#### Merge faculty data from S2018_IS ####
#### HRTOTLT, HRAIANT, HRAIANM, HRAIANW, HRASIAT,HRASIAM, HRASIAW, HRBKAAT, HRBKAAM, HRBKAAW, HRHISPT, HRHISPM, HRHISPW, HRNHPIT, HRNHPIM, HRNHPIW, HRWHITT, HRWHITM, HRWHITW, HR2MORT, HR2MORM, HR2MORW

axes.faculty <-
  c('UNITID', 'HRTOTLT', 'HRAIANT', 'HRAIANM', 'HRAIANW', 'HRASIAT','HRASIAM', 'HRASIAW', 'HRBKAAT', 'HRBKAAM', 'HRBKAAW', 'HRHISPT', 'HRHISPM', 'HRHISPW', 'HRNHPIT', 'HRNHPIM', 'HRNHPIW', 'HRWHITT', 'HRWHITM', 'HRWHITW', 'HR2MORT', 'HR2MORM', 'HR2MORW')

perc.faculty <- read_csv('data_raw/S2018_IS.csv') %>% 
  filter(ARANK == 0, #All ranks
         FACSTAT == 0, #All full-time instructional staff
         SISCAT == 1) %>% #All full-time instructional staff
  select(all_of(axes.faculty)) %>% 
  right_join(perc.student,by = "UNITID")

# Hispanic/Latinx disparity
perc.faculty$disp_hs <- round(perc.faculty$PCTENRHS-
  (perc.faculty$HRHISPT/perc.faculty$HRTOTLT*100),2)
disparity_hs <- filter(perc.faculty, !is.na(disp_hs))

# Black disparity
perc.faculty$faculty_bk <- round(perc.faculty$HRBKAAT/perc.faculty$HRTOTLT*100,2)
perc.faculty$disp_bk <- round(perc.faculty$PCTENRBK-
                                perc.faculty$faculty_bk,2)
disparity_bk <- filter(perc.faculty, !is.na(disp_bk))


#### Map institutions ####
#### 4-year institutions (ICLEVEL==1)
#### Offers Bachelor's degree or higher (HLOFFER>=5)
#### 4-year large (C18SZSET>=15)
#### Remove faith-based institutions (CARNEGIE!=51) 
#### All faculty ranks (ARANK==0)
#### All full-time instructional staff (FACSTAT == 0)
#### Allfull-time insructional staff (SISCAT == 1)

#### Disparity Map - Hispanic/LatinX ####
hisp <- states_sf_pr %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = disparity_hs, 
             aes(x = LONGITUD, y = LATITUDE,
                 color = disp_hs, size = PCTENRHS), shape = 19)+
  scale_color_viridis_c(option = "magma")+
  labs(title = "Academic Diversity Disparity Map - Hispanic/Latinx")+
  theme(legend.position="bottom", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.margin = 
          margin(t = 0, r = 1.5, b = 0, l = 0, unit = "cm"))+
  guides(size = guide_legend(order=1))+
  annotate("text", x = -103, y = 14, 
           label = "Percent Student Hispanic/Latinx")+
  annotate("text", x = -82, y = 14, 
           label = "Percent Disparity")+
  coord_sf(xlim = c(-125, -60), 
                  ylim = c(15, 50), clip = "off")

#### Disparity Map - African American/Black ####
aabk <- ggplot()+
  geom_sf(data=states_sf_pr, fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = disparity_bk, 
             aes(x = LONGITUD, y = LATITUDE,
                 color = disp_bk, size = PCTENRBK,
                 text = paste(INSTNM, "<br>", 
                              "Student:", PCTENRBK,"%", "<br>",
                              "Faculty:", faculty_bk,"%",'<br>',
                              "Disparity:", disp_bk,"%")), 
             shape = 19)+
  scale_color_viridis_c(option="magma")+
  labs(title = "Academic Diversity Disparity Map - African American/Black")+
  theme(legend.position="bottom", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.margin = 
          margin(t = 0, r = 1.5, b = 0, l = 0, unit = "cm"))+
  guides(size = guide_legend(order=1))+
  annotate("text", x = -103, y = 14, 
           label = "Percent Student African American/Black")+
  annotate("text", x = -82, y = 14, 
           label = "Percent Disparity")+
  coord_sf(xlim = c(-125, -60), 
           ylim = c(15, 50), clip = "off")

ggplotly(aabk, tooltip = c('text'))

## NEXT: WORK ON LEGENDS AND LEGEND TITLES...
## #https://towardsdatascience.com/how-to-create-a-plotly-visualization-and-embed-it-on-websites-517c1a78568b