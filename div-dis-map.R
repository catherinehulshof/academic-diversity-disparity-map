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
library(extrafont)
library(shiny)
library(ggpubr)

#font_import(path = "C:/Users/*insert your user name*/AppData/Local/Microsoft/Windows/Fonts", pattern = ".TTF")

#loadfonts()
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
# CARNEGIE: Removed specialized schools (faith-based, medical, Schools of engineering and technology, Schools of business and management, Schools of art, music, and design, Schools of law Teachers colleges, Other specialized institutions) and not accredited (CARNEGIE ==-2), keep tribal (60).

sites <- inst %>% 
  filter(ICLEVEL==1,HLOFFER>=5,
         C18SZSET>=15,
         CARNEGIE < 51 | CARNEGIE ==60, CARNEGIE != -2)

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
#HRTOTLT: Grand total
#HRAIANT/HRAIANM/HRAIANW: American Indian or Alaska Native total/men/women
#HRASIAT/HRASIAM/HRASIAW: Asian total/men/women
#HRBKAAT/HRBKAAM/HRBKAAW: Black or African American total/men/women
#HRHISPT/HRHISPM/HRHISPW Hispanic or Latino total/men/women
#HRNHPIT/HRNHPIM/HRNHPIW: Native Hawaiian or Other Pacific Islander total/men/women
#HRWHITT, HRWHITM, HRWHITW: White total/men/women
#HR2MORT, HR2MORM, HR2MORW: Two or more races total/men/women

axes.faculty <-
  c('UNITID', 'HRTOTLT', 'HRAIANT', 'HRAIANM', 'HRAIANW', 'HRASIAT','HRASIAM', 'HRASIAW', 'HRBKAAT', 'HRBKAAM', 'HRBKAAW', 'HRHISPT', 'HRHISPM', 'HRHISPW', 'HRNHPIT', 'HRNHPIM', 'HRNHPIW', 'HRWHITT', 'HRWHITM', 'HRWHITW', 'HR2MORT', 'HR2MORM', 'HR2MORW', 'HRNHPIT')

perc.faculty <- read_csv('data_raw/S2018_IS.csv') %>% 
  filter(ARANK == 0, #All ranks
         FACSTAT == 0, #All full-time instructional staff
         SISCAT == 1) %>% #All full-time instructional staff
  select(all_of(axes.faculty)) %>% 
  right_join(perc.student,by = "UNITID")

#### Rebuild dataset for shiny ####
faculty <- perc.faculty[,c(1:23, 102, 103)]
faculty$TOTAL <- rowSums(faculty[, c(6,9,12,15)])
faculty$HRASIAT <- faculty$HRASIAT+faculty$HRNHPIT
faculty_long <- faculty %>%
  select(UNITID, HRTOTLT, HRHISPT, HRBKAAT, HRASIAT, 
         HRAIANT, HRWHITT, TOTAL) %>% 
  rename(Hispanic = HRHISPT, Black = HRBKAAT, 
         Asian = HRASIAT, AmeriIndian = HRAIANT,
         White = HRWHITT) %>%
  gather(Faculty_Group, Faculty_PCT, Hispanic:TOTAL) %>% 
  mutate(Faculty_PCT, Faculty_PCT = round(Faculty_PCT/HRTOTLT*100,2)) %>% 
  select(UNITID, Faculty_Group, Faculty_PCT)
  
student <- perc.faculty[,c(1, 24:33)]
student$TOTAL <- rowSums(student[, 3:6])
student_long <- student %>%
  relocate(TOTAL, .after = PCTENRAN) %>%
  rename(Hispanic = PCTENRHS, Black = PCTENRBK, Asian = PCTENRAP, 
         AmeriIndian = PCTENRAN, White = PCTENRWH) %>% 
  gather(Student_Group, Student_PCT, White:TOTAL) %>% 
  select(UNITID, Student_Group, Student_PCT)

#### Join by UNITID and GROUP
groups_fs <- merge(faculty_long,student_long, by.x=c('UNITID', 'Faculty_Group'),by.y = c('UNITID', 'Student_Group'),all=FALSE)
groups_fs$Disparity <- groups_fs$Student_PCT - groups_fs$Faculty_PCT
names(groups_fs)[2] <- c('Group')

# merge LATITUD, LONGITUD and INSTNM
loc <- select(perc.faculty, UNITID, LONGITUD, LATITUDE, INSTNM)
groups_fs <- merge(groups_fs, loc, by = "UNITID") %>% 
  filter(Disparity != "NA")

#### Map institutions ####
#### 4-year institutions (ICLEVEL==1)
#### Offers Bachelor's degree or higher (HLOFFER>=5)
#### 4-year large (C18SZSET>=15)
#### Remove faith-based institutions (CARNEGIE!=51) and other specialized institutions (CARNEGIE == 51:59)
#### All faculty ranks (ARANK==0)
#### All full-time instructional staff (FACSTAT == 0)
#### All full-time instructional staff (SISCAT == 1)

#### Disparity Map - Hispanic/LatinX ####
disparity_hs <- subset(groups_fs, Group == "Hispanic")
hisp <- ggplot() +
  geom_sf(data=states_sf_pr, fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = disparity_hs, 
             aes(x = LONGITUD, y = LATITUDE,
                 color = Disparity, size = Student_PCT,
                 text = paste(INSTNM, "<br>", 
                              "Student: ", Student_PCT,"%", "<br>",
                              "Faculty: ", Faculty_PCT,"%",'<br>',
                              "Disparity: ", Disparity,"%", sep = "")), 
                 shape = 19)+
  scale_color_viridis_c(option = "magma", 
                        name = "Disparity (%)")+
  scale_size("Percent Student", range = c(0, 2))+
  labs(title = "Academic Diversity Disparity Map - Hispanic / Latinx")+
  theme(legend.position="bottom", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.margin = 
          margin(t = 0, r = 1.5, b = 0, l = 0, unit = "cm"),
        text = element_text(family = "Roboto Light"))
ggplotly(hisp, tooltip = c('text'))


#### Disparity Map - African American / Black ####
disparity_bk <- subset(groups_fs, Group == "Black")

aabk <- ggplot()+
  geom_sf(data=states_sf_pr, fill = "grey", color = "#ffffff")+
  theme_map()+
  geom_point(data = disparity_bk, 
             aes(x = LONGITUD, y = LATITUDE,size = Student_PCT, color = Disparity,
                 text = paste(INSTNM, "<br>", 
                              "Student: ", Student_PCT,"%", "<br>",
                              "Faculty: ", Faculty_PCT,"%",'<br>',
                              "Disparity :", Disparity,"%", sep = "")), 
             shape = 19)+
  scale_color_viridis_c(option="magma", 
                        name = "Disparity (%)")+
  scale_size("Percent Student", range = c(0, 3))+
  labs(title = "Academic Diversity Disparity Map - African American / Black")+
  theme(legend.position="bottom", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.margin = 
          margin(t = 0, r = 1.5, b = 0, l = 0, unit = "cm"),
        text = element_text(family = "Roboto Light"))

  #annotate("text", x = -121.5, y = 20,label = "Student \nPopulation  (%)")+
  #geom_point(aes(x = -115, y = 20),size = 0)+
  #geom_point(aes(x = -111, y = 20),size = 1)+
  #geom_point(aes(x = -107, y = 20),size = 2)+
  #geom_point(aes(x = -103, y = 20),size = 3)+
  #annotate("text", x = -112.5, y = 20,label = "0")+
  #annotate("text", x = -109, y = 20,label = "20")+
  #annotate("text", x = -105, y = 20,label = "40")+
  #annotate("text", x = -100, y = 20,label = "60")

ggplotly(aabk, tooltip = c('text'))

## #https://towardsdatascience.com/how-to-create-a-plotly-visualization-and-embed-it-on-websites-517c1a78568b

#write.csv(groups_fs,"shiny_interactivemap/data/group_div_disp.csv",quote=F)
# add text describing zoom
# inset puerto rico map...