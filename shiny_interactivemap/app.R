# load libraries ----
library('shiny')  
library('tableHTML')
library('sf')
library('ggthemes')
library('tidyverse')
library('urbnmapr')
library('rnaturalearth')
library('rnaturalearthhires')
library('plotly')
library('extrafont')
library('ggpubr')
library('rgeos')

# Load data ----
inst <- read_csv('data/HD2018.csv')

ak <- inst %>%
    dplyr::filter(STABBR == "AK", ICLEVEL == 1, HLOFFER >= 5) %>%
    mutate(LONGITUD = LONGITUD + 33, LATITUDE = LATITUDE - 36)

hi <- inst %>%
    dplyr::filter(STABBR == "HI", ICLEVEL == 1, HLOFFER >= 5) %>%
    mutate(LONGITUD = LONGITUD + 49, LATITUDE = LATITUDE + 5.5)

inst <- inst %>%
    dplyr::filter(STABBR != "AK") %>%
    dplyr::filter(STABBR != "HI") %>%
    bind_rows(hi, ak)

# ICLEVEL: Level of Institution. Four or more years.
# CARNEGIE: Carnegie classification: Remove faith-based
# HLOFFER: Highest Degree Offered. Bachelors to Doctorate
# C18SZSET: Institution Classification. At least four years.
# CARNEGIE: Removed specialized schools (faith-based, medical, Schools of engineering and technology, Schools of business and management, Schools of art, music, and design, Schools of law Teachers colleges, Other specialized institutions) and not accredited (CARNEGIE ==-2), keep tribal (60).

sites <- inst %>%
    dplyr::filter(ICLEVEL == 1,
           HLOFFER >= 5,
           C18SZSET >= 15,
           CARNEGIE < 51 | CARNEGIE == 60,
           CARNEGIE != -2)

#HBCU <- sites %>%
#  dplyr::filter(HBCU ==1)

#TRIBAL <- sites %>%
#  dplyr::filter(TRIBAL ==1)

#### State maps from urbnmapr ####
states_sf <- get_urbn_map("states", sf = TRUE)
states_sf <- st_transform(states_sf, 4326) #change CRS to WGS84


#### Puerto Rico map from rnaturalearth ####

pr_basemap <- ne_states(country = "puerto rico",
                        returnclass = "sf")
pr_basemap <- st_transform(pr_basemap, 4326) #change CRS to WGS84
pr_basemap <- pr_basemap %>%
    select(fips, iso_a2, name, geometry) %>%
    rename(state_fips = fips,
           state_abbv = iso_a2,
           state_name = name)

#### Combine maps ####
states_sf_pr = rbind(states_sf, pr_basemap)

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
    c(
        'UNITID',
        'PCTENRWH',
        'PCTENRBK',
        'PCTENRHS',
        'PCTENRAP',
        'PCTENRAN',
        'PCTENRUN',
        'PCTENRNR',
        'PCTENRW',
        'PCTFT1ST',
        'ENRTOT'
    )

perc.student <- read_csv('data/DRVEF2018.csv') %>%
    select(all_of(axes.student)) %>%
    right_join(sites, by = "UNITID")


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
    c(
        'UNITID',
        'HRTOTLT',
        'HRAIANT',
        'HRAIANM',
        'HRAIANW',
        'HRASIAT',
        'HRASIAM',
        'HRASIAW',
        'HRBKAAT',
        'HRBKAAM',
        'HRBKAAW',
        'HRHISPT',
        'HRHISPM',
        'HRHISPW',
        'HRNHPIT',
        'HRNHPIM',
        'HRNHPIW',
        'HRWHITT',
        'HRWHITM',
        'HRWHITW',
        'HR2MORT',
        'HR2MORM',
        'HR2MORW',
        'HRNHPIT'
    )

perc.faculty <- read_csv('data/S2018_IS.csv') %>%
    dplyr::filter(ARANK == 0, #All ranks
           FACSTAT == 0, #All full-time instructional staff
           SISCAT == 1) %>% #All full-time instructional staff
    select(all_of(axes.faculty)) %>%
    right_join(perc.student, by = "UNITID")

#### Rebuild dataset for shiny ####
faculty <- perc.faculty[, c(1:23, 102, 103)]
faculty$TOTAL <- rowSums(faculty[, c(6, 9, 12, 15)])
faculty$HRASIAT <- faculty$HRASIAT + faculty$HRNHPIT
faculty_long <- faculty %>%
    select(UNITID,
           HRTOTLT,
           HRHISPT,
           HRBKAAT,
           HRASIAT,
           HRAIANT,
           HRWHITT,
           TOTAL) %>%
    rename(
        Hispanic = HRHISPT,
        Black = HRBKAAT,
        Asian = HRASIAT,
        AmeriIndian = HRAIANT,
        White = HRWHITT
    ) %>%
    gather(Faculty_Group, Faculty_PCT, Hispanic:TOTAL) %>%
    mutate(Faculty_PCT, Faculty_PCT = round(Faculty_PCT / HRTOTLT * 100, 2)) %>%
    select(UNITID, Faculty_Group, Faculty_PCT)

student <- perc.faculty[, c(1, 24:33)]
student$TOTAL <- rowSums(student[, 3:6])
student_long <- student %>%
    relocate(TOTAL, .after = PCTENRAN) %>%
    rename(
        Hispanic = PCTENRHS,
        Black = PCTENRBK,
        Asian = PCTENRAP,
        AmeriIndian = PCTENRAN,
        White = PCTENRWH
    ) %>%
    gather(Student_Group, Student_PCT, White:TOTAL) %>%
    select(UNITID, Student_Group, Student_PCT)

#### Join by UNITID and GROUP
groups_fs <-
    merge(
        faculty_long,
        student_long,
        by.x = c('UNITID', 'Faculty_Group'),
        by.y = c('UNITID', 'Student_Group'),
        all = FALSE
    )
groups_fs$Disparity <- groups_fs$Student_PCT - groups_fs$Faculty_PCT
names(groups_fs)[2] <- c('Group')

# merge LATITUD, LONGITUD and INSTNM
loc <- select(perc.faculty, UNITID, LONGITUD, LATITUDE, INSTNM)
groups_fs <- merge(groups_fs, loc, by = "UNITID") %>%
    dplyr::filter(Disparity != "NA")

# ui.R ----
ui <- fluidPage(
    tags$head(tags$style(HTML(
        "* {font-family: Roboto Light;}"
    ))),
    titlePanel("Academic Diversity Disparity Map"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "Group",
                # Give the input a name
                label = "1. Select Group",
                # input label displayed in app
                choices = c(
                    "All" = "TOTAL",
                    "African American / Black" = "Black",
                    "American Indian / Alaska Native" = "AmeriIndian",
                    "Asian / Pacific Islander" = "Asian",
                    "Hispanic / Latinx" = "Hispanic"
                ),
                selected = "All"
            ),
            br(),
            p(
                "Data Source: U.S. Department of Education. Institute of Education Sciences, National Center for Education Statistics."
            ),
            p(
                HTML(
                    "Catherine Hulshof, PhD<br/>Biodiversity Data Science<br/>Virginia Commonwealth University"
                )
            )
        ),
        mainPanel(plotlyOutput("plot1"),
                  tags$div(
                      HTML(
                          '<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" property="dct:title" rel="dct:type">Academic Diversity Disparity Map</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="http://www.biodiversityresearchlab.com" property="cc:attributionName" rel="cc:attributionURL">Dr. Catherine Hulshof</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>. The data and code used to produce this map are available on <a href="https://github.com/catherinehulshof/academic-diversity-disparity-map">GitHub</a>. </p>.'
                      )
                  ))
    )
)

choicesVec <-
    c(
        "All" = "TOTAL",
        "African American / Black" = "Black",
        "American Indian / Alaska Native" = "AmeriIndian",
        "Asian / Pacific Islander" = "Asian",
        "Hispanic / Latinx" = "Hispanic"
    )


# server.R ----
server <- function(input, output) {
    output$plot1 <- renderPlotly({
        disp <- ggplot() +
            geom_sf(data = states_sf_pr,
                    fill = "grey",
                    color = "#ffffff") +
            theme_map() +
            geom_point(
                data = groups_fs[groups_fs$Group == input$Group, ],
                alpha = 0.8,
                position = "jitter",
                aes(
                    x = LONGITUD,
                    y = LATITUDE,
                    color = Disparity,
                    size = Student_PCT,
                    text = paste(
                        INSTNM,
                        "<br>",
                        "Student: ",
                        Student_PCT,
                        "%",
                        "<br>",
                        "Faculty: ",
                        Faculty_PCT,
                        "%",
                        '<br>',
                        "Disparity: ",
                        Disparity,
                        "%",
                        sep = ""
                    )
                ),
                shape = 19
            ) +
            scale_color_viridis_c(option = "magma",
                                  name = "Disparity (%)") +
            scale_size("Student Percent", range = c(1, 4)) +
            labs(title = paste("Group:", names(choicesVec)[choicesVec ==
                                                               input$Group], sep = " ")) +
            theme(
                legend.position = "bottom",
                legend.justification = "center",
                legend.title = element_blank(),
                legend.margin =
                    margin(
                        t = 0,
                        r = 1.5,
                        b = 0,
                        l = 0,
                        unit = "cm"
                    ),
                text = element_text(family = "Roboto Light")
            ) +
            annotate(
                "text",
                x = -105,
                y = 17,
                label = "Student Population  (%)",
                size = 3
            ) +
            geom_point(aes(x = -115, y = 14), size = 1) +
            geom_point(aes(x = -109, y = 14), size = 1.5) +
            geom_point(aes(x = -103, y = 14), size = 2) +
            geom_point(aes(x = -97, y = 14), size = 2.5)
        
        ggplotly(disp, tooltip = c('text'))
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
