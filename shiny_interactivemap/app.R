library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from
library(tableHTML) # for make_css function

# Loading data ----
#groups_fs <- read.csv('data/group_div_disp.csv')

# ui.R ----
ui <- fluidPage(
    tags$head(tags$style(HTML("* {font-family: Roboto Light;}"))),
    titlePanel("Academic Diversity Disparity Map"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Group",  # Give the input a name
                        label = "1. Select Group",  # input label displayed in app
                        choices = c("All" = "TOTAL",
                                    "African American / Black" = "Black",
                                    "American Indian / Alaska Native" = "AmeriIndian", 
                                    "Asian / Pacific Islander" = "Asian",
                                    "Hispanic / Latinx" = "Hispanic"), selected = "All"), 
            br(), 
            p("Data Source: U.S. Department of Education. Institute of Education Sciences, National Center for Education Statistics."),
            p(HTML("Catherine Hulshof, PhD<br/>Biodiversity Data Science<br/>Virginia Commonwealth University"))),
        mainPanel(
            plotlyOutput("plot1"),
            tags$div(HTML('<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" property="dct:title" rel="dct:type">Academic Diversity Disparity Map</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="http://www.biodiversityresearchlab.com" property="cc:attributionName" rel="cc:attributionURL">Dr. Catherine Hulshof</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.')))
        )
    )

choicesVec <- c("All" = "TOTAL","African American / Black" = "Black","American Indian / Alaska Native" = "AmeriIndian", "Asian / Pacific Islander" = "Asian","Hispanic / Latinx" = "Hispanic")


# server.R ----
server <- function(input, output) {
    output$plot1 <- renderPlotly({
        disp <- ggplot() +
            geom_sf(data=states_sf_pr, fill = "grey", color = "#ffffff")+
            theme_map()+
            geom_point(data = groups_fs[groups_fs$Group == input$Group,], alpha = 0.8, position = "jitter",
                       aes(x = LONGITUD, y = LATITUDE,
                           color = Disparity, size = Student_PCT,
                           text = paste(INSTNM, "<br>", 
                                        "Student: ", Student_PCT,"%", "<br>",
                                        "Faculty: ", Faculty_PCT,"%",'<br>',
                                        "Disparity: ", Disparity,"%", sep = "")), 
                       shape = 19)+
            scale_color_viridis_c(option = "magma", 
                                  name = "Disparity (%)")+
            scale_size("Student Percent", range = c(1, 4))+
            labs(title = paste("Group:",names(choicesVec)[choicesVec ==input$Group],sep=" "))+
            theme(legend.position="bottom", 
                  legend.justification = "center",
                  legend.title = element_blank(),
                  legend.margin = 
                      margin(t = 0, r = 1.5, b = 0, l = 0, unit = "cm"),
                  text = element_text(family = "Roboto Light"))+
            annotate("text", x = -105, y = 17,
                     label = "Student Population  (%)",size =3)+
            geom_point(aes(x = -115, y = 14),size = 1)+
            geom_point(aes(x = -109, y = 14),size = 1.5)+
            geom_point(aes(x = -103, y = 14),size = 2)+
            geom_point(aes(x = -97, y = 14),size = 2.5)
            #annotate("text", x = -112, y = 20,label = "0")+
            #annotate("text", x = -106, y = 20,label = "20")+
            #annotate("text", x = -100, y = 20,label = "40")+
            #annotate("text", x = -94, y = 20,label = "60")
        
        ggplotly(disp, tooltip = c('text'))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
