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
                        choices = c("All" = "TOTAL","African American / Black" = "Black","American Indian / Alaska Native" = "AmeriIndian", "Asian" = "Asian / Pacific Islander","Hispanic / Latinx" = "Hispanic"), selected = "All"), 
            br(), 
            p("Data from: U.S. Department of Education. Institute of Education Sciences, National Center for Education Statistics."),
            p(HTML("Catherine Hulshof, PhD<br/>Biodiversity Data Science<br/>Virginia Commonwealth University"))),
        mainPanel(plotlyOutput("plot1"))
        )
    )

choicesVec <- c("All" = "TOTAL","African American / Black" = "Black","American Indian / Alaska Native" = "AmeriIndian", "Asian" = "Asian","Hispanic / Latinx" = "Hispanic")

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
            annotate("text", x = -122, y = 20,
                     label = "Student \nPopulation  (%)")+
            geom_point(aes(x = -115, y = 20),size = 1)+
            geom_point(aes(x = -109, y = 20),size = 2)+
            geom_point(aes(x = -103, y = 20),size = 3)+
            geom_point(aes(x = -97, y = 20),size = 4)+
            annotate("text", x = -112, y = 20,label = "0")+
            annotate("text", x = -106, y = 20,label = "20")+
            annotate("text", x = -100, y = 20,label = "40")+
            annotate("text", x = -94, y = 20,label = "60")
        
        ggplotly(disp, tooltip = c('text'))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
