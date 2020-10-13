library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Loading data ----
groups_fs <- read.csv('data/group_div_disp.csv')

# ui.R ----
ui <- fluidPage(
    titlePanel("Diversity Disparity"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Group",  # Give the input a name
                        label = "1. Select Group",  # input label displayed in app
                        choices = c("All" = "TOTAL","African American / Black" = "Black","American Indian / Alaska Native" = "AmeriIndian", "Asian" = "Asian","Hispanic / Latinx" = "Hispanic"), selected = "All")  # Create the choices that can be selected. e.g. Display "All" and link to value "all"
        ),
        mainPanel(plotlyOutput("plot1"))
    )
)

# server.R ----
server <- function(input, output) {
    output$plot1 <- renderPlotly({
        disp <- ggplot() +
            geom_sf(data=states_sf_pr, fill = "grey", color = "#ffffff")+
            theme_map()+
            geom_point(data = groups_fs[groups_fs$Group == input$Group,], 
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
            labs(title = "Academic Diversity Disparity Map")+
            theme(legend.position="bottom", 
                  legend.justification = "center",
                  legend.title = element_blank(),
                  legend.margin = 
                      margin(t = 0, r = 1.5, b = 0, l = 0, unit = "cm"),
                  text = element_text(family = "Roboto Light"))
        ggplotly(disp, tooltip = c('text'))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
