library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Loading data ----
Barley <- as.data.frame(beaven.barley)

# ui.R ----
ui <- fluidPage(
    titlePanel("Diversity Disparity"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "group",  # Give the input a name
                        label = "1. Select Group",  # Give the input a label to be displayed in the app
                        choices = c("All" = "all","African American / Black" = "black","Asian" = "asian","Hispanic / Latinx" = "hispanic","American Indian / Alaska Native" = "amnat","Native Hawaiian / Pacific Islander" = "hawaii"), selected = "All"),  # Create the choices that can be selected. e.g. Display "All" and link to value "all"
            selectInput(inputId = "Gender", 
                        label = "2. Select Gender", 
                        choices = c("All", "Female","Male"),
                                    selected = "All"),
            textInput(inputId = "text", 
                      label = "4. Enter some text to be displayed", "")
        ),
        mainPanel()
    )
)

# server.R ----
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
