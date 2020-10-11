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
            selectInput(inputId = "Group",  # Give the input a name
                        label = "1. Select Group",  # Give the input a label to be displayed in the app
                        choices = c("All" = "TOTAL","African American / Black" = "black","American Indian / Alaska Native" = "amnat", "Asian" = "asian","Hispanic / Latinx" = "hispanic"), selected = "All")  # Create the choices that can be selected. e.g. Display "All" and link to value "all"
        ),
        mainPanel()
    )
)

# server.R ----
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
