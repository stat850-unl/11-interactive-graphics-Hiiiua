#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)


# Read in the data and keep wanted columns
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
cocktails <- cocktails[is.na(cocktails$alcoholic)==FALSE,c('drink','category', 'alcoholic', 'ingredient')]
cocktails$alcoholic[which(cocktails$alcoholic=="Non alcoholic")] <- 'Non Alcoholic'
cocktails <- data.frame(cocktails)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cocktail"),

    # Sidebar with a slider input for alcoholic type 
    sidebarPanel(
        selectInput("alco_type", "Alcoholic Type", choices = unique(cocktails$alcoholic),selected = "Alcoholic")
    ),
    # Plot of barchart
    tabsetPanel(
        tabPanel("Barchart", plotOutput("alcoh"))
    ),
    
    # Input text for ingredient 
    inputPanel(
        textInput("ingredient", "Search by Ingredient",value = 'Cacao'),
    ),
    # Print the name and ingredients
    verbatimTextOutput("recipe")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Filter the specific alcoholic subset 
    cocktail_subset <- reactive({
        cocktails %>% filter(alcoholic %in% input$alco_type)
    })
    # Filter the specific ingredient
    cocktail_ingredient <- reactive({
        cocktails[grepl(input$ingredient,
                        cocktails$ingredient,
                        fixed = TRUE,
                        ignore.case = TRUE ),]
    })
    
    # Renderplot bar chart
    output$alcoh <- renderPlot({
        # generate bins based on input$bins from ui.R
        cocktail_sub <- cocktail_subset()
        ggplot(data = cocktail_sub, aes(x = category, fill = category)) +
            geom_bar(stat = "count")+
            theme_bw() +
            ggtitle(paste("Number of", input$alco_type, "categories"))+
            theme(axis.text.x = element_text(angle=60, hjust = 1))
    })
    # Renderprint the name and ingredients
    output$recipe <- renderPrint({
        cocktail_ing <- cocktail_ingredient()
        cocktail_ing <- cocktail_ing[,c('drink', 'ingredient')]
        unique(cocktail_ing)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
