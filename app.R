#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(DT)


# Define UI for application that draws a histogram
ui <- page_fluid(

    # Application title
    titlePanel("Game Collection"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("num_players",
                        "Number of players today:",
                        min = 1,
                        max = 13,
                        value = c(1, 13),
                        step = 1),
            sliderInput("age_range",
                        "Age recommendations:",
                        min = 4,
                        max = 18,
                        value = c(4, 18), step = 1),
            sliderInput("max_time",
                        "Maximum game time:",
                        min = 15,
                        max = 150,
                        value = 150, step = 15),
            checkboxGroupInput(
              "spieldesjahres",
              "Award winners",
              choices = list("Spiel des Jahres" = "Spiel des Jahres", 
                             "Kennerspiel" = "Kennerspiel", 
                             "Kinderspiel" = "Kinderspiel",
                             "recommended" = "recommended",
                             "other" = "other"),
              selected = c("Spiel des Jahres", "Kennerspiel",
                           "Kinderspiel", "recommended", "other")
            ),
            checkboxGroupInput(
              "type",
              "Type of game",
              choices = list("Board" = "board", 
                             "Card" = "card", 
                             "Dice" = "dice"),
              selected = c("board", "card", "dice")
            ),
            #verbatimTextOutput("if too many games match the above criteria, a random sample will be chosen")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          navset_tab( 
            nav_panel("You could play...", 
                      textOutput("message"),
                      tableOutput("shorttab")),
            nav_panel("If you want more details...",  
                      DTOutput("tab"))
            )
          
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  dataInput <- reactive({
  dat <- read_xlsx("data/game collection.xlsx") %>%
    select(game:year, 
           award = `Spiel des Jahres`,
           bgg = `bgg link`,
           category) %>% 
    rename(complexity = complex) %>% 
    mutate(Age = as.numeric(Age)) %>% 
    replace_na(list(award = "other")) %>% 
    mutate(award = ifelse(award == "nominated", "recommended", award)) %>% 
    mutate(award = factor(award, c("Spiel des Jahres", "Kennerspiel", "Kinderspiel", 
                                   "recommended", "other"))) %>% 
    mutate(num_players = map2(min, max, ~ .x:.y))
  
  pl <- input$num_players[1]:input$num_players[2]
  ag <- input$age_range[1]:input$age_range[2]
  ti <- input$max_time
  
  games_to_try <- dat %>% 
    mutate(check_players = map_lgl(num_players, ~ any(pl %in% .x)),
           check_age = map_lgl(Age, ~ any(ag %in% .x)),
           check_time = time <= ti) %>% 
    filter(check_players, check_age, check_time) %>% 
    filter(award %in% input$spieldesjahres) %>% 
    filter(category %in% input$type)
  
  games_to_try
  
  })
  
  output$totalgames <- renderText({
    k <- nrow(dataInput())
    k
  })
  output$message <- renderText({
    k <- nrow(dataInput())
    msg <- paste("There are", k, "games in your collection that meet the criteria today!")
    msg
  })
    output$tab <- renderDT({
      dataInput() %>% 
        select(game, category, Age, time, complexity, year, award)
    }, filter = "top",
    options = list(pageLength = 25),
    rownames = FALSE)
    
    output$shorttab <- renderTable({
      dataInput() %>% 
        arrange(award, -complexity) %>% 
        group_by(award, category) %>% 
        summarize(games = paste(game, collapse = ", "))
    }, 
    rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
