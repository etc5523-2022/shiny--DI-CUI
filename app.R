library(shiny)
library(tidyverse)
alcohol <- read_csv(here::here("week13_alcohol_global.csv"))


ui <- fluidPage(

    titlePanel("Global alcohol Comsumption"),

    h2("The total litres of pure alcohol"),
    h3("1.1 What number of bins do you stop seeing the count of country distribution ?"),
    fluidRow(
      sidebarLayout(
          sidebarPanel(
              sliderInput("bins",
                          "Number of bins:",
                          min = 0.1,
                          max = 1,
                          value = 0.5)
          ),

          mainPanel(
             plotOutput("distPlot"))
      )
    ),

    h3("1.2 Let's choose a number to see country name which the total litres of pure alcohol is equal or greater than the number."),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          numericInput("n", "The total litres of pure alcohol >= ",
                       value = 10, min = 0, max = 14.4)
        ),

        mainPanel(
          textOutput("countryname") #plot
        )
      )
    ),

    h2("2 Alcohol consumption by country"),

    h3("2.1 Let's see the world consumption of one alcohol "),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          radioButtons("Q1", "Do you like drink?",
                                      choices = c("Yes",
                                                  "No")),
          radioButtons("Q2", "If you like drink, which one would you like to drink?",
                                    choices = c("Beer",
                                                "Spirit",
                                                "Wine")),
          h4("Adjust the number of bins (if needed):"),

          sliderInput("bins2",
                      "Number of bins:",
                      min = 1,
                      max = 30,
                      value = 10)
        ),
        mainPanel(
            plotOutput("alcohol_consumption")
          )
          )
        ),

    h3("2.2 Let's make a plot"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectizeInput("countries", "Select Countries to show the plot",
                         choices = c(alcohol$country),
                         multiple = TRUE
          ),
          radioButtons("alcohol", "Select one alcohol to show the plot",
                       choices = c("Beer",
                                   "Spirit",
                                   "Wine"))
        ),

        mainPanel(
          plotOutput("country_consumption"),
          textOutput("text")
        )
      )
    ),


  fluidRow(
       column(10,
              div(class = "about",
                  uiOutput('about'))
       )
   ),
     includeCSS("styles.css")
)


 server <- function(input, output) {

}


 shinyApp(ui = ui, server = server)
