library(shiny)
library(tidyverse)
library(shinyWidgets)

alcohol <- read_csv(here::here("week13_alcohol_global.csv"))


ui <- fluidPage(

    titlePanel("Global alcohol Comsumption"),

    tabsetPanel(
      tabPanel("1 The total litres of pure alcohol"
,
    h3("1.1 What number of bins do you stop seeing the count of country distribution ?"),
    fluidRow(
      sidebarLayout(
          sidebarPanel(
              sliderInput("bins",
                          "Number of bins:",
                          min = 0.1,
                          max = 1,
                          value = 0.5,
                          step = 0.1)
          ),

          mainPanel(
             plotOutput("distPlot"))
      )
    ),

    h3("1.2 Let's choose number to see country name which the total litres of pure alcohol in the range."),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          numericInput("n1", "The total litres of pure alcohol >=",
                       value = 5, min = 0, max = 14, step = 0.5),
          numericInput("n2", "The total litres of pure alcohol <=",
                       value = 10, min = 0, max = 15, step = 0.5)
      ),
        mainPanel(
          textOutput("countryname")
        )
      )
    ),
),

  tabPanel("2 Alcohol consumption",

    h3("2.1 Let's see the world consumption of alcohol "),
     fluidRow(
      sidebarLayout(
          sidebarPanel(
            radioButtons("Q1", "Do you like drink?",
                         choices = c("Yes",
                                     "No")),
            selectizeInput("type", "If you like drink, which one would you like to drink?
                           (or which one would you like to show in the plot)",
                           choices = c("Beer", "Spirit", "Wine"),
                           selected = "Beer"),

          h5("Adjust the number of bins (if needed):"),
          sliderInput("bins2",
                      "Number of bins:",
                      min = 1,
                      max = 30,
                      value = 10)
        ),
        mainPanel(
            plotOutput("disalcohol")
          )
          )
        ),

    h3("2.2 Let's make a plot"),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectizeInput("countries",
                         "Select Countries to show the plot(less than 10 countries):",
                         choices = c(alcohol$country),
                         selected = c("Guinea-Bissau", "Honduras", "Israel", "Lesotho", "Mauritius", "Samoa", "United Arab Emirates", "Zambia"),
                         multiple = TRUE,
                         options = list(maxItems = 10)
          ),
          radioButtons("servings", "Select one alcohol servings to show the plot",
                       choices = c(  "beer servings",
                                    "spirit servings",
                                   "wine servings"),
                       selected = "beer servings"
                       )
        ),

        mainPanel(
          plotOutput("countrycons")
         # textOutput("text")
        )
      )
    )
    ),

tabPanel("About",
  fluidRow(
       column(10,
              div(class = "about",
                  uiOutput('about'))
       )
   ))),
     includeCSS("styles.css")
)







 server <- function(input, output) {

  output$distPlot <- renderPlot({
    ggplot(alcohol, aes(x = total_litres_of_pure_alcohol))+
      geom_histogram(binwidth = input$bins, color = "white")+
      scale_x_continuous(breaks = seq(0,15,1))+
      xlab("Total litres of pure alcohol")+
      ggtitle("Count of countries which the total litres of pure alcohol")+
      theme(element_text(size = 30))+
      theme_bw(base_size = 14)
     })

  output$countryname <- renderText({
    if (input$n1 > input$n2){

      "Please set the right range."

    } else {
    country_choose <- alcohol %>%
              filter(total_litres_of_pure_alcohol >= input$n1 &
                     total_litres_of_pure_alcohol <= input$n2) %>%
        select(country) %>%
        pull()
    }

     })


  output$disalcohol <- renderPlot({
   if (input$type == "Beer"){
      ggplot(alcohol, aes(x = beer_servings))+
         geom_histogram(binwidth = input$bins2, color = "white")+
         xlab("Beer servings")+
         ggtitle("Count of countries")+
         theme_bw(base_size = 14)

   } else if(input$type == "Spirit"){
      ggplot(alcohol, aes(x = spirit_servings))+
         geom_histogram(binwidth = input$bins2, color = "white")+
         xlab("Spirit servings")+
         ggtitle("Count of countries")+
         theme_bw(base_size = 14)
   } else {
      ggplot(alcohol, aes(x = wine_servings))+
         geom_histogram(binwidth = input$bins2, color = "white")+
         xlab("Wine servings")+
         ggtitle("Count of countries")+
         theme_bw(base_size = 14)

}
   })


  selectcountry <- reactive({
    df <- alcohol %>%
      filter(country %in% input$countries)
    return(df)
  })

  output$countrycons <- renderPlot({

    if (input$servings == "beer servings"){
      selectcountry() %>%
      ggplot( aes(x = country, y = beer_servings))+
        geom_col()+
        xlab("Country")+
        ylab("Beer servings")+
        ggtitle("Beer servings by country")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        theme_bw(base_size = 14)

    } else if(input$servings == "spirit servings"){
      selectcountry() %>%
        ggplot( aes(x = country, y = spirit_servings))+
        geom_col()+
        xlab("Country")+
        ylab("Spirit servings")+
        ggtitle("Spirit servings by country")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        theme_bw(base_size = 14)

    } else {
      selectcountry() %>%
        ggplot( aes(x = country, y = wine_servings))+
        geom_col()+
        xlab("Country")+
        ylab("Wine servings")+
        ggtitle("Wine servings by country")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        theme_bw(base_size = 14)
    }
  })


  # sample1 <- reactive({
  #   sample(c("Awesome!", "That's great!", "Well done!"),
  #          size = 1,
  #          replace = FALSE)
  # })
  #
  # output$text <- renderText({
  #     sample1()
  #
  # })

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}




 shinyApp(ui = ui, server = server)



