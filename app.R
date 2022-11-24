library(shiny)
library(tidyverse)
library(plotly)
library(gghighlight)

alcohol <- read_csv(here::here("week13_alcohol_global.csv"))

map <- map_data("world")

map$region <- gsub("Russia", "Russian Federation", map$region)

map_data <- map %>%
  left_join(alcohol, by = c("region"= "country"))


ui <- fluidPage(

  titlePanel("Global Alcohol Comsumption"),

  tabsetPanel(
    tabPanel("1 The total litres of pure alcohol"
             ,
             h3("1.1 What number of bins do you stop seeing the count of country distribution?"),
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

             h3("1.2 What number of range do you stop seeing country which the total litres of pure alcohol in the range?"),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("Litres", "The range of total litres of pure alcohol",
                               min = 0,
                               max = 15,
                               value = c(0,15),
                               step = 0.1)
                 ),
                 mainPanel(
                   plotlyOutput("countryname")
                 )
               )
             )
    ),

    tabPanel("2 Alcohol servings",

             h3("2.1 Global alcohol sevings of beer, spirit and wine"),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(

                   selectizeInput("type", "Select alcohol servings you would like to show in the plot)",
                                  choices = c("Beer", "Spirit", "Wine"),
                                  selected = "Beer"),

                   sliderInput("range", "The range of servings",
                               min = 0,
                               max = 450,
                               value = c(0,450),
                               step = 50)
                 ),
                 mainPanel(
                   plotlyOutput("disalcohol")
                 )
               )
             ),

             h3("2.2 Let's make a plot"),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput("countries",
                                  "Select Countries to show the plot:",
                                  choices = c(alcohol$country),
                                  selected = alcohol$country[1:50],
                                  multiple = TRUE
                   ),
                   radioButtons("servings", "Select alcohol servings to show the plot",
                                choices = c(  "beer servings",
                                              "spirit servings",
                                              "wine servings"),
                                selected = "beer servings"
                   )
                 ),

                 mainPanel(
                   plotlyOutput("countrycons")
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


  #part1.1
  output$distPlot <- renderPlot({
    ggplot(alcohol, aes(x = total_litres_of_pure_alcohol))+
      geom_histogram(binwidth = input$bins, color = "white", fill = "#9ebcda")+
      scale_x_continuous(breaks = seq(0,15,1))+
      xlab("Total litres of pure alcohol")+
      ggtitle("Distribution of the total litres of pure alcohol")+
      theme(element_text(size = 30))+
      theme_bw(base_size = 14)
  })




  #part1.2
  min <- reactive({input$Litres[1]})
  max <- reactive({input$Litres[2]})

  new_map_data <- reactive({
    new_map_data <- map_data %>%
      filter(total_litres_of_pure_alcohol >= min() &
               total_litres_of_pure_alcohol <= max())

  })

  output$countryname <- renderPlotly({

    p1 <- new_map_data() %>%
      ggplot( aes(x = long, y = lat, group = group,
                  text = sprintf("region: %s<br>total litres of pure alcohol: %s",
                                 region, total_litres_of_pure_alcohol)))+
      geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                   fill = "white", colour = "#252525")+
      geom_polygon( aes(fill = total_litres_of_pure_alcohol), color = "#252525")+
      scale_fill_gradient(low = "#f7fcfd",
                          high = "#4d004b",
                          na.value = "grey",
                          breaks = c(0,2,4,6,8,10,12,14),
                          labels = c(0,2,4,6,8,10,12,14))+
      theme_void()+
      theme(panel.grid = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank())+
      labs(fill = "Litres",
           title = "Global alcohol consumption")

    plotly::ggplotly(p1,tooltip = "text")

  }

  )

  #part2.1

  min_range <- reactive({input$range[1]})
  max_range <- reactive({input$range[2]})

  range_map_data <- reactive({
    if (input$type == "Beer"){
      map_data %>%
        filter(beer_servings >= min_range() &
                 beer_servings <= max_range())
    }else if (input$type == "Spirit"){
      map_data %>%
        filter(spirit_servings >= min_range() &
                 spirit_servings <= max_range())
    }else {
      map_data %>%
        filter(wine_servings >= min_range() &
                 wine_servings <= max_range())
    }

  })

  output$disalcohol <- renderPlotly({
    if (input$type == "Beer"){

      p1 <- range_map_data() %>%
        ggplot( aes(x = long, y = lat, group = group,
                    text = sprintf("region: %s<br>beer servings: %s",
                                   region, beer_servings)))+
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                     fill = "white", colour = "#252525")+
        geom_polygon( aes(fill = beer_servings), color = "#252525")+
        scale_fill_gradient(low = "#ffffcc",
                            high = "#800026",
                            na.value = "grey",
                            breaks = c(0,50,100,150,200,250,300,350,400,450),
                            labels = c(0,50,100,150,200,250,300,350,400,450))+
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank())+
        labs(fill = "Servings",
             title = "Beer global alcohol servings")

      plotly::ggplotly(p1,tooltip = "text")

    } else if(input$type == "Spirit"){
      p1 <- range_map_data() %>%
        ggplot( aes(x = long, y = lat, group = group,
                    text = sprintf("region: %s<br>spirit servings: %s",
                                   region, spirit_servings)))+
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                     fill = "white", colour = "#252525")+
        geom_polygon( aes(fill = spirit_servings), color = "#252525")+
        scale_fill_gradient(low = "#f7fcfd",
                            high = "#00441b",
                            na.value = "grey",
                            breaks = c(0,50,100,150,200,250,300,350,400,450),
                            labels = c(0,50,100,150,200,250,300,350,400,450))+
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank())+
        labs(fill = "Servings",
             title = "Spirit global alcohol servings")

      plotly::ggplotly(p1,tooltip = "text")

    } else {
      p1 <- range_map_data() %>%
        ggplot( aes(x = long, y = lat, group = group,
                    text = sprintf("region: %s<br>wine servings: %s",
                                   region, wine_servings)))+
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                     fill = "white", colour = "#252525")+
        geom_polygon( aes(fill = wine_servings), color = "#252525")+
        scale_fill_gradient(low = "#f7fcfd",
                            high = "#4d004b",
                            na.value = "grey",
                            breaks = c(0,50,100,150,200,250,300,350,400,450),
                            labels = c(0,50,100,150,200,250,300,350,400,450))+
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank())+
        labs(fill = "Servings",
             title = "wine global alcohol servings")

      plotly::ggplotly(p1,tooltip = "text")

    }
  })



  #part2.2
  selectcountry <- reactive({
    map_data %>%
      filter(region %in% input$countries)
  })

  output$countrycons <- renderPlotly({

    if (input$servings == "beer servings"){
      p1 <- selectcountry() %>%
        ggplot( aes(x = long, y = lat, group = group,
                    text = sprintf("region: %s<br>beer servings: %s",
                                   region, beer_servings)))+
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                     fill = "white", colour = "#252525")+
        geom_polygon( aes(fill = beer_servings), color = "#252525")+
        scale_fill_gradient(low = "#ffffcc",
                            high = "#800026",
                            na.value = "grey",
                            breaks = c(0,50,100,150,200,250,300,350,400,450),
                            labels = c(0,50,100,150,200,250,300,350,400,450))+
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank())+
        labs(fill = "Servings",
             title = "Beer global alcohol servings")

      plotly::ggplotly(p1,tooltip = "text")

    } else if(input$servings == "spirit servings"){
      p1 <- selectcountry() %>%
        ggplot( aes(x = long, y = lat, group = group,
                    text = sprintf("region: %s<br>spirit servings: %s",
                                   region, spirit_servings)))+
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                     fill = "white", colour = "#252525")+
        geom_polygon( aes(fill = spirit_servings), color = "#252525")+
        scale_fill_gradient(low = "#f7fcfd",
                            high = "#00441b",
                            na.value = "grey",
                            breaks = c(0,50,100,150,200,250,300,350,400,450),
                            labels = c(0,50,100,150,200,250,300,350,400,450))+
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank())+
        labs(fill = "Servings",
             title = "Spirit global alcohol servings")

      plotly::ggplotly(p1,tooltip = "text")

    } else  {
      p1 <- selectcountry() %>%
        ggplot( aes(x = long, y = lat, group = group,
                    text = sprintf("region: %s<br>wine servings: %s",
                                   region, wine_servings)))+
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group),
                     fill = "white", colour = "#252525")+
        geom_polygon( aes(fill = wine_servings), color = "#252525")+
        scale_fill_gradient(low = "#f7fcfd",
                            high = "#4d004b",
                            na.value = "grey",
                            breaks = c(0,50,100,150,200,250,300,350,400,450),
                            labels = c(0,50,100,150,200,250,300,350,400,450))+
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank())+
        labs(fill = "Servings",
             title = "wine global alcohol servings")

      plotly::ggplotly(p1,tooltip = "text")
    }
  })



  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}




shinyApp(ui = ui, server = server)



