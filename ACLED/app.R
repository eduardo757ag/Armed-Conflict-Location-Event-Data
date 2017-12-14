
ui <- fluidPage(
  titlePanel("Armed Conflict Location Event"), # App title
  sidebarLayout(
    sidebarPanel( # Sidebar panel for inputs
      selectInput(inputId = "EVENT_TYPE", 
                  label = "Event type",
                  choices = unique(ACLED$EVENT_TYPE)
      ),
      selectInput(inputId = "country", 
                  label = "Country",
                  choices = unique(ACLED$country)
      )),
    mainPanel(
      tabsetPanel(
        tabPanel("Armed Conflict Location Event", plotOutput("lineChart"))
        # Output: Line Chart
      )
    )
  ))


server <- function(input, output) {
  
  # make line charts of events
  output$lineChart <- renderPlot({
    chartData <- switch(input$ACLED,
                        "World" = list(dat$gr_w,dat$re_w),
                        "All countries except China" = list(dat$gr_noChina,dat$re_noChina),
                        "Advanced Economies only" = list(dat$gr_advanced,dat$re_advanced),
                        "Eurozone" = list(dat$gr_euro,dat$re_euro) 
    )  
    ACLED$count <- as.numeric(ACLED$count)
    g<-ggplot(data=ACLED,aes(x=year, y=count, group=country))
    g<-g+geom_line() + 
      geom_point() +
      ggtitle(paste("Armed Conflicts")) +
      xlab("Year") + ylab("The number of reported cases")
    g
  })
  
}

shinyApp(ui = ui, server = server)
