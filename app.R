library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(rworldmap)

views <- read.csv("ViewingActivity.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("Netflix View-Activity Analysis."),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Cumulative view time."),
      plotOutput(outputId = "plot_1", height = 250),
      
      hr(),
      
      h4("Session duration distribution."),
      plotOutput(outputId = "plot_2", height = 400),
    ),
    
    mainPanel(
      width = 9, 
      
      hr(),
      
      fluidRow(
        # Margin.
        column(1),
        
        column(8,
               plotOutput(outputId = "plot_4", height = 250),
        ),
        
        column(3,
               h4("How many parallel streams do I need?"),
               helpText("This chart visualizes a time-line representation of the
                         view activity for the date slected below. This helps
                         answer the question of how many concurrently active Netflix
                         streams our account uses."),
               hr(),
               dateInput("date1", "Date:", value = "2020-04-04"),
        )
      ),
      
      hr(),
      
      fluidRow(
        # Margin.
        column(1),
      
        column(8,
               plotOutput(outputId = "plot_3", height = 500)
        ),
        
        column(3,
               h4("Spot any spurious accesses?"),
               helpText("This chart visualizes the location information associated
                        with the view activity on a world map. knowing where we access the account
                        from, this can help sound off early alarms on any potentially
                        un-authorized accesses."),
        )
      )
      
    )
  )
)


server <- function(input, output) {
  
  output$plot_1 <- renderPlot({
    # View time by each user.
    views %>%
      group_by(Profile.Name) %>%
      summarize(time_secs = sum(period_to_seconds(hms(Duration)))) %>%
      mutate(Hours = time_secs / 3600) %>%
      ggplot(aes(x = Profile.Name, y = Hours)) + geom_bar(stat="identity") + xlab("") + ylab("Hours")
    
  })
  
  output$plot_2 <- renderPlot({
    # Distribution of hours of content viewed.
    views %>%
      mutate(Hours = period_to_seconds(hms(Duration)) / 3600) %>%
      ggplot(aes(x = Profile.Name, y = Hours, fill = Profile.Name)) + 
      geom_boxplot(alpha=0.3) +
      theme(legend.position="none") +
      scale_fill_brewer(palette="BuPu") + xlab("")
  })
  

  output$plot_3 <- renderPlot({
    view_time_by_country <- views %>%
      group_by(Country) %>%
      summarize(time_secs = sum(period_to_seconds(hms(Duration)))) %>%
      mutate(Country = word(Country, 1), Hours = time_secs / 3600)
    
    malMap <- joinCountryData2Map(data.frame(view_time_by_country), joinCode = "ISO2", nameJoinColumn = "Country")
    mapCountryData(malMap, nameColumnToPlot="Hours", catMethod = "fixedWidth", missingCountryCol = gray(.8))
    
  })
  
  output$plot_4 <- renderPlot({
    day_of_interest <- input$date1
    views %>%
      mutate(start = as_datetime(Start.Time),
             end = start + hms(Duration),
             day_tmp = as.factor(date(start))) %>%
      rename(User = Profile.Name) %>%
      filter(date(start) == date(day_of_interest)) %>%
      select(User, start, end, day_tmp) %>%
      ggplot(aes(x = start, xend = end, y = User, yend = User, color = User)) +
      geom_segment(size = 7) + xlab("") + ylab("") + facet_wrap(~day_tmp, ncol=1)
  })
  
}

shinyApp(ui = ui, server = server)
