# ===============================================
# Fill in the following fields
# ===============================================
# Title: U2 Song Lyric Analysis 
# Description: Various analyses of the lyrics of U2's discography
# Author: Kalyan Sankar
# Date:


# ===============================================
# Required packages
# ===============================================
library(tidyverse)
library(tidytext)

# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
dat = read.csv('u2-lyrics.csv', col.names = c('album', 'year', 'song', 'lyrics'))

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("U2 Song Lyric Analysis - Kalyan Sankar"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Word Trend Analysis")),
           textInput(inputId = "wordtrend1", 
                        label = "Choose six words", 
                        value = 'love'),
           textInput(inputId = "wordtrend2", 
                     label = NULL, 
                     value = 'feel'),
           textInput(inputId = "wordtrend3", 
                     label = NULL, 
                     value = 'heart'),
           textInput(inputId = "wordtrend4", 
                     label = NULL, 
                     value = 'day'),
           textInput(inputId = "wordtrend5", 
                     label = NULL, 
                     value = 'night'),
           textInput(inputId = "wordtrend6", 
                     label = NULL, 
                     value = 'soul')
    ),
    
    # replace with your widgets
    column(3,
           p(em('Word Frequency Analysis')),
           sliderInput(inputId = "num_freq", 
                       label = "Number of Words",
                       min = 1,
                       max = 20,
                       value = 5,
                       step = 1),
           selectInput(inputId = 'album',
                       label = "Album",
                       choices = c('All Albums' = 'all',
                                   unique(dat$album)),
                       selected = 'all'
                      ),
           checkboxInput(inputId = 'facet',
                         label = 'Facet by Album',
                         value = FALSE)
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           radioButtons(inputId = "arrange", 
                        label = "Order bars by:", 
                        choices = c("decreasing freq" = "arr_dec",
                                    "increasing freq" = "arr_inc",
                                    "alphabetical a-z" = "arr_a2z",
                                    "alphabetical z-a" = "arr_z2a"),
                        selected = "arr_dec")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           sliderInput(inputId = "binwidth",
                       label = "Binwidth",
                       min = 1,
                       max = 20,
                       value = 1),
           checkboxInput(inputId = "facets",
                         label = strong("Facet by letter"),
                         value = FALSE)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("What kind of analysis1?"),
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("What kind of analysis2"),
                       plotOutput("histogram"),
                       hr(),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # Analysis 4: Word Trends
  # Create data set with word count for each word by year
  dat_trend <- reactive({
    #words = c(eval(paste0(input$wordtrend, 1:6)))
    #words = c(input$wordtrend1, input$wordtrend2, input$wordtrend3, input$wordtrend4, input$wordtrend5, input$wordtrend6)
    #dat = dat %>% group_by(year)
    #for (i in words){
    #  dat = dat %>% summarise(paste0)
    #}
    dat %>% group_by(year) %>% summarise('wordtrend1' = str_count(lyrics, pattern = input$wordtrend1) %>% sum(),
                                         'wordtrend2' = str_count(lyrics, pattern = input$wordtrend2) %>% sum(),
                                         'wordtrend3' = str_count(lyrics, pattern = input$wordtrend3) %>% sum(),
                                         'wordtrend4' = str_count(lyrics, pattern = input$wordtrend4) %>% sum(),
                                         'wordtrend5' = str_count(lyrics, pattern = input$wordtrend5) %>% sum(),
                                         'wordtrend6' = str_count(lyrics, pattern = input$wordtrend6) %>% sum())
    #colnames(dat) = c('year', paste('wordtrend', 1:6))
    dat = pivot_longer(
      dat,
      cols = starts_with('wordtrend'),
      names_to = 'words',
      values_to = 'freq')
  })
  
  # Analysis 1: Word Frequency Analysis
  # Create data frame with frequency of each word
  dat_freq <- reactive({
      freq = dat$lyrics %>% 
             tolower() %>% 
             str_split('\\s+') %>% 
             unlist() %>%
             table() %>%
             sort(decreasing = TRUE) %>%
             head(input$num_freq)
      freq = freq %>%
        t() %>%
        as.data.frame()
      freq = select(freq, 'words' = ., Freq)
  })
  
  
  # ===============================================
  # Outputs for the first TAB - Word Trend Analysis
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat_trend(), aes(x = year, y = freq, group = word)) +
      geom_path()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    dat_trend()
  })
  
  
  # ====================================================
  # Outputs for the second TAB - Word Frequency Analysis
  # ====================================================
  
  # code for histogram
  output$histogram <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat_freq(), aes(x = words, y = Freq, fill = Freq)) +
      geom_col() +
      scale_fill_gradient(low='#F0F2F0', high='#000C40')
  })
  
  # code for statistics
  output$table2 <- renderPrint({
    # replace the code below with your code!!!
    dat_freq()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

