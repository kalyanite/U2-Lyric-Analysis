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
library(shinyWidgets)
library(ggrepel)

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
    column(6,
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
    column(6,
           p(em('Word Frequency Analysis')),
           sliderInput(inputId = "num_freq", 
                       label = "Number of Words",
                       min = 1,
                       max = 20,
                       value = 15,
                       step = 1),
           selectInput(inputId = 'album',
                       label = "Album",
                       choices = c('All Albums' = 'all',
                                   unique(dat$album)),
                       selected = 'all'
                      ),
           checkboxInput(inputId = 'facet',
                         label = 'Facet by Album',
                         value = FALSE),
           checkboxInput(inputId = 'stopword',
                         label = 'Exclude Stopwords',
                         value = FALSE)),
  hr()),
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("What kind of analysis1?"),
                       plotOutput("barplot"),
                       fluidRow(
                         column(12, align = 'center',
                                sliderTextInput(inputId = 'time',
                                                label = NULL,
                                                choices = unique(dat$year),
                                                width = validateCssUnit('85%'),
                                                selected = 2017))),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("What kind of analysis2"),
                       plotOutput("histogram"),
                       hr(),
                       dataTableOutput('table2'))
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
    dat_trend = dat %>% group_by(year) %>% summarise('wordtrend1' = str_count(lyrics, pattern = input$wordtrend1) %>% sum(),
                                         'wordtrend2' = str_count(lyrics, pattern = input$wordtrend2) %>% sum(),
                                         'wordtrend3' = str_count(lyrics, pattern = input$wordtrend3) %>% sum(),
                                         'wordtrend4' = str_count(lyrics, pattern = input$wordtrend4) %>% sum(),
                                         'wordtrend5' = str_count(lyrics, pattern = input$wordtrend5) %>% sum(),
                                         'wordtrend6' = str_count(lyrics, pattern = input$wordtrend6) %>% sum())
    #colnames(dat) = c('year', paste('wordtrend', 1:6))
    dat_trend = pivot_longer(
      dat_trend,
      cols = starts_with('wordtrend'),
      names_to = 'words',
      values_to = 'freq')
  })
  
  # Analysis 1: Word Frequency Analysis
  # Create data frame with frequency of each word
  dat_freq <- reactive({
    dat_freq = dat
    if(input$album != 'all'){
      dat_freq = filter(dat, album == input$album)
    }
    lyrics = data.frame(text = dat_freq$lyrics)
    freq = lyrics %>% 
      unnest_tokens(output = word, input = text)
    if(input$stopword){
      freq = freq %>% anti_join(stop_words, by = 'word')
    }
    if(input$facet){
      freq = freq %>% count(word, album)
    }
    else{
      freq = freq %>% count(word)
    }
    freq = freq %>%
      arrange(desc(n)) %>%
      slice_head(n = input$num_freq)

  })
  
  
  # ===============================================
  # Outputs for the first TAB - Word Trend Analysis
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    # replace the code below with your code!!!
    current_year = dat_trend() %>% filter(year == input$time)
    ggplot(data = dat_trend(), aes(x = year, y = freq, color = words)) +
      geom_path() + 
      geom_point(data = current_year) +
      geom_text_repel(data = current_year, aes(label = freq)) +
      scale_color_hue(labels = c(input$wordtrend1, 
                                 input$wordtrend2, 
                                 input$wordtrend3, 
                                 input$wordtrend4, 
                                 input$wordtrend5, 
                                 input$wordtrend6)) +
      xlab('Year') + ylab('Frequency') +
      xlim(NA, input$time) + expand_limits(x=2017)
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    dat_trend()$ %>% 
  })
  
  
  # ====================================================
  # Outputs for the second TAB - Word Frequency Analysis
  # ====================================================
  
  # code for histogram
  output$histogram <- renderPlot({
    # replace the code below with your code!!!
    plot = ggplot(data = dat_freq(), aes(x = reorder(word, -n), y = n, fill = n)) +
      geom_col() +
      scale_fill_gradient(low='#1f4037', high='#7CC4A2') +
      xlab('Words') + ylab('Count')
    if(input$facet) {
      plot = plot + facet_wrap(~album)
    }
    plot
    })
  
  # code for statistics
  output$table2 <- renderDataTable({
    dat_freq()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

