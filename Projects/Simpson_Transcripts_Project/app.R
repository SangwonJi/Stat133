# ===============================================
# Fill in the following fields
# ===============================================
# Title: Text Analysis of the Simpsons Transcripts
# Description:
# Author:Sangwon Ji
# Date:12-02-2022


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(shiny)
library(tidytext)
library(tidyr)
library(tidyverse)
library(wesanderson)
library(dplyr)

# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "simpsons-transcripts.txt")

dat <- read.delim("simpsons-transcripts.txt", sep = "^")
dat= dat[-1,]

simpsons_tokens = unnest_tokens (tbl = dat, output = word, input = text)
# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Text Analysis of The Simpsons Transcripts"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Word Frequency Analysis")),
           numericInput(inputId = "number",
                       label = "Top Frequent Words",
                       value = 10), 
           checkboxInput(inputId = "facets",
                         label = strong("Remove Stopwords?"),
                         value = FALSE)
    ),
    
    # replace with your widgets
           column(3, 
                  p(em("Word Frequency Analysis")),
                  sliderInput(inputId = "select", 
                              label = h3("Season Selection"),
                              min = 1, max = 33, value = 1)
    ),
    
    # replace with your widgets
    column(3,
           p(em("Word Trend Analysis")),
           radioButtons(inputId = "data", 
                        label = "Options to view", 
                        choices = c("With the Data" = "With the Data",
                                    "Trend Only" = "Trend Only",
                                    "Data Only"= "Data Only"),
                        selected = "With the Data")
    ),
    
    column(3,
           p(em("Word Trend Analysis")),
            textInput(inputId = "trend",
                      label = "Try a Word To Check The Trend Over the Seasons!",
                      value = "love"
                      )
           )
  ),

  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency",
                       h3("Word Frequency Analysis"),
                       plotOutput("plot1", height = 800),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Word Trend", 
                       h3("Word Trend Analysis"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  )
)

# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in plot1)
  dat_freq <- reactive({
    simpsons_tokens %>%
      select(season, word) %>%
      count(season, word)
  })
    
  top_words <- reactive({
    dat_freq() %>%
    filter(season== input$select) %>% 
    arrange(desc(n)) %>% 
    slice_head(n = input$number) %>%
      select(n, word)
  })
  
  tidy_top <- reactive({
    dat_freq() %>%
    anti_join(stop_words, by = "word") %>%
      filter(season== input$select) %>% 
      arrange(desc(n)) %>% 
      slice_head(n = input$number) %>%
      select(n, word)
  })
  
  season_word_counts <- reactive({
    dat_freq() %>% 
      anti_join(stop_words, by = "word") %>%
      mutate(season_total = sum(n))
  })
  
  simpsons_tokens1 <- reactive ({
    unnest_tokens (tbl = dat, output = word, input = text)
  })
  
  data_for_plot_2 <- reactive({
    simpsons_tokens1() %>% 
      select(season, word) %>%
      filter(word == input$trend) %>%
      count(season, word)
  })
    
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 <- renderPlot({
    # replace the code below with your code!!!
    if (input$facets) {
      ggplot(data = tidy_top(), aes(x = reorder(word, n), y = n)) +
        geom_col(aes(fill=n)) +
        scale_color_gradient2(low = "#fdd51c",
                              high = "#1a7cbf",
                              midpoint = median(tidy_top$n)) +
        scale_y_continuous(breaks = seq(0, 600, 30)) +
        labs(title = "Frequent Words throughout the Seasons",
             subtitle = "(After Removing Stopwords)",
             fill = "Level") +
        geom_text(aes(label= n), hjust = -0.4) +
        xlab("Word")+
        ylab("Count") +
        coord_flip() +
        theme(axis.title = element_text(face="bold"),
              (axis.text = element_text(face = "bold", size = 10)))
      
    } else {ggplot(data = top_words(), aes(x = fct_rev(reorder(word, -n)), y = n)) +
        scale_color_gradient2(low = "#fdd51c",
                              high = "#1a7cbf") +
        scale_y_continuous(breaks = seq(0, 2500, 250)) +
        labs(title = "Frequent Words throughout the Seasons",
             fill = "Level") +
        geom_text(aes(label= n), hjust = -0.4) +
        xlab("Word")+
        ylab("Count") +
        geom_col(aes(fill=n)) +
        coord_flip() + 
        theme_minimal()
      }
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    if (input$facets) {tidy_top()}
    else {top_words()}
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
  output$plot2 <- renderPlot({
    # replace the code below with your code!!
    if (input$data == ("With the Data")){
      season_word_counts() %>% 
      filter(word %in% c(input$trend)) %>%
      ggplot(aes(season, n/ season_total)) +
      geom_point() +
      geom_smooth() +
      geom_text(aes(label= n), hjust = -0.4) +
      facet_wrap(~ word, scales = "free_y") +
      scale_x_continuous(breaks = seq(1, 33, 3)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = "% frequency of word throughout seasons")
      
    } else if (input$data == ("Trend Only")) {
      season_word_counts() %>% 
        filter(word %in% c(input$trend)) %>%
        ggplot(aes(season, n/ season_total)) +
        geom_smooth() +
        facet_wrap(~ word, scales = "free_y") +
        scale_x_continuous(breaks = seq(1, 33, 3)) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(y = "% frequency of word throughout seasons")
      
    } else if (input$data == ("Data Only")) {
      season_word_counts() %>% 
        filter(word %in% c(input$trend)) %>%
        ggplot(aes(season, n/ season_total)) +
        geom_point() +
        geom_text(aes(label= n), hjust = -0.4) +
        facet_wrap(~ word, scales = "free_y") +
        scale_x_continuous(breaks = seq(1, 33, 1)) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(y = "% frequency of word throughout seasons")
    }
  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    # replace the code below with your code!!!
    data_for_plot_2()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

