# ===============================================
# Fill in the following fields
# ===============================================
# Title: Investment Simulator
# Description: 
# Author: Sangwon Ji
# Date: 11-09

# ===============================================
# Required packages (you can use other packages if you want)
# ===============================================
library(shiny)
library(tidyverse)
library(ggplot2)
library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investment Simulator"),
  fluidRow(
    # Inputs for initial amount, and periodic contributions 
    column(width = 3,
           h4("Money Input"),
           numericInput(inputId = "initial_amount", 
                        label = "Initial Amount", 
                        value = 1000) ,
           numericInput(inputId= "PMT",
                        label = "Periodic Contribution", 
                        value = 360) ,
           
    ),
    
    # Inputs for target amount, and number of years 
    column(width = 3,
           h4("Target and Time"),
           numericInput(inputId = "target", 
                        label = "Target Amount", 
                        value = 5000),
           sliderInput(inputId = "num_years", 
                       label = "Number of Years", 
                       value = 10, 
                       min = 1, 
                       max = 25)
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(width = 3,
           h4("Composition of Profile"),
           sliderInput(inputId = "prop_of_stock", 
                       label = "Proportion of Stocks", 
                       value = 50, 
                       min = 0, 
                       max = 100,
                       step = 5),
           sliderInput(inputId = "prop_of_bond",
                       label = "Proportion of Bonds",
                       value = 50,
                       min = 0,
                       max = 100,
                       step = 5)
    ),
    
    # Inputs for number of simulations, and random seed
    column(width = 3,
            h4("Simulation Parameters"),
            sliderInput(inputId = "num_sim", 
                        label = "Number of Simulations", 
                        value = 50, 
                        min = 0, 
                        max = 100),
            numericInput(inputId = "seed", 
                         label = "Random Seed", 
                         value = 123)
    )
  ),
  
  hr(),
  h4('Simulated Timelines'),
  plotOutput('plot1'),
  
  hr(),
  h4('Probability of Reaching Target'),
  plotOutput('plot_hist'),
  
  hr(),
  fluidRow(
    column(width = 6,
           h4('Summary Statistics'),
           tableOutput('table1')
    ),
    column(width = 6,
           h4('Simulations that went above the Target Amount'),
           tableOutput('table2')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  stock = seq(0, 100, 5)
  bond = seq(100, 0, -5)
  avg = c(5.09, 5.40, 5.70, 6.00, 6.29, 6.57, 6.84, 
                      7.11, 7.37, 7.62, 7.87, 8.11, 8.34, 8.56, 
                      8.78, 8.99, 9.19, 9.38, 9.57, 9.74, 9.91)
  Sd = c(4.03, 3.99, 4.11, 4.37, 4.74, 5.21, 5.74,
                     6.32, 6.94, 7.59, 8.25, 8.94, 9.64, 10.35,
                     11.07, 11.80, 12.54, 13.28, 14.03, 14.79, 15.55)
  
  return_mat <- data.frame(avg, Sd)
  
  balance_mat = reactive({
    balance = matrix(0, nrow = input$num_years + 1, ncol = input$num_sim)
    balance[1, ] = input$initial_amount
    means <- 0.01 * return_mat[(input$prop_of_stock / 5 + 1), 1]
    Sds <- 0.01 * return_mat[(input$prop_of_stock / 5 + 1), 2]
    set.seed(input$seed)
    
    for (sim in 1:input$num_sim) {
      return_rates= rnorm(input$num_years, mean = means, sd = Sds)
      for (Year in 1:input$num_years) {
        balance[Year+1, sim] = (balance[Year,sim] * (1 + return_rates[Year]) + input$PMT)
      }
    }
    
    colnames(balance) = paste0("sim", 1:input$num_sim)
    balance
  })
  
  observeEvent(input$prop_of_stock, {
    updateSliderInput(inputId = "prop_of_bond", value = 100 - input$prop_of_stock)
  })
  
  observeEvent(input$prop_of_bond, {
    updateSliderInput(inputId = "prop_of_stock", value = 100 - input$prop_of_bond)
  })
  
  # -------------------------------------------------------
  # reshape table into long format (for ggplot convenience)
  # -------------------------------------------------------
  dat_sim = reactive({
    tbl = as.data.frame(balance_mat())
    tbl$Year = 0:input$num_years
    
    dat = pivot_longer(
      data = tbl, 
      cols = starts_with("sim"), 
      names_to = "simulation",
      values_to = "amount")
    
    dat
  })
  
  average_year <- reactive({
    df <- dat_sim() %>% 
      filter(amount >= input$target) %>%
      group_by(simulation) %>% 
      summarize(
        low =min(Year))
    mean(df$low) 
  })
  
  # ---------------------------
  # Output: table1 of summaries
  # ---------------------------
  # some numeric statistics for every year
  output$table1 <- renderTable({
    dat_sim() %>%
      group_by(Year) %>%
      summarise(
        min = min(amount),
        max = max(amount),
        average = mean(amount),
        median= median(amount)
      )
  })
  
  
  # code for plot1
  output$plot1 <- renderPlot({
    ggplot(data = dat_sim(), aes(x = factor(Year), y = amount, group = simulation)) +
      geom_line(size= 0.25, alpha = 0.80) + 
      geom_hline(yintercept = input$target, color = "steelblue", alpha = 0.85, linetype = "longdash") +
      geom_vline(xintercept = average_year(),color = "steelblue", alpha =0.85,linetype = "longdash") +
      annotate("text", x= average_year() + 1, y = 1000, label = paste0("Average Year (", average_year(), " Years)"), size = 5, color = "black") +
      annotate("text", x = 2, y = input$target + 360, label = "Target Amount", size = 5, color = "black") +
      labs(x = "Years", y= "Balance ($)", title = paste0("With " , input$prop_of_stock, ",", input$prop_of_bond, "(Stock,Bond proportion) And initial amount of ", input$initial_amount, " periodic contibutions of ", input$PMT, " you are to get ")) +
      theme(plot.title = element_text(face="bold", hjust=0.01), axis.text = element_text(face="bold")) +
      theme(panel.grid.minor = element_blank()) + 
      theme_linedraw()
  })
  
  
  # code for plot2
  output$plot_hist <- renderPlot({
    dat_sim () %>% 
      filter(amount >= input$target) %>% 
      ggplot(aes(x= Year, color=qsec)) +
      geom_histogram(aes(y = ..count../input$num_sim), breaks = c(0:input$num_years), color = "#00abff", fill = "black", binwidth = 0.6) +
      scale_x_continuous(limits = c(0, input$num_years), breaks = c(0: input$num_years),labels = c(0: input$num_years)) +
      labs(x ="Years", y = "Probability", title = "Probability of Reaching Target") + 
      theme(plot.title = element_text(face="bold", hjust=0.01)) +
      theme_linedraw()
  })
  
    
  # code for additional information
  output$table2 <- renderTable({
    dat_sim() %>% 
      group_by(Year) %>%
      summarise(
        count = sum(amount > input$target),
        proportion = count / input$num_sim)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


