library(shiny)
library(ggplot2)
library(dplyr)
library(stats)
library(reshape2)
library(reader)
library(tidyverse)
data<-read_csv(url("https://raw.githubusercontent.com/lbartoszcze/ItalianGDPPC/main/shiny_data.csv"))

ui <- fluidPage(
  titlePanel("Historic GDP per capita of Italy"),
  h4("Comparison between different sources"),
  sidebarLayout(
    sidebarPanel(
      h4("Inputs"),
      radioButtons("dataset", "Which dataset should be used for visualization?",
                   choices = list("Broadberry (2013)" = 1,
                                  "Maddison (2016)" = 2,
                                  "Total Economy Database (2016)" = 3,
                                  "All Databases Combined (Question 1)" = 4, 
                                  "Interpolation and retropolation: Question 2 Approach" = 5), 
                   selected = 1), 
      numericInput("min", "When should the graph start?", value = 790),
      numericInput("max", "When should the graph end?", value = 2020),
      numericInput("y_min", "What should be the minimum on the y-axis?", value = 0),
      numericInput("y_max", "When should the maximum on the y-axis?", value = 10000),
      textInput("line_color", "Line color:", value = "black"),
    ),
    mainPanel(plotOutput("process"),
              conditionalPanel("input.dataset == 1", 
                               h4("Broadberry (2013)"),
                               'This graph uses data from Broadberry, Stephen (2013) Accounting for the great divergence. Economic History Working Papers (184/13). London School of Economics and Political Science, London, UK., accessible at http://eprints.lse.ac.uk/54573/.'),
              conditionalPanel("input.dataset == 2", 
                               h4("Maddison (2016)"),
                               "This graph uses data from Maddison Historical Statistics, compiled by the Maddison Project. A more recent version of the dataset may be available at https://www.rug.nl/ggdc/historicaldevelopment/maddison/?lang=en"
              ),
              
              conditionalPanel("input.dataset == 3", 
                               h4("Total Economy Database (2016)"),
                               "This graph uses data from Total Economy Database, developed by the Groningen Growth and Development Centre (University of Groningen, The Netherlands) and The Conference Board. More recent version of the data set may be available at https://conference-board.org/data/economydatabase/total-economy-database-about"
              ),
              conditionalPanel("input.dataset == 4", 
                               h4("All Databases Combined (Question 1)"),
                               "This graph uses all three datasets to create a cohesive image of the evolution of per capita growth rates. For every year in the range of each dataset, a value is interpolated using linear interopolation. Then, all predictions are averaged."
              ),
              
              conditionalPanel("input.dataset == 5", 
                               h4("Question 2 Approach"),
                               "This graph uses Maddison (2016) for values before 1950 and Total Economy Database (2016) for values after this date. To avoid a sharp breaking point, for the period 1300-1950, this series uses an average of  retropolation and interpolation as defined in: A mixed splicing procedure for economic time series by Angel de la Fuente (2009)."
              ),
    )
  ),
)

server <- function(input, output) {
  
  output$process <- renderPlot({
    if (input$dataset == 1) {
      time <- na.omit(data$years_broadberry)
      value <- as.numeric(na.omit(data$gdppc_broadberry))
      broadberry_data <- data.frame(time,value)
      ggplot(broadberry_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + geom_point(color = input$line_color) + xlab ("Year") + ylab("GDP Per Capita (1990 International $)") + xlim (input$min, input$max)+ylim(input$y_min,input$y_max)+theme_classic() 
    }
    else {
      if (input$dataset == 2) {
        time <- na.omit(data$years_maddison)
        value <- as.numeric(na.omit(data$gdppc_maddison2016))
        maddison_data <- data.frame(time,value)
        ggplot(maddison_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + geom_point(color = input$line_color) + xlab ("Year") + ylab("GDP Per Capita (1990 International $)") + xlim (input$min, input$max)+ theme_classic()+ylim(input$y_min,input$y_max)
      }
      else {
        if(input$dataset == 3){
          time <- na.omit(data$years_TED[1:67])
          value <- as.numeric(na.omit(data$gdppc_TED))
          TED_data <- data.frame(time,value)
          ggplot(TED_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + geom_point(color = input$line_color) + xlab ("Year") + ylab("GDP Per Capita (1990 International $)") + xlim (input$min, input$max)+ theme_classic()+ylim(input$y_min,input$y_max)
          
        }
        else{
          if(input$dataset ==4)
          {time <- na.omit(data$years_q1)
          value <- as.numeric(na.omit(data$gdppc_q1))
          q1_data <- data.frame(time,value)
          ggplot(q1_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + geom_point(color = input$line_color) + xlab ("Year") + ylab("GDP Per Capita (1990 International $)") + xlim (input$min, input$max)+ylim(input$y_min,input$y_max)+theme_classic()}
          else {time <- na.omit(data$years_q2)
          value <- as.numeric(na.omit(data$gdppc_q2))
          q2_data <- data.frame(time,value)
          ggplot(q2_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + geom_point(color = input$line_color) + xlab ("Year") + ylab("GDP Per Capita (1990 International $)") + xlim (input$min, input$max)+ylim(input$y_min,input$y_max)+theme_classic()}
        }
        
      }
    }
    
  })
}

shinyApp(ui = ui, server = server)
