rm(list = ls())
library(shiny)
library("DT")
library("gdata")
library("ggplot2")
library("dplyr")
setwd("/Users/michaellevin/Nedbank ALP Directory")
data <- read.csv("ALP gold.csv", sep = ";", header = T)
data <- subset(data, select = -c(X))
x <- c("Greater Than", "Less Than")

data["Debt:Equity"] = data$Total.Non.Current.Liabilities/data$Total.Book.Equity
data["Liquidity"] = data$Total.Current.Assets/data$Total.Current.Liailities
data["Operating_Margin"] = data$Operating.Profit/data$Total.Revenue
varnames <- colnames(data)
varnames <- varnames[4:16]

ui <- shinyUI(fluidPage(
    titlePanel("Nedbank ALP Project"),
    mainPanel(fluidRow(column(2,textInput( "DE", label = "Debt Equity") ),
                        column(3, selectInput("DEcomp", label = "Choose", choices = x) )), 
              fluidRow(column(2, textInput("Liq", label = "Liquidity Ratio"))
                       ,column(3, selectInput("Liqcomp", label = "Choose", choices = x))), 
              selectInput("graphvar", label = "Choose a variable for the graph", choices = varnames),
              plotOutput("Graph"),
              DT::dataTableOutput('table')
              
              
              
      
  )
))

inputvarlist <- c("DE", "Liq")
conditionvarlist <- c("DEcomp", "Liqcomp")
conditionstores <- list()
conditionstores[["DE"]] <- "Debt:Equity"
conditionstores[["Liq"]] <- "Liquidity"

greaterlesstahn <- function(inputvars, conditionvars) {
  sliceddata <- data
  for (i in 1:length(inputvars)) {
    if(usedvars[[toString(conditionvars[i])]] == "Greater Than") {
      sliceddata <- sliceddata[sliceddata[unlist(conditionstores[[toString(inputvars[i])]])]> usedvars[[toString(inputvars[i])]],]
    }
    else if (usedvars[[toString(conditionvars[i])]] == "Less Than") {
      sliceddata <- sliceddata[sliceddata[unlist(conditionstores[[toString(inputvars[i])]])]< usedvars[[toString(inputvars[i])]],]
    }
  }
  return(sliceddata)
}

server <- function(input, output) {

data_subset <- reactive({   
  sliceddata <- data
  for (i in 1:length(inputvarlist)) {
    if(unlist(input[[conditionvarlist[i]]]) == "Greater Than") {
      sliceddata <- sliceddata[sliceddata[unlist(conditionstores[[toString(inputvarlist[i])]])]> input[[toString(inputvarlist[i])]],]
    }
    else if (unlist(input[[conditionvarlist[i]]]) == "Less Than") {
      sliceddata <- sliceddata[sliceddata[unlist(conditionstores[[toString(inputvarlist[i])]])]< input[[toString(inputvarlist[i])]],]
    }
  }
    return(sliceddata)
  })

data_subset_graph <- reactive({   
  z <- unlist(subset(data, (if(input$DEcomp == "Greater Than"){
    data["Debt:Equity"] > input[["DE"]]
  }
  else if(input$DEcomp == "Less Than") {
    data["Debt:Equity"] < input$DE
  })
  & (if(input$Liqcomp == "Greater Than") {
    data["Liquidity"] > input$Liq
  }
  else if(input$Liqcomp == "Less Than") {
    data["Liquidity"] < input$Liq
  }
  )
  
  , select = Company) )
  
  final_data <- as.data.frame(filter(data, Company %in% z))
  return(final_data)
})


  output$value <- renderText({ input$caption })
  output$table <- DT::renderDataTable({data_subset() })
  output$Graph <- renderPlot(ggplot(data_subset_graph(), aes_string(x = "Year", y = toString(input$graphvar), col = "Company")) + geom_line())

    

}
shinyApp(ui, server)
print(data_subset())