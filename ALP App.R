rm(list = ls())
library(shiny)
library("DT")
library("gdata")
library("ggplot2")
library("dplyr")
library("shinysky")
library("gridExtra")
library("scales")
setwd("/Users/michaellevin/Nedbank ALP Directory")
data <- read.csv("ALP gold.csv", sep = ";", header = T)
industrydata <- read.csv("Industry Data for ALP.csv", sep = ";", header = T)
data <- subset(data, select = -c(X))
Comparison_choices <- c("Greater Than", "Less Than")

for (i in 3:length(colnames(industrydata))) {
  industrydata[[i]] <- as.numeric(sub(",", ".", as.character(industrydata[[i]]), fixed = TRUE))
}


industrydata <- industrydata %>% 
                mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

data["Debt:Equity"] = data$Total.Non.Current.Liabilities/data$Total.Book.Equity
data["Liquidity"] = data$Total.Current.Assets/data$Total.Current.Liailities
data["Operating_Margin"] = data$Operating.Profit/data$Total.Revenue
varnames <- colnames(data)
varnames <- varnames[4:length(varnames)]
industryvarnames <- colnames(industrydata)[3:length(colnames(industrydata))]
companylist <- unname(unlist(unique(data["Company"])))

industry = c("Gold Mining", "Petrochemicals", "Fast Moving Consumer Goods")

ui <- shinyUI(fluidPage(

    titlePanel(title = div(img(src = 'http://cdn.myactive.co.za/wm-695976-cmsimages/nedbank-logo-2017.jpg'), "Nedbank ALP Project"
      )),
    sidebarLayout(
      sidebarPanel(   selectizeInput("Ind", label = "Please choose an Industry", choices = companylist, multiple = TRUE),
                      fluidRow(column(4,textAreaInput("DE", label = "Debt Equity", width = "100%")),
                               column(5, selectInput("DEcomp", label = "Choose", choices = Comparison_choices), offset = 1)), 
                      fluidRow(column(4, textInput("Liq", label = "Liquidity Ratio")), 
                               column(5, selectInput("Liqcomp", label = "Choose", choices = Comparison_choices), offset = 1)), 
                      fluidRow(column(4, textInput("OM", label = "Operating Margin")), 
                               column(5, selectizeInput("OMcomp", label = "Choose", choices = Comparison_choices), offset = 1)),
                      selectInput("graphvar", label = "Choose a variable for the graph", choices = varnames),
                      selectInput("indgraphvar", label = "Choose a industry metric", choices = industryvarnames)),
      mainPanel(
                    tabsetPanel(
                      tabPanel("Graphs",plotOutput("Graph", width = "900px")),
                      tabPanel("Data",DT::dataTableOutput('table'))
                    )
                    
                    
        
      )
    )

              
              
              
              
      
  )
)

inputvarlist <- c("DE", "Liq", "OM")
conditionvarlist <- c("DEcomp", "Liqcomp", "OMcomp")
conditionstores <- list()
conditionstores[["DE"]] <- "Debt:Equity"
conditionstores[["Liq"]] <- "Liquidity"
conditionstores[["OM"]] <- "Operating_Margin"


server <- function(input, output, session) {

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
  final_data = data
  for (i in 1:length(inputvarlist)) {
    if(unlist(input[[conditionvarlist[i]]]) == "Greater Than") {
      final_data <- final_data[final_data[unlist(conditionstores[[toString(inputvarlist[i])]])]> input[[toString(inputvarlist[i])]],]
    }
    else if (unlist(input[[conditionvarlist[i]]]) == "Less Than") {
      final_data <- final_data[final_data[unlist(conditionstores[[toString(inputvarlist[i])]])]< input[[toString(inputvarlist[i])]],]
    }
  }
  final_data <- final_data["Company"]
  graphdata <- as.data.frame(filter(data, Company %in% unlist(final_data)))
  return(graphdata)
})

indreact <- reactive({
  inddata = industrydata
  return(inddata)
})



  output$value <- renderText({ input$caption })
  output$table <- DT::renderDataTable({data_subset() })
  output$Graph <- renderPlot({grid.arrange(ggplot(data_subset_graph(), aes_string(x = "Year", y = toString(input$graphvar), col = "Company")) + geom_line(), 
                                          ggplot(indreact(), aes_string(x = "Date", y = toString(input$indgraphvar))) + geom_smooth(se = F) + 
                                            scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y-")), layout_matrix = rbind(c(1, 2), c(1, 2)))})
  
    
}
shinyApp(ui, server)
