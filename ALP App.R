##################################################
#This is the main application page for the Nedbank ALP project. This project is a r shiny web application for client analytics for 
#investment bankers

rm(list = ls())
library(shiny)
library("DT")
library("gdata")
library("ggplot2")
library("dplyr")
library("shinysky")
library("gridExtra")
library("scales")
library("rPython")
library("httr")
library("xml2")
library("plotly")


setwd("/Users/michaellevin/Nedbank ALP Directory")

#The master data file includes only the financial statement data compiled accross different sources.

data <- readRDS("masterdata.Rdata")

# The industry data for ALP file contains the industry based data. The data is collected from Quandl, using a number of subsources
industrydata <- read.csv("Industry Data for ALP.csv", sep = ";", header = T)
data <- subset(data, select = -c(X))
internaldata = read.csv("ALP Internal Clients data.csv", sep = ";", stringsAsFactors = FALSE)

cleandata = function(internaldata, externaldata) {
  colnames(internaldata)[6] = "Company"
  totaldata = left_join(externaldata, internaldata, by = c("Company", "Year"))
  for (i in 14:17){
    totaldata[i][is.na(totaldata[i])] = 0
  }
  return(totaldata)
}

modeldata = cleandata(internaldata, data)
#The multiplewordcomps function is used to input multiple words into a url in the correct format for get requests

multiplewordcomps = function(compname){
  compname = unlist(strsplit(compname, " "))
  if (!(length(compname) == 1)) {
  holdingvar = compname[1]
  for (i in 1:(length(compname)-1)){
    holdingvar = paste(holdingvar, compname[i+1], sep = "+")
  }
  return(holdingvar) }
  
  else {
    return(compname)
  }
  
}


#The newsstories functions is used to retrieve news stories for companies in the chosen subset in the application and return it as
#as a data table. The news is acquired from the newsapi api. 
newsstories <- function(companylist){
  dateofarticle = c()
  titleofarticle = c()
  descriptionofarticle = c()
  linktoarticle = c()
  for(company in companylist) {
    company = multiplewordcomps(company)
    newsapi = httr::GET(paste("https://api.cognitive.microsoft.com/bing/v7.0/news/search?q=", company ,sep = ""), add_headers("Ocp-Apim-Subscription-Key"= "0f8bf22a727e4ad7ad78a71ad3742aaf"))
    newsapi = content(newsapi)
    newsapi = newsapi$value
    for (article in newsapi){
      dateofarticle = c(dateofarticle, if(is.null(article$datePublished)){"No Date"} else{ substr(article$datePublished, 1, 10)})
      titleofarticle = c(titleofarticle, if(is.null(article$name)) {"No Title"} else {article$name})
      descriptionofarticle = c(descriptionofarticle, if(is.null(article$description)) {"No Description"} else {article$description})
      linktoarticle = c(linktoarticle, if(is.null(article$url)) {"No Link"} else { article$url})
    }
  }
  newstable = data.frame(dateofarticle, titleofarticle, descriptionofarticle, linktoarticle)
  return(newstable)
}



tiereddata = function(data, companylist, Yearchosen) {
  aves = data %>% group_by(.dots= c("Year", "Industry.B")) %>%
    summarise(aveliabs = mean(Total.Non.Current.Liabilities), aveassets = mean(Total.Non.Current.Assets), averev = mean(Total.Revenue), aveopprofit = mean(Operating.Profit),
              avenir = mean(NIR), aveNII = mean(NII), aveintassets = mean(Assets..Avg..YTD.), aveintliabs = mean(Liabilities..Avg..YTD.))
  
  data = data %>% filter(Year == Yearchosen) %>%
    inner_join(aves, by = c("Industry.B", "Year")) %>%
    mutate(exLiabsrank = ifelse(Total.Non.Current.Liabilities > aveliabs, 1, 2), exAssetsrank = ifelse(Total.Non.Current.Assets > aveassets, 1, 2), 
           exrevrank = ifelse(Total.Revenue > averev, 1, 2), exprofrank = ifelse(Operating.Profit > aveopprofit, 1, 2)) %>%
    mutate(innirrank = ifelse(NIR > avenir, 1, 2),inniirank =  ifelse(NII > aveNII, 1, 2), inassrank = ifelse(Assets..Avg..YTD. > aveintassets, 1, 2),
           intliabsrank = ifelse(Liabilities..Avg..YTD. > aveintliabs, 1, 2)) %>%
    mutate(totalexrank = ifelse((exLiabsrank + exAssetsrank + exrevrank + exprofrank) >5, 2, 1), intrank = ifelse((innirrank + inniirank + inassrank +   intliabsrank) > 5, 2, 1))
  data = data[c(1, 3, 34, 35)]
  data = filter(data, Company %in% companylist)
  return(data)
}
test = tiereddata(modeldata, c("AngloGold", "Goldfiends", "Harmony Gold"), 2016)

#Fixing issues where numerical variables had commas for decimals and was turning the data into factors. I think this issue was solved in 
#Industrydata creation file and may be redundent. 

for (i in 3:length(colnames(industrydata))) {
  industrydata[[i]] <- as.numeric(sub(",", ".", as.character(industrydata[[i]]), fixed = TRUE))
}

industrydata <- industrydata %>% 
                mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


#Creation of the financial ratios

data["DebtEquity"] = data$Total.Non.Current.Liabilities/data$Total.Book.Equity
data["Liquidity"] = data$Total.Current.Assets/data$Total.Current.Liabilities
data["Operating_Margin"] = data$Operating.Profit/data$Total.Revenue

#Creation of vectors used in the UI for choice variables. 

varnames <- colnames(data)
varnames <- varnames[4:length(varnames)]
industryvarnames <- colnames(industrydata)[3:length(colnames(industrydata))]
goldind = c("Gold_Price", "Oil.Price", "Dollar.Rand", "GDP.Growth")
foodind = c("Dollar.Rand", "Oil.Price", "GDP.Growth", "CPI", "Consumer.Confidence")
buildind = c("Dollar.Rand", "Oil.Price",  "GDP.Growth", "GDP.Deflator", "Private.fixed.capital.formation")
companylist <- unname(unlist(unique(data["Company"])))
Years = as.integer(c(2011, 2012, 2013, 2014, 2015, 2016, 2017))
Comparison_choices <- c("Greater Than", "Less Than")
companylist = c("All", companylist)

industry = unname(unlist(unique(data["Industry.B"])))
industry = unlist(rbind(c("All", industry)))

####################################################

#User interface
ui <- shinyUI(fluidPage(

    titlePanel(title = div(img(src = 'http://cdn.myactive.co.za/wm-695976-cmsimages/nedbank-logo-2017.jpg'), "               C.IT"
      )),
    sidebarLayout(
      sidebarPanel(  selectizeInput("compnames", label = "Please choose companies", choices = companylist, selected = "All", multiple = TRUE),
                      selectInput("Ind", label = "Please choose an Industry", choices =industry, selected = "All"),
                      fluidRow(column(4,textAreaInput("DE", label = "Debt Equity", width = "100%")),
                               column(5, selectInput("DEcomp", label = "Choose", choices = Comparison_choices), offset = 1)), 
                      fluidRow(column(4, textInput("Liq", label = "Liquidity Ratio")), 
                               column(5, selectInput("Liqcomp", label = "Choose", choices = Comparison_choices), offset = 1)), 
                      fluidRow(column(4, textInput("OM", label = "Operating Margin")), 
                               column(5, selectizeInput("OMcomp", label = "Choose", choices = Comparison_choices), offset = 1)),
                      fluidRow(column(4, selectInput("Ybef", label = "Years To", choices = Years, selected = 2017)), 
                               column(5, selectInput("Yaf", label = "Years From", choices = Years, selected = 2010), offset = 1)),
                      selectInput("graphvar", label = "Choose a variable for the graph", choices = varnames),
                      uiOutput("Conditionalforgraph")),
                      
      mainPanel(
                    tabsetPanel(
                      tabPanel("Data",DT::dataTableOutput('table'),
                               downloadButton("downdata", "Download")),
                      tabPanel("Graphs",
                               br(),
                               plotlyOutput("Graph", width = "900px")),
                      tabPanel("News", 
                               actionLink("gennews", "Click to generate recent news on your selected clients"), 
                               DT::dataTableOutput('newstab') ), 
                      tabPanel("Models", 
                               selectInput("ModelYear", "Please choose a year", choices = c(2015, 2016, 2017)),
                               actionButton("model", "Generate model output."),
                               plotlyOutput("ModelGraph"))
                    )
                    
                    
        
      )
    )

              
              
              
              
      
  )
)

#########################################################################
inputvarlist <- c("DE", "Liq", "OM")
conditionvarlist <- c("DEcomp", "Liqcomp", "OMcomp")
conditionstores <- list()
conditionstores[["DE"]] <- "DebtEquity"
conditionstores[["Liq"]] <- "Liquidity"
conditionstores[["OM"]] <- "Operating_Margin"




########################################################################

# Server Functions

server <- function(input, output, session) {

#The creation of the main data set used. It first validates that the set of data set is non-empty, then
  #The data is filtered based on user input criteria. The output table shows all companies which fell into the chosen
  #criteria. If one which to add additional conditions, the developer would need to edit both the UI as well as the vecor
  #inputvarlist and conditionstores which stores the mapping from UI identifier to variable name in the data. 
data_subset_graph <- reactive({ 
 
  final_data = data
  print(is.null(input$compnames))
 if (!(is.null(input$compnames)))  {
   if (!(input$compnames == "All")) {
  print(is.null(input$compnames))
  final_data = filter(final_data, Company %in% input$compnames)
   }
 }

  if (!(input$Ind == "All")) {
    final_data<- filter(final_data, Industry.B %in% input$Ind)
  }
  else {
    final_data <- final_data
  }
  final_data = filter(final_data, Year <= input[["Ybef"]] & Year >= input[["Yaf"]])
  
 # print(final_data)
  for (i in 1:length(inputvarlist)) {
    validate(
      need(if(unlist(input[[conditionvarlist[i]]]) == "Greater Than") {
        input[[toString(inputvarlist[i])]] < max(final_data[unlist(conditionstores[[toString(inputvarlist[i])]])])
      } 
      else if (unlist(input[[conditionvarlist[i]]]) == "Less Than") {
        input[[toString(inputvarlist[i])]] > min(final_data[unlist(conditionstores[[toString(inputvarlist[i])]])])
        
      }, "No clients match your criteria"
        )
      
    )
    if(unlist(input[[conditionvarlist[i]]]) == "Greater Than") {
      final_data <- final_data[final_data[unlist(conditionstores[[toString(inputvarlist[i])]])]> input[[toString(inputvarlist[i])]],]
    }
    else if (unlist(input[[conditionvarlist[i]]]) == "Less Than") {
      final_data <- final_data[final_data[unlist(conditionstores[[toString(inputvarlist[i])]])]< as.numeric(input[[toString(inputvarlist[i])]]),]
      print(input[[toString(inputvarlist[i])]])
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
  
newsdatatable <- reactive({
  some_data = data
  if (!(input$Ind == "All")) {
    some_data<- filter(some_data, Industry.B %in% input$Ind)
  }
  else {
    some_data <- data
  }
  for (i in 1:length(inputvarlist)) {
    if(unlist(input[[conditionvarlist[i]]]) == "Greater Than") {
      some_data <- some_data[some_data[unlist(conditionstores[[toString(inputvarlist[i])]])]> input[[toString(inputvarlist[i])]],]
    }
    else if (unlist(input[[conditionvarlist[i]]]) == "Less Than") {
      some_data <- some_data[some_data[unlist(conditionstores[[toString(inputvarlist[i])]])]< input[[toString(inputvarlist[i])]],]
    }
  }
  some_data <- unlist(unique(some_data["Company"]))
  
  newsdata = newsstories(some_data)
  
  return(newsdata)
    
})  

modeloutput = eventReactive(input$model,
  {
    some_data = data
    if (!(input$Ind == "All")) {
      some_data<- filter(some_data, Industry.B %in% input$Ind)
   }
    else {
      some_data <- data
    }
    for (i in 1:length(inputvarlist)) {
      if(unlist(input[[conditionvarlist[i]]]) == "Greater Than") {
      some_data <- some_data[some_data[unlist(conditionstores[[toString(inputvarlist[i])]])]> input[[toString(inputvarlist[i])]],]
     }
    else if (unlist(input[[conditionvarlist[i]]]) == "Less Than") {
       some_data <- some_data[some_data[unlist(conditionstores[[toString(inputvarlist[i])]])]< input[[toString(inputvarlist[i])]],]
      }
    }
    some_data <- unlist(unique(some_data["Company"]))
    
    modeldat = tiereddata(modeldata, some_data, input$ModelYear)
    
    return(modeldat)
    
  }
)


  output$value <- renderText({ input$caption })
  output$table <- DT::renderDataTable({data_subset_graph() })
 # output$Graph <- renderPlot({grid.arrange(ggplot(data_subset_graph(), aes_string(x = "Year", y = toString(input$graphvar), col = "Company")) + geom_line(), 
  #                                        ggplot(indreact(), aes_string(x = "Date", y = toString(input$indgraphvar))) + geom_smooth(se = F) + 
   #                                         scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y-")), layout_matrix = rbind(c(1, 2), c(1, 2)))})
  output$Graph <- renderPlotly({subplot(ggplotly(ggplot(data_subset_graph(), aes_string(x = "Year", y = toString(input$graphvar), col = "Company")) + geom_line() + ggtitle(input$graphvar)), 
                                          ggplotly(ggplot(indreact(), aes_string(x = "Date", y = toString(input$indgraphvar))) + geom_smooth(se = F) + 
                                             scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y-"))))})
  
  output$Conditionalforgraph <- renderUI({
    selectInput("indgraphvar", label = "Choose a industry metric", if(input$Ind == "All") {
      choices = industryvarnames
    }
    else if(input$Ind == "Gold") {
      choices = goldind
    }
    else if(input$Ind == "Food Producer"){
      choices = foodind
    }
    else if(input$Ind == "Construction and materials"){
      choices = buildind
    })
  })
  
  output$ModelGraph = renderPlotly({ggplotly(ggplot(modeloutput(), aes(x = intrank, y = totalexrank, col = Company)) + geom_point(position = position_jitter(0.0001)))})
    output$newstab <- DT::renderDataTable({newsdatatable() })
    output$downdata = downloadHandler(
      filename = function() {
        "testdata.csv"
      },
      content = function(file) {
        write.csv(data_subset_graph(), file)
      }
    )
}
shinyApp(ui, server)

filter(data, Company %in% c("AngloGold", "Harmony Gold"))
 
x = "test:this"
