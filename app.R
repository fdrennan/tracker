## portfolioTracker - Shiny app
## load required packages

library(tidyverse)
library(shiny)
library(shinythemes)
library(quantmod)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(data.table)

#Initialize exampleData as global variable, used in "Upload CSV" tabset as download link
exampleData = fread("exampleData.csv")

ui = fluidPage(theme = shinytheme("united"),
               
               titlePanel(title = "shinyTracker"),
               
               #Render Inputs
               sidebarLayout(
                 
                 sidebarPanel(
                   tabsetPanel(
                     id = "tabset",
                     tabPanel(
                       "Enter Information",
                       tags$br(),
                       textInput(inputId = "tickers", label = "Enter stock tickers separated by commas :", value = "AAPL", width = "400px", placeholder = 'AAPL'),
                       textInput(inputId = "shares", label = "Enter the weight of each stock separated by commas :",value = "1", width = "400px", placeholder = "100"),
                       dateRangeInput(inputId = "dates", label = "Date Range :", start = Sys.Date()-365 , end = Sys.Date(), separator = "to", width = "400px"),
                       radioButtons(inputId = "plotType1", label = "Plot as:", choices = c("Each separate security" = "sep", "One basket of securities" = "basket"), selected = "sep"),
                       actionButton(inputId = "plot1", label = "Plot chart"),
                       actionButton(inputId = "clear1", label = "Clear"),
                       downloadButton(outputId = "download1", label = "Download data")
                     ),
                     tabPanel(
                       "Upload CSV",
                       tags$br(),
                       tags$b("Your file should have the column names 'Ticker' and 'Weight'"),
                       tags$br(),
                       tags$br(),
                       downloadLink(outputId = "example", label = "Example CSV"),
                       tags$br(),
                       tags$br(),
                       fileInput(inputId = "csv",label =  "Choose CSV File",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                       dateRangeInput(inputId = "dates", label = "Date Range :", start = Sys.Date()-365 , end = Sys.Date(), separator = "to", width = "400px"),
                       radioButtons(inputId = "plotType2", label = "Plot as:", choices = c("Each separate security" = "sep", "One basket of securities" = "basket"), selected = "sep"),
                       actionButton(inputId = "plot2", label = "Plot chart"),
                       actionButton(inputId = "clear2", label = "Clear"),
                       downloadButton(outputId = "download2", label = "Download data"),
                       tags$br(),
                       tags$br(),
                       tags$em("If you have a lot of stock tickers, wait a few seconds for the plot to load; pulling data takes time.")
                     )
                   ), width = 3
                 ),
                 
                 #Outputs
                 mainPanel(plotOutput(outputId = "graph"))
               )
)

server = function(input, output){
  
  #Fix list of stock tickers
  tickersInput = reactive({
    if(input$tabset == "Enter Information"){
      tickers = as.list(strsplit(input$tickers,",")[[1]])
      tickers = gsub(" ","",x=tickers)
      tickers = tickers[tickers != ""]
    }
    
    if(input$tabset == "Upload CSV"){
      inFile = input$csv
      tickers = fread(inFile$datapath, select = c("Ticker"))
      tickers = tickers$Ticker
    }

    return(tickers)
  })
  
  #Fix list of weights
  sharesInput = reactive({
    if(input$tabset == "Enter Information"){
      shares = as.list(strsplit(input$shares,",")[[1]])
      shares = gsub(" ","",x=shares)
      shares = shares[shares != ""]
      shares = as.numeric(shares)  
    }
    
    if(input$tabset == "Upload CSV"){
      inFile = input$csv
      shares = fread(inFile$datapath, select = c("Weight"))
      shares = shares$Weight
      shares = as.numeric(shares)
    }
    
    return(shares)
  })
  
  #Use the inputs to generate portfolioValue dataframe for ggplot
  portfolioValue = reactive({
    stocks = tickersInput() #retrieve the tickers
    shares = sharesInput() #retrieve the # of shares
    
    #base is the main dataframe where we store the portfolio value, initialized here
    base = as.data.frame(getSymbols(Symbols = stocks[1], env = NULL, from = input$dates[1], to = input$dates[2], src = 'yahoo')) #create base used to calculate portfolio value
    
    base = base[-c(1:5)] #removes unnecessary columns provided by quantmod, we only want adjusted closing prices
    
    base$date = as.character(rownames(base)) #allows access to dates in between from & to
    
    #reverse the order for calculation of returns
    attach(base)
    base = base[rev(order(date)),]
    detach(base)
    
    dates = base$date
    
    base = as.matrix(base[,1])
    colnames(base)[1] = stocks[1]
    rownames(base) = dates
    
    #update base to include rest of tickers
    if(length(stocks) > 1){
      for(i in 2:length(stocks)){
        tryCatch(
          {
            data = as.data.frame(getSymbols(Symbols = stocks[i], env = NULL, from = input$dates[1], to = input$dates[2], src = 'yahoo'))
            
            data = data[-c(1:5)] ##removes unnecessary columns provided by quantmod, we only want adjusted closing prices
            
            data$date = as.character(rownames(data))
            
            attach(data)
            data = data[rev(order(date)),]
            detach(data)
            
            dates = data$date
            
            data = as.matrix(data[,1])
            colnames(data)[1] = stocks[i]
            rownames(data) = dates
            
            base = cbind(base,data)
          }
          , error = function(e){print("ERROR: One of your stock tickers may be invalid")}
        )
      }
    }
    
    #update base to reflect total market value of shares - Prices * Shares
    for(i in 1:ncol(base)){
      base[,i] = base[,i]*shares[i]
    }
    
    #create separate object to track dividends
    dividends = matrix(0, nrow = nrow(base), ncol = ncol(base))
    colnames(dividends) = colnames(base)
    rownames(dividends) = rownames(base)
    
    #calculate total dividend earnings by date for each stock in portfolio
    for(i in 1:length(colnames(dividends))){
      #tryCatch statement to handle error thrown when stock hasn't paid dividend
      tryCatch(
        {
          divs = as.matrix(getDividends(colnames(dividends)[i], from = input$dates[1], to = input$dates[2]))
          divs = divs*shares[i]
          
          dividends[rownames(divs),colnames(dividends)[i]] = divs 
          
        }
        , error = function(e){}
      )
    }
    
    #collapse dividend earnings into one column named cash
    cashBalance = matrix(NA, nrow = nrow(dividends), ncol = 1)
    colnames(cashBalance) = "Cash"
    rownames(cashBalance) = rownames(dividends)
    
    for(i in length(cashBalance):1){
      if(i == length(cashBalance)){
        cashBalance[i,] = sum(dividends[i,])
      }
      else{
        cashBalance[i,] = cashBalance[i+1,] + sum(dividends[i,])
      }
    }
    
    #add dividends to the base
    base = cbind(base,cashBalance)
    
    #calculate the portfolio value at each date, then calculate cumulative return
    portfolioValue = as.data.frame(rowSums(base))
    colnames(portfolioValue) = "Value"
    portfolioValue$Date = rownames(base)
    portfolioValue$Date = as.Date(portfolioValue$Date)
    rownames(portfolioValue) = NULL
    
    returns = matrix(NA,nrow = length(portfolioValue$Value),ncol = 1)
    colnames(returns) = "Cumulative Return"
    
    for(i in 1:length(portfolioValue$Value)){
      returns[i,] = (portfolioValue$Value[i]/portfolioValue$Value[length(portfolioValue$Value)])-1 
    }
    
    portfolioValue$Cumulative_Return = returns
    
    #return portfolioValue dataframe
    return(portfolioValue)
    
  })
  
  #stockValue is essentially the same as portfolioValue, but returns a modified dataframe that allows for plotting multiple columns on same ggplot
  #chose to create two separate reactives so that users can easily switch between "plotting as separate" and "plotting as basket", since once both calculated it will be instantaneous
  stockValue = reactive({
    
    stocks = tickersInput() #retrieve the tickers
    shares = sharesInput() #retrieve the # of shares
    
    #base is the main dataframe where we store the portfolio value, initialized here
    base = as.data.frame(getSymbols(Symbols = stocks[1], env = NULL, from = input$dates[1], to = input$dates[2], src = 'yahoo')) #create base used to calculate portfolio value
    
    base = base[-c(1:5)] #removes unnecessary columns provided by quantmod, we only want adjusted closing prices
    
    base$date = as.character(rownames(base)) #allows access to dates inbetween from & to
    
    #reverse the order for calculation of returns
    attach(base)
    base = base[rev(order(date)),]
    detach(base)
    
    dates = base$date
    
    base = as.matrix(base[,1])
    colnames(base)[1] = stocks[1]
    rownames(base) = dates
    
    #update base to include rest of tickers
    if(length(stocks) > 1){
      for(i in 2:length(stocks)){
        tryCatch(
          {
            data = as.data.frame(getSymbols(Symbols = stocks[i], env = NULL, from = input$dates[1], to = input$dates[2], src = 'yahoo'))
            
            data = data[-c(1:5)] ##removes unnecessary columns provided by quantmod, we only want adjusted closing prices
            
            data$date = as.character(rownames(data))
            
            attach(data)
            data = data[rev(order(date)),]
            detach(data)
            
            dates = data$date
            
            data = as.matrix(data[,1])
            colnames(data)[1] = stocks[i]
            rownames(data) = dates
            
            base = cbind(base,data)
          }
          , error = function(e){print("ERROR: One of your stock tickers may be invalid")}
        )
      }
    }
    
    #update base to reflect total market value of shares - Prices * Shares
    for(i in 1:ncol(base)){
      base[,i] = base[,i]*shares[i]
    }
    
    returns = matrix(NA, nrow = nrow(base)-1, ncol = ncol(base))
    colnames(returns) = colnames(base)
    rownames(returns) = rownames(base)[1:length(rownames(base))-1]
    
    for(i in 1:nrow(returns)){
      for(j in 1:ncol(returns)){
        returns[i,j] = ((base[i,j]/base[nrow(base),j])-1)
      }
    }
    
    returns = as.data.frame(returns)
    returns$Dates = as.Date(rownames(returns))
    rownames(returns) = NULL
    
    returns = melt(returns, "Dates")
    
    return(returns)
  })
  
  #Code to make "Plot" and "Clear" buttons work
  #set kernel for whether to display plot; initially null so no plot is shown. Plot will show when kernel is not null
  kernel = reactiveValues(v = NULL)
  
  observeEvent(eventExpr = input$plot1,
               {
                 kernel$v = 1
               }             
  )
  
  observeEvent(eventExpr = input$clear1,
               {
                 kernel$v = NULL             
               }             
  )
  #clears plot upon switching tabs
  observeEvent(eventExpr = input$tabset,
               {
                 kernel$v = NULL             
               }             
  )
  
  observeEvent(eventExpr = input$plot2,
               {
                 kernel$v = 1               
               }
  )
  
  observeEvent(eventExpr = input$clear2,
               {
                 kernel$v = NULL               
               }
  )
  
  #Render outputs
  output$graph = renderPlot({
    if (is.null(kernel$v)){
      return()
    }
    else {
      #Necessary to ensure that the right plot shows on the right tabset
      #without this, the plot for "Enter Information" tabset will show up on "Upload CSV"
      if (input$tabset == "Enter Information"){
        if (input$plotType1 == "sep"){
          ggplot(stockValue(), aes(x=Dates,y=value,col=variable)) + geom_line(size=1) + scale_x_date(date_labels = "%b-%Y") + theme_solarized_2(light = FALSE) + scale_colour_solarized() + xlab("") + ylab("Percentage Return") + ggtitle(paste0("Performance since ", input$dates[1])) + scale_colour_discrete(name = "Ticker", breaks=tickersInput(), labels = tickersInput())
        }
        else{
          ggplot(portfolioValue(), aes(Date,Cumulative_Return)) + geom_line(color = "green", size=1) + scale_x_date(date_labels = "%b-%Y") + theme_solarized_2(light = FALSE) + scale_colour_solarized() + xlab("") + ylab("Percentage Return") + ggtitle(paste0("Portfolio performance since ", input$dates[1]))
        }
      }
      else {
        if (input$plotType2 == "sep"){
          ggplot(stockValue(), aes(x=Dates,y=value,col=variable)) + geom_line(size=1) + scale_x_date(date_labels = "%b-%Y") + theme_solarized_2(light = FALSE) + scale_colour_solarized() + xlab("") + ylab("Percentage Return") + ggtitle(paste0("Performance since ", input$dates[1])) + scale_colour_discrete(name = "Ticker", breaks=tickersInput(), labels = tickersInput())
        }
        else{
          ggplot(portfolioValue(), aes(Date,Cumulative_Return)) + geom_line(color = "green", size=1) + scale_x_date(date_labels = "%b-%Y") + theme_solarized_2(light = FALSE) + scale_colour_solarized() + xlab("") + ylab("Percentage Return") + ggtitle(paste0("Portfolio performance since ", input$dates[1]))
        }
      }
    }
  })
  
  #Handle Example CSV link on "Upload CSV"
  output$example = downloadHandler(
    filename = "example.csv",
    content = function(file) {
      write.csv(exampleData, file)
    }
  )
  
  #Code to make download buttons work; simply return the stockValue() and portfolioValue() dfs as CSV
  output$download1 = downloadHandler(
    filename = function() {
      paste("shinyTracker-", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      if (is.null(kernel$v)){
        return()
      }
      else {
        if (input$tabset == "Enter Information"){
          if (input$plotType1 == "sep"){
            write.csv(stockValue(),file)
          }
          else{
            write.csv(portfolioValue(),file)
          }
        }
      }
    }
  )
  
  output$download2 = downloadHandler(
    filename = function() {
      paste("shinyTracker-", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      if (is.null(kernel$v)){
        return()
      }
      else {
        if (input$tabset == "Upload CSV"){
          if (input$plotType2 == "sep"){
            write.csv(stockValue(),file)
          }
          else{
            write.csv(portfolioValue(),file)
          }
        }
      }
    }
  )
  
  
}

shinyApp(ui = ui, server = server)

