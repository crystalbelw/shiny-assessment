library(shiny)
library(readxl)
library(ggplot2)

ui <- fluidPage(
  titlePanel("R Shiny Assessment"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload file', accept = c(".xlsx")
      )
    ),
    
    mainPanel(
      h4(HTML("<b>Cumulative paid claims ($): Loss year vs Development year</b>")),
      tableOutput('table1'),
      tags$hr(),
      plotOutput('plot1', width = "100%"))
  )
)

server <- function(input, output) {
  
  output$table1 <- renderTable(
    {
      if(is.null(input$file1))     
        return(NULL) 
      
      df<-read_excel(input$file1$datapath, sheet = "Assignment", col_names = TRUE, range = "B16:F19")
      
      losses <- c(unlist(df[1,1]), unlist(df[2,1]), unlist(df[3,1]))
      devyear_data <- df[,-1]
      years <- c(losses, unlist(devyear_data[,1]), unlist(devyear_data[,2]), unlist(devyear_data[,3]), unlist(devyear_data[,4]))
      
      table <- matrix(data = years, nrow= 3, ncol = 5, byrow = FALSE)
      colnames(table) <- paste(c(colnames(df)))
      table
    }, bordered = TRUE, spacing = 'l', align = 'c', digits = 0)
  
  output$plot1 <- renderPlot({
    
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    
    df<-read_excel(inFile$datapath, sheet = "Assignment", col_names = TRUE, range = "B16:F19")
    
    lossyear <- rep(c(unlist(df[1,1]), unlist(df[2,1]), unlist(df[3,1])), each = 4)
    df <- df[,-1]
    devyear <- c(rep(colnames(df), 3))
    years <- c(unlist(df[1,]), unlist(df[2,]), unlist(df[3,]))
    dataf <- data.frame(devyear, years, lossyear)
    
    ggplot(data = dataf,aes(x = devyear, group = lossyear, color = factor(lossyear), fill = factor(lossyear))) +
      geom_line(aes(y = years),stat = "identity") + geom_point(aes(y = years),stat = "identity") + geom_text(aes(label = round(years, digit = 0), y = years, color = factor(lossyear)),position = "identity",vjust = -1, hjust = 0.5, size = 4, show.legend = FALSE) + ylim(500000,1500000) + labs(title = "Cumulative paid claims ($)",x = "Development year", y = "Claims paid", color = "Loss year", fill = "Loss year", size = 4) + theme(plot.title = element_text(hjust =0.5))
  })
}

shinyApp(ui, server)
