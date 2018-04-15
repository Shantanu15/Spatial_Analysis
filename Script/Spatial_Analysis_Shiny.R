library(shiny)
library(shinydashboard)
library(plotly)
library(rgdal)
library(dplyr)
library(ggplot2)
library(maptools)



ui <- dashboardPage(
  dashboardHeader(title = "Spatial Analysis - 1st Try!"),
  
  dashboardSidebar(
    selectInput("state_1", "Choose the first State:", 
                list('Bihar','Jharkhand')),
    selectInput("state_2", "Choose the second State:", 
                list('Bihar','Jharkhand')
    )),
  
  dashboardBody(
    fluidRow(
      
      column(6,plotOutput('Plot_1', height = "500px"), hover= hoverOpts(id = "plot_hover_1")),
      column(6, plotOutput('Plot_2', height = "500px"),hover= hoverOpts(id = "plot_hover_2"))
      
    ),
  
  fluidRow(
    column(6, verbatimTextOutput("info_hover_1"))
    
  ),
    
  fluidRow(
      column(6, verbatimTextOutput("info_hover_2")) 
)
))

server <- function(input,output){
  
  full_map <- readOGR(dsn = "E:\\Google Drive\\R\\Maps\\maps-master\\Districts\\Census_2011", layer = "2011_Dist" ) 
  out_data <- read.csv("E:\\Google Drive\\R\\Maps\\Data\\combined_states.csv", stringsAsFactors = FALSE)
  
  dataset1 <- reactive({
    out_data_select_1 <- dplyr::filter(out_data,State == input$state_1)
    select_state_1  <- full_map[full_map$ST_NM == input$state_1,]
    select_state_1@data$id <- rownames(select_state_1@data)
    select_state_1.df <- fortify(select_state_1)
    select_state_1.df <- dplyr::left_join(select_state_1.df,select_state_1@data, by = "id")
    select_state_1.df$censuscode <- as.numeric(as.character(select_state_1.df$censuscode))
    select_state_1.df <- left_join(select_state_1.df,out_data_select_1, by = "censuscode")
    select_state_1.df <- select(select_state_1.df, -District)
    
  })
  
  observe({
    
    data_1 <- dataset1()
    
    
    
    
    output$Plot_1 <- renderPlot({ggplot(data_1, aes(x=long, y= lat, group= group)) + geom_polygon(aes(fill=Out_amt)) + geom_path(color = "grey", linestyle = 2) + coord_equal() + scale_fill_gradient(low = "#ffffcc", high = "blue", space = "Lab", na.value = "grey50", guide = "colourbar")})
    
    
  })  
  
  ##### Replicating for Plot 2 - will think later
  dataset2 <- reactive({
    out_data_select_2 <- dplyr::filter(out_data,State == input$state_2)
    select_state_2  <- full_map[full_map$ST_NM == input$state_2,]
    select_state_2@data$id <- rownames(select_state_2@data)
    select_state_2.df <- fortify(select_state_2)
    select_state_2.df <- dplyr::left_join(select_state_2.df,select_state_2@data, by = "id")
    select_state_2.df$censuscode <- as.numeric(as.character(select_state_2.df$censuscode))
    select_state_2.df <- left_join(select_state_2.df,out_data_select_2, by = "censuscode")
    select_state_2.df <- select(select_state_2.df, -District)
    
  })
  
  observe({
    
    data_2 <- dataset2()
    
    
    
    output$Plot_2 <-renderPlot({ggplot(data_2, aes(x=long, y= lat, group= group)) + geom_polygon(aes(fill=Out_amt)) + geom_path(color = "grey", linestyle = 2) + coord_equal() + scale_fill_gradient(low = "#ffffcc", high = "blue", space = "Lab", na.value = "grey50", guide = "colourbar")})
    
    output$info_hover_2 <- renderText({
      
      if(!is.null(input$plot_hover_2)){
        hover =input$plot_hover_2
        paste0("x=", "y=", "/n")
        }
    })  
    
    
     }) 
  
  
}




shinyApp(ui=ui,server = server)