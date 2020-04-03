library(shiny)
library(ggplot2)      #Packages needed 
library(ggrepel)
library(tidyverse)
library(readxl)
library(gginnards)
library(gridExtra)
library(httpuv)
library(shinyWidgets)

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Above + Beyond Charting Tool"),
  mainPanel(tabsetPanel(
    tabPanel("Vertical Chart", plotOutput("chartv", width = "100%", height="700px")),
    tabPanel("Horizontial Chart", plotOutput("charth", width = "100%", height="700px")))),
  fluidRow(column(2,wellPanel(
    selectInput("xl", "Please Select X Axis Label",choices = c(" ","Enterprsing Potential","Achievement Potential",
                                                               "Independence Potential","People Orientation","Analytical Orientation", 
                                                               "Comfort With Conflict", "Emotional Quotient"), selected = " "),
    selectInput("yl", "Please Select Y Axis Label",choices = c(" ","Enterprsing Potential","Achievement Potential",
                                                               "Independence Potential","People Orientation","Analytical Orientation", 
                                                               "Comfort With Conflict", "Emotional Quotient"), selected = " "),
    selectInput("x", "Please Select X Axis",choices = c("EP","AP","IP","PO",
                                                        "AO", "CWC", "EQ","NULL")),
    selectInput("y", "Please Select Y Axis", choices =c("EP","AP","IP","PO",
                                                        "AO", "CWC", "EQ","NULL")),
    
    selectInput("fv", "Please Select Vertical Facet", choices =c("Level", "Performance","NULL"), selected = "NULL"),
    selectInput("fh", "Please Select Horizontal Facet", choices =c("Level", "Performance","NULL"), selected = "NULL")),
    actionButton("button","Update Chart"))),
  
  column(3,wellPanel(
    numericInput("by","Please Select Break Points For Y Axis", 10,
                 min = 0, max = 30),
    numericInput("byx","Please Select Break Points For X Axis", 10,
                 min = 0, max = 30),
    textInput("h", "Set the High Color", "Black"),
    textInput("m", "Set the Mid Color", "Black"),
    textInput("l", "Set the Low Color", "Black"),
    numericInput("mp", "Set the Midpoint Value", 15))),
  
  fluidRow(column(4,wellPanel(
    numericRangeInput("scalexr", "Please Select Range For X Axis",
                      value = c(-100,115)),
    sliderInput("scalex1", "Please Select Start For X Axis", 
                min = -115, max = 115, value = -100, width = 1000),
    sliderInput("scalex2", "Please Select End For X Axis", 
                min = -115, max = 115, value = 115, width = 1000),
    h5("Sample Text May Use to Detail Inputs"),
    numericRangeInput("scaleyr", "Please Select Range For Y Axis",
                      value = c(-100,115)),
    sliderInput("scaley1", "Please Select Start For Y Axis", 
                min = -115, max = 115, value = -100, width = 1000),
    sliderInput("scaley2", "Please Select End For Y Axis", 
                min = -115, max = 115, value = 115, width = 1000),
    downloadButton('downloadPlot1', 'Download Vertical Plot'),
    downloadButton('downloadPlot2', 'Download Horizontial Plot'),
  )))
  
  
  
)


# Define server logic required to draw the plot

server <- function(input, output, session) {
  
  eb1 <- read_excel("SAMPLE DNA Analysis 1.xlsm", 
                    sheet = "Employee Information",
                    col_names = TRUE)
  
  
  # Puts the excel into a dataframe
  
  eb2 <- data.frame(eb1, stringsAsFactors = F)
  
  # Subset allows us to set the criteria 
  # & means AND | means OR
  
  eb3 <- subset(eb2, ...3 == "Tanya McCagherty" | ...11 == "Tanya McCagherty" | 
                  ...9 == "Tanya McCagherty" | ...10 == "Tanya McCagherty") 
  
  # Will remove any columns with <NA> 
  
  eb4 <- eb3[ , colSums(is.na(eb3)) == 0]
  
  
  
  #Creates a new column for level for the facet_grid
  eb4 <- mutate(eb4, level = ifelse(...9 %in% "Tanya McCagherty", "Level 1",
                                    ifelse(...10 %in% "Tanya McCagherty", "Level 2",
                                           ifelse(...11 %in% "Tanya McCagherty", "Level 3", "Leader"))))
  
  eb4$perf <- as.factor(eb4$...5)
  
  eb4$lvls <- as.character(eb4$level)
  
  #These 4 functions hold the values from the data frame
  
  axis_x_input <- eventReactive(input$button,{
    switch(input$x,
           "EP" = as.integer(eb4[,"...20"]),"AP" =as.integer(eb4[,"...21"]),"IP" = as.integer(eb4[,"...22"]),
           "PO" = as.integer(eb4[,"...23"]), "AO" = as.integer(eb4[,"...24"]), "CWC" =as.integer(eb4[,"...25"]), 
           "EQ" =as.integer( eb4[,"...30"]), "NULL" = 0)
  })
  
  axis_y_input <- eventReactive(input$button,{
    switch(input$y,
           "EP" = as.integer(eb4[,"...20"]),"AP" =as.integer(eb4[,"...21"]),"IP" = as.integer(eb4[,"...22"]),
           "PO" = as.integer(eb4[,"...23"]), "AO" = as.integer(eb4[,"...24"]), "CWC" =as.integer(eb4[,"...25"]), 
           "EQ" =as.integer( eb4[,"...30"]), "NULL" = 0)
  })
  
  facet_v_input <- eventReactive(input$button,{
    switch(input$fv,
           "Performance" = as.character(eb4[,"perf"]),"Level" = as.character(eb4[,"lvls"]), "NULL" = " ")
  })
  
  facet_h_input <- eventReactive(input$button,{
    switch(input$fh,
           "Performance" = as.character(eb4[,"perf"]),"Level" = as.character(eb4[,"lvls"]), "NULL" = " ")
  })
  
  plotOutput1 <- function(){
    ggplot(data = eb4) +
      
      
      geom_point(mapping = aes(x=axis_x_input(), y=axis_y_input(),size =.5, color =axis_y_input())) +
      
      scale_color_gradient2(low = input$l, mid = input$m, 
                            high = input$h, midpoint = input$mp) +
      facet_grid(.~facet_v_input()) +
      labs(
        x= input$xl,                       
        y= input$yl,
        color = " ") + 
      
      geom_label_repel(box.padding = .25, point.padding = .25, aes(x=axis_x_input(), y=axis_y_input(),label=as.character(...3))) +               
      guides(size = FALSE)+
      scale_x_continuous(limits = input$scalexr,breaks = seq(from = input$scalex1, to = input$scalex2, by = input$byx)) +
      scale_y_continuous(limits = input$scaleyr,breaks = seq(from = input$scaley1, to = input$scaley2, by = input$by)) +
      theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
             plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
             panel.grid.major = element_blank(), # get rid of major grid
             panel.grid.minor = element_blank(), # get rid of minor grid
             legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
             legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
             legend.position = "left",
             legend.key.height = unit(4.45,"cm"),
             legend.text = element_blank())
    
    
  }
  plotOutput2 <- function(){
    ggplot(data = eb4) +
      
      
      geom_point(mapping = aes(x=axis_x_input(), y=axis_y_input(),size =.5, color =-axis_x_input())) +
      
      scale_color_gradient2(low = input$h, mid = input$m, 
                            high = input$l, midpoint = input$mp) +
      facet_grid(facet_h_input()~.) +
      labs(
        x= input$xl,                       
        y= input$yl,
        color = " ") + 
      
      geom_label_repel(box.padding = .25, point.padding = .25, aes(x=axis_x_input(), y=axis_y_input(),label=as.character(...3))) +               
      guides(size = FALSE)+
      scale_x_reverse(limits = input$scalexr,breaks = seq(from = input$scalex1, to = input$scalex2, by = input$byx)) +
      scale_y_continuous(limits = input$scaleyr,breaks = seq(from = input$scaley1, to = input$scaley2, by = input$by)) +
      theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
             plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
             panel.grid.major = element_blank(), # get rid of major grid
             panel.grid.minor = element_blank(), # get rid of minor grid
             legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
             legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
             legend.position = "bottom",
             legend.key.width = unit(6.5,"cm"),
             legend.text = element_blank())
    
  }
  
  output$chartv <- renderPlot({
    print(plotOutput1())
  }) 
  
  output$charth <- renderPlot({
    print(plotOutput2())
  }) 
  
  output$downloadPlot1 <- downloadHandler(
    filename = "FileName.png",
    content = function(file) {
      png(file, width = 1000 , height = 700, bg = "transparent" )
      print(plotOutput1())
      dev.off()
    })
  
  output$downloadPlot2 <- downloadHandler(
    filename = "FileName.png",
    content = function(file) {
      png(file, width = 1000 , height = 700, bg = "transparent" )
      print(plotOutput2())
      dev.off()
      
    }) 
}
# Run the application 
shinyApp(ui = ui, server = server)
