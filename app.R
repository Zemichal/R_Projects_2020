library(shiny)
library(ggplot2)      #Packages needed 
library(ggrepel)
library(tidyverse)
library(readxl)
library(gginnards)
library(gridExtra)
library(httpuv)


# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Above + Beyond Charting Tool"),
  mainPanel(plotOutput("chart", width = "100%", height="1000px"))
  ,
  fluidRow(column(1,
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
           column(1, offset = 1,
                  numericInput("by","Please Select Break Points For Y Axis", 10,
                               min = 0, max = 30),
                  numericInput("byx","Please Select Break Points For X Axis", 10,
                               min = 0, max = 30),
                  textInput("h", "Set the High Color", "Black"),
                  textInput("m", "Set the Mid Color", "Black"),
                  textInput("l", "Set the Low Color", "Black"),
                  numericInput("mp", "Set the Midpoint Value", NULL)),
           column(4,
                           sliderInput("scalexr", "Please Select Range For X Axis",
                                       min = -100, max = 115, value = c(-50,75), width = 1000),
                           sliderInput("scalex1", "Please Select Start For X Axis", 
                                       min = -100, max = 115, value = -100, width = 1000),
                           sliderInput("scalex2", "Please Select End For X Axis", 
                                       min = -100, max = 115, value = 115, width = 1000)),
                    column(4,
                                    sliderInput("scaleyr", "Please Select Range For Y Axis", 
                                                min = -100, max = 115, value = c(-50,75), width = 1000),
                                    sliderInput("scaley1", "Please Select Start For Y Axis", 
                                                min = -100, max = 115, value = -100, width = 1000),
                                    sliderInput("scaley2", "Please Select End For Y Axis", 
                                                min = -100, max = 115, value = 115, width = 1000),
                           downloadButton('downloadPlot', 'Download Plot'))
  )
)

           
      
  

           



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  eb1 <- read_excel("SAMPLE DNA Analysis 1.xlsm", 
                    sheet = "Employee Information",
                    col_names = TRUE)
  
  
  # Puts the excel into a dataframe
  
  eb2 <- data.frame(eb1, stringsAsFactors = F)
  
  # Subset allows us to set the criteria 
  
  eb3 <- subset(eb2, ...3 == "Tanya McCagherty" | ...11 == "Tanya McCagherty" | ...9 == "Tanya McCagherty" | ...10 == "Tanya McCagherty") # & means AND | means OR
  
  # Will remove any columns with <NA> 
  
  eb4 <- eb3[ , colSums(is.na(eb3)) == 0]
  
  eb4$...5 <- factor(eb4$...5, levels = c("Exceeds","Meets","Does Not Meet"))
  
  #Creates a new column for level for the facet_grid
  eb4 <- mutate(eb4, level = ifelse(...9 %in% "Tanya McCagherty", "Level 1",
                                    ifelse(...10 %in% "Tanya McCagherty", "Level 2",
                                           ifelse(...11 %in% "Tanya McCagherty", "Level 3", "Leader"))))
  axis_x_input <- reactive({
    switch(input$x,
           "EP" = as.integer(eb4[,"...20"]),"AP" =as.integer(eb4[,"...21"]),"IP" = as.integer(eb4[,"...22"]),
           "PO" = as.integer(eb4[,"...23"]), "AO" = as.integer(eb4[,"...24"]), "CWC" =as.integer(eb4[,"...25"]), 
           "EQ" =as.integer( eb4[,"...30"]), "NULL" = 0)
  })
  
  axis_y_input <- reactive({
    switch(input$y,
           "EP" = as.integer(eb4[,"...20"]),"AP" =as.integer(eb4[,"...21"]),"IP" = as.integer(eb4[,"...22"]),
           "PO" = as.integer(eb4[,"...23"]), "AO" = as.integer(eb4[,"...24"]), "CWC" =as.integer(eb4[,"...25"]), 
           "EQ" =as.integer( eb4[,"...30"]), "NULL" = 0)
  })
  facet_v_input <- reactive({
    switch(input$fv,
           "Performance" = as.character(eb4[,"...5"]),"Level" =eb4[,"level"], "NULL" = " ")
  })
  facet_h_input <- reactive({
    switch(input$fh,
           "Performance" =as.character(eb4[,"...5"]),"Level" =eb4[,"level"], "NULL" = " ")
  })
  
  plotOutput <- function(){
    ggplot(data = eb4) +
      
      
      # geom_point(mapping = aes(x=axis_x_input(), y=axis_y_input(),size =.5, color =axis_y_input())) +
      geom_point(mapping = aes(x=axis_x_input(), y=axis_y_input(),size =.5, color =axis_x_input())) +
      
      scale_color_gradient2(low = input$l, mid = input$m, 
                            high = input$h, midpoint = input$mp) +
      facet_grid(facet_h_input() ~ facet_v_input(), as.table = FALSE) +
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
             legend.position = "bottom",
             # legend.key.height = unit(3.95,"cm"),
             legend.key.width = unit(6.5,"cm"),
             legend.text = element_blank())
    
  }
  
  output$chart <- renderPlot({
    print(plotOutput())
  }) 
  
  output$downloadPlot <- downloadHandler(
    filename = "nameFile",
    content = function(file) {
      png(file, width = 1000 , height = 700, bg = "transparent" )
      print(plotOutput())
      dev.off()
      
    }) 
}
# Run the application 
shinyApp(ui = ui, server = server)