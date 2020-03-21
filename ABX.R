library(ggplot2)      #Packages needed 
library(ggrepel)
library(tidyverse)
library(readxl)
library(gginnards)
library(gridExtra)

rm(list=ls())
gc(reset = TRUE) #Use this after running the code a few times to clear up memory 

# FIle used must be in the same location as R

eb1 <- read_excel("SAMPLE DNA Analysis 1.xlsm", 
                  sheet = "Employee Information",
                col_names = TRUE)


# Puts the excel into a dataframe

eb2 <- data.frame(eb1, stringsAsFactors = F)



# Subset allows us to set the criteria 

eb3 <- subset(eb2, ...3 == "Tanya McCagherty" | ...11 == "Tanya McCagherty" | 
                ...9 == "Tanya McCagherty" | ...10 == "Tanya McCagherty") # & means AND | means OR

# Will remove any columns with <NA> 

eb4 <- eb3[ , colSums(is.na(eb3)) == 0]


# facet_grid(eb4$...46 ~ eb4$...5, as.table = FALSE)

eb4$...5 <- factor(eb4$...5, levels = c("Does Not Meet","Meets","Exceeds"))

#Creates a new column for level to used in facet_grid
eb4 <- mutate(eb4, level = ifelse(...9 %in% "Tanya McCagherty", "Level 1",
                              ifelse(...10 %in% "Tanya McCagherty", "Level 2",
                                  ifelse(...11 %in% "Tanya McCagherty", "Level 3", "Leader"))))
#EP Vertical

ep1 <- ggplot(data = eb4, 
              mapping = aes(x=as.integer(0),                 
                            y=as.integer(...20),
                            color = as.integer(...20),            
                            group = ...20)) +
  
            geom_point(alpha = 1, size = 2) +
  
            scale_color_gradient2(low = "red4", mid = "gold3", 
                                  high = "green4", midpoint = 15) +
               facet_grid(. ~ ...5, as.table = FALSE) +
               
            labs(
                  x= "Enterprsing Potential",                       
                   y= "",
                   color = " ") + 
              
              geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
              guides(shape = FALSE)




ep1 <- ep1 +
    
  scale_y_continuous(limits = c(-50,85), breaks = seq(from = -50, to = 85, by = 10)) +
theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
       plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
       panel.grid.major = element_blank(), # get rid of major grid
       panel.grid.minor = element_blank(), # get rid of minor grid
       legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
       legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank(),
       legend.position = "left",
       legend.key.height = unit(3.95,"cm"),
       legend.text = element_blank())
ep1

# EP Horizontal

ep2 <- ggplot(data = eb4, 
              mapping = aes(y=as.integer(0),                 
                            x=as.integer(...20),
                            color = as.integer(...20),            
                            group = ...20)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "red4", mid = "gold3", 
                        high = "green4", midpoint = 15) +
  facet_grid(level ~ ., as.table = FALSE) +
  
  labs(
    x= "Enterprsing Potential",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)

ggsave(ep1, filename = "test.png",  bg = "transparent", height = 13, width = 13)


ep2 <- ep2 +
  
  scale_x_continuous(limits = c(-50,85), breaks = seq(from = -50, to = 85, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "bottom",
         legend.key.width = unit(5.95,"cm"),
         legend.text = element_blank(),
         strip.text.y = element_text(angle = 0))

ep2

 # eb6 <-delete_layers(eb5,"GeomLabelRepel") #removes labels
 
 # grid.arrange(eb5,eb6,ncol=1)

 
 # ggsave(ep2, filename = "test.png",  bg = "transparent", height = 13, width = 13)
    
# AP Vertical

ap1 <- ggplot(data = eb4, 
              mapping = aes(x=as.integer(0),                 
                            y=as.integer(...21),
                            color = as.integer(...21),            
                            group = ...21)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "red", mid = "green4", 
                        high = "red", midpoint = 10) +
  facet_grid(. ~ level, as.table = FALSE) +
  
  labs(
    x= "Achievement Potential",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




ap1 <- ap1 +
  
  scale_y_continuous(limits = c(-40,50), breaks = seq(from = -40, to = 50, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.position = "left",
         legend.key.height = unit(3.95,"cm"),
         legend.text = element_blank())
ap1
ggsave(ap1, filename = "ap_vert.png",  bg = "transparent", height = 13, width = 13)

# AP Horizontal

ap2 <- ggplot(data = eb4, 
              mapping = aes(y=as.integer(0),                 
                            x=as.integer(...21),
                            color = as.integer(...21),            
                            group = ...21)) +
  
  geom_point(alpha = 1, size = 2) +
  scale_color_gradient2(low = "red", mid = "green4", 
                        high = "red", midpoint = 10) +
  facet_grid(level ~ ., as.table = FALSE) +
  
  labs(
    x= "Achievement Potential",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




ap2 <- ap2 +
  
  scale_x_continuous(limits = c(-40,50), breaks = seq(from = -40, to = 50, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "bottom",
         legend.key.width = unit(5.95,"cm"),
         legend.text = element_blank(),
         strip.text.y = element_text(angle = 0))
ap2

#IP Vertical

ip1 <- ggplot(data = eb4, 
              mapping = aes(x=as.integer(0),                 
                            y=as.integer(...22),
                            color = as.integer(...22),            
                            group = ...22)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "orange", mid = "green4", 
                        high = "red", midpoint = -5) +
  facet_grid(. ~ level, as.table = FALSE) +
  
  labs(
    x= "Independence Potential",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




ip1 <- ip1 +
  
  scale_y_continuous(limits = c(-70,50), breaks = seq(from = -70, to = 50, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.position = "left",
         legend.key.height = unit(3.95,"cm"),
         legend.text = element_blank())
ip1
# IP Horizontal

ip2 <- ggplot(data = eb4, 
              mapping = aes(y=as.integer(0),                 
                            x=as.integer(...22),
                            color = as.integer(...22),            
                            group = ...22)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "orange", mid = "green4", 
                        high = "red", midpoint = -5) +
  facet_grid(level ~ ., as.table = FALSE) +
  
  labs(
    x= "Independence Potential",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




ip2 <- ip2 +
  
  scale_x_continuous(limits = c(-70,50), breaks = seq(from = -70, to = 50, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "bottom",
         legend.key.width = unit(5.95,"cm"),
         legend.text = element_blank(),
         strip.text.y = element_text(angle = 0))
ip2

#PO Vertical

po1 <- ggplot(data = eb4, 
              mapping = aes(x=as.integer(0),                 
                            y=as.integer(...23),
                            color = as.integer(...23),            
                            group = ...23)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "red4", mid = "gold3", 
                        high = "green4", midpoint = 5) +
  facet_grid(. ~ level, as.table = FALSE) +
  
  labs(
    x= "People Orientation",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




po1 <- po1 +
  
  scale_y_continuous(limits = c(-40,60), breaks = seq(from = -40, to = 60, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.position = "left",
         legend.key.height = unit(3.95,"cm"),
         legend.text = element_blank())
po1
# PO Horizontal

po2 <- ggplot(data = eb4, 
              mapping = aes(y=as.integer(0),                 
                            x=as.integer(...23),
                            color = as.integer(...23),            
                            group = ...23)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "red4", mid = "gold3", 
                        high = "green4", midpoint = 5) +
  facet_grid(level ~ ., as.table = FALSE) +
  
  labs(
    x= "People Orientation",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




po2 <- po2 +
  
  scale_x_continuous(limits = c(-40,60), breaks = seq(from = -40, to = 60, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "bottom",
         legend.key.width = unit(5.95,"cm"),
         legend.text = element_blank(),
         strip.text.y = element_text(angle = 0))
po2


# Cwc Vertical

cwc1 <- ggplot(data = eb4, 
              mapping = aes(x=as.integer(0),                 
                            y=as.integer(...25),
                            color = as.integer(...25),            
                            group = ...25)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "orangered", mid = "green4", 
                        high = "orangered", midpoint = 15) +
  facet_grid(. ~ level, as.table = FALSE) +
  
  labs(
    x= "Comfort With Conflict",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




cwc1 <- cwc1 +
  
  scale_y_continuous(limits = c(-40,50), breaks = seq(from = -40, to = 50, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.position = "left",
         legend.key.height = unit(3.95,"cm"),
         legend.text = element_blank())
cwc1

# Cwc Horizontal

cwc2 <- ggplot(data = eb4, 
               mapping = aes(y=as.integer(0),                 
                             x=as.integer(...25),
                             color = as.integer(...25),            
                             group = ...25)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "orangered", mid = "green4", 
                        high = "orangered", midpoint = 15)+
  facet_grid(level ~ ., as.table = FALSE) +
  
  labs(
    x= "Comfort With Conflict",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




cwc2 <- cwc2 +
  
  scale_x_continuous(limits = c(-40,50), breaks = seq(from = -40, to = 50, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "bottom",
         legend.key.width = unit(5.95,"cm"),
         legend.text = element_blank(),
         strip.text.y = element_text(angle = 0))
cwc2

# EQ Vertical

eq1 <- ggplot(data = eb4, 
              mapping = aes(x=as.integer(0),                 
                            y=as.integer(...30),
                            color = as.integer(...30),            
                            group = ...30)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "red4", mid = "gold3", 
                        high = "green4", midpoint = 60) +
  facet_grid(. ~ level, as.table = FALSE) +
  
  labs(
    x= "Emotional Quotient",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




eq1 <- eq1 +
  
  scale_y_continuous(limits = c(30,100), breaks = seq(from = 30, to = 100, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.position = "left",
         legend.key.height = unit(3.95,"cm"),
         legend.text = element_blank())
eq1

# EQ Horizontal

eq2 <- ggplot(data = eb4, 
              mapping = aes(y=as.integer(0),                 
                            x=as.integer(...30),
                            color = as.integer(...30),            
                            group = ...30)) +
  
  geom_point(alpha = 1, size = 2) +
  
  scale_color_gradient2(low = "red4", mid = "gold3", 
                        high = "green4", midpoint = 60) +
  facet_grid(level ~ ., as.table = FALSE) +
  
  labs(
    x= "Emotional Quotient",                       
    y= "",
    color = " ") + 
  
  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(...3))) +               
  guides(shape = FALSE)




eq2 <- eq2 +
  
  scale_x_continuous(limits = c(30,100), breaks = seq(from = 30, to = 100, by = 10)) +
  theme( panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "bottom",
         legend.key.width = unit(5.95,"cm"),
         legend.text = element_blank(),
         strip.text.y = element_text(angle = 0))
eq2



#Flipped Shapes

fs <- ggplot(data = eb4,
              mapping = aes(x=as.integer(eb4$...20),
                            y=as.integer(0),
                            shape=eb4$...5,
                            color = as.integer(eb4$...20),
                            group = eb4$...20)) +


  geom_point(alpha = 1, size = 2) +


  scale_color_gradient2(low = "red4", mid = "gold3",
                        high = "green4",
                        midpoint = 15) +


  labs(x= "Enterprsing Potential", y= "", shape = "Performance", color = " ") +


  geom_label_repel(box.padding = .25, point.padding = .25, aes(label=as.character(eb4$...3))) +


  guides(shape = FALSE) +

  #Doc
  scale_x_continuous(limits = c(-15,85), breaks = seq(from = -15, to = 85, by = 5)) +


  theme( panel.background = element_rect(fill = "transparent"),
         plot.background = element_rect(fill = "transparent", color = NA),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         legend.position = "bottom",
         legend.key.width = unit(5.95,"cm"),
         legend.text = element_blank())

fs
