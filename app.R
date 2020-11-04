library(reshape2)
library(plotly)
library(dplyr)
library(readr)
library(tidyverse)
#To read in the csv:
cereal <- read_delim(file = "/Users/Warren.Sink/Desktop/R projects/team-project-s01-purple/APP/cereal.csv", delim = ";")
cereal <- cereal[-c(1),]
cereal$calories <- as.numeric(cereal$calories)
cereal$protein <- as.numeric(cereal$protein)
cereal$fat <- as.numeric(cereal$fat)
cereal$sodium <- as.numeric(cereal$sodium)
cereal$fiber <- as.numeric(cereal$fiber)
cereal$carbo <- as.numeric(cereal$carbo)
cereal$sugars <- as.numeric(cereal$sugars)
cereal$potass <- as.numeric(cereal$potass)
cereal$vitamins <- as.numeric(cereal$vitamins)
cereal$weight <- as.numeric(cereal$weight)
cereal$cups <- as.numeric(cereal$cups)
cereal$rating <- as.numeric(cereal$rating)
cereal$shelf <- as.factor(cereal$shelf)

cereal$mfr[cereal$mfr=="G"] <- "General Mills"
cereal$mfr[cereal$mfr=="P"] <- "Post"
cereal$mfr[cereal$mfr=="K"] <- "Kellogs"
cereal$mfr[cereal$mfr=="Q"] <- "Quakers"
cereal$mfr[cereal$mfr=="R"] <- "Ralston"
cereal$mfr[cereal$mfr=="N"] <- "Nabisco"
cereal$mfr[cereal$mfr=="A"] <- "Homestat Farm"
cereal$type[cereal$type=="C"] <- "Cold"
cereal$type[cereal$type=="H"] <- "Hot"

cereal$calories_1cup <- 1/(cereal$cups/cereal$calories)
cereal$protein_1cup <- 1/(cereal$cups/cereal$protein)
cereal$fat_1cup <- 1/(cereal$cups/cereal$fat)
cereal$sodium_1cup <- 1/(cereal$cups/cereal$sodium)
cereal$fiber_1cup <- 1/(cereal$cups/cereal$fiber)
cereal$carbo_1cup <- 1/(cereal$cups/cereal$carbo)
cereal$sugars_1cup <- 1/(cereal$cups/cereal$sugars)
cereal$potass_1cup <- 1/(cereal$cups/cereal$potass)
cereal$vitamins_1cup <- 1/(cereal$cups/cereal$vitamins)
cereal2<-cereal%>%
  select(name,shelf, mfr, type, rating, calories_1cup, protein_1cup, fat_1cup, sodium_1cup, fiber_1cup, carbo_1cup, sugars_1cup, potass_1cup, vitamins_1cup)

#create new variables for all the quantitative variables which account for the serving size (those values will be for one serving size)
cereal$calories_1cup <- 1/(cereal$cups/cereal$calories)
cereal$protein_1cup <- 1/(cereal$cups/cereal$protein)
cereal$fat_1cup <- 1/(cereal$cups/cereal$fat)
cereal$sodium_1cup <- 1/(cereal$cups/cereal$sodium)
cereal$fiber_1cup <- 1/(cereal$cups/cereal$fiber)
cereal$carbo_1cup <- 1/(cereal$cups/cereal$carbo)
cereal$sugars_1cup <- 1/(cereal$cups/cereal$sugars)
cereal$potass_1cup <- 1/(cereal$cups/cereal$potass)
cereal$vitamins_1cup <- 1/(cereal$cups/cereal$vitamins)
cereal2<-cereal%>%
  select(name,shelf, mfr, type, rating, calories_1cup, protein_1cup, fat_1cup, sodium_1cup, fiber_1cup, carbo_1cup, sugars_1cup, potass_1cup, vitamins_1cup)

cereal2 <- cereal2%>% #create variables which we can change to transform them into a dummies
  mutate(general_mills=mfr, kellogs=mfr, nabisco=mfr, ralston=mfr, post=mfr)

cereal2 <- cereal2 %>% #create the STEM dummy
  mutate(general_mills = case_when(
    general_mills == "General Mills" ~ 1,
    general_mills == "Kellogs" ~ 0,
    general_mills == "Post" ~ 0,
    general_mills == "Ralston" ~ 0,
    general_mills == "Quakers" ~ 0,
    general_mills == "Nabisco" ~ 0,
  ))

cereal2 <- cereal2 %>% #create the STEM dummy
  mutate(kellogs = case_when(
    kellogs == "General Mills" ~ 0,
    kellogs == "Kellogs" ~ 1,
    kellogs == "Post" ~ 0,
    kellogs == "Ralston" ~ 0,
    kellogs == "Quakers" ~ 0,
    kellogs == "Nabisco" ~ 0,
  ))

cereal2 <- cereal2 %>% #create the STEM dummy
  mutate(nabisco = case_when(
    nabisco == "General Mills" ~ 0,
    nabisco == "Kellogs" ~ 0,
    nabisco == "Post" ~ 0,
    nabisco == "Ralston" ~ 0,
    nabisco == "Quakers" ~ 0,
    nabisco == "Nabisco" ~ 1,
  ))

cereal2 <- cereal2 %>% #create the STEM dummy
  mutate(ralston = case_when(
    ralston == "General Mills" ~ 0,
    ralston == "Kellogs" ~ 0,
    ralston == "Post" ~ 0,
    ralston == "Ralston" ~ 1,
    ralston == "Quakers" ~ 0,
    ralston == "Nabisco" ~ 0,
  ))

cereal2 <- cereal2 %>% #create the STEM dummy
  mutate(post = case_when(
    post == "General Mills" ~ 0,
    post == "Kellogs" ~ 0,
    post == "Post" ~ 1,
    post == "Ralston" ~ 0,
    post == "Quakers" ~ 0,
    post == "Nabisco" ~ 0,
  ))


cereal_kmeans <- cereal

cereal_kmeans$type <- ifelse(cereal_kmeans$type == "Hot", 1, 0)

cereal %>%
  count(mfr)

cereal_kmeans <- cereal_kmeans %>%
  mutate(Kellogs = ifelse(mfr == "Kellogs", 1, 0)) %>%
  mutate(General_Mills = ifelse(mfr == "General Mills", 1, 0)) %>%
  select(-mfr)

cereal2 <- cereal2%>% #create variables which we can change to transform them into a dummies
  mutate(shelf2=shelf, shelf3=shelf)

cereal2 <- cereal2 %>% #create the STEM dummy
  mutate(shelf2 = case_when(
    shelf2 == 2 ~ 1,
    shelf2 == 1 ~ 0,
    shelf2 == 3 ~ 0,
  ))

cereal2 <- cereal2 %>% #create the STEM dummy
  mutate(shelf3 = case_when(
    shelf3 == 2 ~ 0,
    shelf3 == 1 ~ 0,
    shelf3 == 3 ~ 1,
  ))

#Making the 3D scatterplot and regression plane
x1 <- cereal2$sugars_1cup
x2 <- cereal2$fiber_1cup
y <- cereal2$rating
df <- data.frame(x1, x2, y);


### Estimation of the regression plane
mod <- lm(y ~ x1+x2, data = df, na.action =     
            na.omit);

cf.mod <- coef(mod)

### Calculate z on a grid of x-y values
x1.seq <- seq(min(x1),max(x1),length.out=231)
x2.seq <- seq(min(x2),max(x2),length.out=231)
rating <- t(outer(x1.seq, x2.seq, function(x1,x2) 
  cf.mod[1]+cf.mod[2]*x1+cf.mod[3]*x2))

cereal_kmeans <- cereal_kmeans[,-1]

ui <- fluidPage(
  titlePanel("Cereal!"),
  sidebarLayout(
    #headerPanel('Cereal k-means clustering'),
    
    mainPanel(
      tabsetPanel(id = "bab",
                  type = "tabs", 
                  tabPanel( value = "Kmns", "K means", plotOutput('plot1')), 
                  tabPanel("3D Plane", 
                           plotlyOutput('plot2'), 
                           verbatimTextOutput("hover"),
                           verbatimTextOutput("click"))
      )
    ),
    conditionalPanel(condition = "input.bab == 'Kmns'",
                     sidebarPanel(
                       selectInput('xcol', 'X Variable', names(cereal_kmeans)),
                       selectInput('ycol', 'Y Variable', names(cereal_kmeans),
                                   selected=names(cereal_kmeans)[[2]]),
                       numericInput('clusters', 'Cluster count', 3,
                                    min = 1, max = 9)
                     ),
    )
  )
)


server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    cereal_kmeans[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
  output$plot2 <- renderPlotly({
    plot_ly(x=~x1.seq, y=~x2.seq, z=~rating, colors = 
              c("#f5cb11", "#b31d83"),type="surface") %>%
      add_trace(data=df, x=x1, y=x2, z=y, mode="markers", 
                type="scatter3d", 
                marker = list(color=cols, opacity=0.7, symbol=105)) %>%
      layout(title = 'Rating predicted by Sugar and Protein Content', scene = list(aspectmode = "manual", aspectratio = list(x=1, 
                                                                                                                             y=1, z=1), xaxis = list(title = "x = Sugars (grams)", range = c(0,20)), yaxis = 
                                                                                     list(title = "y = Protein (grams)", range = c(0,20)), zaxis = 
                                                                                     list(title = "Rating", range = c(0,100) )))
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
}

shinyApp(ui = ui, server = server)