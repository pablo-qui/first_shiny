#
#This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(tidyverse)
require("shinyjs")
library(Stat2Data)
data(Blood1)

Blood1$Smoke <- as.factor(Blood1$Smoke)
Blood1$Overwt <- as.factor(Blood1$Overwt)


Smokers <- Blood1 %>% filter(Smoke==1)
NonSmokers <- Blood1 %>% filter(Smoke==0)
Fit <- Blood1 %>% filter(Overwt==0)
Overweight <- Blood1 %>% filter(Overwt==1)
Obese <- Blood1 %>% filter(Overwt==2)

levels(Blood1$Smoke) <- c('Non Smoker','Smoker')
levels(Blood1$Overwt) <- c('Proper Weight','Overweight','Obese')
population <- c("General", "Smokers","Non Smokers","Proper Weight","Overweight","Obese")
# Define UI for application that draws densities
ui <- navbarPage( "Sytolic Pressure Dataset",
                  tabPanel("Densities",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel( h4("Select the group of population of which you want to see the systolic pressure density." ),
                                        h5("You can select all form the same class to see a comparison of the densities."),
                                        h5("Smoking habits:"),
                            checkboxInput("smo", label = "Smokers", value = F),
                            checkboxInput("non", label = "Non Smokers", value = F),
                            h5("Weight class:"),
                            checkboxInput("pro", label = "Proper Weight", value = F),
                            checkboxInput("over", label = "Overweight", value = F),
                            checkboxInput("obe", label = "Obese", value = F)
                            
                            
                          ),
                          
                          
                          mainPanel(
                            
                            splitLayout(
                            plotOutput(outputId ="density"),
                            plotOutput(outputId = "density2")
                            )#splitlayout
                            
                            #vertical layout
                            )#mainpanel
                          
                        )#sidebarlayout
                        
                      )#fluidpage
                           ),#tabpanel
                  tabPanel("Frequentist Information",
                           fluidPage(
                             sidebarLayout(
                               sidebarPanel(selectInput('table', "Select the groups of population you want information of.",
                                                choices = character(0),selected = 1
                                                )),
                               mainPanel(verbatimTextOutput("summary"))
                             )#sidebarlayout
                             
                           )#fluidpage
                           ),#tabpanel
                   
                  tabPanel("References",
                           p(tags$button(class="btn btn-default", 
                                         `data-toggle`="collapse", 
                                         `data-target`="#hola",
                                         "References")),
                           div(class="collapse", id="hola",
                               div(class="card card-body",
                                   includeMarkdown("references.md")
                               )),
                           
                  ), #  tabPanel
                  useShinyjs()
) # navbarPage




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectInput(session, "table",
                    choices = population,
                    selected = NULL
  )

  output$density = renderPlot({
    p <- ggplot(Blood1)
    if (input$smo && input$non){
      ggplot(Blood1,aes(SystolicBP,color=Smoke))+geom_density()}
    else if (input$non){
      ggplot(NonSmokers,aes(SystolicBP))+geom_density(color="blue")+ggtitle("Non Smokers")}
    else if (input$smo){ggplot(Smokers,aes(SystolicBP))+geom_density(color="blue")+ggtitle("Smokers")}
    
  })
  output$density2 = renderPlot({
    
    if (input$pro && input$over && input$obe){
      ggplot(Blood1,aes(SystolicBP,color=Overwt))+geom_density()}
    else if (input$pro){
      ggplot(Fit,aes(SystolicBP))+geom_density(color="blue")+ggtitle("Proper Weight")}
    else if (input$over){
      ggplot(Overweight,aes(SystolicBP))+geom_density(color="blue")+ggtitle("Overweight")}
    else if (input$obe){
      ggplot(Obese,aes(SystolicBP))+geom_density(color="blue")+ggtitle("Obese") }
    
  })
  
  datasetInput <- reactive({
    switch(input$table,
           "General" = Blood1[1],
           "Smokers" = Smokers[1],
           "Non Smokers" = NonSmokers[1],
           "Proper Weight"=Fit[1],
            "Overweight"=Overweight[1],
             "Obese"=Obese[1])
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
