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


list_choices <-  unique(msleep$vore)
list_choices = list_choices[!is.na(list_choices)]
names(list_choices) = paste0(list_choices,"vore")

Smokers <- Blood1 %>% filter(Smoke==1)
NonSmokers <- Blood1 %>% filter(Smoke==0)
Fit <- Blood1 %>% filter(Overwt==0)
Overweight <- Blood1 %>% filter(Overwt==1)
Obese <- Blood1 %>% filter(Overwt==2)

levels(Blood1$Smoke) <- c('Non Smoker','Smoker')
levels(Blood1$Overwt) <- c('Proper Weight','Overweight','Obese')
population <- c("Smokers","Non Smokers","Proper Weight","Overweight","Obese")
# Define UI for application that draws a histogram
ui <- navbarPage( "Sytolic Pressure Dataset",
                  tabPanel("Densities",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            checkboxInput("smo", label = "Smokers", value = F),
                            checkboxInput("non", label = "Non Smokers", value = F),
                            checkboxInput("pro", label = "Proper Weight", value = F),
                            checkboxInput("over", label = "Overweight", value = F),
                            checkboxInput("obe", label = "Obese", value = F)
                            
                          ),
                          
                          
                          mainPanel(
                            plotOutput(outputId ="density" )
                          )
                          
                        )#sidebarlayout
                        
                      )#fluidpage
                           ),#tabpanel
                  tabPanel("Proportions"
                           ),#tabpanel
                  tabPanel("msleep",
                           fluidPage(
                             sidebarLayout(
                               sidebarPanel( 
                                 selectInput("select", label= h3("Plot by type of alimentation"),
                                             choices=character(0),
                                             selected=1)
                                 
                               ),
                               mainPanel(
                                 plotOutput(outputId = "plot", click = "plot_click"),
                                 tableOutput("info")
                               )
                             )
                             
                           )),
                  tabPanel("Random generator",
                           sidebarLayout(position = "right",
                                         
                                         sidebarPanel(
                                           selectInput("dist", label = h3("Select the distribution"), 
                                                       choices = list(Normal="rnorm", Uniform="runif", Exponential="rexp"),
                                                       selected = 1),
                                           sliderInput("n_sample", label = h3("Number of samples"), min = 10, 
                                                       max = 100, value = 50),
                                           actionButton("goButton", "Go!"),
                                           fluidRow(
                                             h3(style = "margin-left: 20px; margin-bottom: 0px;", "Number of bins"),
                                             column(2,
                                                    div(style = "margin-top: 37px", checkboxInput("auto_bins", label = "auto", value = TRUE))
                                             ),
                                             column(10,
                                                    sliderInput("n_bins", label="", min = 1, max = 50, value = 30)
                                             )
                                           ), # fluidRow
                                           downloadButton("report", "Generate report")
                                         ), # sidebarPanel
                                         mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Plot", plotOutput("histPlot")),
                                                       tabPanel("Summary", verbatimTextOutput("histSummary")),
                                                       tabPanel("Table", tableOutput("histTable"))
                                           )
                                         ) # mainPanel
                           ) # sidebarLayout
                  ), #  tabPanel
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


col_scale <- scale_colour_discrete(limits = list_choices)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  updateSelectInput(session, "select",
                    choices = list_choices,
                    selected = tail(list_choices, 1)
  )
  

  output$density = renderPlot({
    p <- ggplot(Blood1)
    if (input$smo && input$non){
      ggplot(Blood1,aes(SystolicBP,color=Smoke))+geom_density()}
    else if (input$non){
      ggplot(NonSmokers,aes(SystolicBP))+geom_density(color="blue")}
    else if (input$smo){ggplot(NonSmokers,aes(SystolicBP))+geom_density(color="blue")}
    else if (input$pro && input$over && input$obe){
      ggplot(Blood1,aes(SystolicBP,color=Overwt))+geom_density()
    }
    
    
    else if (input$pro){
      ggplot(Fit,aes(SystolicBP))+geom_density(color="blue")}
    else if (input$over){
      ggplot(Overweight,aes(SystolicBP))+geom_density(color="blue")}
    else if (input$obe){
      ggplot(Obese,aes(SystolicBP))+geom_density(color="blue")}
    
  })
  
  output$plot = renderPlot({
    ggplot(msleep %>% filter(vore==input$select)
           , aes(bodywt, sleep_total, colour = vore)) +
      scale_x_log10() +
      col_scale +
      geom_point()
  })
  
  output$info <- renderTable({
    nearPoints(msleep 
               %>% filter(vore == input$select) 
               %>% select(name, bodywt,  sleep_total, sleep_rem, sleep_cycle ), 
               input$plot_click, threshold = 100, maxpoints = 3,
               addDist = TRUE)
  }) 
  
  samples <- reactive({
    input$goButton
    dist <- eval(parse(text=paste(input$dist)))
    dist(isolate(input$n_sample))
  })
  
  observe(if(input$auto_bins) disable("n_bins") else enable("n_bins"))
  
  output$histPlot <- renderPlot(
    hist(samples(), main="Random Generation", 
         breaks = if(!input$auto_bins) {input$n_bins} else {"Sturges"})
  )
  output$histSummary <- renderPrint(summary(samples()))
  output$histTable <- renderTable(samples())
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        n_sample = isolate(input$n_sample), 
        dist = isolate(input$dist), 
        breaks = if(!isolate(input$auto_bins)) {isolate(input$n_bins)} else {"Sturges"}
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
