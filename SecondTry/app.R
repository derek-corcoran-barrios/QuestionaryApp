
library(shiny)

shinyApp(
    ui = tagList(
        navbarPage("",
                   
                   tabPanel("DogImages",
                            fluidRow(
                                column(width = 6,
                                       uiOutput('mytabs')
                                ) )
                            
                   )#cerrado tab panel 2
                   
        ) #cerrado de navbar Panel    
        
    ), #cerrado ui
    
    server <- function(input, output) {
        
        
        #####################################
        # Tabs breeds
        
        output$mytabs = renderUI({
            Breeds <- c(1:3)
            Random <- sample(Breeds, 3)
            myTabs = lapply(Random, function(i){
                
                
                tabPanel(paste("Breed", i),
                         
                         ###
                         fluidPage(
                             fluidRow(
                                 column(6,
                                        wellPanel(
                                            img(src = paste0("Dog",i, ".jpg"), height = 300, width = 300))),
                                         renderUI({
                                             Habitats <- c(1:3)
                                             RandomH <- sample(Habitats, 3)
                                             Values <- reactiveValues(j = 1)
                                             output$Numb <- renderText(Values$j)
                                             observe({
                                                 input$New_Button
                                                 isolate(Values$j <- Values$j + 1)
                                             })
                                             
                                             
                                             
                                             column(6,actionButton("New_Button", "Next house"),
                                                    textOutput("Numb"),
                                                    wellPanel(
                                                        uiOutput(paste0("PlotHouse",RandomH[Values$j])))
                                                    
                                             )
                                             
                                         })
                             )
                         ))
                ####
            })
            
            
            do.call(tabsetPanel, myTabs)
        })
        
        output$PlotHouse1 <- renderUI({
            img(src = "House1.jpg", height = 300, width = 300)
        })
        
        output$PlotHouse2 <- renderUI({
            img(src = "House2.jpg", height = 300, width = 300)
        })
        
        output$PlotHouse3 <- renderUI({
            img(src = "House3.jpg", height = 300, width = 300)
        })
        
        
    }
    
) #cerrado de shiny