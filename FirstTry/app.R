
library(shiny)
library(raster)
library(rgdal)

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
            Species <- c(1:4)
            Random <- sample(Species, 4)
            myTabs = lapply(Random, function(i){
                
                
                tabPanel(paste("Breed", i),
                         
                         ###
                         fluidPage(
                             fluidRow(
                                 column(6,
                                        wellPanel(
                                            plotOutput(paste0("PlotBreed",i)))),
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
                                            wellPanel(
                                                plotOutput(paste0("PlotHouse",RandomH[1]))),
                                            textOutput("Numb")
                                     )
                                     
                                 })
                             )
                         ))
                ####
            })
            
            
            do.call(tabsetPanel, myTabs)
        })
        
        output$PlotBreed1 <- renderPlot({
            a <- stack("Images/border_collie.jpg")
            plotRGB(a)
        })
        
        output$PlotBreed2 <- renderPlot({
            b <- stack("Images/German-Shepherd.jpg")
            plotRGB(b)
        })
        
        output$PlotBreed3 <- renderPlot({
            c <- stack("Images/Yorkshire.jpg")
            plotRGB(c)
        })  
    
        output$PlotHouse1 <- renderPlot({
            d <- stack("Images/Appartment.jpeg")
            plotRGB(d)
        })
        
        output$PlotHouse2 <- renderPlot({
            e <- stack("Images/House.jpg")
            plotRGB(e)
        })
        
        output$PlotHouse3 <- renderPlot({
            f <- stack("Images/cottage.jpg")
            plotRGB(f)
        })
        
        
    }
    
) #cerrado de shiny