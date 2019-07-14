library(shiny)
Habitats <- c(1:3)
RandomH <- sample(Habitats, 3)


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
        
        
        global <- reactiveValues(nr = 1)    
        
        observeEvent(input$whichTab, {
            global$nr <- 1 
        })
        
        observeEvent(input$New_Button1, {
            global$nr <- min(global$nr + 1, max(RandomH))
        })
        
        observeEvent(input$New_Button2, {
            global$nr <- min(global$nr + 1, max(RandomH))
        })
        
        observeEvent(input$New_Button3, {
            global$nr <- min(global$nr + 1, max(RandomH))
        })
        
        
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
                                     
                                     column(6, actionButton(inputId = paste0("New_Button", i), "Next house"),
                                            wellPanel(
                                                uiOutput(paste0("PlotHouse", i)))
                                     )
                                     
                                 })
                             )
                         ))
                ####
            })
            
            
            do.call(tabsetPanel, c(id = "whichTab", myTabs))
        })
        
        output$PlotHouse1 <- renderUI({
            img(src = paste0("House", RandomH[global$nr],".jpg"), height = 300, width = 300)
        })
        
        output$PlotHouse2 <- renderUI({
            img(src = paste0("House", RandomH[global$nr],".jpg"), height = 300, width = 300)
        })
        
        output$PlotHouse3 <- renderUI({
            img(src = paste0("House", RandomH[global$nr],".jpg"), height = 300, width = 300)
        })
        
        
    }
    
) #cerrado de shiny