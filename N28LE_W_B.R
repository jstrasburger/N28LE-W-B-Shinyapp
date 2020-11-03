library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fixedPage(
                      headerPanel("N28LE Weight & Balance")
                      ,sidebarPanel(numericInput("FuelBurn", "Fuel Burn:", 10.5, min = 8.5, max = 18),
                                    numericInput("Est_Time_enr", "Estimated Time En Route:", 2.0, min = 0.1, max = 5.0),
                                    numericInput("Pilot_and_Co", "Pilot and Co-Pilot Weight:", 180, min = 0, max = 500),
                                    numericInput("aftpassenger", "Aft Passengers Weight", 20, min = 0, max = 500),
                                    numericInput("Fuel_gal", "Fuel on Departure", 34, min = 0, max = 48),
                                    numericInput("Baggage", "Baggage Compartment", 60, min = 0, max = 200),
                                    verbatimTextOutput("value"),
                                    width = 3
                      )
                      ,mainPanel(plotOutput("weightandbalanceplot"), width = 8)
                      
)

server <- function(input, output) {
  output$weightandbalanceplot <- renderPlot({
    Licensed_Empty <- 1743
    LicenseEaad <- 89.78
    
    
    Oil_8quarts <- 15
    oil_aad <- 24.5
    
    Pilot_and_Co <- input$Pilot_and_Co
    Pilot_and_Coaad <- 80.5
    
    aftpassenger <- input$aftpassenger
    aftpassengeraad <- 118.1
    
    Fuel <- input$Fuel_gal*6
    Fuelaad <- 95
    
    Baggage_compartment <- input$Baggage
    Baggage_compartmentaad <- 142.8
    
    retract_mom <- 819
    
    #N28LE W&B
    empty_mom <- Licensed_Empty*LicenseEaad
    oil_mom <- Oil_8quarts*oil_aad
    pico_mom <- Pilot_and_Co*Pilot_and_Coaad
    aft_pass_mom <- aftpassenger*aftpassengeraad
    Fuel_mom <- Fuel*Fuelaad
    Baggage_compartment_mom <- Baggage_compartment * Baggage_compartmentaad
    retract_mom <- retract_mom
    
    Weight <- sum(Licensed_Empty,Oil_8quarts,Pilot_and_Co,aftpassenger,Fuel,Baggage_compartment)
    Mom <- sum(empty_mom, oil_mom, pico_mom, aft_pass_mom, Fuel_mom, Baggage_compartment_mom, retract_mom)
    
    CG.gearup.TO <- Mom/Weight
    CG.geardn.TO <- (Mom-retract_mom)/Weight
    
    #N28LE W&B LANDING
    FuelBurn <- input$FuelBurn * input$Est_Time_enr
    Fuelreman <- Fuel - (FuelBurn*6) 
    NewFuelmom <- Fuelreman*Fuelaad
    
    
    Landing_weight <- sum(Licensed_Empty,Oil_8quarts,Pilot_and_Co,aftpassenger,Fuelreman,Baggage_compartment)
    Landing_mom <- sum(empty_mom, oil_mom, pico_mom, aft_pass_mom, NewFuelmom, Baggage_compartment_mom, retract_mom)
    
    CG.gearup.LAND <- Landing_mom/Landing_weight
    CG.geardn.LAND <- (Landing_mom-retract_mom)/Landing_weight
    
    #Zero Fuel
    Fuel_empty <- sum(Licensed_Empty,Oil_8quarts,Pilot_and_Co,aftpassenger,Baggage_compartment)
    Fuel_empty_mom <- sum(empty_mom, oil_mom, pico_mom, aft_pass_mom, Baggage_compartment_mom, retract_mom)
    
    CG.gearup.Empty <- Fuel_empty_mom/Fuel_empty
    CG.geardn.Empty <- (Fuel_empty_mom-retract_mom)/Fuel_empty
    
    ##Plot
    
    #constraints
    segments8LE <- data.frame(x0 = c(80,80,82,87.3,93,80)
                              ,y0 = c(0,1800,2300,2650,2650,0)
                              ,x1 = c(80,82,87.3,93,93,93)
                              ,y1= c(1800,2300,2650,2650,0,0))
    
    #DATA FRAME CONSTRUCTIOn
    Gear_Down.CGs <- c(CG.geardn.TO, CG.geardn.LAND, CG.geardn.Empty)
    Gear_Up.CGs <- c(CG.gearup.TO, CG.gearup.LAND, CG.gearup.Empty)
    Phase <- c("Takeoff", "Landing", "Zero_Fuel")
    Configuration <- c("Gear Down", "Gear Down", "Gear Down", "Gear Up", "Gear Up", "Gear Up")
    
    CG_Data_Frame <- data.frame(c(Gear_Down.CGs, Gear_Up.CGs),c(Weight, Landing_weight, Fuel_empty), Phase, Configuration)
    colnames(CG_Data_Frame) <- c("Moment", "Weight", "Phase", "Configuration")
    
    
    
    
    CG_Data_Frame %>%
      ggplot(aes(x = Moment, y = Weight)) + geom_point(aes(shape = Configuration, color = Phase), size = 3) + 
      geom_segment(aes(x = segments8LE$x0 
                       ,y = segments8LE$y0
                       , xend = segments8LE$x1 
                       ,yend = segments8LE$y1)
                   ,size = 1.1) +
      labs(title = "Center of Gravity Report")+
      xlab("MOMENT in IN/LBS") +
      ylab("WEIGHT")
  }
   
  )
  
  #output$value <- renderText({ input$FuelBurn })
}
shinyApp(ui, server)


