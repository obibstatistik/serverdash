rm(list = ls())
library(shiny)
library(rCharts)

ui =pageWithSidebar(
  headerPanel("Test"),
  
  sidebarPanel(),
  mainPanel(
    showOutput("SectorCharting", "nvd3")
  )
)


server = function(input, output) {
  
  output$SectorCharting <-renderChart2({
    hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
    n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
                type = 'multiBarChart')
    return(n1)
  })
}


runApp(list(ui = ui, server = server))