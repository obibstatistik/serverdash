rm(list = ls())
library(shiny)
library(rCharts)
library(ggplot2)

#data <- read.csv("https://raw.githubusercontent.com/kilimba/data/master/data2.csv")
#agegroup_mapping <- read.csv("https://raw.githubusercontent.com/kilimba/data/master/agegroup.csv")
#data <- merge(data,agegroup_mapping,by.x="agegrp",by.y="agegroup")

ui = pageWithSidebar(
  headerPanel("Population"),
  
  sidebarPanel(
  ),
  mainPanel(
    #showOutput("myChart", "nvd3"),
    showOutput("d1Chart", "nvd3")
  )
)

server = function(input, output) {
  
  f <- system("sadf -d -T /var/log/sysstat/sa10 -- -q | grep $(hostname) | grep 21 | grep -v RESTART", intern =  TRUE)
  fuf <- read.table(text=f,sep = ";")
  names(fuf) <- c("hostname","interval","timestamp","runqsz","plistsz","ldavg1","ldavg5","ldavg15","blocked")
  ff <- select(fuf,6:8)
  ff
  
  output$d1Chart <- renderChart2({
    
    #example 48 timeAxis
    data( economics, package = "ggplot2" )
    economics$date = format(economics$date, "%Y-%m-%d")
    d1 <- dPlot(
      x = "date",
      y = "uempmed",
      data = economics,
      type = "line",
      height = 400,
      width = 700,
      bounds = list(x=50,y=20,width=650,height=300)
    )
    d1$xAxis(
      type = "addTimeAxis",
      inputFormat = "%Y-%m-%d",
      outputFormat = "%b %Y"
    )
    d1
    
    #test out additional layer/series functionality
    d1$layer(
      x = "date",
      y = "psavert",
      data = NULL,
      type = "line"
    )
    d1
    
    #plot$save("d1.html")
  })
  
  output$myChart <- renderChart2({
    #selection <-  data[data$mapping == "0-4",]
    #selection <- data[data$mapping == input$agegrp,]
    selection <- ff
    
    #d3.timeParse("%Y-%m-%d %H:%M:%S")
    
    #selection <- subset(data,mapping == input$agegrp)
    
    plot <- nPlot(ldavg15 ~ timestamp, 
                #group = 'variable', 
                data = fuf, 
                type = 'lineChart')
    
    plot$xAxis(
      #type = "addTimeAxis",
      #inputFormat = "%Y-%m-%d %H:%M:%S",
      #inputFormat = "#!function(d) {return d3.timeParse('%Y-%m-%d %H:%M:%S')(new Date( d * 86400000 ));}!#",
      axisLabel = 'Time'
      #outputFormat = "%H:%M:%S",
      #tickFormat="#!function(d) {return d3.time.format('%H:%M:%S')(new Date( d * 86400000 ));}!#"
    )
     
    #plot$xAxis( axisLabel = 'Time',tickFormat="#!function(d) {return d3.time.format('%H:%M:%S')(new Date( d * 86400000 ));}!#" )
    #try new interactive guidelines feature
    plot$yAxis(axisLabel = "Load avg.")
    #plot$xAxis(axisLabel = "Time")
    plot$chart(useInteractiveGuideline=TRUE)
    plot$print(include_assets=T)
    
    #plot <- nPlot(n ~ year,
    #              data = selection,
    #              type = "lineChart",
    #              group = "sex")
    ## Add axis labels and format the tooltip
    #plot$yAxis(axisLabel = "Load avg.", width = 62)
    #plot$xAxis(axisLabel = "Time")
    plot$set(width=1200, height=900) #probably pixel size
    
    plot$save("ac.html")
    plot
  })
}

shinyApp(ui = ui, server = server)