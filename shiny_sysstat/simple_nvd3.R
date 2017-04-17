library('dplyr')
library('shiny')
library('ggplot2')
library('ecm')

library(rCharts)

#source("linechart.R")

# TODO: todays load averages in 3 layered graphs using f <- system("sadf -d -T /var/log/sysstat/sa10 -- -q | grep $(hostname) | grep -v RESTART", intern =  TRUE)
#       log mem each minute with free, to get the available column (not in sar since it's some algorithmic estimate) and use here
#       nics with i/o of each. In kb/s
#       also, disk-usage

ui <- fluidPage(
  fluidRow(
    column(width=9,
           showOutput("myChart", "nvd3")
          
    )
  )
)

server <- function(input, output) {
  
  #f <- system("sadf -d -T /var/log/sysstat/sa10 -- -q | grep $(hostname) | grep 21 | grep -v RESTART", intern =  TRUE)
  f <- system("sadf -d -T /var/log/sysstat/sa10 -- -q | grep $(hostname) | grep 21 | grep -v RESTART | sed s/\\,/\\./g", intern =  TRUE)
  fuf <- read.table(text=f,sep = ";")
  names(fuf) <- c("hostname","interval","timestamp","runqsz","plistsz","ldavg1","ldavg5","ldavg15","blocked")
  #fuf['timestamp'] <- apply(fuf['timestamp'], 1, function(timestamp) as.character(as.integer( as.POSIXct( timestamp ) , tz = "Europe/Copenhagen" ) * 1000 ))
  fuf['timestamp'] <- apply(fuf['timestamp'], 1, function(timestamp) as.integer( as.POSIXct( timestamp ) , tz = "Europe/Copenhagen" ) * 1000 )
  #fuf['ldavg1'] <- format(fuf['ldavg1'],decimal.mark=".")
  # as.numeric(gsub(",", ".", gsub("\\.", "", fuf['ldavg1'])))
  #fuf['timestamp'] <- as.character(fuf['timestamp'])
  #ff <- select(fuf,as.integer( as.POSIXct( as.Date(timestamp) ) , tz = "Europe/Copenhagen" ) * 1000)
  ff <- select(fuf,3,6:8)
  ff
  
  output$myChart <- renderChart2({

    ## {title: Pie Chart}
    p4 <- nPlot(~ cyl, data = mtcars, type = 'pieChart')
    p4
    
    ## {title: Line with Focus Chart }
    ecm <- reshape2::melt(economics[,c('date', 'uempmed', 'psavert')], id = 'date')
    p7 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineWithFocusChart')
    #test format dates on the xAxis
    #also good test of javascript functions as parameters
    #dates from R to JSON will come over as number of days since 1970-01-01
    #so convert to milliseconds 86400000 in a day and then format with d3
    #on lineWithFocusChart type xAxis will also set x2Axis unless it is specified
    p7$xAxis( tickFormat="#!function(d) {return d3.time.format('%H:%M:%S')(new Date( d * 86400000 ));}!#" )
    #test xAxis also sets x2Axis
    p7
    
    ## {title: InteractiveGuidline(Multi-Tooltips) on Line}
    p9 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineChart')
    p9$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
    #try new interactive guidelines feature
    p9$chart(useInteractiveGuideline=TRUE)
    p9
    
    cat(as.character(fuf['plistsz']))
    
    ## {title: InteractiveGuidline(Multi-Tooltips) on Line}
    p8 <- nPlot(ldavg1 ~ timestamp, data = fuf, type = 'lineChart')
    #p8$xAxis( tickFormat="#!function(d) {return d3.time.format( '%H:%M:%S')(new Date( d * 86400000 ));}!#" )
    #p8$yAxis(height=400)
    #p8$chart(forceY = c(0, 10))
    #try new interactive guidelines feature
    p8$chart(useInteractiveGuideline=TRUE)
    p8

    
  })
}

shinyApp(ui = ui, server = server)