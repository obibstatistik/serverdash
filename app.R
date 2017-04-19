library("R.utils")
library(shinydashboard)
library(dplyr)
library(foreach)
library(reshape2)
library(shiny)
library(rCharts)
library(data.table) 

source("functions.R")


ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Shiny-server"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Server info", tabName = "info", icon = icon("server", lib="font-awesome")),
      menuItem("Brugere", tabName = "users", icon = icon("user-o", lib="font-awesome")),
      menuItem("Netværk", tabName = "network", icon = icon("sitemap", lib="font-awesome")),
      menuItem("Shiny app logs", tabName = "applogs", icon = icon("file-text-o", lib="font-awesome")),
      menuItem("Shiny og R pakker", tabName = "packages", icon = icon("gift", lib="font-awesome")),
      menuItem("System statistik", tabName = "sysstat", icon = icon("line-chart", lib="font-awesome"))
    )
  ),  
  
  dashboardBody(   
    
    tabItems(
      
      tabItem(tabName = "network",
              fluidPage(
                fluidRow(
                  column(12,headerPanel("Aktive TCP forbindelser"),wellPanel(verbatimTextOutput("lsof")))
                )
              )
      ),
      tabItem(tabName = "packages",
              fluidPage(
                fluidRow(
                  column(6,headerPanel("Shiny og R pakker"),wellPanel(tableOutput("packages1"))),
                  column(6,wellPanel(tableOutput("packages2")))
                )
              )
      ),      
      tabItem(tabName = "info",
              fluidPage(
                fluidRow(
                  column(6,
                         headerPanel("Oppetid"),wellPanel(verbatimTextOutput("uptime")),
                         headerPanel("Nginx version"),wellPanel(verbatimTextOutput("nginxver"))
                  ),
                  column(6,headerPanel("Partitioner"),wellPanel(verbatimTextOutput("df"))),
                  column(6,headerPanel("Ubuntu version"),wellPanel(verbatimTextOutput("lsb"))),
                  column(6,headerPanel("RAM og swap"),wellPanel(verbatimTextOutput("mem"))),
                  column(6,headerPanel("Shiny-server version"),wellPanel(verbatimTextOutput("shinysrv"))),
                  column(6,headerPanel("R version"),wellPanel(verbatimTextOutput("rversion")))
                )
              )
      ),     
      tabItem(tabName = "applogs",
                fluidPage(
                  tags$style(type = "text/css", ".content-wrapper {height: 1700px}"),
                  uiOutput("logs")
                )
      ),
      tabItem(tabName = "users",
              fluidPage(
                fluidRow(
                  column(6,
                         headerPanel("Linux Brugere lige nu"),
                         wellPanel(
                           verbatimTextOutput("w")
                         )
                  ),
                  column(6,
                         headerPanel("Seneste Linux logins"),
                           wellPanel(
                             verbatimTextOutput("last")
                           )
                  )
                )
              )
      ),
      tabItem(tabName = "sysstat",
              fluidPage(
                fluidRow(
                  column(width=1,
                         checkboxGroupInput("loadgroup", "Load averages:",
                                            choices = c("ldavg1","ldavg5","ldavg15"),
                                            selected = c("ldavg1")
                         )
                  ),
                  column(width=11,
                         showOutput("loadchart", "nvd3")
                  )
                ),
                fluidRow(
                  column(width=1,
                         checkboxGroupInput("memgroup", "Memory use:",  
                                            choices = c("total","used","free","shared","buffcache","available"),
                                            selected = c("available")
                         )
                  ),
                  column(width=11,
                         showOutput("memchart", "nvd3")
                  )
                ),
                fluidRow(
                  column(width=1,
                         checkboxGroupInput("netgroup", "Network use:",  
                                            choices = c("tcpcount"),
                                            selected = c("tcpcount")
                         )
                  ),
                  column(width=11,
                         showOutput("netchart", "nvd3")
                  )
                ),
                fluidRow(
                  column(width=1,
                         checkboxGroupInput("top5group", "Top 5 processes:", 
                                            choices = c("top1proc","top2proc","top3proc","top4proc","top5proc"),
                                            selected = c("top1proc")
                         )
                  ),
                  column(width=11,
                         showOutput("top5chart", "nvd3")
                  )
                )
              )
      )
    )
  )
)


############################## SERVER #################################
server <- shinyServer(function(input, output, session) {
  
  allDirs <- list.dirs(path = "..", recursive=FALSE)
  
  dirs <- unlist(lapply(allDirs, function(dir) {
    if (file.exists(paste0(dir,"/readme.md"))) {
      gsub(".*/", "", dir)
    }
    else NULL
  }))
  
  len <- length(dirs)
  
  output$logs <- renderUI({
    plot_output_list <- lapply(1:len, function(i) {
      plotname <- paste0(dirs[i])
        fluidRow (
          column(12,
            wellPanel(
              headerPanel(plotname),
              htmlOutput(plotname)
            )
          )
        )
    })
    tagList(plot_output_list)
  })
 
  logdir <- "/var/log/shiny-server/"
  
  foreach(dir=dirs) %do% {
    vaffel <- system( paste0("ls -tr ",logdir,dir,"* | tail -1" ), intern = TRUE)
    plotname <- paste0(dir)
    cat(vaffel)
    output[[plotname]] <- logTail(session,paste0(vaffel), 10, 2000)
  }
  
  # get the version and depends columns of installed.packages() into a dataframe
  instpacks <- as.data.frame(installed.packages(),include.rownames=FALSE)
  
  # order the dataframe alphabetically by it's row names
  instpacks <- instpacks[ order(row.names(instpacks)), ]
  
  instpacks <- instpacks %>% distinct(Package,Version,Depends)
  
  # split the dataframe into two for aesthetic reasons
  firsthalf <- instpacks[1:nrow(instpacks) / 2,]
  secondhalf <- instpacks[nrow(instpacks) / 2 + 1:nrow(instpacks),]
  
  # TODO run some of these after others and after initial page rendering or something might hang?
  output$last <- runCmd(session,"last",10,5000)
  output$uptime <- runCmd(session,"uptime","all",1000)
  output$mem <- runCmd(session,"free",6,1000)
  output$w <- runCmd(session,"w","all",5000)
  output$lsof <- runCmd(session,"lsof -i tcp","all",5000)
  output$df <- runCmd(session,"df -h","all",5000)
  output$lsb <- runCmd(session,"lsb_release -a 2>/dev/null",6,50000)
  output$nginxver <- runCmd(session,"/usr/sbin/nginx -v 2>&1","all",50000)
  output$shinysrv <- runCmd(session,"apt-cache showpkg shiny-server","all",50000)
  output$packages1 <- renderTable(firsthalf,include.rownames=FALSE)
  output$packages2 <- renderTable(secondhalf,include.rownames=FALSE)
  output$rversion <- renderPrint(R.version)
  
  ############ BELOW HERE IS ALL SYSSTATS CHARTS ###############
  
  serverdata <- read.csv("/home/nemo/serverstats.log",sep=';')
  
  names(serverdata) <- c("datetime","ldavg1","ldavg5","ldavg15","total","used","free","shared","buffcache","available","top1proc","top1cpu","top2proc","top2cpu","top3proc","top3cpu","top4proc","top4cpu","top5proc","top5cpu","tcpcount")
  
  loadavg <- select(serverdata,1:4)
  mem <- select(serverdata,1,5:10)
  top5 <- select(serverdata,1,11:20)
  net <- select(serverdata,1,21)
  
  # split the load averages out to their own rows, so we can use the new column as a group var
  # See: http://stackoverflow.com/questions/20464107/how-to-convert-multiple-columns-to-individual-rows-in-r
  loadavg <- melt(loadavg, id.vars="datetime")
  mem <- melt(mem, id.vars="datetime")
  
  #names(loadavg) <- c("datetime","loadavgtype","value")
  
  # We need to reshape top5 dataframe to get the correct group variables  
  top5a <- select(top5,1,2,3)
  top5a <- melt(top5a,id.vars=c("datetime","top1cpu"),measure.vars = c("top1proc"))
  top5b <- select(top5,1,4,5)
  top5b <- melt(top5b,id.vars=c("datetime","top2cpu"),measure.vars = c("top2proc"))
  top5c <- select(top5,1,6,7)
  top5c <- melt(top5c,id.vars=c("datetime","top3cpu"),measure.vars = c("top3proc"))
  top5d <- select(top5,1,8,9)
  top5d <- melt(top5d,id.vars=c("datetime","top4cpu"),measure.vars = c("top4proc"))
  top5e <- select(top5,1,10,11)
  top5e <- melt(top5e,id.vars=c("datetime","top5cpu"),measure.vars = c("top5proc"))
  top5  <- rbindlist(list(top5a,top5b,top5c,top5d,top5e))
  
  # Create som reactive inputs to filter rows/graphlines based on group variables
  loadInput <- reactive({
    loadavg %>% filter(variable == input$loadgroup)
  })
  memInput <- reactive({
    mem %>% filter(variable == input$memgroup)
  })
  netInput <- reactive({
    net %>% filter(variable == input$netgroup)
  })
  top5Input <- reactive({
    top5 %>% filter(variable == input$top5group)
  })
  
  # Load averages chart with 1,5,15 minute lines
  output$loadchart <- renderChart2({
    
    loadavg_plot <- nPlot(value ~ datetime, 
                          data = loadInput(),
                          group = "variable",
                          height = 400,
                          width = 1400,
                          type = "lineChart")
    
    loadavg_plot$xAxis(axisLabel = 'Klokkeslæt',
                       rotateLabels = -45,
                       tickFormat="#!function(d) {return d3.time.format('%H:%M:%S')(new Date( d ));}!#")
    
    loadavg_plot$yAxis(axisLabel = 'Load average')
    
    loadavg_plot$chart(useInteractiveGuideline=TRUE,
                       margin=list(top = 30, right = 20, bottom = 100, left = 100)
    )
    
    loadavg_plot
  })
  
  # Memory usage chart
  output$memchart <- renderChart2({
    
    memchart_plot <- nPlot(value ~ datetime, 
                           data = memInput(),
                           group = "variable",
                           height = 400,
                           width = 1400,
                           type = "lineChart")
    
    memchart_plot$xAxis(axisLabel = 'Klokkeslæt',
                        rotateLabels = -45,
                        tickFormat="#!function(d) {return d3.time.format('%H:%M:%S')(new Date( d ));}!#")
    
    memchart_plot$yAxis(axisLabel = 'Kilobytes')
    
    memchart_plot$chart(useInteractiveGuideline=TRUE,
                        margin=list(top = 30, right= 20, bottom= 100, left= 100)
    )
    
    memchart_plot
  })
  
  # Network traffic chart with number of tcp connections. TODO add some throughput
  output$netchart <- renderChart2({
    
    net_plot <- nPlot(tcpcount ~ datetime, 
                      data = net,
                      height = 400,
                      width = 1400,
                      type = "lineChart")
    
    net_plot$xAxis(axisLabel = 'Klokkeslæt',
                   rotateLabels = -45,
                   tickFormat="#!function(d) {return d3.time.format('%H:%M:%S')(new Date( d ));}!#")
    
    net_plot$yAxis(axisLabel = 'Antal TCP forbindelser')
    
    net_plot$chart(useInteractiveGuideline=FALSE,
                   margin=list(top = 30, right= 20, bottom= 100, left= 100)
    )
    
    net_plot
  })
  
  # Top5 processes chart with a line for each process and the amplitude equal to percent of cpu time 
  output$top5chart <- renderChart2({
    
    top5_plot <- nPlot(top1cpu ~ datetime, 
                       data = top5Input(),
                       #data = top5,
                       group = "variable",
                       height = 400,
                       width = 1400,
                       type = "lineChart")
    
    top5_plot$xAxis(axisLabel = 'Klokkeslæt',
                    rotateLabels = -45,
                    tickFormat="#!function(d) {return d3.time.format('%H:%M:%S')(new Date( d ));}!#")
    
    top5_plot$yAxis(axisLabel = '% af CPU tid')
    
    top5_plot$chart(useInteractiveGuideline=FALSE,
                    margin=list(top= 30, right= 20, bottom= 100, left= 100),
                    tooltipContent = "#! function(key, x, y, e){ 
                                      return '<p>' + key + ' ' + e.point.datetime + '</p>' +
                                      '<p>' + e.point.value + '</p>'
                                      } !#"
    )
    
    top5_plot
  })
  
  # Custom tooltip setup. See: http://stackoverflow.com/questions/17524227/rcharts-rnvd3-tooltip-customisation
  #                            http://stackoverflow.com/questions/23686633/rcharts-nvd3-custom-tooltip
  #                            https://github.com/ramnathv/rCharts/issues/430
  
})

shinyApp(ui = ui, server = server)                    
