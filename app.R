library("R.utils")
library(shinydashboard)
library(dplyr)
library(foreach)

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
      menuItem("Shiny og R pakker", tabName = "packages", icon = icon("gift", lib="font-awesome"))
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
                  column(6,headerPanel("RAM og swap"),wellPanel(verbatimTextOutput("mem"))),
                  column(6,headerPanel("Ubuntu version"),wellPanel(verbatimTextOutput("lsb"))),
                  column(6,headerPanel("Shiny-server version"),wellPanel(verbatimTextOutput("shinysrv"))),
                  column(6,headerPanel("R version"),wellPanel(verbatimTextOutput("rversion")))
                )
              )
      ),     
      tabItem(tabName = "applogs",
              mainPanel(
                fluidPage(
                  tags$style(type = "text/css", ".content-wrapper {height: 1700px}"),
                  uiOutput("logs")
                )
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
                ))))
  ))

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
    
    # flowLayout for the dynamically created elements. See:
    # http://stackoverflow.com/questions/39067679/how-do-i-make-dynamically-created-inputs-in-r-shiny-flow-like-normal-inputs-in-f
    plot_output_list <- lapply(1:len, function(i) {
      plotname <- paste0(dirs[i])
      UI <- paste0("column(4,
                   wellPanel(
                   headerPanel("," plotname ", "),
                   htmlOutput("," plotname ", ")
                   )
      )",
                   collapse = ", ")
      #print(UI)
      eval(parse(text = UI))
    })
    
    tagList(plot_output_list)
    
  })
  
  logdir <- "/var/log/shiny-server/"
  
  foreach(dir=dirs) %do% {
    vaffel <- system( paste0("ls -tr ",logdir,dir,"* | tail -1" ), intern = TRUE)
    plotname <- paste0(dir)
    cat(vaffel)
    output[[plotname]] <- logTail(session,paste0(vaffel), 20, 2000)
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
  output$nginxver <- runCmd(session,"nginx -v","all",50000)
  output$shinysrv <- runCmd(session,"apt-cache showpkg shiny-server","all",50000)
  output$packages1 <- renderTable(firsthalf,include.rownames=FALSE)
  output$packages2 <- renderTable(secondhalf,include.rownames=FALSE)
  output$rversion <- renderPrint(R.version)
  
})

shinyApp(ui = ui, server = server)                    