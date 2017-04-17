library('dplyr')
#library('magrittr')
library('shiny')
#library('shinyjs')

source("linechart.R")

# TODO: todays load averages in 3 layered graphs using f <- system("sadf -d -T /var/log/sysstat/sa10 -- -q | grep $(hostname) | grep -v RESTART", intern =  TRUE)
#       log mem each minute with free, to get the available column (not in sar since it's some algorithmic estimate) and use here
#       nics with i/o of each. In kb/s
#       also, disk-usage

ui <- fluidPage(
  fluidRow(
    column(width=9,
           lineChartOutput("loadavg")
    )
  ),
  fluidRow(
    column(width=9,
           lineChartOutput("mem")
    )
  )
)

server <- function(input, output, session) {
  
  # TODO: to make the server load charts most useful for figuring out what is eating your resources, you need to align them perfectly timewise
  #       i.e the ticks of the x / time axes need to align. Then you need to have cpu load / loadavg., mem usage, nic usage AND e.g. top 5 most
  #       active processes (cpu- and/or mem- and/or net-wise) at any given time. 
  #       Nemmest måde, at synkronisere på er vel bare at køre bl.a. sar og free fra samme lognings-script fra cron (cron-fil?) og om nødvendigt
  #       sørge for at deres tidsstempel er ens (eller cirka ens vel. der skal vel bare være samme antal ticks right?)
  #       bør der ikke både være en graf med ANTAL tcp forbindelse og båndbredde pr. proces (bare top 5 måske), da begge dele vel kan være en indikator ifm.
  #       resource-hogging.

  # used and available seem to be each others oposite! Do a stacked barchart between used, available
  output$mem <- renderLineChart({
    
    free <- read.csv("/home/nemo/free.log",sep = ";")
    names(free) <- c("epoch","timestamp","total","used","free","shared","buffcache","available")
    #free['available'] <- as.integer(free['available'])
    avail <- select(free,1,4,5)  # If I only send in a 2-columned dataframe, the last frame that gets used in the binding-file is prbably turned to 
                                 # a list instead o f a dataframe thus causing an error
    avail
  })
    
  output$loadavg <- renderLineChart({
    
    f <- system("sadf -d -T /var/log/sysstat/ -- -q | grep $(hostname) | grep -v RESTART | sed s/\\,/\\./g", intern =  TRUE)
    f <- read.csv(text=f,sep = ";")
    names(f) <- c("hostname","interval","timestamp","runqsz","plistsz","ldavg1","ldavg5","ldavg15","blocked")
    f <- select(f,1,6:8)
    #rownames(f) <- f[, -1]
    f
  })
}

shinyApp(ui = ui, server = server)