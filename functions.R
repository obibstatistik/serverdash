# include these functions in your app.R with source('functions.R')

# Function for running arbitrary shell command
runCmd <- function(session, shellcmd, lines2keep, millis) {
  if (millis != "none") autoInvalidate <- reactiveTimer(millis, session)
  out <- renderText({
    autoInvalidate()
    text <- system(shellcmd, intern = TRUE)
    if (lines2keep != "all") length(text) <- lines2keep 
    text[is.na(text)] <- ""
    paste(text, collapse = '\n')
  })
  return(out)
}
    
# Function for tailing arbitrary log
logTail <- function(session, logName, lines2keep, milis) {
  
  shellcmd <- paste(paste0("tail -",lines2keep),logName) # brug shell kommandoen tail m. lines2keep antal linjer
  
  authlogPoll <- reactivePoll(milis, session,
     # This function returns the time that the logfile was last
     # modified
      checkFunc = function() {
       if (file.exists(logName))
         file.info(logName)$mtime[1]
       else
         ""
     },
     # This function returns the content of the logfile
     valueFunc = function() {
       system(shellcmd, intern = TRUE) # intern er nødvendig ellers returneres cmd output ikke til shiny
     }
  )
  
  out <- renderText({
      # Read the text, and make it a consistent number of lines so that the output box doesn't grow in height.
      text <- authlogPoll()
      length(text) <- lines2keep 
      text[is.na(text)] <- ""
      paste(text, collapse = '\n')
  })
  return(out)
}
