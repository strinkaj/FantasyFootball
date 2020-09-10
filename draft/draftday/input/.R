  
  ## --
  ## draft day data input app
  ##
  ## --
  
  # global ----
  
  library(rhandsontable)
  library(shiny)
  library(data.table)
  
  source("../.R")
  
  # data load ----
  
  load(file = "../../office.RData")
  
  # data entry dashboard ---
  
  editTable <- function(DF, outdir=getwd(), outfilename="table"){
  
    ui <- shinyUI(fluidPage(
  
      titlePanel("Draft Pick Entries"),
      sidebarLayout(
        sidebarPanel(
          helpText("Enter and Save"),
          br(), 
  
          wellPanel(
            h3("Save"), 
            actionButton("save", "Save table")
          )        
  
        ),
  
        mainPanel(
  
          rHandsontableOutput("hot")
  
        )
      )
    ))
  
    server <- shinyServer(function(input, output) {
  
      values <- reactiveValues()
  
      ## Handsontable
      observe({
        if (!is.null(input$hot)) {
          DF = hot_to_r(input$hot)
        } else {
          if (is.null(values[["DF"]]))
            DF <- DF
          else
            DF <- values[["DF"]]
        }
        values[["DF"]] <- DF
      })
  
      output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF))
          rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
          hot_col(
            col = "first_name"
            ,type = "autocomplete"
            ,source = unique(p$first_name)
            ,strict = TRUE
          ) %>%
          hot_col(
            col = "last_name"
            ,type = "autocomplete"
            ,source = unique(p$last_name)
            ,strict = TRUE
          ) %>%
          hot_col(
            col = "position"
            ,type = "autocomplete"
            ,source = unique(p$position)
            ,strict = TRUE
          )
      })
  
      ## Save 
      observeEvent(input$save, {
        finalDF <- isolate(values[["DF"]])
        saveRDS(
          finalDF
          ,file = file.path(outdir, sprintf("%s.rds", outfilename)))
      })
  
    })
  
    ## run app 
    runApp(list(ui=ui, server=server))
    return(invisible())
  }
  
  draft_input <- 
    data.table(
      cbind(
        "pick" = seq(1,192)
        ,"dp" = rep(c(seq(1,10),rev(seq(1,10))),8)
        ,"first_name" = ""
        ,"last_name" = ""
        ,"position" = ""
      )
    )
  
  for(i in 1:180){
  draft_input[pick == i,dp := which(snake ==i,arr.ind = TRUE)[1] ]
  }
    
  editTable(draft_input,outdir = "../picks/",outfilename = "currentoffice")

