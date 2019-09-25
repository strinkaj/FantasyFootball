
## --
## roster - free agents and waiver wire
##
## --

# global ----
library(rhandsontable)
library(shiny)
library(data.table)

# data load ----

load(file = "../../input/.RData")

# check current week

current_week

weeks_to_end

d <- 
  p_future[
    avg_type == "weighted"
    ,.(
    week
    ,id
    ,first_name
    ,last_name
    ,team
    ,position
    ,floor
    ,points
    ,ceiling
    )
  ]

d0 <- 
  p_current[
    avg_type == "weighted"
    ,.(
    id
    ,first_name
    ,last_name
    ,team
    ,position
    ,floor
    ,points
    ,ceiling
    )
  ]

d[,play := 0]

d[floor > 0,play := 1]

d[ ,score := 0.85^(week - current_week)*ceiling^(6/12)*points^(4/12)*floor^(2/12)]

d0[ ,score := ceiling^(6/12)*points^(4/12)*floor^(2/12)]

d <-
  d[
    ,.(
      play = sum(play)
      ,floor = sum(floor)
      ,points = sum(points)
      ,score = sum(score)/sum(play)
      ,ceiling = sum(ceiling)
      )
    ,by =
      .(
      id
      ,first_name
      ,last_name
      ,team
      ,position
      ) 
    ]

d0 <-
  d0[
    ,.(
      floor = sum(floor)
      ,points = sum(points)
      ,score = sum(score)
      ,ceiling = sum(ceiling)
      )
    ,by =
      .(
      id
      ,first_name
      ,last_name
      ,team
      ,position
      ) 
    ]

# roster updates

input <- readRDS("current.rds")


  editTable <- function(DF, outdir=getwd(), outfilename="table"){
  
    ui <- shinyUI(fluidPage(
  
      titlePanel("Current Roster"),
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
            ,source = unique(d$first_name)
            ,strict = TRUE
          ) %>%
          hot_col(
            col = "last_name"
            ,type = "autocomplete"
            ,source = unique(d$last_name)
            ,strict = TRUE
          ) %>%
          hot_col(
            col = "position"
            ,type = "autocomplete"
            ,source = unique(d$position)
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
  

  editTable(input,outfilename = "current")

  
#

input <- readRDS("current.rds")


#


d[,drafted:=0]

d[,dp:=0]

d[
  input
  ,`:=`(
    drafted = 1
    ,dp = as.numeric(i.dp)
  )
  ,on = 
    .(
    first_name
    ,last_name
    ,position
  )
]

d0[,drafted:=0]

d0[,dp:=0]

d0[
  input
  ,`:=`(
    drafted = 1
    ,dp = as.numeric(i.dp)
  )
  ,on = 
    .(
    first_name
    ,last_name
    ,position
  )
]

wr_avail <- 
  d[drafted == 0 & position == "WR",
  ][order(-score),]

n <- 
  wr_avail[
    (round(score,0) - sum(1*0.85^c(weeks_to_end)/length(weeks_to_end))  > 
       (d[
          dp == 2
          & position == "WR"
        ,][order(score)
        ,][1,score]
        ) 
    ),.N ]

for (i in 1:n){
  
print(
  cat(
    "\t \t \t \t \t "
    ,unlist(
      wr_avail[i,.(first_name,last_name,round(score,1))]
    )
  )
)

print(
  d[
    dp == 2 & position == "WR" & score + sum(1*0.85^c(weeks_to_end))/play < 
      wr_avail[
        i,round(score,1)],
      ][order(score),
      ][1:i, 
        .(
          first_name
          ,last_name
          ,"score" = round(score,1)
        )
      ][!is.na(first_name),]
)

}

rb_avail <- 
  d[drafted == 0 & position == "RB",
  ][order(-score),]

n <- 
  rb_avail[
    (round(score,0) - sum(1*0.85^c(weeks_to_end)/length(weeks_to_end))  > 
       (d[
          dp == 2
          & position == "RB"
        ,][order(score)
        ,][1,score]
        ) 
    ),.N ]

for (i in 1:n){
  
print(
  cat(
    "\t \t \t \t \t "
    ,unlist(
      rb_avail[i,.(first_name,last_name,round(score,1))]
    )
  )
)

print(
  d[
    dp == 2 & position == "RB" & score + sum(1*0.85^c(weeks_to_end))/play < 
      rb_avail[
        i,round(score,1)],
      ][order(score),
      ][1:i, 
        .(
          first_name
          ,last_name
          ,"score" = round(score,1)
        )
      ][!is.na(first_name),]
)

}

te_avail <- 
  d[drafted == 0 & position == "TE",
  ][order(-score),]

n <- 
  te_avail[
    (round(score,0) - sum(1*0.85^c(weeks_to_end)/length(weeks_to_end))  > 
       (d[
          dp == 2
          & position == "TE"
        ,][order(score)
        ,][1,score]
        ) 
    ),.N ]

for (i in 1:n){
  
print(
  cat(
    "\t \t \t \t \t "
    ,unlist(
      te_avail[i,.(first_name,last_name,round(score,1))]
    )
  )
)

print(
  d[
    dp == 2 & position == "TE" & score + sum(1*0.85^c(weeks_to_end))/play < 
      te_avail[
        i,round(score,1)],
      ][order(score),
      ][1:i, 
        .(
          first_name
          ,last_name
          ,"score" = round(score,1)
        )
      ][!is.na(first_name),]
)

}


View(
d[
(team != "FA")
&(drafted == 0|dp == 2)
&(position == "WR")
,
][
order(-score),
][
1:35
,.(
  dp
  ,first_name
  ,last_name
  ,play
  ,team
  ,position
  ,"floor" = round(floor,0)
  ,"points" = round(points,0)
  ,"score" = round(score,2)
  ,"ceiling" = round(ceiling,0)
)
]
)

View(
d0[
(team != "FA")
&(drafted == 0|dp == 2)
&(position == "WR")
,
][
order(-score),
][
1:35
,.(
  dp
  ,first_name
  ,last_name
  ,team
  ,position
  ,"floor" = round(floor,0)
  ,"points" = round(points,0)
  ,"score" = round(score,1)
  ,"ceiling" = round(ceiling,0)
)
]
)

View(
d[
(team != "FA")
&(drafted == 0|dp == 2)
&(position == "RB")
,
][
order(-score),
][
1:10
,.(
  dp
  ,first_name
  ,last_name
  ,play
  ,team
  ,position
  ,"floor" = round(floor,0)
  ,"points" = round(points,0)
  ,"score" = round(score,2)
  ,"ceiling" = round(ceiling,0)
)
]
)

View(
d0[
(team != "FA")
&(drafted == 0|dp == 2)
&(position == "RB")
,
][
order(-score),
][
1:10
,.(
  dp
  ,first_name
  ,last_name
  ,team
  ,position
  ,"floor" = round(floor,0)
  ,"points" = round(points,0)
  ,"score" = round(score,1)
  ,"ceiling" = round(ceiling,0)
)
]
)

View(
d[
(team != "FA")
&(drafted == 0|dp == 2)
&(position == "TE")
,
][
order(-score),
][
1:10
,.(
  dp
  ,first_name
  ,last_name
  ,team
  ,position
  ,"floor" = round(floor,0)
  ,"points" = round(points,0)
  ,"score" = round(score,2)
  ,"ceiling" = round(ceiling,0)
)
]
)

View(
d0[
(team != "FA")
&(drafted == 0|dp == 2)
&(position == "TE")
,
][
order(-score),
][
1:10
,.(
  dp
  ,first_name
  ,last_name
  ,team
  ,position
  ,"floor" = round(floor,0)
  ,"points" = round(points,0)
  ,"score" = round(score,0)
  ,"ceiling" = round(ceiling,0)
)
]
)

View(
d0[
(team != "FA")
&(drafted == 0|dp == 2)
&(position == "DST")
,
][
order(-score),
][
1:20
,.(
  dp
  ,first_name
  ,last_name
  ,team
  ,position
  ,"floor" = round(floor,2)
  ,"points" = round(points,2)
  ,"score" = round(score,2)
  ,"ceiling" = round(ceiling,2)
)
]
)

