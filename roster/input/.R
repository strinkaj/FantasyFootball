
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

d1 <- 
  p_future[
    avg_type == "weighted"
    & week == current_week + 1
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

d2 <- 
  p_future[
    avg_type == "weighted"
    & week == current_week + 2
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

d[ 
  ,score := 
    (
    0.85^(week - current_week)*
    ceiling^(6/10)*
    points^(3/10)*
    floor^(1/10)
    ) 
]

d[ ,play_disc := (0.85^(week - current_week)*play) ]

d[ ,ceiling_disc := (0.85^(week - current_week)*ceiling) ]

d[ ,floor_disc := (0.85^(week - current_week)*floor) ]

d0[ ,score := ceiling^(3/10)*floor^(7/10)]

d1[ ,score := ceiling^(6/10)*points^(3/10)*floor^(1/10)]

d <-
  d[
    ,.(
      play = sum(play)
      ,floor = sum(floor_disc)/sum(play_disc)
      ,score = sum(score)/sum(play_disc)
      ,ceiling = sum(ceiling_disc)/sum(play_disc)
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

d1[,drafted:=0]

d1[,dp:=0]

d1[
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

one_disc_point <- sum(0.85^seq(0,length(weeks_to_end)-1))/length(weeks_to_end)

wr_avail <- 
  d[
    drafted == 0 & 
    position == "WR"
  ,][
    order(-score)
  ,]

n <- 
  wr_avail[
    round(score,0) - one_disc_point  > 
    (d[
      dp == 8 & position == "WR",
    ][
      order(score),
    ][
      1, score
    ]
    ) 
    ,.N 
  ]

if (n == 0){

  print("WRs- all set")

} else {    
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
      dp == 8 & 
      position == "WR" & 
      score + one_disc_point < 
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
  
}

View(
  d[
    (team != "FA")
    &(drafted == 0|dp == 8)
    &(position == "WR")
  ,
  ][
    order(-score),
  ][
    1:15
    ,.(
    dp
    ,first_name
    ,last_name
    ,team
    ,position
    ,"floor" = round(floor, 2)
    ,"score" = round(score, 2)
    ,"ceiling" = round(ceiling, 2)
    )
  ]
)

View(
  d0[
    (team != "FA")
    &(drafted == 0|dp == 8)
    &(position == "WR")
  ,
  ][
    order(-score),
  ][
    1:15
    ,.(
    dp
    ,first_name
    ,last_name
    ,team
    ,position
    ,"floor" = round(floor, 2)
    ,"score" = round(score, 2)
    ,"ceiling" = round(ceiling, 2)
    )
  ]
)

View(
  d1[
    (team != "FA")
    &(drafted == 0|dp == 8)
    &(position == "WR")
  ,
  ][
    order(-score),
  ][
    1:15
    ,.(
    dp
    ,first_name
    ,last_name
    ,team
    ,position
    ,"floor" = round(floor, 2)
    ,"score" = round(score, 2)
    ,"ceiling" = round(ceiling, 2)
    )
  ]
)



rb_avail <- 
  d[
    drafted == 0 & 
    position == "RB"
  ,][
    order(-score)
  ,]

n <- 
  rb_avail[
    round(score,0) - one_disc_point  > 
    (d[
      dp == 8 & position == "RB",
    ][
      order(score),
    ][
      1, score
    ]
    ) 
    ,.N 
  ]

if (n == 0){

  print("RBs- all set")

} else {    
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
      dp == 8 & 
      position == "RB" & 
      score + one_disc_point < 
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
  
}


View(
  d[
    (team != "FA")
    &(drafted == 0|dp == 8)
    &(position == "RB")
  ,
  ][
    order(-score),
  ][
    1:15
    ,.(
    dp
    ,first_name
    ,last_name
    ,team
    ,position
    ,"floor" = round(floor, 2)
    ,"score" = round(score, 2)
    ,"ceiling" = round(ceiling, 2)
    )
  ]
)

View(
  d0[
    (team != "FA")
    &(drafted == 0|dp == 8)
    &(position == "RB")
  ,
  ][
    order(-score),
  ][
    1:15
    ,.(
    dp
    ,first_name
    ,last_name
    ,team
    ,position
    ,"floor" = round(floor, 2)
    ,"score" = round(score, 2)
    ,"ceiling" = round(ceiling, 2)
    )
  ]
)

View(
  d[
    (team != "FA")
    &(drafted == 0|dp == 8)
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
      ,"floor" = round(floor, 2)
      ,"score" = round(score, 2)
      ,"ceiling" = round(ceiling, 2)
    )
  ]
)

View(
  d0[
    (team != "FA")
    &(drafted == 0|dp == 8)
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
      ,"floor" = round(floor, 2)
      ,"score" = round(score, 2)
      ,"ceiling" = round(ceiling, 2)
    )
  ]
)


View(
  d0[
    (team != "FA")
    &(drafted == 0|dp == 8)
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
      ,"floor" = round(floor, 2)
      ,"points" = round(points, 2)
      ,"score" = round(score, 2)
      ,"ceiling" = round(ceiling, 2)
    )
  ]
)

View(
  d1[
    (team != "FA")
    &(drafted == 0|dp == 8)
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
      ,"floor" = round(floor, 2)
      ,"points" = round(points, 2)
      ,"score" = round(score, 2)
      ,"ceiling" = round(ceiling, 2)
    )
  ]
)

View(
  d1[
    (team != "FA")
    &(drafted == 0|dp == 8)
    &(position == "QB")
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
      ,"floor" = round(floor, 2)
      ,"points" = round(points, 2)
      ,"score" = round(score, 2)
      ,"ceiling" = round(ceiling, 2)
    )
  ]
)


View(
  d0[
    (team != "FA")
    &(dp == 8|id =="14104")  ,
  ][
    order(-score),
  ][
    1:20
    ,.(
      dp
      ,id
      ,first_name
      ,last_name
      ,team
      ,position
      ,"floor" = round(floor, 2)
      ,"points" = round(points, 2)
      ,"score" = round(score, 2)
      ,"ceiling" = round(ceiling, 2)
    )
  ]
)

