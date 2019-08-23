
## --
## draft day picks
##
## --

# global ----

library(data.table)

source("../../.R")

# data load ----

load(file = "../../.RData")

# set up draft data ----
  
draft <- 
  p[
    avg_type == "weighted"
    ,.(
    id
    ,rank
    ,first_name
    ,last_name
    ,team
    ,position
    ,floor
    ,points
    ,ceiling
    ,points_wk1
    ,points_wk2
    ,points_wk3
    )
  ]

draft[
  yahoo
  ,`:=`(adp = i.adp)
  ,on = "id"
]

draft <- draft[order(adp),]

draft[,drafted:=0]

draft[,pick:=0]

draft[,dp:=0]

# draft picks ----

# pick #2

pos_accum <- update_draft()

pos_accum <- c("RB" = 0,"QB" = 0,"WR" = 0,"TE" = 0,"DST" = 0)

draft_pick(2)

# pick #23

pos_accum <- update_draft()

draft_pick(23)

# pick #26

pos_accum <- update_draft()

draft_pick(26)
