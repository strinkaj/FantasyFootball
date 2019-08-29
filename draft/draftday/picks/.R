
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

# no Free Agents
p <- p[team != "FA",]
p <- p[team != "FA*",]
p <- p[last_name != "Luck",]

  
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

# pick #2 ROUND 1

pos_accum <- update_draft()

pos_accum <- c("RB" = 0,"QB" = 0,"WR" = 0,"TE" = 0,"DST" = 0)

draft_pick(2)

# pick #23 ROUND 2

pos_accum <- update_draft()

draft_pick()

# pick #26 ROUND 3

pos_accum <- update_draft()

draft_pick(26)

# pick #47 ROUND 4

pos_accum <- update_draft()

draft_pick(47)

# pick #50 ROUND 5

pos_accum <- update_draft()

draft_pick(50)

# pick #71 ROUND 6

pos_accum <- update_draft()

draft_pick(71)

# pick #74 ROUND 7

pos_accum <- update_draft()

draft_pick(74)

# pick #95 ROUND 8

pos_accum <- update_draft()

draft_pick(95)

# pick #98 ROUND 9

pos_accum <- update_draft()

draft_pick(98)

# pick #119 ROUND 10

pos_accum <- update_draft()

draft_pick(119)

# pick #122 ROUND 11

pos_accum <- update_draft()

draft_pick(122)

# pick #143 ROUND 12

pos_accum <- update_draft()

draft_pick(143)

# pick #146 ROUND 13

pos_accum <- update_draft()

draft_pick(146)

# pick #167 ROUND 14

pos_accum <- update_draft()

draft_pick(167)

# pick #170 ROUND 15

pos_accum <- update_draft()

draft_pick(170)
