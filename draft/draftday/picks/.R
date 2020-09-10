
## --
## draft day picks
##
## --

# global ----

library(data.table)

source("../../.R")

# data load ----

load(file = "../../office.RData")

# set up draft data ----

# no Free Agents
p <- p[team != "FA",]
p <- p[team != "FA*",]
p <- p[last_name != "Luck",]

# first three weeks

p_future <- 
  p_future[
    avg_type == "weighted"
    ,
  ]

p_future <-
  dcast(
    p_future
    ,id ~ week
    ,value.var = "points"
    ,fun.aggregate = mean
  )

colnames(p_future) <- c("id","points_wk1","points_wk2","points_wk3")
  
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
    )
  ]

draft <- merge(draft,p_future, all.x = TRUE, by = "id")

draft[
  d_adp
  ,`:=`(adp = i.yahoo)
  ,on = "id"
]

draft <- draft[order(adp),]

draft[,drafted:=0]

draft[,pick:=0]

draft[,dp:=0]

# draft picks ----

# pick #8 ROUND 1

pos_accum <- update_draft()

pos_accum <- c("RB" = 0,"QB" = 0,"WR" = 0,"TE" = 0,"DST" = 0)

draft_pick(8)

# pick #14 ROUND 2

pos_accum <- update_draft()

draft_pick(13)

# pick #35 ROUND 3

pos_accum <- update_draft()

draft_pick(28)

# pick #38 ROUND 4

pos_accum <- update_draft()

draft_pick(33)

# pick #59 ROUND 5

pos_accum <- update_draft()

draft_pick(48)

# pick #62 ROUND 6

pos_accum <- update_draft()

draft_pick(53)

# pick #83 ROUND 7

pos_accum <- update_draft()

draft_pick(68)

# pick #86 ROUND 8

pos_accum <- update_draft()

draft_pick(73)

# pick #98 ROUND 9

pos_accum <- update_draft()

draft_pick(88)

# pick #119 ROUND 10

pos_accum <- update_draft()

draft_pick(93)

# pick #122 ROUND 11

pos_accum <- update_draft()

draft_pick(108)

# pick #143 ROUND 12

pos_accum <- update_draft()

draft_pick(113)

# pick #146 ROUND 13

pos_accum <- update_draft()

draft_pick(128)

# pick #167 ROUND 14

pos_accum <- update_draft()

draft_pick(133)

