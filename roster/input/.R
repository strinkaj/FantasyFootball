
## --
## roster - free agents and waiver wire
##
## --

# global ----

library(data.table)

# data load ----

load(file = "../../input/.RData")


d <- 
  p1[
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

input <- readRDS("../../draft/draftday/picks/current.rds")



p1[,drafted:=0]

p1[,pick:=0]

p1[,dp:=0]

p1[
  input
  ,`:=`(
    drafted = 1
    ,pick = as.numeric(i.pick)
    ,dp = as.numeric(i.dp)
  )
  ,on = 
    .(
    first_name
    ,last_name
    ,position
  )
]

p1[,pick:=0]

p1[,dp:=0]

p1[
  input
  ,`:=`(
    drafted = 1
    ,pick = as.numeric(i.pick)
    ,dp = as.numeric(i.dp)
  )
  ,on = 
    .(
    first_name
    ,last_name
    ,position
  )
]


p2[,drafted:=0]


p2[,pick:=0]

p2[,dp:=0]

p2[
  input
  ,`:=`(
    drafted = 1
    ,pick = as.numeric(i.pick)
    ,dp = as.numeric(i.dp)
  )
  ,on = 
    .(
    first_name
    ,last_name
    ,position
  )
]
