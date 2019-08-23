
## --
## simulated drafts for testing
##
## --


# global ----

library(data.table)

source("../.R")

# data load ----

load(file = "../.RData")

# test - the adp draft ----

# other draft positions pick based on adp

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

# ___ pick 1 ----

draft[1,drafted:=1]

draft[1,pick := 1]

draft[pick == 1, dp := which(snake == 1,arr.ind = TRUE)[,1]]

pos_accum <- c("RB" = 0,"QB" = 0,"WR" = 0,"TE" = 0,"DST" = 0)

snake[2,]

# ___ pick 2 ----

draft[
  ,points_ra :=
    weights[[1]](2)*floor +
    weights[[2]](2)*points +
    weights[[3]](2)*ceiling +
    weights[[4]](2)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](2)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](2)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]


pos_choice(current_pick = 2,pos_accum = pos_accum,pos_best = pos_best)
#RB 
choice <- "RB"


pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 2
  ,dp = 2
  )
]

# ___ picks 3-22 ----

for(i in 3:22){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 23 ----

draft[
  ,points_ra :=
    weights[[1]](23)*floor +
    weights[[2]](23)*points +
    weights[[3]](23)*ceiling +
    weights[[4]](23)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](23)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](23)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 23,pos_accum = pos_accum,pos_best = pos_best)
#WR
choice <- "WR"


pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 23
  ,dp = 2
  )
]

# ___ picks 24-25 ----

for(i in 24:25){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 26 ----

draft[
  ,points_ra :=
    weights[[1]](26)*floor +
    weights[[2]](26)*points +
    weights[[3]](26)*ceiling +
    weights[[4]](26)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](26)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](26)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 26,pos_accum = pos_accum,pos_best = pos_best)
#TE
choice <- "TE"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 26
  ,dp = 2
  )
]

# ___ picks 27-46 ----

for(i in 27:46){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 47 ----

draft[
  ,points_ra :=
    weights[[1]](47)*floor +
    weights[[2]](47)*points +
    weights[[3]](47)*ceiling +
    weights[[4]](47)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](47)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](47)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 47,pos_accum = pos_accum,pos_best = pos_best)
#RB
choice <- "RB"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 47
  ,dp = 2
  )
]

# ___ picks 48-49 ----

for(i in 48:49){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  


# ___ pick 50 ----

draft[
  ,points_ra :=
    weights[[1]](50)*floor +
    weights[[2]](50)*points +
    weights[[3]](50)*ceiling +
    weights[[4]](50)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](50)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](50)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 50,pos_accum = pos_accum,pos_best = pos_best)
#RB
choice <- "RB"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 50
  ,dp = 2
  )
]

# ___ picks 51-70 ----

for(i in 51:70){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 71 ----

draft[
  ,points_ra :=
    weights[[1]](71)*floor +
    weights[[2]](71)*points +
    weights[[3]](71)*ceiling +
    weights[[4]](71)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](71)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](71)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 71,pos_accum = pos_accum,pos_best = pos_best)
#WR
choice <- "WR"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 71
  ,dp = 2
  )
]

# ___ picks 72:73 ----

for(i in 72:73){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 74 ----

draft[
  ,points_ra :=
    weights[[1]](74)*floor +
    weights[[2]](74)*points +
    weights[[3]](74)*ceiling +
    weights[[4]](74)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](74)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](74)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 74,pos_accum = pos_accum,pos_best = pos_best)
#WR
choice <- "WR"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 74
  ,dp = 2
  )
]

# ___ picks 75-94 ----

for(i in 75:94){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 95 ----

draft[
  ,points_ra :=
    weights[[1]](50)*floor +
    weights[[2]](50)*points +
    weights[[3]](50)*ceiling +
    weights[[4]](50)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](50)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](50)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 95,pos_accum = pos_accum,pos_best = pos_best)
#QB
choice <- "QB"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 95
  ,dp = 2
  )
]


# ___ picks 96-97 ----

for(i in 96:97){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 98 ----

draft[
  ,points_ra :=
    weights[[1]](98)*floor +
    weights[[2]](98)*points +
    weights[[3]](98)*ceiling +
    weights[[4]](98)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](98)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](98)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 98,pos_accum = pos_accum,pos_best = pos_best)
#TE
choice <- "TE"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 98
  ,dp = 2
  )
]

# ___ picks 99-121 ----

for(i in 99:121){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 122 ----

draft[
  ,points_ra :=
    weights[[1]](122)*floor +
    weights[[2]](122)*points +
    weights[[3]](122)*ceiling +
    weights[[4]](122)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](122)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](122)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 122,pos_accum = pos_accum,pos_best = pos_best)
#WR
choice <- "WR"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 122
  ,dp = 2
  )
]


# ___ picks 123-142 ----

for(i in 123:142){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 143 ----

draft[
  ,points_ra :=
    weights[[1]](143)*floor +
    weights[[2]](143)*points +
    weights[[3]](143)*ceiling +
    weights[[4]](143)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](143)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](143)*points_wk1
]

pos_best <- 
  draft[
    drafted == 0
    &!is.na(points_ra)
    ,.("max"= max(points_ra))
    ,by = position
  ]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 143,pos_accum = pos_accum,pos_best = pos_best)
#RB
choice <- "RB"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 143
  ,dp = 2
  )
]

# ___ picks 144-145 ----

for(i in 144:145){

fl <- 
  draft[
    drafted != 1
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# ___ pick 146 ----

draft[drafted != 1, ][order(-points_ra),][1:20,]

# Kirk Cousins

draft[
  first_name == "Kirk"
  &last_name == "Cousins"
  ,`:=`(
  drafted = 1
  ,pick = 146
  ,dp = 2
  )
]


# adp - how'd we do

draft[drafted ==1&dp!=2,sum(points)/11]

draft[dp==2,sum(points)]

draft[drafted ==1&dp!=2,sum(ceiling)/sum(points)]

draft[dp==2,sum(ceiling)/sum(points)]




# test - wr madness ----

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
    ,points_ra
    ,points
    ,ceiling
    )
  ]

draft[
  yahoo
  ,`:=`(adp = i.adp)
  ,on = "id"
]

draft <- draft[order(adp),]

draft[,drafted:=0]

# pick 1

fl <- 
  draft[
    position == "WR"
    ,.(first_name,last_name)
  ][1,  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 1
  ,dp =  1
  )
]


pos_accum <- c("RB" = 0,"QB" = 0,"WR" = 0,"TE" = 0,"DST" = 0)

snake[2,]


# pick 2

pos_best <- draft[drafted == 0&!is.na(points_ra),.("max"= max(points_ra)),by = position]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 2,pos_accum = pos_accum,pos_best = pos_best)
#RB
choice <- "RB"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 2
  ,dp = 2
  )
]

# picks 3-22

for(i in 3:22){

fl <- 
  draft[
    drafted != 1
    & position == "WR"
  ][order(-points),
  ][1
    ,.(first_name,last_name)
  ]
  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# pick 23

pos_best <- draft[drafted == 0&!is.na(points_ra),.("max"= max(points_ra)),by = position]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 23,pos_accum = pos_accum,pos_best = pos_best)
#RB
choice <- "RB"


pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 23
  ,dp = 2
  )
]

# picks 24-25

for(i in 24:25){

fl <- 
  draft[
    drafted != 1
    & position == "WR"
  ][order(-points),
  ][1
    ,.(first_name,last_name)
  ]

  
draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = i
  ,dp =  which(snake == i, arr.ind = T)[1]
  )
]

}  

# pick 26

pos_best <- draft[drafted == 0&!is.na(points_ra),.("max"= max(points_ra)),by = position]

pos_best[
  draft
  ,`:=`
  (first_name = i.first_name
  ,last_name = i.last_name)
  ,on = c(max = "points_ra")
]

pos_choice(current_pick = 26,pos_accum = pos_accum,pos_best = pos_best)
#RB
choice <- "RB"

pos_accum[choice] <- pos_accum[choice] + 1

fl <- 
  pos_best[
    position==choice
    ,.(first_name,last_name)
  ]

draft[
  first_name == fl[,1]
  &last_name == fl[,2]
  ,`:=`(
  drafted = 1
  ,pick = 26
  ,dp = 2
  )
]


