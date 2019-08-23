
## --
## draft functions and settings
##
## --


# global ----

library(data.table)


# snake draft matrix ----

snake <- 
  matrix(
    NA
    ,nrow = 12
    ,ncol = 15
  )

snake[,1] <- seq(1,12)

for (i in 2:15){
  
  if(i %% 2 == 0){
  snake[,i] <- 
    rev(
      seq(
        max(snake[,i-1]) + 1
        ,max(snake[,i-1]) + 12
      )
    )
  } else {
  snake[,i] <- 
    seq(
      max(snake[,i-1]) + 1,
      max(snake[,i-1]) + 12
    )
  }
  
}


# risk weighting ----

weights <-
  list(
  approxfun(
    x = c(1,45,10000)
    ,y = c(1,0,0)
  ) 
  ,approxfun(
    x = c(1,45,90,10000)
    ,y = c(0,1,0,0)
  )  
  ,approxfun(
    x = c(1,45,90,140,10000)
    ,y = c(0,0,1,0,0)
  )
  ,approxfun(
    x = c(1,90,140,150,10000)
    ,y = c(0,0,1,0,0)
  )
  ,approxfun(
    x = c(1,140,150,170,10000)
    ,y = c(0,0,1,0,0)
  ) 
  ,approxfun(
    x = c(1,150,170,10000)
    ,y = c(0,0,1,1)
  )
)


# starting likelihood by position ----

te <- 
  approxfun(
    x = c(1,2,3,200)
    ,y = 1.2*c(0.6,0.4,0,0)
  )

qb <- 
  approxfun(
    x = c(1,2,3,200)
    ,y = c(0.5,0.5,0,0)
  )

rb <- 
  approxfun(
    x = c(1,2,3,4,5,6,200)
    ,y = c(0.8,0.7,0.5,0.25,0.15,0,0)
  )

wr <- 
  approxfun(
    x = c(1,2,3,4,5,6,7,200)
    ,y = c(0.6,0.6,0.5,0.35,0.20,0.15,0,0)
  )

dst <- 
  approxfun(
    x = c(1,2,3,200)
    ,y = c(0.8,0.2,0,0)
  )

k <- 
  approxfun(
    x = c(1,2,3,200)
    ,y = c(0.8,0.2,0,0)
  )

# choice of player position to draft ---- 

# inputs : 
#    current_pick: current draft pick
#    position_accum: currently drafted team by position
#    pos_best: best available player by position 
# required global objects
#    draft
#    snake

pos_choice <- 
  function(current_pick,pos_accum,pos_best){


  player_ids <- 
    draft[
      drafted == 0
      ,.(
      id
      ,"adp" = round(adp)
      ,position
      ,points_ra
      ,points
      )
    ]

  qbid <- player_ids[position=="QB",id]

  rbid <- player_ids[position=="RB",id]

  wrid <- player_ids[position=="WR",id]

  teid <- player_ids[position=="TE",id]

  dstid <- player_ids[position=="DST",id]

  kid <- player_ids[position=="K",id]

  
  adp <- player_ids[,adp]

  names(adp) <- player_ids[,id]

  adp[is.na(adp)] <- 200

  N <- player_ids[,.N]

  avail <- 
    matrix(
      0
      ,nrow = N
      ,ncol = 181-current_pick
    )
  
  dimnames(avail) <- 
    list(
      paste0(player_ids[,id])
      ,paste0("pick.",seq(current_pick,180,1))
    )


  
  for(j in seq(2,181-current_pick,1)){

    for (i in player_ids[,id]){
        
    # Pr(avail at pick j+1 | avail at j) = Pr(avail at pick j+1 and avail at j) / Pr( avail at j)  
    # avail at j + 1 is a subset of avail at j 
    #  =>  Pr(avail at pick j+1 and avail at j) = Pr(avail at pick j+1) 
    # so, Pr(avail at pick j+1 | avail at j) = Pr(avail at pick j+1) / Pr( avail at j)
      
      avail[i,j] <- 
      (1 - ppois(q = current_pick - 1 + j, lambda = adp[i]))/(1 - ppois(q = current_pick, lambda = adp[i]))
  
    }
  
  }

  avail[which(avail[,2] == 0),2:(181-current_pick)] <- 1

  avail[, 1] <- 1
  
  points <- player_ids[,points]

  points[points < 0] <- 0

  points[is.na(points)] <- 0

  pp_ind <- matrix(0, nrow = N, ncol = 181-current_pick)

  colnames(pp_ind) <- paste0("pick.",seq(current_pick,180))

  rownames(pp_ind) <- paste0(player_ids[,id])

  pp_ind <- avail*points

  pp_pos <- matrix(0, nrow = 5, ncol = 181-current_pick)

  colnames(pp_pos) <- paste0("pick.",seq(current_pick,180))

  rownames(pp_pos) <- c("RB","WR","QB","TE","DST")

  for (i in seq(1,181-current_pick)){

    pp_pos["QB",i] <- mean(-sort(-pp_ind[qbid,i])[1:5])

    pp_pos["WR",i] <- mean(-sort(-pp_ind[wrid,i])[1:5])

    pp_pos["RB",i] <- mean(-sort(-pp_ind[rbid,i])[1:5])

    pp_pos["TE",i] <- mean(-sort(-pp_ind[teid,i])[1:5])

    pp_pos["DST",i] <- mean(-sort(-pp_ind[dstid,i])[1:5])

  }
  
  val_pos <- c(0,0,0,0,0)
  
  names(val_pos) <- c("RB","WR","QB","TE","DST")
  
  dp <- which(snake == current_pick, arr.ind = T)[1]
  rd <- which(snake == current_pick, arr.ind = T)[2]
  
  val_pos["RB"] <-
  (
  rb(pos_accum["RB"] + 1)*pos_best[position == "RB",max] + 
  rb(pos_accum["RB"] + 2)*pp_pos["RB",snake[dp,rd+1] - current_pick]
  ) -
  (
  0 + 
  rb(pos_accum["RB"] + 1)*pp_pos["RB",snake[dp,rd+1] - current_pick] + 
  rb(pos_accum["RB"] + 2)*pp_pos["RB",snake[dp,rd+2] - current_pick]
  )
  
  val_pos["QB"] <-
  (
  qb(pos_accum["QB"] + 1)*pos_best[position == "QB",max] + 
  qb(pos_accum["QB"] + 2)*pp_pos["QB",snake[dp,rd+1] - current_pick]
  ) -
  (
  0 + 
  qb(pos_accum["QB"] + 1)*pp_pos["QB",snake[dp,rd+1] - current_pick] + 
  qb(pos_accum["QB"] + 2)*pp_pos["QB",snake[dp,rd+2] - current_pick]
  )

  val_pos["WR"] <-
  (
  wr(pos_accum["WR"] + 1)*pos_best[position == "WR",max] + 
  wr(pos_accum["WR"] + 2)*pp_pos["WR",snake[dp,rd+1] - current_pick]
  ) -
  (
  0 + 
  wr(pos_accum["WR"] + 1)*pp_pos["WR",snake[dp,rd+1] - current_pick] + 
  wr(pos_accum["WR"] + 2)*pp_pos["WR",snake[dp,rd+2] - current_pick]
  )

  val_pos["TE"] <-
  (
  te(pos_accum["TE"] + 1)*pos_best[position == "TE",max] + 
  te(pos_accum["TE"] + 2)*pp_pos["TE",snake[dp,rd+1] - current_pick]
  ) -
  (
  0 + 
  te(pos_accum["TE"] + 1)*pp_pos["TE",snake[dp,rd+1] - current_pick] + 
  te(pos_accum["TE"] + 2)*pp_pos["TE",snake[dp,rd+2] - current_pick]
  )
  
  val_pos["DST"] <-
  (
  dst(pos_accum["DST"] + 1)*pos_best[position == "DST",max] + 
  dst(pos_accum["DST"] + 2)*pp_pos["DST",snake[dp,rd+1] - current_pick]
  ) -
  (
  0 + 
  dst(pos_accum["DST"] + 1)*pp_pos["DST",snake[dp,rd+1] - current_pick] + 
  dst(pos_accum["DST"] + 2)*pp_pos["DST",snake[dp,rd+2] - current_pick]
  )

  return(val_pos)
  
}

# draft pick function ---- 

draft_pick <-
  function(current_pick){

draft[
  ,points_ra :=
    weights[[1]](current_pick)*floor +
    weights[[2]](current_pick)*points +
    weights[[3]](current_pick)*ceiling +
    weights[[4]](current_pick)*(ceiling+(points_wk1+points_wk2+points_wk3))/2 +
    weights[[5]](current_pick)*(points_wk1+points_wk2+points_wk3) +
    weights[[6]](current_pick)*points_wk1
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
  ,last_name = i.last_name
  ,team = i.team)
  ,on = c(max = "points_ra")
]

print(
  round(
    pos_choice(
      current_pick = current_pick
      ,pos_accum = pos_accum
      ,pos_best = pos_best
    )
  ,0)
)

print(pos_best)

View(
  title = "RB"
     ,x =
draft[
  drafted == 0
  & position == "RB"
  ,
][
  order(-points_ra)
  ,
][
  1:5
  ,.(
    first_name
    ,last_name
    ,team
    ,position
    ,rank
    ,adp
    ,"points_ra" = round(points_ra,0)
    ,"floor" = round(floor,0)
    ,"ceiling" = round(ceiling,0)
    ,"points_wk1" = round(points_wk1,0)
  )
]
)

View(
  title = "QB"
     ,x =
draft[
  drafted == 0
  & position == "QB"
  ,
][
  order(-points_ra)
  ,
][
  1:5
  ,.(
    first_name
    ,last_name
    ,team
    ,position
    ,rank
    ,adp
    ,"points_ra" = round(points_ra,0)
    ,"floor" = round(floor,0)
    ,"ceiling" = round(ceiling,0)
    ,"points_wk1" = round(points_wk1,0)
  )
]
)


View(
  title = "WR"
     ,x =
draft[
  drafted == 0
  & position == "WR"
  ,
][
  order(-points_ra)
  ,
][
  1:5
  ,.(
    first_name
    ,last_name
    ,team
    ,position
    ,rank
    ,adp
    ,"points_ra" = round(points_ra,0)
    ,"floor" = round(floor,0)
    ,"ceiling" = round(ceiling,0)
    ,"points_wk1" = round(points_wk1,0)
  )
]
)

View(
  title = "TE"
     ,x =
draft[
  drafted == 0
  & position == "TE"
  ,
][
  order(-points_ra)
  ,
][
  1:5
  ,.(
    first_name
    ,last_name
    ,team
    ,position
    ,rank
    ,adp
    ,"points_ra" = round(points_ra,0)
    ,"floor" = round(floor,0)
    ,"ceiling" = round(ceiling,0)
    ,"points_wk1" = round(points_wk1,0)
  )
]
)


}


# draft update function ----

update_draft <-
  function(){

draft_input <- readRDS("current.rds")

draft[,drafted:=0]

draft[,pick:=0]

draft[,dp:=0]

draft[
  draft_input
  ,`:=`(
    drafted = 1
    ,pick = as.numeric(i.pick)
    ,dp = as.numeric(i.dp)
  )
  ,on = 
    .(
    first_name
    ,last_name
    ,team
    ,position
  )
]

pos_acc <- as.numeric(unlist(draft[dp==2,.N,by=position][,2]))

names(pos_acc) <- as.character(unlist(draft[dp==2,.N,by=position][,1]))

for (j in c("RB","QB","WR","TE","DST")){
  
  if(! j %in% names(pos_acc) ) {
    pos_acc <- c(pos_acc,j = 0)
    names(pos_acc)[length(pos_acc)] <- j
    }
  
}

return(pos_acc)    

}