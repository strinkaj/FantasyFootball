
## --
## data pull
##
## --


# global ----

library(data.table)
library(ffanalytics)

weeks <- 
  data.table(
    "date" = 
      seq(
        as.Date("2020-09-06")
        ,as.Date("2021-01-02")
      ,by = 1
      )
    ,"w" = rep(1:17,times = 1, each = 7)
  )


if (Sys.Date() < min(weeks$date)){
  
  current_week <- 0

} else {
    
  current_week <- weeks[date == Sys.Date(),w]

}

weeks_to_end <- seq(current_week,16)

# sources ----


# pull all sources for the current week
src_current <- 
  c(
  "CBS" 
#  ,"ESPN"
#  ,"FantasyData"
  ,"FantasyPros" 
  ,"FantasySharks" 
#  ,"FFToday" 
  ,"FleaFlicker" 
  ,"NumberFire" 
#  ,"Yahoo" 
  ,"FantasyFootballNerd" 
#  ,"NFL"
#  ,"RTSports"
#  ,"Walterfootball"
  )


# only some sources forecast out - watch carefully
src_future <- 
  c(
    # "CBS" 
    # ,"ESPN"
    # ,"FantasyData"
    "FantasyPros" 
    ,"FantasySharks" 
    # ,"FFToday" 
    # ,"FleaFlicker" 
    # ,"NumberFire" 
    # ,"Yahoo" 
    # ,"FantasyFootballNerd" 
    # ,"NFL"
    # ,"RTSports"
    # ,"Walterfootball"
  )

# data scrape ----

if (current_week == 0){

  d_draft <-
    scrape_data(
      src = src_current
      ,week = 0
      ,pos = c("QB","WR","RB","TE","DST")
    )
  
  d_adp <- get_adp()
  
  d_current <- list()
  
  for (i in 1:3){
    
    Sys.sleep(500)
    
    d_current[[i]] <-
      scrape_data(
        src = src_future
        ,week = i
        ,pos = c("QB","WR","RB","TE","DST")
      )
    
  }
  
  
} else {
  
  d_current <-
    scrape_data(
      src = src_current
      ,week = current_week
      ,pos = c("QB","WR","RB","TE","DST")
    )
  
  d_future <- replicate(list(),n = length(weeks_to_end))
  

  for (i in 1:length(weeks_to_end)){

    Sys.sleep(500)
      
    d_future[[i]] <-
      scrape_data(
        src = src_future
        ,week = weeks_to_end[i]
        ,pos = c("QB","WR","RB","TE","DST")
      )

  }
  
}

# save raw data ----

if (current_week == 0){

  save(d_draft,d_adp,d_current,file = paste0("../draft/draftday/.RData"))
  
  
} else {

save(d_current,d_future,player_table,file = paste0(Sys.Date(),".RData"))

}













# set leauge rules ---- 

rules <-
  list(
    pass =
      list(
        pass_att = 0
        ,pass_comp = 0
        ,pass_inc = 0
        ,pass_yds = 0.04
        ,pass_tds = 4
        ,pass_int = -2
        ,pass_40_yds = 0
        ,pass_300_yds = 0
        ,pass_350_yds = 1
        ,pass_400_yds = 2
      )
    
    ,rush =
      list(
        all_pos = TRUE
        ,rush_yds = 0.1
        ,rush_att = 0.1
        ,rush_40_yds = 0
        ,rush_tds = 6
        ,rush_100_yds = 0
        ,rush_150_yds = 1
        ,rush_200_yds = 2
      )
      
    ,rec = 
      list(
        all_pos = TRUE
        ,rec = 1
        ,rec_yds = 0.1
        ,rec_tds = 6
        ,rec_40_yds = 0
        ,rec_100_yds = 0
        ,rec_150_yds = 1
        ,rec_200_yds = 2
      )
      
    ,misc = 
      list(
        all_pos = TRUE
        ,fumbles_lost = -2 
        ,fumbles_total = 0
        ,sacks = 1
        ,two_pts = 2
      )
      
    ,kick =
      list(
        xp = 1.0
        ,fg_0019 = 3.0
        ,fg_2029 = 3.0
        ,fg_3039 = 3.0
        ,fg_4049 = 4
        ,fg_50 = 5.0
        ,fg_miss = -1
      )
      
    ,ret =
      list(
        all_pos = TRUE
        ,return_tds = 6
        ,return_yds = 0
      )
      
    ,idp =
      list(
        all_pos = TRUE
        ,idp_solo = 1
        ,idp_asst = 0.5
        ,idp_sack = 2
        ,idp_int = 3,idp_fum_force = 3
        ,idp_fum_rec = 2
        ,idp_pd = 1
        ,idp_td = 6
        ,idp_safety = 2
      )
      
    ,dst = 
      list(
        dst_fum_rec = 2
        ,dst_int = 2
        ,dst_safety = 2
        ,dst_sacks = 1
        ,dst_td = 6
        ,dst_blk = 2
        ,dst_ret_yds = 0
        ,dst_pts_allowed = 0
      )
      
    ,pts_bracket =
      list(
        list(threshold = 0, points = 8)
        ,list(threshold = 6, points = 6)
        ,list(threshold = 13, points = 4)
        ,list(threshold = 14, points = 2)
        ,list(threshold = 17, points = 2)
        ,list(threshold = 21, points = 2)
        ,list(threshold = 27, points = 1)
        ,list(threshold = 34, points = -1)
        ,list(threshold = 45, points = -2)
        ,list(threshold = 99, points = -3)
      )

)

# make projections ---- 

proj <-
  function(dl){

    p <- projections_table(dl,scoring_rules = rules)

    p <- p %>% add_player_info()

    p <- data.table(p)
    
    return(p)

  }

if (current_week == 0){

p <- proj(d_draft) 

p_future <- lapply(X = d_current, FUN = proj)

for (i in 1:3){
  
  p_future[[i]][ ,week := i ] 
  
}

p_future <- rbindlist(p_future)

save(current_week,weeks_to_end,p,d_draft,d_adp,p_future, file = "../draft/.RData")
  
} else {

p_current <- proj(d_current)

p_future <- lapply(X = d_future, FUN = proj)

for (i in 1:length(weeks_to_end)){

 p_future[[i]][ ,week := i + current_week - 1] 
   
}

p_future <- rbindlist(p_future)

save(current_week,weeks_to_end,p_future,p_current, file = ".RData")

}
