
## --
## draft data pull
##
## --


# global ----

library(data.table)
library(ffanalytics)

src <- 
  c(
  "CBS" # has projections
  ,"ESPN"
  ,"FantasyData"
  ,"FantasyPros" # has projections
  ,"FantasySharks" # has projections
#  ,"FFToday" # errors out august 16th
  ,"FleaFlicker"
  ,"NumberFire" # has projections
  ,"Yahoo" # has projections
  ,"FantasyFootballNerd"
  ,"NFL" # has projections
  ,"RTSports"
  ,"Walterfootball"
  )

d <- scrape_data(src = src)

unique(d$QB$data_src)

save(d,file = "8-17.RData")

rules <-
  list(

pass =
  list(
    pass_att = 0
    ,pass_comp = 0
    ,pass_inc = 0
    ,pass_yds = 0.04
    ,pass_tds = 4
    ,pass_int = -1
    ,pass_40_yds = 0
    ,pass_300_yds = 0
    ,pass_350_yds = 0
    ,pass_400_yds = 0
  )

,rush =
  list(
    all_pos = TRUE
    ,rush_yds = 0.1
    ,rush_att = 0
    ,rush_40_yds = 0
    ,rush_tds = 6
    ,rush_100_yds = 0
    ,rush_150_yds = 0
    ,rush_200_yds = 0
  )
  
,rec = 
  list(
    all_pos = TRUE
    ,rec = 0
    ,rec_yds = 0.1
    ,rec_tds = 6
    ,rec_40_yds = 0
    ,rec_100_yds = 0
    ,rec_150_yds = 0
    ,rec_200_yds = 0
  )
  
,misc = 
  list(
    all_pos = TRUE
    ,fumbles_lost = 0 
    ,fumbles_total = 0
    ,sacks = 1
    ,two_pts = 0
  )
  
,kick =
  list(
    xp = 1.0
    ,fg_0019 = 3.0
    ,fg_2029 = 3.0
    ,fg_3039 = 3.0
    ,fg_4049 = 4.0
    ,fg_50 = 5.0
    ,fg_miss = 0.0
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
    ,dst_blk = 1.5
    ,dst_ret_yds = 0
    ,dst_pts_allowed = 0
  )
  
,pts_bracket =
  list(
    list(threshold = 0, points = 7)
    ,list(threshold = 6, points = 4)
    ,list(threshold = 20, points = 1)
    ,list(threshold = 27, points = 0)
    ,list(threshold = 34, points = -1)
    ,list(threshold = 99, points = -4)
  )

)

p <- projections_table(d,scoring_rules = rules)

p <- p %>% add_player_info()

p <- p %>% add_ecr() %>% add_risk() 

yahoo <- yahoo_draft()


p <- data.table(p)

yahoo <- data.table(yahoo)

dst_relate <- 
  data.table(
  cbind(
  "yahoo" =
  c("Chi","LAR","Jax","Bal","Min","LAC","Cle","Hou","NE","Atl","NO","Car","SF","GB","Det","Dal","Pit","Ten","KC","NYJ","Phi","Den","Sea","Ind","Buf","NYG","Ari","Cin","LAR","Oak","TB","Mia","Was")
  ,"name" =
  c("CHI","LAR","JAC","BAL","MIN","LAC","CLE","HOU","NEP","ATL","NOS","CAR","SFO","GBP","DET","DAL","PIT","TEN","KCC","NYJ","PHI","DEN","SEA","IND","BUF","NYG","ARI","CIN","LAR","OAK","TBB","MIA","WAS")
  )
)

yahoo[
  dst_relate
  ,other_name := i.name
  ,on = c(team = "yahoo")
]

yahoo[
  p[pos == "DST",]
  ,dst_id := i.id
  ,on = c(other_name = "team")
]

yahoo[
  pos == "DEF"
  &is.na(id)
  ,id := dst_id
]

save(p,yahoo, file = ".RData")
