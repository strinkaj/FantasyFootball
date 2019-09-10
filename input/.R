
## --
## draft data pull
##
## --


# global ----

library(data.table)
library(ffanalytics)

# sources ----

src <- 
  c(
  "CBS" # has projections
  ,"ESPN"
  ,"FantasyData"
  ,"FantasyPros" # has projections
  ,"FantasySharks" # has projections
#  ,"FFToday" # errors out august 23rd
  ,"FleaFlicker" # has projections
  ,"NumberFire" # has projections
  ,"Yahoo" # has projections
  ,"FantasyFootballNerd"
  ,"NFL" # has projections
  ,"RTSports"
  ,"Walterfootball"
  )

src1 <- 
  c(
  "CBS" # no wk1 projections
  ,"ESPN"
  ,"FantasyData"
  ,"FantasyPros" # has projections
  ,"FantasySharks" # has projections
  ,"FFToday" # errors out august 16th
  ,"FleaFlicker" # no wk1 projections
  ,"NumberFire" # has projections
  ,"Yahoo" # no wk1 projections
  ,"FantasyFootballNerd" # has projections
  ,"NFL" # has projections
  ,"RTSports"
  ,"Walterfootball"
  )

# data scrape ----

# d <- 
#   scrape_data(
#     src = src
#     ,week = 0
#     ,pos = c("QB","WR","RB","TE","DST")
#   )

# d1 <- 
#   scrape_data(
#     src = src1
#     ,week = 1
#     ,pos = c("QB","WR","RB","TE","DST")
#   )

d2 <- 
  scrape_data(
    src = src1
    ,week = 2
    ,pos = c("QB","WR","RB","TE","DST")
    )

d3 <- 
  scrape_data(
    src = src1
    ,week = 3
    ,pos = c("QB","WR","RB","TE","DST")
  )

d4 <- 
  scrape_data(
    src = src1
    ,week = 4
    ,pos = c("QB","WR","RB","TE","DST")
  )

d5 <- 
  scrape_data(
    src = src1
    ,week = 5
    ,pos = c("QB","WR","RB","TE","DST")
  )

# save raw data ----

# save(d,d1,d2,d3,player_table,file = paste0(Sys.Date(),".RData"))
save(d2,d3,d4,d5,player_table,file = paste0(Sys.Date(),".RData"))


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
        ,rec = 0.5
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
        ,dst_blk = 2
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

# make projections ---- 

# p <- projections_table(d,scoring_rules = rules)
# 
# p <- p %>% add_player_info()

# p1 <- projections_table(d1,scoring_rules = rules)
# 
# p1 <- p1 %>% add_player_info()
# 

p2 <- projections_table(d2,scoring_rules = rules)

p2 <- p2 %>% add_player_info()

p3 <- projections_table(d3,scoring_rules = rules)

p3 <- p3 %>% add_player_info()

p4 <- projections_table(d4,scoring_rules = rules)

p4 <- p4 %>% add_player_info()

p5 <- projections_table(d5,scoring_rules = rules)

p5 <- p5 %>% add_player_info()

# p <- data.table(p)

# p1 <- data.table(p1)

p2 <- data.table(p2)

p3 <- data.table(p3)

p4 <- data.table(p4)

p5 <- data.table(p5)

# yahoo <- yahoo_draft()
# 
# yahoo <- data.table(yahoo)
# 
# dst_relate <- 
#   data.table(
#   cbind(
#   "yahoo" =
#     c("Chi","LAR","Jax","Bal","Min","LAC","Cle","Hou","NE","Atl","NO","Car","SF","GB","Det","Dal","Pit","Ten","KC","NYJ","Phi","Den","Sea","Ind","Buf","NYG","Ari","Cin","LAR","Oak","TB","Mia","Was")
#   ,"name" =
#     c("CHI","LAR","JAC","BAL","MIN","LAC","CLE","HOU","NEP","ATL","NOS","CAR","SFO","GBP","DET","DAL","PIT","TEN","KCC","NYJ","PHI","DEN","SEA","IND","BUF","NYG","ARI","CIN","LAR","OAK","TBB","MIA","WAS")
#   )
# )
# 
# yahoo[
#   dst_relate
#   ,other_name := i.name
#   ,on = c(team = "yahoo")
# ]
# 
# yahoo[
#   p[pos == "DST",]
#   ,dst_id := i.id
#   ,on = c(other_name = "team")
# ]
# 
# yahoo[
#   pos == "DEF"
#   &is.na(id)
#   ,id := dst_id
# ]

# p[
#   p1[avg_type == "robust",]
#   ,points_wk1 := i.points
#   ,on = "id"
# ]
# 
# p[
#   p2[avg_type == "robust",]
#   ,points_wk2 := i.points
#   ,on = "id"
# ]
# 
# p[
#   p3[avg_type == "robust",]
#   ,points_wk3 := i.points
#   ,on = "id"
# ]


# save data ---- 

#save(p,p1,p2,p3,yahoo, file = ".RData")
#save(p1,p2,p3,yahoo, file = ".RData")
#save(p,yahoo, file = "../draft/.RData")
save(p2,p3,p4,p5, file = ".RData")

#fwrite(p,"p.csv")

# fwrite(p1,"p1.csv")

fwrite(p2,"p2.csv")

fwrite(p3,"p3.csv")

fwrite(p4,"p4.csv")

fwrite(p5,"p5.csv")

# fwrite(yahoo,"yahoo.csv")
