

###Monte Carlo Simulation on a Golf Tournament  ###

###
rm(list=ls())

library(dplyr)
library(magrittr)



### Primary Variables to Adjust ####
Save_Location_Sims <-  paste0("output/Archive/Event_Sims_",Sys.Date(),".csv")

points.distrib <- read.csv("data/points_distribution.csv")
Tournament <- read.csv("data/tournament.csv")
Tournament$Country <- NULL
Ratings <- read.csv("data/ratings.csv")
Trials <- 1
df2 <- data.frame()

Rat<- Ratings

i=1
Sys.time()
while(i<=4){
  print(i)
  Tournament_Projection <- merge(Tournament,Rat[,c("player_id","player_name","rank","event_id","projected_rating", "projected_stdev")],by = c("player_id"),all.x = TRUE)
  Tournament_Projection <- Tournament_Projection[, c(1, 2, 3, 5, 7, 8)]
  colnames(Tournament_Projection)[2] <- "player_name"
  colnames(Tournament_Projection)[3]<- "event_id"
  Tournament_Projection$projected_rating[is.na(Tournament_Projection$projected_rating)] <- 3.0
  Tournament_Projection$projected_stdev[is.na(Tournament_Projection$projected_stdev)] <- 3.0
  Tournament_Projection$projected_stdev <- Tournament_Projection$projected_stdev - 0.5
  
  #str(Tournament_Projection)
  
  
  ### Simulation Engine ####
  Sim_Once <- function(Data,Iteration) {
    #   # For prototyping:
    #   Data = Tournament_Projection
    #   Iteration = 1
    
    Sim_Result_1 <- unlist(lapply(Data[,c("projected_stdev")],function(x) rnorm(1,sd = x)))
    Sim_Result_2 <- unlist(lapply(Data[,c("projected_stdev")],function(x) rnorm(1,sd = x)))
    Sim_Result_3 <- unlist(lapply(Data[,c("projected_stdev")],function(x) rnorm(1,sd = x)))
    Sim_Result_4 <- unlist(lapply(Data[,c("projected_stdev")],function(x) rnorm(1,sd = x)))
    
    Results_Frame <- cbind.data.frame (Sim_Result_1,Sim_Result_2,Sim_Result_3,Sim_Result_4)
    
    Result <- Data[,c("player_name","player_id","event_id")]
    
    Result$Sim_Result_Raw <- (rowSums(Results_Frame)  + 4*Data$projected_rating)
    
    Result$Sim_Result <- Result$Sim_Result_Raw # + Result$Round_1
    
    Result$Sim_Result_Rounded <- round(Result$Sim_Result)  # round to nearest stroke
    #print( Result$Sim_Result_Rounded)
    Result %<>% group_by(event_id) %>%
      mutate( Rank = min_rank(Sim_Result),
              Rank_Round = min_rank(Sim_Result_Rounded),
              Rank_Final = Rank_Round) %>% ungroup()  # Rank the results
    
    Result$Rank_Final[Result$Rank_Round==1 & !Result$Rank==1] <- 2  # Break ties for winner
    
    Result %<>% group_by(event_id) %>%
      ungroup()  # Rank the results  
    
    # View(Result)
    
    return (Result)
    
  }
  
  
  library(data.table)
  
  fg2 <- data.frame()
  
  Monte_Carlo_Sim <- function(Data,Iterations) {
    results <- (replicate(Iterations, Sim_Once(Data)))
    #str(results)
    # while(i<=4){
    #   results.dataframe <- as.data.frame(results)
    #   fg <-rbindlist(results.dataframe[i])
    #   fg<-fg[order(fg$Rank_Final),]
    #   fg$Points <- points.distrib$Points
    #   print(fg)
    #   fg2<- rbind(fg,fg2)
    #   i=i+1
    # }
    
    #print(as.data.frame(fg2))
    Output <- as.data.frame(rbindlist(as.data.frame(results)))
    return(Output)
  }
  
  
  Sys.time()
  Trial_Sim <- Monte_Carlo_Sim (Tournament_Projection,Trials)
  Sys.time()
  #str(Trial_Sim)
  
  
  library(dplyr)
  Player_ID_Group <- group_by(Trial_Sim, player_id)
  Finishes <- summarise(Player_ID_Group,
                        Win = sum(Rank == 1)/Trials, 
                        Top_5 = sum(Rank<6)/Trials, 
                        Top_10 = sum(Rank<11)/Trials)
  
  
  Tournament_Projection_Out <-
    merge(Tournament_Projection,Finishes,by = c("player_id"))
  
  Tournament_Projection_Out %<>% group_by(event_id) %>%
    mutate(Win_Rank = min_rank(-Win)
    ) %>% 
    ungroup()
  
  
  Tournament_Projection_Out <- arrange(Tournament_Projection_Out,event_id, Win_Rank, rank)
  Tournament_Projection_Out$Win <-NULL
  Tournament_Projection_Out$Top_5<-NULL
  Tournament_Projection_Out$Top_10<-NULL
  Tournament_Projection_Out$rank<-NULL
  Tournament_Projection_Out$rank<- 1:85
  Tournament_Projection_Out$Win_Rank<-NULL
  #Tournament_Projection_Out<-   Tournament_Projection_Out[, c(1, 2, 7,  3 )] 
  
  Rat <- Tournament_Projection_Out
  Tournament_Projection_Out$points <- points.distrib$Points
  i=i+1
  df2<- rbind(Tournament_Projection_Out,df2)
  
}
Sys.time()
df3<-ddply(df2,.(player_id,player_name),summarize,total_points=sum(points))
#write.csv(df3, "10000_8.csv", row.names = FALSE)

#write.csv(Tournament_Projection_Out, file = "Output/Current_Event_Simulation_.csv", row.names = FALSE)

#write.csv(Tournament_Projection_Out, file = Save_Location_Sims, row.names = FALSE)

