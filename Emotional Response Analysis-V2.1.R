rm(list = ls())

set.seed(1234)
#setwd("/Users/abheekb-25/Documents/Abheek_WAT/OneDrive - Dentsu Aegis Network/2 Projects/Syndicated/Biometric Behavioral Labs/Application")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,tidyr,tidyverse,reshape,lubridate,plyr,data.table,tibble,zoo,ggplot2,ggrepel,matrixStats)

#packages used- dplyr,tidyr,tidyverse,reshape,lubridate,plyr,data.table,tibble,zoo

# STEP1-Getting data in with time in seconds ----

#a) data input
data_feelings <- read.csv("MASTER_DATASET_FINAL.csv")
colnames(data_feelings) <- c("Video_Clip","Respondent","Gender","Age",
                             "Timestamp",
                             "Only_time","Delta","Theta",
                             "LowAlpha","MeanAlpha",
                             "HighAlpha","LowBeta","MeanBeta",
                             "HighBeta","LowGamma","MeanGamma","HighGamma","delit_hr",
                             "delit_min","delit_sec","delit_msec","time_in_msec","time_in_sec")
#View(data_feelings)


#c) new frame
data_addition <- data.frame(matrix(ncol = ncol(data_feelings), nrow=0))
colnames(data_addition) <- colnames(data_feelings)

#d) loop to get time in seconds for every adv, every individual
for (i in 1:length(unique(data_feelings$Respondent))) {
  
  for (j in 1:length(unique(data_feelings$Video_Clip))){
    
    data_subset_taken<- subset(data_feelings, data_feelings$Respondent==unique(data_feelings$Respondent)[i]
                               & data_feelings$Video_Clip==unique(data_feelings$Video_Clip)[j])
    data_subset_taken$time_final_sec <- data_subset_taken$time_in_sec-min(data_subset_taken$time_in_sec)
    data_addition <- rbind(data_addition, data_subset_taken)
    
  }
  
}


# STEP2-Input ----
#All Variables except Ad can have null values
Ad <- "5(SYRIA)"
Gen <- NULL
Start_Age <- 20
End_Age <- 30

# STEP3-Subsetting using variables ----

#Condition on StartingAge and EndingAge
if(is.null(Start_Age) & is.null(End_Age) ){
  
  Data_forsubset<-subset(data_addition,data_addition$Video_Clip == Ad 
                         &data_addition$Gender==Gen,
                         select = Video_Clip:time_final_sec)
  
  
  
} else if(is.null(Gen) ){
  
  Data_forsubset<-subset(data_addition,data_addition$Video_Clip == Ad
                         &data_addition$Age>=Start_Age&data_addition$Age<End_Age,
                         select = Video_Clip:time_final_sec)
  
  
} else if(is.null(Gen)&is.null(Start_Age)&is.null(End_Age)){
  
  Data_forsubset<-subset(data_addition,data_addition$Video_Clip == Ad,
                         select = Video_Clip:time_final_sec)
  
  
  
  
}else {
  
  
  Data_forsubset<-subset(data_addition,data_addition$Video_Clip == Ad 
                         &data_addition$Age>=Start_Age&data_addition$Age<End_Age 
                         &data_addition$Gender==Gen,
                         select = Video_Clip:time_final_sec)
}

View(Data_forsubset)


# STEP4-Creation of 4 dataset using time_seconds ----

#a)Creation of Delta dataframe
Delta_graph<-data.frame(Data_forsubset$time_final_sec,
                        Data_forsubset$Respondent,Data_forsubset$Delta)
#View(Delta_graph)

Delta_graph_Spread<-spread(Delta_graph,Data_forsubset.Respondent,Data_forsubset.Delta)
#View(Delta_graph_Spread)

#taking mean
Delta_graph_Spread.df<-Delta_graph_Spread
Delta_graph_Spread.df[is.na(Delta_graph_Spread.df)] <- 0


if(ncol(Delta_graph_Spread.df)==2){
  
  Delta_graph_Spread.df$meanofallviewers_Delta<-Delta_graph_Spread.df[2]

} else {

Delta_graph_Spread.df$meanofallviewers_Delta <-
  (rowSums(Delta_graph_Spread.df[,2:(ncol(Delta_graph_Spread.df))]))/(rowSums(Delta_graph_Spread.df[,2:(ncol(Delta_graph_Spread.df))] != 0))

}
#Delta_graph_Spread.df$denom_values<-
# rowSums(Delta_graph_Spread.df[,2:(ncol(Delta_graph_Spread.df)-2)] != 0)
#(Delta_graph_Spread.df$denom_values)

#Delta_graph_Spread.df$meanofallviewers_Delta<-
# Delta_graph_Spread.df$sumofallviewers_Delta/Delta_graph_Spread.df$denom_values
View(Delta_graph_Spread.df)
#rowSums (Delta_graph_Spread.df[2] :Delta_graph_Spread.df[5], na.rm = TRUE)


#b)Creation of Theta dataframe
Theta_graph<-data.frame(Data_forsubset$time_final_sec,
                        Data_forsubset$Respondent,Data_forsubset$Theta)
#View(Theta_graph)

Theta_graph_Spread<-spread(Theta_graph,Data_forsubset.Respondent,Data_forsubset.Theta)
#View(Theta_graph_Spread)

#taking mean
Theta_graph_Spread.df<-Theta_graph_Spread
Theta_graph_Spread.df[is.na(Theta_graph_Spread.df)] <- 0

if(ncol(Theta_graph_Spread.df)==2){
  
  Theta_graph_Spread.df$meanofallviewers_Theta<-Theta_graph_Spread.df[2]
  
} else {
  
  Theta_graph_Spread.df$meanofallviewers_Theta <-
    (rowSums(Theta_graph_Spread.df[,2:(ncol(Theta_graph_Spread.df))]))/(rowSums(Theta_graph_Spread.df[,2:(ncol(Theta_graph_Spread.df))] != 0))
  
}


View(Theta_graph_Spread.df)

#c)Creation of Alpha dataframe
Alpha_graph<-data.frame(Data_forsubset$time_final_sec,
                        Data_forsubset$Respondent,Data_forsubset$MeanAlpha)
#View(Alpha_graph)

Alpha_graph_Spread<-spread(Alpha_graph,Data_forsubset.Respondent,Data_forsubset.MeanAlpha)
#View(Alpha_graph_Spread)

#taking mean
Alpha_graph_Spread.df<-Alpha_graph_Spread
Alpha_graph_Spread.df[is.na(Alpha_graph_Spread.df)] <- 0

if(ncol(Alpha_graph_Spread.df)==2){
  
  Alpha_graph_Spread.df$meanofallviewers_Alpha<-Alpha_graph_Spread.df[2]
  
} else {
  
  Alpha_graph_Spread.df$meanofallviewers_Alpha <-
    (rowSums(Alpha_graph_Spread.df[,2:(ncol(Alpha_graph_Spread.df))]))/(rowSums(Alpha_graph_Spread.df[,2:(ncol(Alpha_graph_Spread.df))] != 0))
  
}


#View(Alpha_graph_Spread.df)


#d)Creation of Beta dataframe
Beta_graph<-data.frame(Data_forsubset$time_final_sec,
                       Data_forsubset$Respondent,Data_forsubset$MeanBeta)
#View(Beta_graph)

Beta_graph_Spread<-spread(Beta_graph,Data_forsubset.Respondent,Data_forsubset.MeanBeta)
#View(Beta_graph_Spread)

#Taking mean
Beta_graph_Spread.df<-Beta_graph_Spread
Beta_graph_Spread.df[is.na(Beta_graph_Spread.df)] <- 0

if(ncol(Beta_graph_Spread.df)==2){
  
  Beta_graph_Spread.df$meanofallviewers_Beta<-Beta_graph_Spread.df[2]
  
} else {
  
  Beta_graph_Spread.df$meanofallviewers_Beta <-
    (rowSums(Beta_graph_Spread.df[,2:(ncol(Beta_graph_Spread.df))]))/(rowSums(Beta_graph_Spread.df[,2:(ncol(Beta_graph_Spread.df))] != 0))
  
}

#View(Beta_graph_Spread.df)

#         4 DATAFRAMES
#View(Delta_graph_Spread.df)
#View(Theta_graph_Spread.df)
#View(Alpha_graph_Spread.df)
#View(Beta_graph_Spread.df)

# Single Data Frame
all.df <- as.data.frame(Delta_graph_Spread.df[,1], drop = F)
all.df <- cbind(all.df, Delta_graph_Spread.df[,"meanofallviewers_Delta"], 
                Theta_graph_Spread.df[,"meanofallviewers_Theta"],
                Alpha_graph_Spread.df[,"meanofallviewers_Alpha"],
                Beta_graph_Spread.df[,"meanofallviewers_Beta"])

colnames(all.df) <- c("Time", "Delta", "Theta", "Alpha", "Beta")

#View(all.df)

all.single.df <- gather(all.df, "Waves", "values", -Time)

View(all.single.df)

# STEP5-Visualization ----


filename <- paste("EEG-Feelings-",Ad,"-",ifelse(is.null(Gen), "Male & Female",Gen), 
                  ifelse((Start_Age+End_Age)== 0,", All ages",
                         ifelse(Start_Age == 0,
                                ifelse(End_Age == 0, "-All ages", paste(", Less than ", End_Age, " years", sep = "")),
                                ifelse(End_Age == 0, paste(", More than ", Start_Age, " years", sep = ""),
                                       paste(", ",Start_Age, "-",End_Age," years",sep = "")))),".png",sep = "")


title_name <- paste("EEG-Feelings-",Ad,"-",ifelse(is.null(Gen), "Male & Female",Gen), 
                    ifelse((Start_Age+End_Age)== 0,", All ages",
                           ifelse(Start_Age == 0,
                                  ifelse(End_Age == 0, "-All ages", paste(", Less than ", End_Age, " years", sep = "")),
                                         ifelse(End_Age == 0, paste(", More than ", Start_Age, " years", sep = ""),
                                                paste(", ",Start_Age, "-",End_Age," years",sep = "")))),sep = "")


all.single.df <- all.single.df %>% group_by(Waves) %>% mutate(med = mean(values))


a <- ggplot(all.single.df, aes(y=values, x=Time, colour = factor(Waves)), show.legend = F)

a1 <- facet_grid(Waves ~., scales = "free_y")

b <- geom_line()

c <- labs(title=title_name, y="Values", x="Time (seconds)")

d <- theme(legend.position = 'none', axis.text.y=element_blank(), 
            axis.ticks.x=element_blank(), panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            axis.ticks.y = element_blank(),  
            panel.grid.minor.y = element_blank(),
            plot.title = element_text(hjust = 0.5))

e <- geom_hline(aes(yintercept = med, group = Waves), linetype = "dashed", alpha=0.5, colour = "navy")

f <- geom_smooth(method='lm', linetype = "dashed", size = 0.3, alpha=0.3, colour = "black")

# g <- geom_hline(stat = 'summary', fun.y = "quantile", fun.args = list(0.9), linetype=2, colour="blue")

plot <- a + a1 + b + c + d + e + f

plot