##### Example code for behavioural metrics calculations #####

#read in clean detection data (filtered for false detections, stationary detections removed)
data <- read.csv("2017 detections cut.csv")

#order by smolt and time
data2 <- data %>% arrange(tag_sn, detection_timestamp_utc) %>% distinct()

#remove mobile detections
data2 <- data2 %>% filter(order != "00VR")


##### total number of detections ####

#number of rows (detections) for each smolt
dets_sum <- data2 %>% group_by(tag_sn) %>% summarise(total_num_dets = n())



##### total days with detections ####

#how many distinct days detected on
days_sum <- data2 %>% group_by(tag_sn) %>% summarise(num_days = n_distinct(julianday))



##### time b/w release and last ####

#convert datetime to posix format
data2$detection_timestamp_utc <- as.POSIXct(data2$detection_timestamp_utc, tz="UTC", format="%Y-%m-%d %H:%M:%S")

#calculate total time between first (release) and last detection
time_sum <- data2 %>% arrange(tag_sn, detection_timestamp_utc) %>% group_by(tag_sn) %>%
  mutate(total_time = max(detection_timestamp_utc)- min(detection_timestamp_utc))

mean(time_sum$total_time) #in sec

#convert from seconds to days
time_sum <- time_sum %>% select(tag_sn, total_time_sec=total_time)
time_sum <- distinct(time_sum)
time_sum <- time_sum %>% mutate(total_time_min=total_time_sec/60)
time_sum <- time_sum %>% mutate(total_time_hr=total_time_min/60)
time_sum <- time_sum %>% mutate(total_time_day=total_time_hr/24)



###### dets above Stew/Shubie confluence ####

#filter for receivers upstream of confluence
upstream <- data2 %>% filter(station %in% c("SH006", "SH005", "SH004", "SH003", "SH002", "SH001")) #old trunk to treaty truckhouse

#number of rows (detections) at these receivers
upstream <- upstream %>% group_by(tag_sn) %>% summarise(num_conflu_dets = n())


###### number of detections at each receiver ####

#group by smolt and receiver, count number of rows (Detections)
dets <- data2 %>% group_by(tag_sn, receiver_sn) %>% summarise(n=n())

#min and max number of dets at a single receiver for each smolt
dets2 <- dets %>% group_by(tag_sn) %>% summarise(max_det=max(n), min_det=min(n))


##### Summary so far ####

#join results so far into single df
summary <- left_join(days_sum, dets_sum, by="tag_sn")
summary <- left_join(summary, dets2, by="tag_sn")
summary <- left_join(summary, time_sum %>% select(tag_sn, total_time_day), by="tag_sn")
summary <- left_join(summary, upstream, by="tag_sn")


##### number of reversals ####

revs <- data2 #make copy of df

#no MB in 2017 - removed MB in 2018 and 2019

#need to make all SH mouth equal (14-16 -> 14)
revs$num_station <- gsub("[A-Z]", "", revs$order) #make column for just number from order
revs$num_station[revs$area=="SH mouth"] <- 14 #set all SH mouth receivers to same number


#make column for lag (previous receiver number)
revs <- revs %>% arrange(tag_sn, detection_timestamp_utc) %>% group_by(tag_sn)  %>% 
  mutate(num_prev_station=lag(num_station))

revs$num_station <- as.numeric(revs$num_station)
revs$num_prev_station <- as.numeric(revs$num_prev_station)

#subtract previous station number from current station
revs <- revs %>% mutate(direction = num_station - num_prev_station)
#revs2 <- revs %>% filter(direction != 0)
revs$direction[is.na(revs$direction)] <- 0

#if negative, smolt is moving upstream, if not, moving downstream
revs$direction <- ifelse(revs$direction < 0, "upstream", "downstream")

#make column for previous swimming direction
revs <- revs %>% arrange(tag_sn, detection_timestamp_utc) %>% group_by(tag_sn)  %>% 
  mutate(prev_direction=lag(direction))

revs$prev_direction[is.na(revs$prev_direction)] <- "downstream" #from release to first detection

#check up and down for smolts above confluence
revs %>% filter(order %in% c("07SH", "08SH","09SH", "10SH", "11SH")) %>% select(tag_sn) %>% distinct()
# 1 1262412
# 2 1262413
# 3 1262415
# 4 1262423
# 5 1262432
# 6 1262433
# 7 1262439
# 8 1262440
# 9 1262442
# 10 1262451


check2 <- revs %>% filter(tag_sn==1262412) 
check2 <- check2 %>% filter(num_station != num_prev_station)

revs$direction[revs$num_station==10 & revs$num_prev_station==6] <- "upstream"

check2 <- revs %>% filter(tag_sn==1262413) #good

check2 <- revs %>% filter(tag_sn==1262415) 

revs$direction[revs$num_station==11 & revs$num_prev_station==6] <- "upstream"

check2 <- revs %>% filter(tag_sn==1262423)

check2 <- revs %>% filter(tag_sn==1262432)

check2 <- revs %>% filter(tag_sn==1262433)
revs$direction[revs$num_station==11 & revs$num_prev_station==5] <- "upstream"

check2 <- revs %>% filter(tag_sn==1262439)

check2 <- revs %>% filter(tag_sn==1262440)

check2 <- revs %>% filter(tag_sn==1262442)

check2 <- revs %>% filter(tag_sn==1262451)


#count number of reversals by individuals
rev_sum <- revs %>% group_by(tag_sn) %>% filter(direction!=prev_direction) %>% 
  summarise(num_reversals=n()) #switch from down to upstream or up to downstream

#join to summary df
summary <- left_join(summary, rev_sum)


##### Time on bass spawning grounds #####

#main spawning ground Moxam's (06ST) to Eddy Pool (02ST)

data2$spawn <- ifelse(data2$order %in% c("02ST", "03ST", "04ST", "05ST", "06ST"), 1, 0)
#1 if receiver on bass spawning ground, 0 if not

#create column to mark each time smolt enters and leaves spawning grounds
dets <- data2 %>%  arrange(tag_sn, detection_timestamp_utc) %>% group_by(tag_sn) %>%
  mutate(start = c(0, diff(spawn) != 0), run=cumsum(start))

#format datetime
dets$detection_timestamp_utc <- as.POSIXct(dets$detection_timestamp_utc, tz="UTC", format="%Y-%m-%d %H:%M:%S")

#calculate time for each period on and off spawning grounds
dets2 <- dets %>% dplyr::group_by(tag_sn, spawn, run) %>% 
  dplyr::mutate(time = max(detection_timestamp_utc)- min(detection_timestamp_utc)) 

#filter down to distinct events
s_d <- dets2 %>% select(tag_sn, spawn, run, time) %>% distinct()

#filter for periods on spawning grounds and sum time
spawn_d2 <- s_d %>% dplyr::group_by(tag_sn) %>% filter(spawn==1) %>% summarise(time_spawngrounds=sum(time))
mean(spawn_d2$time_spawngrounds) #sec

#convert from sec to hrs
spawn_d2$time_spawngrounds <- spawn_d2$time_spawngrounds/60 #min
spawn_d2$time_spawngrounds <- spawn_d2$time_spawngrounds/60 #hrs
spawn_d2$time_spawngrounds_hr <- as.numeric(spawn_d2$time_spawngrounds)

#join results to summary df 
summary <- left_join(summary, spawn_d2)


##### distance - RIVER KM ####

#load in distance df 
rx <- read.csv("2017 receiver distances rkm.csv")

#make another df for upstream movement (reverse)
rev_rx <- rx
rev_rx <- rev_rx %>% rename(order=1, up_dist_km=2, prev_order=3) #ex: now at 00 was at 07

#duplicate detection df 
data3 <- data2

#make all Shubie mouth receivers one station
data3$order <- as.character(data3$order)
data3$order[data3$area=="SH mouth"] <- "SH"

#make column for previous station
data3 <- data3 %>% arrange(tag_sn, detection_timestamp_utc) %>% group_by(tag_sn) %>% 
  mutate(prev_order=lag(order))

#remove double release detections
data3 <- data3 %>% filter(order != prev_order| order=="00ST") 

#add distances between receivers to detection history
data3 <- left_join(data3, rx, by=c("order", "prev_order"))
data3 <- left_join(data3, rev_rx, by=c("order", "prev_order"))

#put downstream and upstream distance into one column
data3 <- data3 %>% mutate(dist_km = coalesce(down_dist_km, up_dist_km))

#release distance = 0 
data3$dist_km[data3$receiver_sn=="release"] <- 0.00

#check if NAs where smolt went by a receiver undetected and edit in excel
is.na(data3$dist_km)

#sum distance travelled
dist_df <- data3 %>%
  group_by(tag_sn) %>%
  mutate(total_riverkm=sum(dist_km))

###now get time to calc speed ###

#format time 
dist_df$detection_timestamp_utc <- as.POSIXct(dist_df$detection_timestamp_utc, tz="UTC", format="%Y-%m-%d %H:%M:%S")

#calculate time between consecutive detections
dist_df <- dist_df %>% arrange(tag_sn, detection_timestamp_utc) %>% group_by(tag_sn) %>% 
  mutate(timediff=detection_timestamp_utc-lag(detection_timestamp_utc))

#convert dist from km to m to get m/s speed
dist_df <- dist_df %>% mutate(dist_m=dist_km*1000)

#calculate speed between each detection
dist_df$timediff <- as.numeric(dist_df$timediff)
dist_df <- dist_df %>% mutate(speed_ms=dist_m/timediff)

#remove detections at same receiver
dist_df2 <- dist_df %>% filter(dist_m != 0)

#dist, time, and speed b/w dets for river, no mobile, live and pred

#join to summary df
summary <- left_join(summary, dist_df2 %>% select(tag_sn, total_riverkm))
summary <- summary %>% distinct()


##### step length ####

#make copy of df with distances and speed
df <- dist_df2

df$num_station <- gsub("[A-Z]", "", df$order) #make column for just number from order
df$num_prev_station <- gsub("[A-Z]", "", df$prev_order)

df$num_station[df$area=="SH mouth"] <- 14 #set all SH mouth receivers to same number
df$num_prev_station[df$prev_order =="SH"] <- 14
df$num_station <- as.numeric(df$num_station)
df$num_prev_station <- as.numeric(df$num_prev_station)

# #subtract previous station number from current station
df <- df %>% mutate(direction = num_station - num_prev_station)

# #if negative, smolt is moving upstream, if not, moving downstream
df$direction <- ifelse(df$direction < 0, "upstream", "downstream")

#same checks as in reversal section for upstream of confluence
df$direction[df$num_station==10 & df$num_prev_station==6] <- "upstream"
df$direction[df$num_station==11 & df$num_prev_station==6] <- "upstream"
df$direction[df$num_station==11 & df$num_prev_station==5] <- "upstream"


df$direction_no <- ifelse(df$direction=="downstream", 0, 1) #down=0, up=1

#mark periods of downstream and upstream movement
df <- df %>%  arrange(tag_sn, detection_timestamp_utc) %>% group_by(tag_sn) %>%
  mutate(start = c(0, diff(direction_no) != 0), run=cumsum(start))

#sum distance for each period
df2 <- df %>% dplyr::group_by(tag_sn, direction, run) %>% dplyr::summarise(step_length=sum(dist_km)) 

#calculate max and average upstream distance travelled in a single period
dist_sum <- df2 %>% dplyr::group_by(tag_sn) %>% dplyr::filter(direction=="upstream") %>% 
  dplyr::summarise(max_up_distkm=max(step_length), avg_up_distkkm=mean(step_length))

#sum total upstream distance travelled
dist_sum <- left_join(dist_sum, df2 %>% group_by(tag_sn) %>% filter(direction=="upstream") %>% 
                        summarise(tot_up_distkm=sum(step_length)))

#no MB
#no mobile

#join to summary df
summary <- left_join(summary, dist_sum)


#speed up and down
#no MB
#no mobile

#calc speed avgs and max for downstreama and upstream

df3 <- df %>% filter(direction=="downstream") %>% group_by(tag_sn) %>% 
  summarize(avg_speed_down=mean(speed_ms))

df3 <- left_join(df3, df %>%  filter(direction=="upstream") %>% group_by(tag_sn) %>% 
                   summarize(avg_speed_up=mean(speed_ms)))


df3 <- left_join(df3,df %>% filter(direction=="downstream") %>% group_by(tag_sn) %>% 
                   summarize(max_speed_down=max(speed_ms)))

df3 <- left_join(df3,df %>% filter(direction=="upstream") %>% group_by(tag_sn) %>% 
                   summarize(max_speed_up=max(speed_ms)))

#join to summary df
summary <- left_join(summary, df3)

###### clean up summary df #####
colnames(summary)

#tag_sn - n=50
#num_days = total days with detections
#total_num_dets 
#max_det = max number of detections at a single receiver
#min_det = min number of detections at a single receiver
#num_conflu_dets = number of detections above stew/shubie confluence
#total_time_day = days b/w first and last detection in days
#num_reversals = number of times change in direction b/w upstream and downstream movement
#time_spawngrounds = time on bass spawning grounds in hrs
#total_riverkm - distance travelled pred and live
#speed (m/s)
#no MB
#avg up, , max up, avg down, max down
#step lengths - upstream distance travelled (km)
#no MB
#max up, avg up, total up
#migration rate - total dist travelled/total time alive (km/day)
summary$total_time_day <- as.numeric(summary$total_time_day)
summary$mig_rate <- summary$total_riverkm/summary$total_time_day

summary$time_spawngrounds <- as.numeric(summary$time_spawngrounds)
summary[is.na(summary)] <- 0 #make NAs 0

#write.csv(summary, "2017 behavioural metrics cut and no VR.csv", row.names = F)






##### Example code for k-means clustering #####

library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(remotes)
devtools::install_github("o1iv3r/FeatureImpCluster")
library(FeatureImpCluster)
library(flexclust)

#read in behavioural metrics
metrics <- read.csv("2017 behavioural metrics cut and no VR.csv")

#set seed to get same results
set.seed(41)

#make tag SN row name
metrics2 <- metrics %>% remove_rownames %>% column_to_rownames(var="tag_sn")

#centers (by mean) and scales (by sd) metrics
metrics2 <- scale(metrics2) 


#run k means cluster analysis
k <- kmeans(metrics2, centers=3) #centers (k) = 3 for the 3 fate groups 

#look at results
k

#visualize clusters 
?fviz_cluster
fviz_cluster(k, data=metrics2, geom="point", main="2017 Cluster plot") #cluster plot


#variable importance plots
res <- kcca(metrics2,k=3)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(metrics2))
plot(FeatureImp_res)


#make df with cluster number and tag SN
df <- data.frame(k$cluster) 
df <- rownames_to_column(df, "tag_sn")
#add cluster numbers to metrics df
metrics$tag_sn <- as.character(metrics$tag_sn)
metrics <- left_join(metrics, df)
#save df



##### Example code for random forest #####

#code adapted from https://github.com/StatQuest/random_forest_demo

library(ggplot2)
library(tidyverse)
#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library(caret)
#install.packages("ROCR")
library(ROCR)
#install.packages("pROC")
library(pROC)

#read in data
data <- read.csv("2017 behavioural metrics cut and no VR.csv")

#add fate column to metrics df
f <- read.csv("2017 detections cut.csv")
f <- f %>% select(tag_sn, fate_1) %>% distinct()
data <- left_join(data, f)

#set seed to get same results
set.seed(41)

#remove individuals with suspect fate from model training df and add to a new df
unknown <- data %>% filter(tag_sn %in% c(1262412, 1262413, 1262414, 1262418, 1262423, 1262432, 1262433, 1262435, 1262436, 1262439, 1262440, 1262441, 1262450, 1262455, 1262444, 1262446))
data2 <- data %>% filter(!tag_sn %in% c(1262412, 1262413, 1262414, 1262418, 1262423, 1262432, 1262433, 1262435, 1262436, 1262439, 1262440, 1262441, 1262450, 1262455, 1262444, 1262446))

#make tag SN row name
data2 <- data2 %>% remove_rownames %>% column_to_rownames(var="tag_sn")
unknown <- unknown %>% remove_rownames %>% column_to_rownames(var="tag_sn")

#run initial model
m1 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T) #default ntree=500
m1 #look at OOB and class error

#tune number of trees
#try model with ntree=1000
m2 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1000)
m2


#make df for changes in OOB and class error rates with change in ntree
m2_oob_error <- data.frame(
  Trees=rep(1:nrow(m2$err.rate), times=4),
  Type=rep(c("OOB", "M", "P", "S"), each=nrow(m2$err.rate)),
  Error=c(m2$err.rate[,"OOB"], 
          m2$err.rate[,"M"], 
          m2$err.rate[,"P"],
          m2$err.rate[,"S"]))

#plot df look for when error rates stabilize - if not stable at 1000 increase ntree and check again
ggplot(data=m2_oob_error, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
#unclear if stable at 1000, check 1500

#try ntree=1500
m3 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1500)
m3


m3_oob_error <- data.frame(
  Trees=rep(1:nrow(m3$err.rate), times=4),
  Type=rep(c("OOB", "M", "P", "S"), each=nrow(m3$err.rate)),
  Error=c(m3$err.rate[,"OOB"], 
          m3$err.rate[,"M"], 
          m3$err.rate[,"P"],
          m3$err.rate[,"S"]))

ggplot(data=m3_oob_error, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
#this shows stable at 500

#so use 1000

#no tune mtry (number of variables tried at each node) at ntree=1000
#default mtry is the square root of the number of variables

#calculate OOB error for mtry 1 to 10
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(fate_1~., data=data2, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
## find the optimal value for mtry (gives lowest OOB error)
which(oob.values == min(oob.values))
#2,3 both have min OOB error
#use 3

#have inbalanced classes (fate groups) so need to tune class weights

#run model with ntree=1000 and mtry=3 to check class error rates without class weights
m5 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1000, mtry=3)
m5

#now try different combinations of class weights - assigned to classes in alphabetical order (M,P,S)
#try to minimize and balance class error rates
m6 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1000, mtry=3, classwt=c(2,1,5))
m6
#same as m5

m7 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1000, mtry=3, classwt=c(2,1,10))
m7
#better

m8 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1000, mtry=3, classwt=c(5,2,20))
m8
#same as m7

m9 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1000, mtry=3, classwt=c(5,2,25))
m9
#same as m7

m10 <- randomForest(fate_1 ~ ., data2, proximity=T, importance=T, ntree=1000, mtry=3, classwt=c(10,1,100))
m10
#same

#m7 is best were getting
#use as FINAL MODEL

#ROC and AUC

#make df with number of votes for each fate group by smolt
m_v <- as.data.frame(m7$votes)
#col 1= M, 2=P, 3=S

#plot ROC curve for each class in layers
roc(data2$fate_1, m_v$M, plot=T, legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="red", 
    lwd=4, print.auc=TRUE)
plot.roc(data2$fate_1, m_v$P, percent=TRUE, col="blue", lwd=4, 
         print.auc=TRUE, add=TRUE, print.auc.y=35)
plot.roc(data2$fate_1, m_v$S, percent=TRUE, col="green", lwd=4,
         print.auc=TRUE, add=TRUE, print.auc.y=20)
legend("topleft", legend=c("M", "P", "S"), col=c("red", "blue", "green"), lwd=4, cex=0.6)


#use final model to predict fate class of the suspect smolts
m7predict <- predict(m7, unknown[,-18]) #remove original fate column 

#add predicted fate to df
unknown$predict_f <- m7predict


#variable importance plots 
varImpPlot(m7)



#partial dependence plots for most important variables

#pot probability of belonging to each fate class with change in variable
#layers for each class
partialPlot(m7, data2, total_riverkm, "P", col="blue", main="", 
            xlab="Distance travelled (river km)", ylab="class probability", ylim=c(-5.5,4))
partialPlot(m7, data2, total_riverkm, "M", col="red", add=T)
partialPlot(m7, data2, total_riverkm, "S", col="green", add=T)
legend("topright", legend=c("M", "P", "S"), col=c("red", "blue", "green"), lwd=2, cex=0.8)


