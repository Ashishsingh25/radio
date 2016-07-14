library(ggplot2)
library(dplyr)
library(sp)
library(geosphere)
setwd("/mnt/data/Saurabh/ashish/virgin/")
source("/mnt/data/Saurabh/ashish/Apple/Predata_fun.R")

#######################################################################################################
chg.count <- function(x,y){
  temp.return = data.frame()
  for(i in 1:nrow(y)){
    temp = x[x$Mac_id == y[i,1],]
    temp.return[i,1] = y[i,1]
    temp.return[i,2] = sum(diff((temp$floor_id))!= 0)
  }
  names(temp.return)[1:2] <- c("Mac.id","chg")
  #temp.return<- temp.return[with(temp.return, order(-chg)),]
  return(temp.return)
}

time.check <- function(x){
  temp.visitor.table = subset(as.data.frame(table(x$Mac_id)),Freq>0)
  for(i in 1:nrow(temp.visitor.table)){
    temp.user = x[x$Mac_id == temp.visitor.table[i,1],]
    temp.visitor.table[i,3] = length(unique(temp.user$timestamp)) - nrow(temp.user)
  }
  names(temp.visitor.table)[1:3] <- c("Mac.id","Freq","check")
  temp.visitor.table<- temp.visitor.table[with(temp.visitor.table, order(-check)),]
  return(temp.visitor.table)
}

inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}

set.session <- function(user.x){
  count = 1
  user.x$session.id[1] = count
  for(i in 2:nrow(user.x)){
    if(user.x$floor_id[i-1] != user.x$floor_id[i]){
      inc(count)
    }
    user.x$session.id[i] = count 
  }
  return(user.x)
}

get.session <- function(user.x){
  session.table = subset(as.data.frame(table(user.x$session.id)),Freq>0)
  # session.table <- session.table[with(session.table, order(-Freq)),]
  user.session = data.frame()
  for(i in 1:nrow(session.table)){
    temp = user.x[user.x$session.id == session.table[i,1],]
    user.session[i,1] = session.table[i,1]
    user.session[i,2] = temp$floor_id[1]
    user.session[i,3] = temp$Mac_id[1]
    if(session.table[i,2]==1){
      user.session[i,4]=temp$timestamp[1]
      user.session[i,5]=temp$timestamp[1]
    }
    else{
      user.session[i,4]=min(temp$timestamp)
      user.session[i,5]=max(temp$timestamp)
    }
    user.session[i,6]= diff(range(temp$timestamp))
    user.session[i,7] = nrow(temp)
  }
  names(user.session)[1:7] <- c("session.id","floor.id","Mac_id","Start","End","duration","no.tuples")
  return(user.session)
}

get.distance <- function(x){
  return.temp = data.frame()
  visitor.table = as.data.frame(unique(x$Mac_id))
  names(visitor.table)[1] = "Mac_id"
  count = 0
  for(i in visitor.table$Mac_id){
    user.x = x[x$Mac_id == i,]
    user.x$x.diff <- c(0, diff(user.x$X))
    user.x$y.diff <- c(0, diff(user.x$Y))
    for(i in 1:nrow(user.x)){
      user.x[i,10] <- sqrt(sum(user.x$x.diff[i]^2 + user.x$y.diff[i]^2))
      temp <- atan2(user.x$y.diff[i],user.x$x.diff[i])
      user.x[i,11] <- temp*180/pi
    }
    names(user.x)[10:11]=c("distance","angle")
    return.temp = rbind(return.temp,user.x)
    inc(count)
    print(count)
  }
  return(return.temp)
}

visitor.in.poly <- function(x,y){
  temp = ncol(x)+1
  for(i in 1:nrow(x)){
    x[i,temp] = point.in.polygon(x$X[i],x$Y[i],y$x.cord,y$y.cord)
  }
  print("tuples marked")
  names(x)[temp] = "pip"
  poly.table = subset(as.data.frame(table(x$pip)),Freq>0)
  temp2 = x[!x$pip == 0 ,]
  user.table = as.data.frame(unique(temp2$Mac_id))
  names(user.table)[1] = "Mac_id"
  return.temp = data.frame()
  for(i in user.table$Mac_id){
    temp3 = x[x$Mac_id == i,]
    return.temp = rbind(return.temp, temp3)
  }
  return(return.temp)
}

# euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))


#######################################################################################################
ap.1 = read.csv("chesham_ap_floor_1.txt",header=FALSE,quote = "",stringsAsFactors = FALSE)
names(ap.1)[1:4]<-c("Access_point","floor_id","X","Y")
ap.1$floor_id = 1
ap.2 = read.csv("chesham_ap_floor_2.txt",header=FALSE,quote = "",stringsAsFactors = FALSE)
names(ap.2)[1:4]<-c("Access_point","floor_id","X","Y")
ap.2$floor_id = 2
cp.1 = read.csv("chesham_cp_floor_1.txt",header=FALSE,quote = "",stringsAsFactors = FALSE)
names(cp.1)[1:4]<-c("Calb_point","calb_id","X","Y")
cp.1$floor_id = 1
cp.2 = read.csv("chesham_cp_floor_2.txt",header=FALSE,quote = "",stringsAsFactors = FALSE)
names(cp.2)[1:4]<-c("Calb_point","calb_id","X","Y")
cp.2$floor_id = 2
data.full = read.csv("chesham.dat",header=FALSE,quote = "",stringsAsFactors = FALSE)
names(data.full)[1:6]<-c("Venue","floor_id","Mac_id","X","Y","timestamp")
data.full <- data.full[with(data.full, order(Mac_id,timestamp)),]
data.full$ID<-seq.int(nrow(data.full))
data.12 = data.full[!data.full$floor_id == 0,]

########################## Timestamp check ############################################################

data.12.time = time.check(data.12)
unique(data.12.time$check)
rm(data.12.time)
data.full.time = time.check(data.full)
unique(data.full.time$check)
rm(data.full.time)

#######################################################################################################

floor.0 = data.full[data.full$floor_id == 0,]
floor.1 = data.full[data.full$floor_id == 1,]
floor.2 = data.full[data.full$floor_id == 2,]

visitor.1 = subset(as.data.frame(table(floor.1$Mac_id)),Freq>0)
visitor.1$Freq=NULL
visitor.2 = subset(as.data.frame(table(floor.2$Mac_id)),Freq>0)
visitor.2$Freq = NULL
visitor.common = merge(visitor.1,visitor.2)
visitor.1.uniq = anti_join(visitor.1,visitor.common)
visitor.2.uniq = anti_join(visitor.2,visitor.common)
visitor.chg = chg.count(data.12,visitor.common)

session.full = data.frame()
for(i in visitor.common$Var1){
  user.x = data.12[data.12$Mac_id == i,]
  user.x = set.session(user.x)
  user.session = get.session(user.x)
  session.full = rbind(session.full,user.session)
}
session.duration.table = subset(as.data.frame(table(session.full$duration)))

user.x = data.12[data.12$Mac_id == "\"0026B0284FB8\"",]
user.x = set.session(user.x)
user.session = get.session(user.x)

############################## Get distance and #################################################################

# user.x = data.12[data.12$Mac_id == "\"0026B0284FB8\"",]
# user.x$x.diff <- c(0, diff(user.x$X))
# user.x$y.diff <- c(0, diff(user.x$Y))
# for(i in 1:nrow(user.x)){
#   user.x[i,10] <- sqrt(sum(user.x$x.diff[i]^2 + user.x$y.diff[i]^2))
#   temp <- atan2(user.x$y.diff[i],user.x$x.diff[i])
#   user.x[i,11] <- temp*180/pi
# }
# names(user.x)[10]="distance"

data.v2 = get.distance(data.12)
#data.v2 = format(data.v2,scientific=TRUE) 

########################### Point in polygon #####################################################################

x.cord = c(800,1000,1000,800,800)
y.cord = c(200,200,400,400,200)

poly.cord = data.frame(x.cord,y.cord)
tuples.poly=temp
visit.poly = visitor.in.poly(data.v2 , poly.cord)
visit.poly = visit.poly[visit.poly$floor_id==2,]

temp = visit.poly[visit.poly$Mac_id == "\"00102056ECE3\"", ]



