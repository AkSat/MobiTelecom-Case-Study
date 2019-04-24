library(dplyr)
library(ggplot2)
library(car)
library(glue)
library(caret)

setwd("F:/Data Science in R - Topics/Student Files jig10122_Lab/Student Files/Final Case Study")
list.files(pattern = ".csv")

telecom <- read.csv("telecomfinal.csv",sep = ",",header = TRUE)

#Data quality report

nRows <- nrow(telecom)
nCols <- ncol(telecom)

varNames <- 1
varDataType <- 1
noRecords <- 1
MissingVal <- 1
UniqueVal <- 1
MinVal <- 1
MaxVal <- 1
MeanVal <- 1
pl5 <- 1
pl10 <- 1
pl25 <- 1
pl50 <- 1
pl75 <- 1
pl90 <- 1
pl95 <- 1
q1 <- 1


#retdays is an important factor and cannot be deleted due to its increased
#number of missing values

#as per data documentation,
# "Missing values for this variable can be assumed to mean there have been 
# no retention calls made by the customer."

#Hence replacing the missing values with 0 retention calls
summary(telecom$retdays)
unique(telecom$retdays)
getIND <- which(is.na(telecom$retdays))
telecom$retdays[getIND] <- 0





for(i in 1:nCols){
  getColName <- names(telecom)[i]
  getValue <- telecom[,getColName]
  
  varNames[i] <- names(telecom)[i]
  varDataType[i] <- class(getValue)
  noRecords[i] <- length(telecom[,getColName])
  MissingVal[i] <- length(which(is.na(telecom[,getColName])))
  UniqueVal[i] <- length(unique(telecom[,getColName]))
  MinVal[i] <- ifelse(class(getValue)=="integer"|class(getValue)=="numeric",
                      min(telecom[,getColName],na.rm = TRUE),0)
  MaxVal[i] <- ifelse(class(getValue)=="integer"|class(getValue)=="numeric",
                      max(telecom[,getColName],na.rm = TRUE),0)
  MeanVal[i] <- ifelse(class(getValue)=="integer"|class(getValue)=="numeric",
                       mean(telecom[,getColName],na.rm = TRUE),0)
  
  
  if((class(getValue)=="integer"|class(getValue)=="numeric")){
    q1 <- quantile(getValue,p=(1:100)/100,na.rm = TRUE)
    pl5[i] <- round(q1[[5]],2)
    pl10[i] <- round(q1[[10]],2)
    pl25[i] <- round(q1[[25]],2)
    pl50[i] <- round(q1[[50]],2)
    pl75[i] <- round(q1[[75]],2)
    pl90[i] <- round(q1[[90]],2)
    pl95[i] <- round(q1[[95]],2)
  }
  else
  {
    pl5[i] <- 0
    pl10[i] <- 0
    pl25[i] <- 0
    pl50[i] <- 0
    pl75[i] <- 0
    pl90[i] <- 0
    pl95[i] <- 0
  }
  
}

dataAvailable <- noRecords - MissingVal
availPerc <- round((dataAvailable/noRecords),6)
missingPerc <- round((MissingVal/noRecords),6)


qualityReport <- data.frame("VariableName" = varNames,"DataType" = varDataType,
                            NoOfRecords = noRecords,DataAvailable = dataAvailable,
                            AvailablePerc = availPerc, MissingPerc = missingPerc,
                            MissingValues = MissingVal,UniqueValues = UniqueVal,
                            Min = round(MinVal,2), Max = MaxVal,
                            Mean = round(MeanVal,2),
                            `5th Percentile` = pl5, `10th Percentile` = pl10,
                            `25th Percentile` = pl25,`50th Percentile` = pl50,
                            `75th Percentile` = pl75,`90th Percentile` = pl90,
                            `95th Percentile` = pl95) 


write.csv(qualityReport,
          "F://Final Case Study//Telecom_Quality_Report.csv",
          row.names = FALSE)


dim(telecom)






#Variables with a lot of missing values are omitted from the analysis
#Considering the ideal 70% figure, variables having over 70% missing values
#will be removed from our analysis data




t1 <- telecom


getOmitVars <- qualityReport$VariableName[which(qualityReport$MissingPerc <= 0.30)]
getOmitVars <- as.character(unlist(getOmitVars))

getOmitVars1 <- qualityReport$VariableName[which(qualityReport$MissingPerc > 0.30)]


varIND <- which(names(t1) %in%  getOmitVars)
t1 <- t1[,varIND]


#sort(names(t1))


#get categorical variables:
nCols <- ncol(t1)
getFactorVars <- as.character(nRows)
k <- 1
for(j in 1:nCols){
  getColName <- names(t1)[j]
  getValue <- t1[,getColName]
  
  if(class(getValue)=="factor")
  {
    getFactorVars[k] <- getColName
    k <- k + 1
  }
}



#get categorical variables:
nCols <- ncol(t1)
getContiVars <- as.character(nRows)
k1 <- 1
for(j1 in 1:nCols){
  getColName <- names(t1)[j1]
  getValue <- t1[,getColName]
  
  if(class(getValue)=="numeric" | class(getValue) == "integer")
  {
    getContiVars[k1] <- getColName
    k1 <- k1 + 1
  }
}



catVars <- sort(getFactorVars)
#,.......earlier 21 categorical variables

contiVars <- sort(getContiVars)



#Profiling on categorical variables 


# ........................... Variable : AREA ..........................
unique(t1$area)

neInd <- which(t1$area %in% c("PHILADELPHIA AREA","NEW ENGLAND AREA",
                              "NEW YORK CITY AREA"))
mwInd <- which(t1$area %in% c("OHIO AREA","GREAT LAKES AREA","CHICAGO AREA"))

sInd <- which(t1$area %in% c("PHILADELPHIA AREA",
                             "DC/MARYLAND/VIRGINIA AREA","SOUTH FLORIDA AREA",
                             "DALLAS AREA","HOUSTON AREA","CENTRAL/SOUTH TEXAS AREA","ATLANTIC SOUTH AREA",
                             "TENNESSEE AREA","NORTH FLORIDA AREA"))

wInd <- which(t1$area %in% c("LOS ANGELES AREA","CALIFORNIA NORTH AREA",
                             "NORTHWEST/ROCKY MOUNTAIN AREA"))

naIND <- which(is.na(t1$area))

t1$area1 <- as.character(t1$area)
t1$area1[neInd] <- as.character("NORTHEAST AREA")
t1$area1[mwInd] <- as.character("MIDWEST AREA")
t1$area1[sInd] <- as.character("SOUTH AREA")
t1$area1[wInd] <- as.character("WEST AREA")
t1$area1[naIND] <- as.character("MISSING")
t1$area1 <- as.factor(t1$area1)
unique(t1$area1)


names(t1)
t1 <- t1[,-34]




# ........................... Variable : ASL_FLAG ..........................
unique(t1$asl_flag)
t1 %>% count(churn,levels = asl_flag) %>% filter(churn==1) -> dASL
dASL$N <- unclass(t1%>% filter(asl_flag %in% dASL$levels) %>% count(asl_flag))[[2]]
dASL$churn_rate <- dASL$n/dASL$N
dASL$VarName <- rep("asl_flag",nrow(dASL))

dASL <- as.data.frame(dASL)

# t <- table(t1$churn,t1$asl_flag)
# t2 <- data.frame(t)
# names(t2) <- c("churn","asl_flag","n")
# t3 <- t2 %>% filter(churn==1)
# t3$Total = unclass(t2 %>% group_by(asl_flag) %>% summarise(N=sum(n)))[["N"]]


# ........................... Variable : CAR_BUY ..........................
unique(t1$car_buy)

t1 %>% count(churn,levels = car_buy) %>% filter(churn==1) -> dCB
dCB$N <- unclass(t1%>% filter(car_buy %in% dCB$levels) %>% count(car_buy))[[2]]
dCB$churn_rate <- dCB$n/dCB$N
dCB$VarName <- rep("car_buy",nrow(dCB))

dCB <- as.data.frame(dCB)


#check no. of missing values for car_buy - 1% of data
#deleting the missing values
indCB <- which(is.na(t1$car_buy))
t1  <- t1[-indCB,]







# ........................... Variable : crclscod ..........................
summary(t1$crclscod)
sort(unique(t1$crclscod))
t1 %>% count(churn,levels = crclscod) %>% filter(churn==1) -> dCRC
dCRC$N <- unclass(t1%>% filter(crclscod %in% dCRC$levels) %>% count(crclscod))[[2]]
dCRC$churn_rate <- dCRC$n/dCRC$N
dCRC$VarName <- rep("crclscod",nrow(dCRC))

dCRC_df <- data.frame(dCRC) 


#combining levels by frequency rate

t1 %>% group_by(crclscod) %>% summarize(freq = n()) %>% arrange(desc(freq)) -> c1
c1$Perc <- c1$freq/ sum(c1$freq) * 100

table(t1$churn)/nrow(t1)

c1$class <- ifelse(c1$Perc <= 5,"Worst","Best")




t2 <- data.frame("crclscod" = t1$crclscod)
t2$order <- seq(1:nrow(t2))
dCRC1 <- data.frame("crclscod" = c1$crclscod,CClass = c1$class)
m1 <- merge(x = t2,y = dCRC1,by.x = "crclscod",all.x = TRUE)
m1 <- m1[order(m1$order),]

colSums(is.na(m1))

t1$crclscod1 <- m1$CClass
class(t1$crclscod1)

colSums(is.na(t1))

names(t1)
t1 <- t1[,-31]





# ........................... Variable : csa ..........................
unique(t1$csa)
getIND <- which(is.na(t1$csa))


t1 %>% count(churn,levels = csa) %>% filter(churn==1) -> dCSA
dCSA$N <- unclass(t1%>% filter(csa %in% dCSA$levels) %>% count(csa))[[2]]
dCSA$churn_rate <- dCSA$n/dCSA$N 
dCSA$VarName <- rep("csa",nrow(dCSA))
dCSA <- as.data.frame(dCSA)

getIND <- which(is.na(t1$csa))

t1$csa1 <- ifelse(is.na(t1$csa),"Missing",as.character(t1$csa))
t1$csa1 <- as.factor(t1$csa1)


#removing the column from analysis as it does not give much information
names(t1)
t1 <- t1[,-54]



# ........................... Variable : ethnic ..........................

unique(t1$ethnic)
t1 %>% count(churn,levels = ethnic) %>% filter(churn==1) -> de
de$N <- unclass(t1%>% filter(ethnic %in% de$levels) %>% count(ethnic))[[2]]
de$churn_rate <- round(de$n/de$N,2)
de$VarName <- rep("ethnic",nrow(de))
de <- as.data.frame(de)

t1$ethnic1 <- as.factor(t1$ethnic)
names(t1)
t1 <- t1[,-36]




# ........................... Variable : hnd_webcap ..........................

unique(t1$hnd_webcap)
t1 %>% count(churn,levels = hnd_webcap) %>% filter(churn==1) -> dw
dw$N <- unclass(t1%>% filter(hnd_webcap %in% dw$levels) %>% count(hnd_webcap))[[2]]
dw$churn_rate <- round(dw$n/dw$N,2)
dw$VarName <- rep("hnd_webcap",nrow(dw))
dHNDw <- as.data.frame(dw)

#the churn rates are different for all
#Hence replacing NA with value "Missing"

t1$hnd_webcap1 <- ifelse(is.na(t1$hnd_webcap),"Missing",as.character(t1$hnd_webcap))
t1$hnd_webcap1 <- as.factor(t1$hnd_webcap1)
names(t1)
t1 <- t1[,-34]

nrow(t1)




# ........................... Variable : marital ..........................

unique(t1$marital)
t1 %>% count(churn,levels = marital) %>% filter(churn==1) -> dm
dm$N <- unclass(t1%>% filter(marital %in% dm$levels) %>% count(marital))[[2]]
dm$churn_rate <- round(dm$n/dm$N,2)
dm$VarName <- rep("marital",nrow(dm))
dmarital <- as.data.frame(dm)

#creating dummy variables due to similar event rates

table(t1$marital)

unique(trim(t1$marital))

names(t1)

t1$marital1 <- as.factor(t1$marital)
names(t1)
t1 <- t1[,-34]



# ........................... Variable : prizm_social_one ..........................

unique(t1$prizm_social_one)

str(t1$prizm_social_one)
t1 %>% count(churn,levels = prizm_social_one) %>% filter(churn==1) -> dp
dp$N <- unclass(t1%>% filter(prizm_social_one %in% dp$levels) %>% count(prizm_social_one))[[2]]
dp$churn_rate <- round(dp$n/dp$N,3)
dp$VarName <- rep("prizm_social_one",nrow(dp))
dpso <- as.data.frame(dp)

#The churn rate for missing values is same as that of level 'T'. Hence, imputing.

t1$prizm_social_one1 <- t1$prizm_social_one
t1$prizm_social_one1 <- ifelse(is.na(t1$prizm_social_one1),"T",as.character(t1$prizm_social_one1))

unique(t1$prizm_social_one1)
t1 %>% count(churn,levels = prizm_social_one1) %>% filter(churn==1) -> dp
dp$N <- unclass(t1%>% filter(prizm_social_one1 %in% dp$levels) %>% count(prizm_social_one1))[[2]]
dp$churn_rate <- round(dp$n/dp$N,3)
dp$VarName <- rep("prizm_social_one1",nrow(dp))


names(t1)

t1 <- t1[,-32]





# ........................... Variable : refurb_new ..........................

unique(t1$refurb_new)
t1 %>% count(churn,levels = refurb_new) %>% filter(churn==1) -> drn
drn$N <- unclass(t1%>% filter(refurb_new %in% drn$levels) %>% count(refurb_new))[[2]]
drn$churn_rate <- round(drn$n/drn$N,3)
drn$VarName <- rep("refurb_new",nrow(drn))
drfn <- as.data.frame(drn)

#since there is only one missing value , we delete the record

drnIND <- which(is.na(t1$refurb_new))
t1 <- t1[-drnIND,]
nrow(t1)





####################............. Profiling Continuous Variables ..........##################



# ........................... Variable : actvsubs..........................

summary(t1$actvsubs)

d1 <- t1 %>% mutate(dec = ntile(actvsubs,2)) %>% count(dec,churn) %>%
  filter(churn == 1) 
d1$N <- unclass(t1 %>% mutate(dec = ntile(actvsubs,2)) %>% count(dec) %>%
                  unname())[[2]]

t1$actvsubs1 <- as.factor(t1$actvsubs)
unique(t1$actvsubs1)


t1 %>% count(churn,levels = actvsubs) %>% filter(churn==1) -> da
da$N <- unclass(t1%>% filter(actvsubs %in% da$levels) %>% count(actvsubs))[[2]]
da$churn_rate <- round(da$n/da$N,3)
da$VarName <- rep("actvsubs",nrow(da))
dactv <- as.data.frame(da)

names(t1)

t1 <- t1[,-37]






# ........................... Variable : age1..........................

d1 <- t1 %>% mutate(dec = ntile(age1,4)) %>% count(dec,churn) %>%
  filter(churn == 1)
d1$N <- unclass(t1 %>% mutate(dec = ntile(age1,4)) %>% count(dec) %>% unname())[[2]]
d1$churn_perc <- d1$n/d1$N
d1$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(age1,4)) %>% group_by(dec) %>%
                            summarize(min(age1)))[[2]]
d1$LessThan <- unclass(t1 %>% mutate(dec = ntile(age1,4)) %>% group_by(dec) %>%
                         summarize(max(age1)))[[2]]
d1$varName <- rep("age1",nrow(d1))

t1$age1_N <- ifelse(t1$age1 == 0,"Default",
                    ifelse((t1$age1>0 & t1$age1<=36),"Young",
                           ifelse((t1$age1>36 & t1$age1<=48),"Middle","Old")))

dAGE1 <- as.data.frame(d1)

summary(t1$age1)

names(t1)
t1 <- t1[,-33]




# ........................... Variable : age2..........................

# t1 %>% mutate(dec = ntile(age2,3)) %>% group_by(dec,churn) %>% summarize(n()) %>%
#   filter(churn==1)

# t1 %>% mutate(dec = ntile(age2,3)) %>% group_by(dec) %>% summarize(n()) 

d2 <- unclass(t1 %>% mutate(dec = ntile(age2,3)) %>% group_by(dec) %>%
                summarize(min(age2)))[[2]]

d1$New <- d2


d1 <- t1 %>% mutate(dec = ntile(age2,3)) %>% count(dec,churn) %>%
  filter(churn == 1)
d1$N <- unclass(t1 %>% mutate(dec = ntile(age2,3)) %>% count(dec) %>% unname())[[2]]
d1$churn_perc <- d1$n/d1$N
d1$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(age2,3)) %>% group_by(dec) %>%
                            summarize(min(age2)))[[2]]
d1$LessThan <- unclass(t1 %>% mutate(dec = ntile(age2,3)) %>% group_by(dec) %>%
                         summarize(max(age2)))[[2]]
d1$varName <- rep("age2",nrow(d1))
dAGE2 <- as.data.frame(d1)

summary(t1$age2)
t1$age2_N <- ifelse(t1$age2 == 0,"Default",
                    ifelse((t1$age2>0 & t1$age2<=42),"Young-Middle","Old"))
# t1$age2_N <- ifelse(t1$age2 == 0,"Default",
#                     ifelse((t1$age2>0 & t1$age2<=36),"Young",
#                            ifelse((t1$age2>36 & t1$age2<=48),"Middle","Old")))

names(t1)
t1 <- t1[,-33]




# ........................... Variable : forgntvl..........................

unique(t1$refurb_new)
summary(t1$forgntvl)
t1$forgntvl1 <- as.factor(t1$forgntvl)
t1$forgntvl1 <-factor(t1$forgntvl1 ,levels = 0:1,labels = c("No","Yes"))

names(t1)
t1 <- t1[,-36]



# ........................... Variable : hnd_price..........................

str(t1$hnd_price)#...........bin the data
summary(t1$hnd_price)
sort(unique(t1$hnd_price))

d1 <- t1 %>% mutate(dec = ntile(hnd_price,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d1$N <- unclass(t1 %>% mutate(dec = ntile(hnd_price,10)) %>% count(dec) %>% 
                  unname())[[2]]
d1$churn_perc <- d1$n/d1$N
d1$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(hnd_price,10)) %>% 
                            group_by(dec) %>% summarize(min(hnd_price)))[[2]]
d1$LessThan <- unclass(t1 %>% mutate(dec = ntile(hnd_price,10)) %>% 
                         group_by(dec) %>% summarize(max(hnd_price)))[[2]]
d1$varName <- rep("hnd_price",nrow(d1))
dHNDp <- as.data.frame(d1)

quantile(t1$hnd_price,p=(0:10)/10,na.rm = TRUE)



#impute NA values by taking 5th decile value, i.e between 40% and 50%

t1$hnd_price1 <- t1$hnd_price
getIND <- which(is.na(t1$hnd_price1))
t1$hnd_price1[getIND] <- 89.98990



which(is.na(t1$hnd_price1))
t1$hnd_price1 <- as.factor(t1$hnd_price1)

t1 %>% count(churn,levels = hnd_price1) %>% filter(churn==1) -> dh
dh$N <- unclass(t1%>% filter(hnd_price1 %in% dh$levels) %>% count(hnd_price1))[[2]]
dh$churn_rate <- (dh$n/dh$N)
dh$VarName <- rep("hnd_price1",nrow(dh))
dhp <- as.data.frame(dh)


summary(t1$hnd_price)
names(t1)

t1 <- t1[,-34]







# ........................... Variable : income ..........................
summary(t1$income)
sort(unique(t1$income))

t1$income1 <- as.factor(t1$income)

t1 %>% count(churn,levels = income1) %>% filter(churn==1) -> dh
dh$N <- unclass(t1%>% filter(income1 %in% dh$levels) %>% count(income1))[[2]]
dh$churn_rate <- (dh$n/dh$N)
dh$VarName <- rep("income1",nrow(dh))
dincome <- as.data.frame(dh)

getIND <- which(is.na(t1$income1))
t1$income1[getIND] <- "7"


table(t1$income)

#since  the churn rates are all similar,we create dummy for each

# t1$income1_1 <- ifelse(t1$income1 == "1",1,0)
# t1$income1_2 <- ifelse(t1$income1 == "2",1,0)
# t1$income1_3 <- ifelse(t1$income1 == "3",1,0)
# t1$income1_4 <- ifelse(t1$income1 == "4",1,0)
# t1$income1_5 <- ifelse(t1$income1 == "5",1,0)
# t1$income1_6 <- ifelse(t1$income1 == "6",1,0)
# t1$income1_7 <- ifelse(t1$income1 == "7",1,0)
# t1$income1_8 <- ifelse(t1$income1 == "8",1,0)
# t1$income1_9 <- ifelse(t1$income1 == "9",1,0)
# 
# 
# t1$income1_1 <- factor(t1$income1_1,levels = 0:1,labels = c("No","Yes"))
# t1$income1_2 <- factor(t1$income1_2,levels = 0:1,labels = c("No","Yes"))
# t1$income1_3 <- factor(t1$income1_3,levels = 0:1,labels = c("No","Yes"))
# t1$income1_4 <- factor(t1$income1_4,levels = 0:1,labels = c("No","Yes"))
# t1$income1_5 <- factor(t1$income1_5,levels = 0:1,labels = c("No","Yes"))
# t1$income1_6 <- factor(t1$income1_6,levels = 0:1,labels = c("No","Yes"))
# t1$income1_7 <- factor(t1$income1_7,levels = 0:1,labels = c("No","Yes"))
# t1$income1_8 <- factor(t1$income1_8,levels = 0:1,labels = c("No","Yes"))
# t1$income1_9 <- factor(t1$income1_9,levels = 0:1,labels = c("No","Yes"))
# 




names(t1)

t1 <- t1[,-12]
sort(names(t1))






# ........................... Variable : models ..........................
plot(t1$hnd_price1,t1$totrev)

summary(t1$models)
unique(t1$models)
length(which(is.na(t1$models)))

t1$models1 <- as.factor(t1$models)
plot(t1$models1,t1$totrev)

t1 %>% count(churn,levels = models1) %>% filter(churn==1) -> dMod
dMod$N <- unclass(t1%>% filter(models1 %in% dMod$levels) %>% count(models1))[[2]]
dMod$churn_rate <- (dMod$n/dMod$N)
dMod$VarName <- rep("models1",nrow(dMod))
dMod <- as.data.frame(dMod)
dMod %>% arrange(churn_rate)

unique(t1$models1)



names(t1)

t1 <- t1[,-32]




# ........................... Variable : mtrcycle ..........................
unique(t1$mtrcycle)
t1$mtrcycle1 <- as.factor(t1$mtrcycle)
t1$mtrcycle1<-factor(t1$mtrcycle1,levels = 0:1,labels = c("No","Yes"))


# ........................... Variable : truck ..........................
unique(t1$truck)
t1$truck1<- as.factor(t1$truck)
t1$truck1<-factor(t1$truck1,levels = 0:1,labels = c("No","Yes"))


names(t1)

t1 <- t1[,-c(34,36)]



# ........................... Variable : uniqsubs ..........................
unique(t1$uniqsubs)

t1$uniqsubs1 <- as.factor(t1$uniqsubs)

t1 %>% count(churn,levels = uniqsubs1) %>% filter(churn==1) -> du
du$N <- unclass(t1%>% filter(uniqsubs1 %in% du$levels) %>% count(uniqsubs1))[[2]]
du$churn_rate <- (du$n/du$N)
du$VarName <- rep("uniqsubs1",nrow(du))
duniqS <- as.data.frame(du)
du %>% arrange(churn_rate)


names(t1)

t1 <- t1[,-32]





##.........................Creating dummies : ..............................

# t1 %>% count(churn,levels = hnd_price1) %>% filter(churn==1) -> d1
# d1$N <- unclass(t1%>% filter(hnd_price1 %in% d1$levels) %>% count(hnd_price1))[[2]]
# d1$churn_rate <- (d1$n/d1$N)
# d1$VarName <- rep("hnd_price1",nrow(d1))
# d1 <- as.data.frame(d1)
# d1 %>% arrange(churn_rate)
# 
# sort(unique(t1$hnd_price1))
# class(t1$hnd_price1)
# 
# unique(t1$actvsubs1)
# # t1$actvsubs1 <- ifelse(t1$actvsubs1 == "7" | t1$actvsubs1 == "11","7-11",
# #                        as.character(t1$actvsubs1))
# #
# # t1$hnd_price1 <- ifelse(t1$hnd_price1 == "119.9899902"|t1$hnd_price1 == "159.9899902",
# #                        "119or159",
# #                        as.character(t1$hnd_price1))
# #
# # t1$actvsubs1 <- as.factor(t1$actvsubs1)
# # t1$hnd_price1 <- as.factor(t1$hnd_price1)
# 
# # aa2 <- as.data.frame(table(actvsubs2))
# aa <- as.data.frame(table(t1$actvsubs1))
# 
# 
# hh <- as.data.frame(table(t1$hnd_price1))
# 
# 
# getActvSubs <- aa[aa$Freq == 1,]$Var1
# getAS_ind <- which(t1$actvsubs1 %in% getActvSubs)
# 
# t1 <- t1[-getAS_ind,]
# 
# getHDPrice <- hh[hh$Freq == 1,]$Var1
# getHNP_ind <- which(t1$hnd_price1 %in% getHDPrice)
# 
# t1 <- t1[-getHNP_ind,]
# 
# sort(unique(t1$hnd_price1))
# sort(unique(t1$actvsubs1))
# 
# t1$hnd_price1_9.9899 <- ifelse(t1$hnd_price1 == "9.989997864",1,0)
# t1$hnd_price1_29.9899 <- ifelse(t1$hnd_price1 == "29.98999023",1,0)
# t1$hnd_price1_39.9899 <- ifelse(t1$hnd_price1 == "39.98999023",1,0)
# t1$hnd_price1_59.9899 <- ifelse(t1$hnd_price1 == "59.98999023",1,0)
# t1$hnd_price1_79.9899 <- ifelse(t1$hnd_price1 == "79.98999023",1,0)
# t1$hnd_price1_89.9899 <- ifelse(t1$hnd_price1 == "89.9899",1,0)
# t1$hnd_price1_99.9899 <- ifelse(t1$hnd_price1 == "99.98999023",1,0)
# t1$hnd_price1_129.9899 <- ifelse(t1$hnd_price1 == "129.9899902",1,0)
# t1$hnd_price1_149.9899 <- ifelse(t1$hnd_price1 == "149.9899902",1,0)
# t1$hnd_price1_179.9899 <- ifelse(t1$hnd_price1 == "179.9899902",1,0)
# t1$hnd_price1_199.9899 <- ifelse(t1$hnd_price1 == "199.9899902",1,0)
# t1$hnd_price1_239.9899 <- ifelse(t1$hnd_price1 == "239.9899902",1,0)
# t1$hnd_price1_249.9899 <- ifelse(t1$hnd_price1 == "249.9899902",1,0)
# t1$hnd_price1_299.9899 <- ifelse(t1$hnd_price1 == "299.9899902",1,0)
# t1$hnd_price1_399.9899 <- ifelse(t1$hnd_price1 == "399.9899902",1,0)
# t1$hnd_price1_499.9899 <- ifelse(t1$hnd_price1 == "499.9899902",1,0)
# t1$hnd_price1_499.9899 <- ifelse(t1$hnd_price1 == "499.9899902",1,0)
# #t1$hnd_price1_119or159 <- ifelse(t1$hnd_price1 == "119or159",1,0)
# 
# 
# 
# t1$hnd_price1_9.9899<-factor(t1$hnd_price1_9.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_29.9899<-factor(t1$hnd_price1_29.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_39.9899<-factor(t1$hnd_price1_39.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_59.9899<-factor(t1$hnd_price1_59.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_79.9899<-factor(t1$hnd_price1_79.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_89.9899<-factor(t1$hnd_price1_89.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_99.9899<-factor(t1$hnd_price1_99.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_129.9899<-factor(t1$hnd_price1_129.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_149.9899<-factor(t1$hnd_price1_149.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_179.9899<-factor(t1$hnd_price1_179.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_199.9899<-factor(t1$hnd_price1_199.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_239.9899<-factor(t1$hnd_price1_239.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_249.9899<-factor(t1$hnd_price1_249.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_299.9899<-factor(t1$hnd_price1_299.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_399.9899<-factor(t1$hnd_price1_399.9899,levels = 0:1,labels = c("No","Yes"))
# t1$hnd_price1_499.9899<-factor(t1$hnd_price1_499.9899,levels = 0:1,labels = c("No","Yes"))
# #t1$hnd_price1_119or159<-factor(t1$hnd_price1_119or159,levels = 0:1,labels = c("No","Yes"))
# 
# #
# #
# # t1 <- t1[,-71]
# #
# # names(t1)
# #
# #
# #
# # sort(unique(t1$actvsubs1))
# #
# t1$actvsubs1_0 <- ifelse(t1$actvsubs1 == "0",1,0)
# t1$actvsubs1_1 <- ifelse(t1$actvsubs1 == "1",1,0)
# t1$actvsubs1_2 <- ifelse(t1$actvsubs1 == "2",1,0)
# t1$actvsubs1_3 <- ifelse(t1$actvsubs1 == "3",1,0)
# t1$actvsubs1_4 <- ifelse(t1$actvsubs1 == "4",1,0)
# t1$actvsubs1_5 <- ifelse(t1$actvsubs1 == "5",1,0)
# t1$actvsubs1_6 <- ifelse(t1$actvsubs1 == "6",1,0)
# #t1$actvsubs1_7or11 <- ifelse(t1$actvsubs1 == "7-11",1,0)
# 
# 
# t1$actvsubs1_0<-factor(t1$actvsubs1_0,levels = 0:1,labels = c("No","Yes"))
# t1$actvsubs1_1<-factor(t1$actvsubs1_1,levels = 0:1,labels = c("No","Yes"))
# t1$actvsubs1_2<-factor(t1$actvsubs1_2,levels = 0:1,labels = c("No","Yes"))
# t1$actvsubs1_3<-factor(t1$actvsubs1_3,levels = 0:1,labels = c("No","Yes"))
# t1$actvsubs1_4<-factor(t1$actvsubs1_4,levels = 0:1,labels = c("No","Yes"))
# t1$actvsubs1_5<-factor(t1$actvsubs1_5,levels = 0:1,labels = c("No","Yes"))
# t1$actvsubs1_6<-factor(t1$actvsubs1_6,levels = 0:1,labels = c("No","Yes"))
# #t1$actvsubs1_7or11 <- factor(t1$actvsubs1_7or11,levels = 0:1,labels = c("No","Yes"))
# 
# 
# 
# 
# names(t1)
# #
# t1 <- t1[,-c(67,71)]


# ........................... Variable : adjrev ..........................

summary(t1$adjrev)

#removal of outliers
b1 <- boxplot(t1$adjrev)
getOUT <- which(t1$adjrev %in% b1$out)
t1$adjrev[getOUT] <- mean(t1$adjrev)
summary(t1$adjrev)

quantile(t1$adjrev,p = (0:100)/100,na.rm = TRUE)
quantile(t1$adjrev,p = (990:1000)/1000,na.rm = TRUE)
qqPlot(t1$adjrev)

d_adjrev <- t1 %>% mutate(dec = ntile(adjrev,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_adjrev$N <- unclass(t1 %>% mutate(dec = ntile(adjrev,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_adjrev$churn_perc <- d_adjrev$n/d_adjrev$N
d_adjrev$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(adjrev,10)) %>% 
                                  group_by(dec) %>% summarize(min(adjrev)))[[2]]
d_adjrev$LessThan <- unclass(t1 %>% mutate(dec = ntile(adjrev,10)) %>% 
                               group_by(dec) %>% summarize(max(adjrev)))[[2]]
d_adjrev$varName <- rep("adjrev",nrow(d_adjrev))

d_adjrev <- as.data.frame(d_adjrev)



# ........................... Variable : adjmou ..........................

summary(t1$adjmou)

#removal of outliers
b1 <- boxplot(t1$adjmou)
getOUT <- which(t1$adjmou %in% b1$out)
t1$adjmou[getOUT] <- mean(t1$adjmou)
summary(t1$adjmou)

quantile(t1$adjmou,p = (0:100)/100,na.rm = TRUE)
quantile(t1$adjmou,p = (990:1000)/1000,na.rm = TRUE)
qqPlot(t1$adjmou)

#.....shows exponential curve......requires transformation

qqPlot((t1$adjmou)^0.333)

d_adjmou <- t1 %>% mutate(dec = ntile(adjmou,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_adjmou$N <- unclass(t1 %>% mutate(dec = ntile(adjmou,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_adjmou$churn_perc <- d_adjmou$n/d_adjmou$N
d_adjmou$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(adjmou,10)) %>% 
                                  group_by(dec) %>% summarize(min(adjmou)))[[2]]
d_adjmou$LessThan <- unclass(t1 %>% mutate(dec = ntile(adjmou,10)) %>% 
                               group_by(dec) %>% summarize(max(adjmou)))[[2]]
d_adjmou$varName <- rep("adjmou",nrow(d_adjmou))


d_adjmou <- as.data.frame(d_adjmou)




# ........................... Variable : adjqty ..........................

summary(t1$adjqty)

#removal of outliers
b1 <- boxplot(t1$adjqty)
getOUT <- which(t1$adjqty %in% b1$out)
t1$adjqty[getOUT] <- mean(t1$adjqty)
summary(t1$adjqty)

quantile(t1$adjqty,p = (0:100)/100,na.rm = TRUE)
quantile(t1$adjqty,p = (990:1000)/1000,na.rm = TRUE)
qqPlot(t1$adjqty)

#.....shows exponential curve......requires transformation

qqPlot((t1$adjqty)^0.333)

d_adjqty <- t1 %>% mutate(dec = ntile(adjqty,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_adjqty$N <- unclass(t1 %>% mutate(dec = ntile(adjqty,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_adjqty$churn_perc <- d_adjqty$n/d_adjqty$N
d_adjqty$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(adjqty,10)) %>% 
                                  group_by(dec) %>% summarize(min(adjqty)))[[2]]
d_adjqty$LessThan <- unclass(t1 %>% mutate(dec = ntile(adjqty,10)) %>% 
                               group_by(dec) %>% summarize(max(adjqty)))[[2]]
d_adjqty$varName <- rep("adjqty",nrow(d_adjqty))

d_adjqty <- as.data.frame(d_adjqty)





# ........................... Variable : avg3mou ..........................

summary(t1$avg3mou)

#removal of outliers
b1 <- boxplot(t1$avg3mou)
getOUT <- which(t1$avg3mou %in% b1$out)
t1$avg3mou[getOUT] <- mean(t1$avg3mou)
summary(t1$avg3mou)

quantile(t1$avg3mou,p = (0:100)/100,na.rm = TRUE)
quantile(t1$avg3mou,p = (990:1000)/1000,na.rm = TRUE)
qqPlot(t1$avg3mou)

#.....shows exponential curve......requires transformation

qqPlot((t1$avg3mou)^0.333)

d_avg3mou <- t1 %>% mutate(dec = ntile(avg3mou,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_avg3mou$N <- unclass(t1 %>% mutate(dec = ntile(avg3mou,10)) %>% count(dec) %>% 
                         unname())[[2]]
d_avg3mou$churn_perc <- d_avg3mou$n/d_avg3mou$N
d_avg3mou$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(avg3mou,10)) %>% 
                                   group_by(dec) %>% summarize(min(avg3mou)))[[2]]
d_avg3mou$LessThan <- unclass(t1 %>% mutate(dec = ntile(avg3mou,10)) %>% 
                                group_by(dec) %>% summarize(max(avg3mou)))[[2]]
d_avg3mou$varName <- rep("avg3mou",nrow(d_avg3mou))

d_avg3mou <- as.data.frame(d_avg3mou)



# ........................... Variable : avg3qty ..........................

summary(t1$avg3qty)

#removal of outliers
b1 <- boxplot(t1$avg3qty)
getOUT <- which(t1$avg3qty %in% b1$out)
t1$avg3qty[getOUT] <- mean(t1$avg3qty)
summary(t1$avg3qty)

quantile(t1$avg3qty,p = (0:100)/100,na.rm = TRUE)
quantile(t1$avg3qty,p = (990:1000)/1000,na.rm = TRUE)
qqPlot(t1$avg3qty)

#.....shows exponential curve......requires transformation

qqPlot((t1$avg3qty)^0.333)

d_avg3qty <- t1 %>% mutate(dec = ntile(avg3qty,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_avg3qty$N <- unclass(t1 %>% mutate(dec = ntile(avg3qty,10)) %>% count(dec) %>% 
                         unname())[[2]]
d_avg3qty$churn_perc <- d_avg3qty$n/d_avg3qty$N
d_avg3qty$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(avg3qty,10)) %>% 
                                   group_by(dec) %>% summarize(min(avg3qty)))[[2]]
d_avg3qty$LessThan <- unclass(t1 %>% mutate(dec = ntile(avg3qty,10)) %>% 
                                group_by(dec) %>% summarize(max(avg3qty)))[[2]]
d_avg3qty$varName <- rep("avg3qty",nrow(d_avg3qty))

d_avg3qty <- as.data.frame(d_avg3qty)



# ........................... Variable : avg6mou ..........................

summary(t1$avg6mou)

#treatment of NA's 
#qqPlot((t1$avg6mou)^0.333)

d_avg6mou <- t1 %>% mutate(dec = ntile(avg6mou,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_avg6mou$N <- unclass(t1 %>% mutate(dec = ntile(avg6mou,10)) %>% count(dec) %>% 
                         unname())[[2]]
d_avg6mou$churn_perc <- d_avg6mou$n/d_avg6mou$N
d_avg6mou$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(avg6mou,10)) %>% 
                                   group_by(dec) %>% summarize(min(avg6mou)))[[2]]
d_avg6mou$LessThan <- unclass(t1 %>% mutate(dec = ntile(avg6mou,10)) %>% 
                                group_by(dec) %>% summarize(max(avg6mou)))[[2]]
d_avg6mou$varName <- rep("avg6mou",nrow(d_avg6mou))
d_avg6mou <- as.data.frame(d_avg6mou)


quantile(t1$avg6mou,p = (0:10)/10,na.rm = TRUE)
mean(t1$avg6mou,na.rm=TRUE)

getIND <- which(is.na(t1$avg6mou))
t1$avg6mou[getIND] <- mean(t1$avg6mou,na.rm=TRUE)



#removal of outliers
b1 <- boxplot(t1$avg6mou)
getOUT <- which(t1$avg6mou %in% b1$out)
t1$avg6mou[getOUT] <- mean(t1$avg6mou)
summary(t1$avg6mou)

quantile(t1$avg6mou,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$avg6mou)

#.....shows exponential curve......requires transformation

qqPlot((t1$avg6mou)^0.333)






# ........................... Variable : avg6qty ..........................

summary(t1$avg6qty)

#treatment of NA's 

d_avg6qty <- t1 %>% mutate(dec = ntile(avg6qty,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_avg6qty$N <- unclass(t1 %>% mutate(dec = ntile(avg6qty,10)) %>% count(dec) %>% 
                         unname())[[2]]
d_avg6qty$churn_perc <- d_avg6qty$n/d_avg6qty$N
d_avg6qty$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(avg6qty,10)) %>% 
                                   group_by(dec) %>% summarize(min(avg6qty)))[[2]]
d_avg6qty$LessThan <- unclass(t1 %>% mutate(dec = ntile(avg6qty,10)) %>% 
                                group_by(dec) %>% summarize(max(avg6qty)))[[2]]
d_avg6qty$varName <- rep("avg6qty",nrow(d_avg6qty))
d_avg6qty <- as.data.frame(d_avg6qty)



quantile(t1$avg6qty,p = (0:10)/10,na.rm = TRUE)
mean(t1$avg6qty,na.rm=TRUE)

getIND <- which(is.na(t1$avg6qty))
t1$avg6qty[getIND] <- mean(t1$avg6qty,na.rm=TRUE)



#removal of outliers
b1 <- boxplot(t1$avg6qty)
getOUT <- which(t1$avg6qty %in% b1$out)
t1$avg6qty[getOUT] <- mean(t1$avg6qty)
summary(t1$avg6qty)

quantile(t1$avg6qty,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$avg6qty)




# ........................... Variable : avgmou ..........................

summary(t1$avgmou)



d_avgmou <- t1 %>% mutate(dec = ntile(avgmou,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_avgmou$N <- unclass(t1 %>% mutate(dec = ntile(avgmou,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_avgmou$churn_perc <- d_avgmou$n/d_avgmou$N
d_avgmou$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(avgmou,10)) %>% 
                                  group_by(dec) %>% summarize(min(avgmou)))[[2]]
d_avgmou$LessThan <- unclass(t1 %>% mutate(dec = ntile(avgmou,10)) %>% 
                               group_by(dec) %>% summarize(max(avgmou)))[[2]]
d_avgmou$varName <- rep("avgmou",nrow(d_avgmou))
d_avgmou <- as.data.frame(d_avgmou)


#removal of outliers
b1 <- boxplot(t1$avgmou)
getOUT <- which(t1$avgmou %in% b1$out)
t1$avgmou[getOUT] <- mean(t1$avgmou)
summary(t1$avgmou)

quantile(t1$avgmou,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$avgmou)





# ........................... Variable : avgqty ..........................

summary(t1$avgqty)



d_avgqty <- t1 %>% mutate(dec = ntile(avgqty,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_avgqty$N <- unclass(t1 %>% mutate(dec = ntile(avgqty,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_avgqty$churn_perc <- d_avgqty$n/d_avgqty$N
d_avgqty$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(avgqty,10)) %>% 
                                  group_by(dec) %>% summarize(min(avgqty)))[[2]]
d_avgqty$LessThan <- unclass(t1 %>% mutate(dec = ntile(avgqty,10)) %>% 
                               group_by(dec) %>% summarize(max(avgqty)))[[2]]
d_avgqty$varName <- rep("avgqty",nrow(d_avgqty))
d_avgqty <- as.data.frame(d_avgqty)


#removal of outliers
b1 <- boxplot(t1$avgqty)
getOUT <- which(t1$avgqty %in% b1$out)
t1$avgqty[getOUT] <- mean(t1$avgqty)
summary(t1$avgqty)

quantile(t1$avgqty,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$avgqty)




# ........................... Variable : avgrev ..........................

summary(t1$avgrev)



d_avgrev <- t1 %>% mutate(dec = ntile(avgrev,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_avgrev$N <- unclass(t1 %>% mutate(dec = ntile(avgrev,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_avgrev$churn_perc <- d_avgrev$n/d_avgrev$N
d_avgrev$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(avgrev,10)) %>% 
                                  group_by(dec) %>% summarize(min(avgrev)))[[2]]
d_avgrev$LessThan <- unclass(t1 %>% mutate(dec = ntile(avgrev,10)) %>% 
                               group_by(dec) %>% summarize(max(avgrev)))[[2]]
d_avgrev$varName <- rep("avgrev",nrow(d_avgrev))
d_avgrev <- as.data.frame(d_avgrev)


#removal of outliers
b1 <- boxplot(t1$avgrev)
getOUT <- which(t1$avgrev %in% b1$out)
t1$avgrev[getOUT] <- mean(t1$avgrev)
summary(t1$avgrev)

quantile(t1$avgrev,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$avgrev)

plot(t1$avgrev,t1$totrev)





#Creating derived variables
t1$attempt_mean <- t1$plcd_dat_Mean + t1$plcd_vce_Mean
t1$complete_mean <- t1$comp_dat_Mean + t1$comp_vce_Mean






# ........................... Variable : blck_dat_Mean ..........................

summary(t1$blck_dat_Mean)



d_blck_dat_Mean <- t1 %>% mutate(dec = ntile(blck_dat_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_blck_dat_Mean$N <- unclass(t1 %>% mutate(dec = ntile(blck_dat_Mean,2)) %>% count(dec) %>% 
                               unname())[[2]]
d_blck_dat_Mean$churn_perc <- d_blck_dat_Mean$n/d_blck_dat_Mean$N
d_blck_dat_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(blck_dat_Mean,2)) %>% 
                                         group_by(dec) %>% summarize(min(blck_dat_Mean)))[[2]]
d_blck_dat_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(blck_dat_Mean,2)) %>% 
                                      group_by(dec) %>% summarize(max(blck_dat_Mean)))[[2]]
d_blck_dat_Mean$varName <- rep("blck_dat_Mean",nrow(d_blck_dat_Mean))
d_blck_dat_Mean <- as.data.frame(d_blck_dat_Mean)


#removal of outliers
b1 <- boxplot(t1$blck_dat_Mean)
getOUT <- which(t1$blck_dat_Mean %in% b1$out)
t1$blck_dat_Mean[getOUT] <- mean(t1$blck_dat_Mean)
summary(t1$blck_dat_Mean)

quantile(t1$blck_dat_Mean,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$blck_dat_Mean)

plot(t1$blck_dat_Mean,t1$totrev)






# ........................... Variable : callwait_Mean ..........................

summary(t1$callwait_Mean)



d_callwait_Mean <- t1 %>% mutate(dec = ntile(callwait_Mean,3)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_callwait_Mean$N <- unclass(t1 %>% mutate(dec = ntile(callwait_Mean,3)) %>% count(dec) %>% 
                               unname())[[2]]
d_callwait_Mean$churn_perc <- d_callwait_Mean$n/d_callwait_Mean$N
d_callwait_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(callwait_Mean,3)) %>% 
                                         group_by(dec) %>% summarize(min(callwait_Mean)))[[2]]
d_callwait_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(callwait_Mean,3)) %>% 
                                      group_by(dec) %>% summarize(max(callwait_Mean)))[[2]]
d_callwait_Mean$varName <- rep("callwait_Mean",nrow(d_callwait_Mean))
d_callwait_Mean <- as.data.frame(d_callwait_Mean)


#removal of outliers
b1 <- boxplot(t1$callwait_Mean)
getOUT <- which(t1$callwait_Mean %in% b1$out)
t1$callwait_Mean[getOUT] <- mean(t1$callwait_Mean)
summary(t1$callwait_Mean)

quantile(t1$callwait_Mean,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$callwait_Mean)






# ........................... Variable : callwait_Range ..........................

summary(t1$callwait_Range)



d_callwait_Range <- t1 %>% mutate(dec = ntile(callwait_Range,3)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_callwait_Range$N <- unclass(t1 %>% mutate(dec = ntile(callwait_Range,3)) %>% count(dec) %>% 
                                unname())[[2]]
d_callwait_Range$churn_perc <- d_callwait_Range$n/d_callwait_Range$N
d_callwait_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(callwait_Range,3)) %>% 
                                          group_by(dec) %>% summarize(min(callwait_Range)))[[2]]
d_callwait_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(callwait_Range,3)) %>% 
                                       group_by(dec) %>% summarize(max(callwait_Range)))[[2]]
d_callwait_Range$varName <- rep("callwait_Range",nrow(d_callwait_Range))
d_callwait_Range <- as.data.frame(d_callwait_Range)


#removal of outliers
b1 <- boxplot(t1$callwait_Range)
getOUT <- which(t1$callwait_Range %in% b1$out)
t1$callwait_Range[getOUT] <- mean(t1$callwait_Range)
summary(t1$callwait_Range)

quantile(t1$callwait_Range,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$callwait_Range)




# ........................... Variable : change_mou ..........................

summary(t1$change_mou)



d_change_mou <- t1 %>% mutate(dec = ntile(change_mou,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_change_mou$N <- unclass(t1 %>% mutate(dec = ntile(change_mou,10)) %>% count(dec) %>% 
                            unname())[[2]]
d_change_mou$churn_perc <- d_change_mou$n/d_change_mou$N
d_change_mou$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(change_mou,10)) %>% 
                                      group_by(dec) %>% summarize(min(change_mou)))[[2]]
d_change_mou$LessThan <- unclass(t1 %>% mutate(dec = ntile(change_mou,10)) %>% 
                                   group_by(dec) %>% summarize(max(change_mou)))[[2]]
d_change_mou$varName <- rep("change_mou",nrow(d_change_mou))

d_change_mou <- as.data.frame(d_change_mou)


#deleting missing values
getIND <- which(is.na(t1$change_mou))
t1 <- t1[-getIND,]


#removal of outliers
b1 <- boxplot(t1$change_mou)
getOUT <- which(t1$change_mou %in% b1$out)
t1$change_mou[getOUT] <- mean(t1$change_mou)
summary(t1$change_mou)

quantile(t1$change_mou,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$change_mou)







# ........................... Variable :ccrndmou_Range ..........................

summary(t1$ccrndmou_Range)



d_ccrndmou_Range <- t1 %>% mutate(dec = ntile(ccrndmou_Range,3)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_ccrndmou_Range$N <- unclass(t1 %>% mutate(dec = ntile(ccrndmou_Range,3)) %>% count(dec) %>% 
                                unname())[[2]]
d_ccrndmou_Range$churn_perc <- d_ccrndmou_Range$n/d_ccrndmou_Range$N
d_ccrndmou_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(ccrndmou_Range,3)) %>% 
                                          group_by(dec) %>% summarize(min(ccrndmou_Range)))[[2]]
d_ccrndmou_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(ccrndmou_Range,3)) %>% 
                                       group_by(dec) %>% summarize(max(ccrndmou_Range)))[[2]]
d_ccrndmou_Range$varName <- rep("ccrndmou_Range",nrow(d_ccrndmou_Range))
d_ccrndmou_Range <- as.data.frame(d_ccrndmou_Range)


#removal of outliers
b1 <- boxplot(t1$ccrndmou_Range)
getOUT <- which(t1$ccrndmou_Range %in% b1$out)
t1$ccrndmou_Range[getOUT] <- mean(t1$ccrndmou_Range)
summary(t1$ccrndmou_Range)

quantile(t1$ccrndmou_Range,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$ccrndmou_Range)






# ........................... Variable :comp_dat_mean ..........................


summary(t1$comp_dat_mean)
class(t1$comp_dat_Mean)
head(t1$comp_dat_Mean)


# ........................... Variable :comp_vce_Mean ..........................

summary(t1$comp_vce_Mean)



d_comp_vce_Mean <- t1 %>% mutate(dec = ntile(comp_vce_Mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_comp_vce_Mean$N <- unclass(t1 %>% mutate(dec = ntile(comp_vce_Mean,10)) %>% count(dec) %>% 
                               unname())[[2]]
d_comp_vce_Mean$churn_perc <- d_comp_vce_Mean$n/d_comp_vce_Mean$N
d_comp_vce_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(comp_vce_Mean,10)) %>% 
                                         group_by(dec) %>% summarize(min(comp_vce_Mean)))[[2]]
d_comp_vce_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(comp_vce_Mean,10)) %>% 
                                      group_by(dec) %>% summarize(max(comp_vce_Mean)))[[2]]
d_comp_vce_Mean$varName <- rep("comp_vce_Mean",nrow(d_comp_vce_Mean))
d_comp_vce_Mean <- as.data.frame(d_comp_vce_Mean)


#removal of outliers
b1 <- boxplot(t1$comp_vce_Mean)
getOUT <- which(t1$comp_vce_Mean %in% b1$out)
t1$comp_vce_Mean[getOUT] <- mean(t1$comp_vce_Mean)
summary(t1$comp_vce_Mean)

quantile(t1$comp_vce_Mean,p = (0:100)/100,na.rm = TRUE)




# ........................... Variable : custcare_Mean ..........................

summary(t1$custcare_Mean)



d_custcare_Mean <- t1 %>% mutate(dec = ntile(custcare_Mean,3)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_custcare_Mean$N <- unclass(t1 %>% mutate(dec = ntile(custcare_Mean,3)) %>% count(dec) %>% 
                               unname())[[2]]
d_custcare_Mean$churn_perc <- d_custcare_Mean$n/d_custcare_Mean$N
d_custcare_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(custcare_Mean,3)) %>% 
                                         group_by(dec) %>% summarize(min(custcare_Mean)))[[2]]
d_custcare_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(custcare_Mean,3)) %>% 
                                      group_by(dec) %>% summarize(max(custcare_Mean)))[[2]]
d_custcare_Mean$varName <- rep("custcare_Mean",nrow(d_custcare_Mean))
d_custcare_Mean <- as.data.frame(d_custcare_Mean)


#removal of outliers
b1 <- boxplot(t1$custcare_Mean)
getOUT <- which(t1$custcare_Mean %in% b1$out)
t1$custcare_Mean[getOUT] <- mean(t1$custcare_Mean)
summary(t1$custcare_Mean)

quantile(t1$custcare_Mean,p = (0:100)/100,na.rm = TRUE)
qqPlot(t1$ccrndmou_Range)






# ........................... Variable :  da_Mean  ..........................

summary(t1$da_Mean)



d_da_Mean <- t1 %>% mutate(dec = ntile(da_Mean,4)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_da_Mean$N <- unclass(t1 %>% mutate(dec = ntile(da_Mean,4)) %>% count(dec) %>% 
                         unname())[[2]]
d_da_Mean$churn_perc <- d_da_Mean$n/d_da_Mean$N
d_da_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(da_Mean,4)) %>% 
                                   group_by(dec) %>% summarize(min(da_Mean)))[[2]]
d_da_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(da_Mean,4)) %>% 
                                group_by(dec) %>% summarize(max(da_Mean)))[[2]]
d_da_Mean$varName <- rep("da_Mean",nrow(d_da_Mean))
d_da_Mean <- as.data.frame(d_da_Mean)


#removal of outliers
b1 <- boxplot(t1$da_Mean)
getOUT <- which(t1$da_Mean %in% b1$out)
t1$da_Mean[getOUT] <- mean(t1$da_Mean)
summary(t1$da_Mean)

quantile(t1$da_Mean,p = (0:100)/100,na.rm = TRUE)





# ........................... Variable :  da_Range  ..........................

summary(t1$da_Range)



d_da_Range <- t1 %>% mutate(dec = ntile(da_Range,4)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_da_Range$N <- unclass(t1 %>% mutate(dec = ntile(da_Range,4)) %>% count(dec) %>% 
                          unname())[[2]]
d_da_Range$churn_perc <- d_da_Range$n/d_da_Range$N
d_da_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(da_Range,4)) %>% 
                                    group_by(dec) %>% summarize(min(da_Range)))[[2]]
d_da_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(da_Range,4)) %>% 
                                 group_by(dec) %>% summarize(max(da_Range)))[[2]]
d_da_Range$varName <- rep("da_Range",nrow(d_da_Range))
d_da_Range <- as.data.frame(d_da_Range)


#removal of outliers
b1 <- boxplot(t1$da_Range)
getOUT <- which(t1$da_Range %in% b1$out)
t1$da_Range[getOUT] <- mean(t1$da_Range)
summary(t1$da_Range)

quantile(t1$da_Range,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  datovr_Mean  ..........................

summary(t1$datovr_Mean)



d_datovr_Mean <- t1 %>% mutate(dec = ntile(datovr_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_datovr_Mean$N <- unclass(t1 %>% mutate(dec = ntile(datovr_Mean,2)) %>% count(dec) %>% 
                             unname())[[2]]
d_datovr_Mean$churn_perc <- d_datovr_Mean$n/d_datovr_Mean$N
d_datovr_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(datovr_Mean,2)) %>% 
                                       group_by(dec) %>% summarize(min(datovr_Mean)))[[2]]
d_datovr_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(datovr_Mean,2)) %>% 
                                    group_by(dec) %>% summarize(max(datovr_Mean)))[[2]]
d_datovr_Mean$varName <- rep("datovr_Mean",nrow(d_datovr_Mean))
d_datovr_Mean <- as.data.frame(d_datovr_Mean)


#removal of outliers
b1 <- boxplot(t1$datovr_Mean)
getOUT <- which(t1$datovr_Mean %in% b1$out)
t1$datovr_Mean[getOUT] <- mean(t1$datovr_Mean)
summary(t1$datovr_Mean)

quantile(t1$datovr_Mean,p = (0:100)/100,na.rm = TRUE)






# ........................... Variable :  datovr_Range  ..........................

summary(t1$datovr_Range)



d_datovr_Range <- t1 %>% mutate(dec = ntile(datovr_Range,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_datovr_Range$N <- unclass(t1 %>% mutate(dec = ntile(datovr_Range,2)) %>% count(dec) %>% 
                              unname())[[2]]
d_datovr_Range$churn_perc <- d_datovr_Range$n/d_datovr_Range$N
d_datovr_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(datovr_Range,2)) %>% 
                                        group_by(dec) %>% summarize(min(datovr_Range)))[[2]]
d_datovr_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(datovr_Range,2)) %>% 
                                     group_by(dec) %>% summarize(max(datovr_Range)))[[2]]
d_datovr_Range$varName <- rep("datovr_Range",nrow(d_datovr_Range))
d_datovr_Range <- as.data.frame(d_datovr_Range)


#removal of outliers
b1 <- boxplot(t1$datovr_Range)
getOUT <- which(t1$datovr_Range %in% b1$out)
t1$datovr_Range[getOUT] <- mean(t1$datovr_Range)
summary(t1$datovr_Range)

quantile(t1$datovr_Range,p = (0:100)/100,na.rm = TRUE)





# ........................... Variable :  drop_blk_Mean  ..........................

summary(t1$drop_blk_Mean)


d_drop_blk_Mean <- t1 %>% mutate(dec = ntile(drop_blk_Mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_drop_blk_Mean$N <- unclass(t1 %>% mutate(dec = ntile(drop_blk_Mean,10)) %>% count(dec) %>% 
                               unname())[[2]]
d_drop_blk_Mean$churn_perc <- d_drop_blk_Mean$n/d_drop_blk_Mean$N
d_drop_blk_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(drop_blk_Mean,10)) %>% 
                                         group_by(dec) %>% summarize(min(drop_blk_Mean)))[[2]]
d_drop_blk_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(drop_blk_Mean,10)) %>% 
                                      group_by(dec) %>% summarize(max(drop_blk_Mean)))[[2]]
d_drop_blk_Mean$varName <- rep("drop_blk_Mean",nrow(d_drop_blk_Mean))
d_drop_blk_Mean <- as.data.frame(d_drop_blk_Mean)


#removal of outliers
b1 <- boxplot(t1$drop_blk_Mean)
getOUT <- which(t1$drop_blk_Mean %in% b1$out)
t1$drop_blk_Mean[getOUT] <- mean(t1$drop_blk_Mean)
summary(t1$drop_blk_Mean)

quantile(t1$drop_blk_Mean,p = (0:100)/100,na.rm = TRUE)





# ........................... Variable :  drop_dat_Mean  ..........................

summary(t1$drop_dat_Mean)



d_drop_dat_Mean <- t1 %>% mutate(dec = ntile(drop_dat_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_drop_dat_Mean$N <- unclass(t1 %>% mutate(dec = ntile(drop_dat_Mean,2)) %>% count(dec) %>% 
                               unname())[[2]]
d_drop_dat_Mean$churn_perc <- d_drop_dat_Mean$n/d_drop_dat_Mean$N
d_drop_dat_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(drop_dat_Mean,2)) %>% 
                                         group_by(dec) %>% summarize(min(drop_dat_Mean)))[[2]]
d_drop_dat_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(drop_dat_Mean,2)) %>% 
                                      group_by(dec) %>% summarize(max(drop_dat_Mean)))[[2]]
d_drop_dat_Mean$varName <- rep("drop_dat_Mean",nrow(d_drop_dat_Mean))
d_drop_dat_Mean <- as.data.frame(d_drop_dat_Mean)


#removal of outliers
b1 <- boxplot(t1$drop_dat_Mean)
getOUT <- which(t1$drop_dat_Mean %in% b1$out)
t1$drop_dat_Mean[getOUT] <- mean(t1$drop_dat_Mean)
summary(t1$drop_dat_Mean)

quantile(t1$drop_dat_Mean,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  drop_vce_Mean  ..........................

summary(t1$drop_vce_Mean)



d_drop_vce_Mean <- t1 %>% mutate(dec = ntile(drop_vce_Mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_drop_vce_Mean$N <- unclass(t1 %>% mutate(dec = ntile(drop_vce_Mean,10)) %>% count(dec) %>% 
                               unname())[[2]]
d_drop_vce_Mean$churn_perc <- d_drop_vce_Mean$n/d_drop_vce_Mean$N
d_drop_vce_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(drop_vce_Mean,10)) %>% 
                                         group_by(dec) %>% summarize(min(drop_vce_Mean)))[[2]]
d_drop_vce_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(drop_vce_Mean,10)) %>% 
                                      group_by(dec) %>% summarize(max(drop_vce_Mean)))[[2]]
d_drop_vce_Mean$varName <- rep("drop_vce_Mean",nrow(d_drop_vce_Mean))
d_drop_vce_Mean <- as.data.frame(d_drop_vce_Mean)


#removal of outliers
b1 <- boxplot(t1$drop_vce_Mean)
getOUT <- which(t1$drop_vce_Mean %in% b1$out)
t1$drop_vce_Mean[getOUT] <- mean(t1$drop_vce_Mean)
summary(t1$drop_vce_Mean)

quantile(t1$drop_vce_Mean,p = (0:100)/100,na.rm = TRUE)




# ........................... Variable :  drop_vce_Range  ..........................

summary(t1$drop_vce_Range)



d_drop_vce_Range <- t1 %>% mutate(dec = ntile(drop_vce_Range,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_drop_vce_Range$N <- unclass(t1 %>% mutate(dec = ntile(drop_vce_Range,10)) %>% count(dec) %>% 
                                unname())[[2]]
d_drop_vce_Range$churn_perc <- d_drop_vce_Range$n/d_drop_vce_Range$N
d_drop_vce_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(drop_vce_Range,10)) %>% 
                                          group_by(dec) %>% summarize(min(drop_vce_Range)))[[2]]
d_drop_vce_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(drop_vce_Range,10)) %>% 
                                       group_by(dec) %>% summarize(max(drop_vce_Range)))[[2]]
d_drop_vce_Range$varName <- rep("drop_vce_Range",nrow(d_drop_vce_Range))
d_drop_vce_Range <- as.data.frame(d_drop_vce_Range)


#removal of outliers
b1 <- boxplot(t1$drop_vce_Range)
getOUT <- which(t1$drop_vce_Range %in% b1$out)
t1$drop_vce_Range[getOUT] <- mean(t1$drop_vce_Range)
summary(t1$drop_vce_Range)

quantile(t1$drop_vce_Range,p = (0:100)/100,na.rm = TRUE)






# ........................... Variable :  eqpdays  ..........................

summary(t1$eqpdays)



d_eqpdays <- t1 %>% mutate(dec = ntile(eqpdays,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_eqpdays$N <- unclass(t1 %>% mutate(dec = ntile(eqpdays,10)) %>% count(dec) %>% 
                         unname())[[2]]
d_eqpdays$churn_perc <- d_eqpdays$n/d_eqpdays$N
d_eqpdays$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(eqpdays,10)) %>% 
                                   group_by(dec) %>% summarize(min(eqpdays)))[[2]]
d_eqpdays$LessThan <- unclass(t1 %>% mutate(dec = ntile(eqpdays,10)) %>% 
                                group_by(dec) %>% summarize(max(eqpdays)))[[2]]
d_eqpdays$varName <- rep("eqpdays",nrow(d_eqpdays))
d_eqpdays <- as.data.frame(d_eqpdays)


#removal of outliers
b1 <- boxplot(t1$eqpdays)
getOUT <- which(t1$eqpdays %in% b1$out)
t1$eqpdays[getOUT] <- mean(t1$eqpdays)
summary(t1$eqpdays)

quantile(t1$eqpdays,p = (0:100)/100,na.rm = TRUE)

getIND <- which(t1$eqpdays < 0)

#Removing negative number of days
t1$eqpdays[getIND] <- 0
summary(t1$eqpdays)





# ........................... Variable :  iwylis_vce_Mean  ..........................

summary(t1$iwylis_vce_Mean)



d_iwylis_vce_Mean <- t1 %>% mutate(dec = ntile(iwylis_vce_Mean,4)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_iwylis_vce_Mean$N <- unclass(t1 %>% mutate(dec = ntile(iwylis_vce_Mean,4)) %>% count(dec) %>% 
                                 unname())[[2]]
d_iwylis_vce_Mean$churn_perc <- d_iwylis_vce_Mean$n/d_iwylis_vce_Mean$N
d_iwylis_vce_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(iwylis_vce_Mean,4)) %>% 
                                           group_by(dec) %>% summarize(min(iwylis_vce_Mean)))[[2]]
d_iwylis_vce_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(iwylis_vce_Mean,4)) %>% 
                                        group_by(dec) %>% summarize(max(iwylis_vce_Mean)))[[2]]
d_iwylis_vce_Mean$varName <- rep("iwylis_vce_Mean",nrow(d_iwylis_vce_Mean))
d_iwylis_vce_Mean <- as.data.frame(d_iwylis_vce_Mean)


#removal of outliers
b1 <- boxplot(t1$iwylis_vce_Mean)
getOUT <- which(t1$iwylis_vce_Mean %in% b1$out)
t1$iwylis_vce_Mean[getOUT] <- mean(t1$iwylis_vce_Mean)
summary(t1$iwylis_vce_Mean)

quantile(t1$iwylis_vce_Mean,p = (0:100)/100,na.rm = TRUE)





# ........................... Variable :  months  ..........................

summary(t1$months)



d_months <- t1 %>% mutate(dec = ntile(months,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_months$N <- unclass(t1 %>% mutate(dec = ntile(months,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_months$churn_perc <- d_months$n/d_months$N
d_months$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(months,10)) %>% 
                                  group_by(dec) %>% summarize(min(months)))[[2]]
d_months$LessThan <- unclass(t1 %>% mutate(dec = ntile(months,10)) %>% 
                               group_by(dec) %>% summarize(max(months)))[[2]]
d_months$varName <- rep("months",nrow(d_months))
d_months <- as.data.frame(d_months)


#removal of outliers
b1 <- boxplot(t1$months)
getOUT <- which(t1$months %in% b1$out)
t1$months[getOUT] <- mean(t1$months)
summary(t1$months)

quantile(t1$months,p = (0:100)/100,na.rm = TRUE)






# ........................... Variable :  mou_Mean  ..........................

summary(t1$mou_Mean)



d_mou_Mean <- t1 %>% mutate(dec = ntile(mou_Mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_mou_Mean$N <- unclass(t1 %>% mutate(dec = ntile(mou_Mean,10)) %>% count(dec) %>% 
                          unname())[[2]]
d_mou_Mean$churn_perc <- d_mou_Mean$n/d_mou_Mean$N
d_mou_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(mou_Mean,10)) %>% 
                                    group_by(dec) %>% summarize(min(mou_Mean)))[[2]]
d_mou_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(mou_Mean,10)) %>% 
                                 group_by(dec) %>% summarize(max(mou_Mean)))[[2]]
d_mou_Mean$varName <- rep("mou_Mean",nrow(d_mou_Mean))
d_mou_Mean <- as.data.frame(d_mou_Mean)


#removal of outliers
b1 <- boxplot(t1$mou_Mean)
getOUT <- which(t1$mou_Mean %in% b1$out)
t1$mou_Mean[getOUT] <- mean(t1$mou_Mean)
summary(t1$mou_Mean)

quantile(t1$mou_Mean,p = (0:100)/100,na.rm = TRUE)





# ........................... Variable :  mou_opkv_Range  ..........................

summary(t1$mou_opkv_Range)



d_mou_opkv_Range <- t1 %>% mutate(dec = ntile(mou_opkv_Range,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_mou_opkv_Range$N <- unclass(t1 %>% mutate(dec = ntile(mou_opkv_Range,10)) %>% count(dec) %>% 
                                unname())[[2]]
d_mou_opkv_Range$churn_perc <- d_mou_opkv_Range$n/d_mou_opkv_Range$N
d_mou_opkv_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(mou_opkv_Range,10)) %>% 
                                          group_by(dec) %>% summarize(min(mou_opkv_Range)))[[2]]
d_mou_opkv_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(mou_opkv_Range,10)) %>% 
                                       group_by(dec) %>% summarize(max(mou_opkv_Range)))[[2]]
d_mou_opkv_Range$varName <- rep("mou_opkv_Range",nrow(d_mou_opkv_Range))
d_mou_opkv_Range <- as.data.frame(d_mou_opkv_Range)


#removal of outliers
b1 <- boxplot(t1$mou_opkv_Range)
getOUT <- which(t1$mou_opkv_Range %in% b1$out)
t1$mou_opkv_Range[getOUT] <- mean(t1$mou_opkv_Range)
summary(t1$mou_opkv_Range)

quantile(t1$mou_opkv_Range,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  mou_pead_Mean  ..........................

summary(t1$mou_pead_Mean)



d_mou_pead_Mean <- t1 %>% mutate(dec = ntile(mou_pead_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_mou_pead_Mean$N <- unclass(t1 %>% mutate(dec = ntile(mou_pead_Mean,2)) %>% count(dec) %>% 
                               unname())[[2]]
d_mou_pead_Mean$churn_perc <- d_mou_pead_Mean$n/d_mou_pead_Mean$N
d_mou_pead_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(mou_pead_Mean,2)) %>% 
                                         group_by(dec) %>% summarize(min(mou_pead_Mean)))[[2]]
d_mou_pead_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(mou_pead_Mean,2)) %>% 
                                      group_by(dec) %>% summarize(max(mou_pead_Mean)))[[2]]
d_mou_pead_Mean$varName <- rep("mou_pead_Mean",nrow(d_mou_pead_Mean))
d_mou_pead_Mean <- as.data.frame(d_mou_pead_Mean)


#removal of outliers
b1 <- boxplot(t1$mou_pead_Mean)
getOUT <- which(t1$mou_pead_Mean %in% b1$out)
t1$mou_pead_Mean[getOUT] <- mean(t1$mou_pead_Mean)
summary(t1$mou_pead_Mean)

quantile(t1$mou_pead_Mean,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  mou_Range  ..........................

summary(t1$mou_Range)



d_mou_Range <- t1 %>% mutate(dec = ntile(mou_Range,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_mou_Range$N <- unclass(t1 %>% mutate(dec = ntile(mou_Range,10)) %>% count(dec) %>% 
                           unname())[[2]]
d_mou_Range$churn_perc <- d_mou_Range$n/d_mou_Range$N
d_mou_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(mou_Range,10)) %>% 
                                     group_by(dec) %>% summarize(min(mou_Range)))[[2]]
d_mou_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(mou_Range,10)) %>% 
                                  group_by(dec) %>% summarize(max(mou_Range)))[[2]]
d_mou_Range$varName <- rep("mou_Range",nrow(d_mou_Range))
d_mou_Range <- as.data.frame(d_mou_Range)


#removal of outliers
b1 <- boxplot(t1$mou_Range)
getOUT <- which(t1$mou_Range %in% b1$out)
t1$mou_Range[getOUT] <- mean(t1$mou_Range)
summary(t1$mou_Range)

quantile(t1$mou_Range,p = (0:100)/100,na.rm = TRUE)






# ........................... Variable :  opk_dat_Mean  ..........................

summary(t1$opk_dat_Mean)



d_opk_dat_Mean <- t1 %>% mutate(dec = ntile(opk_dat_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_opk_dat_Mean$N <- unclass(t1 %>% mutate(dec = ntile(opk_dat_Mean,2)) %>% count(dec) %>% 
                              unname())[[2]]
d_opk_dat_Mean$churn_perc <- d_opk_dat_Mean$n/d_opk_dat_Mean$N
d_opk_dat_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(opk_dat_Mean,2)) %>% 
                                        group_by(dec) %>% summarize(min(opk_dat_Mean)))[[2]]
d_opk_dat_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(opk_dat_Mean,2)) %>% 
                                     group_by(dec) %>% summarize(max(opk_dat_Mean)))[[2]]
d_opk_dat_Mean$varName <- rep("opk_dat_Mean",nrow(d_opk_dat_Mean))
d_opk_dat_Mean <- as.data.frame(d_opk_dat_Mean)


#removal of outliers
b1 <- boxplot(t1$opk_dat_Mean)
getOUT <- which(t1$opk_dat_Mean %in% b1$out)
t1$opk_dat_Mean[getOUT] <- mean(t1$opk_dat_Mean)
summary(t1$opk_dat_Mean)

quantile(t1$opk_dat_Mean,p = (0:100)/100,na.rm = TRUE)








# ........................... Variable :  ovrmou_Mean  ..........................

summary(t1$ovrmou_Mean)



d_ovrmou_Mean <- t1 %>% mutate(dec = ntile(ovrmou_Mean,4)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_ovrmou_Mean$N <- unclass(t1 %>% mutate(dec = ntile(ovrmou_Mean,4)) %>% count(dec) %>% 
                             unname())[[2]]
d_ovrmou_Mean$churn_perc <- d_ovrmou_Mean$n/d_ovrmou_Mean$N
d_ovrmou_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(ovrmou_Mean,4)) %>% 
                                       group_by(dec) %>% summarize(min(ovrmou_Mean)))[[2]]
d_ovrmou_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(ovrmou_Mean,4)) %>% 
                                    group_by(dec) %>% summarize(max(ovrmou_Mean)))[[2]]
d_ovrmou_Mean$varName <- rep("ovrmou_Mean",nrow(d_ovrmou_Mean))
d_ovrmou_Mean <- as.data.frame(d_ovrmou_Mean)


#removal of outliers
b1 <- boxplot(t1$ovrmou_Mean)
getOUT <- which(t1$ovrmou_Mean %in% b1$out)
t1$ovrmou_Mean[getOUT] <- mean(t1$ovrmou_Mean)
summary(t1$ovrmou_Mean)

quantile(t1$ovrmou_Mean,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  ovrrev_Mean  ..........................

summary(t1$ovrrev_Mean)



d_ovrrev_Mean <- t1 %>% mutate(dec = ntile(ovrrev_Mean,4)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_ovrrev_Mean$N <- unclass(t1 %>% mutate(dec = ntile(ovrrev_Mean,4)) %>% count(dec) %>% 
                             unname())[[2]]
d_ovrrev_Mean$churn_perc <- d_ovrrev_Mean$n/d_ovrrev_Mean$N
d_ovrrev_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(ovrrev_Mean,4)) %>% 
                                       group_by(dec) %>% summarize(min(ovrrev_Mean)))[[2]]
d_ovrrev_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(ovrrev_Mean,4)) %>% 
                                    group_by(dec) %>% summarize(max(ovrrev_Mean)))[[2]]
d_ovrrev_Mean$varName <- rep("ovrrev_Mean",nrow(d_ovrrev_Mean))

d_ovrrev_Mean <- as.data.frame(d_ovrrev_Mean)

#removal of outliers
b1 <- boxplot(t1$ovrrev_Mean)
getOUT <- which(t1$ovrrev_Mean %in% b1$out)
t1$ovrrev_Mean[getOUT] <- mean(t1$ovrrev_Mean)
summary(t1$ovrrev_Mean)

quantile(t1$ovrrev_Mean,p = (0:100)/100,na.rm = TRUE)








# ........................... Variable :  owylis_vce_Range  ..........................

summary(t1$owylis_vce_Range)



d_owylis_vce_Range <- t1 %>% mutate(dec = ntile(owylis_vce_Range,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_owylis_vce_Range$N <- unclass(t1 %>% mutate(dec = ntile(owylis_vce_Range,10)) %>% count(dec) %>% 
                                  unname())[[2]]
d_owylis_vce_Range$churn_perc <- d_owylis_vce_Range$n/d_owylis_vce_Range$N
d_owylis_vce_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(owylis_vce_Range,10)) %>% 
                                            group_by(dec) %>% summarize(min(owylis_vce_Range)))[[2]]
d_owylis_vce_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(owylis_vce_Range,10)) %>% 
                                         group_by(dec) %>% summarize(max(owylis_vce_Range)))[[2]]
d_owylis_vce_Range$varName <- rep("owylis_vce_Range",nrow(d_owylis_vce_Range))

d_owylis_vce_Range <- as.data.frame(d_owylis_vce_Range)

#removal of outliers
b1 <- boxplot(t1$owylis_vce_Range)
getOUT <- which(t1$owylis_vce_Range %in% b1$out)
t1$owylis_vce_Range[getOUT] <- mean(t1$owylis_vce_Range)
summary(t1$owylis_vce_Range)

quantile(t1$owylis_vce_Range,p = (0:100)/100,na.rm = TRUE)








# ........................... Variable :  plcd_dat_Mean  ..........................

summary(t1$plcd_dat_Mean)



d_plcd_dat_Mean <- t1 %>% mutate(dec = ntile(plcd_dat_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_plcd_dat_Mean$N <- unclass(t1 %>% mutate(dec = ntile(plcd_dat_Mean,2)) %>% count(dec) %>% 
                               unname())[[2]]
d_plcd_dat_Mean$churn_perc <- d_plcd_dat_Mean$n/d_plcd_dat_Mean$N
d_plcd_dat_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(plcd_dat_Mean,2)) %>% 
                                         group_by(dec) %>% summarize(min(plcd_dat_Mean)))[[2]]
d_plcd_dat_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(plcd_dat_Mean,2)) %>% 
                                      group_by(dec) %>% summarize(max(plcd_dat_Mean)))[[2]]
d_plcd_dat_Mean$varName <- rep("plcd_dat_Mean",nrow(d_plcd_dat_Mean))
d_plcd_dat_Mean <- as.data.frame(d_plcd_dat_Mean)


#removal of outliers
b1 <- boxplot(t1$plcd_dat_Mean)
getOUT <- which(t1$plcd_dat_Mean %in% b1$out)
t1$plcd_dat_Mean[getOUT] <- mean(t1$plcd_dat_Mean)
summary(t1$plcd_dat_Mean)

quantile(t1$plcd_dat_Mean,p = (0:100)/100,na.rm = TRUE)








# ........................... Variable :  plcd_vce_Mean  ..........................

summary(t1$plcd_vce_Mean)



d_plcd_vce_Mean <- t1 %>% mutate(dec = ntile(plcd_vce_Mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_plcd_vce_Mean$N <- unclass(t1 %>% mutate(dec = ntile(plcd_vce_Mean,10)) %>% count(dec) %>% 
                               unname())[[2]]
d_plcd_vce_Mean$churn_perc <- d_plcd_vce_Mean$n/d_plcd_vce_Mean$N
d_plcd_vce_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(plcd_vce_Mean,10)) %>% 
                                         group_by(dec) %>% summarize(min(plcd_vce_Mean)))[[2]]
d_plcd_vce_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(plcd_vce_Mean,10)) %>% 
                                      group_by(dec) %>% summarize(max(plcd_vce_Mean)))[[2]]
d_plcd_vce_Mean$varName <- rep("plcd_vce_Mean",nrow(d_plcd_vce_Mean))
d_plcd_vce_Mean <- as.data.frame(d_plcd_vce_Mean)


#removal of outliers
b1 <- boxplot(t1$plcd_vce_Mean)
getOUT <- which(t1$plcd_vce_Mean %in% b1$out)
t1$plcd_vce_Mean[getOUT] <- mean(t1$plcd_vce_Mean)
summary(t1$plcd_vce_Mean)

quantile(t1$plcd_vce_Mean,p = (0:100)/100,na.rm = TRUE)









# ........................... Variable :  recv_sms_Mean  ..........................

summary(t1$recv_sms_Mean)



d_recv_sms_Mean <- t1 %>% mutate(dec = ntile(recv_sms_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_recv_sms_Mean$N <- unclass(t1 %>% mutate(dec = ntile(recv_sms_Mean,2)) %>% count(dec) %>% 
                               unname())[[2]]
d_recv_sms_Mean$churn_perc <- d_recv_sms_Mean$n/d_recv_sms_Mean$N
d_recv_sms_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(recv_sms_Mean,2)) %>% 
                                         group_by(dec) %>% summarize(min(recv_sms_Mean)))[[2]]
d_recv_sms_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(recv_sms_Mean,2)) %>% 
                                      group_by(dec) %>% summarize(max(recv_sms_Mean)))[[2]]
d_recv_sms_Mean$varName <- rep("recv_sms_Mean",nrow(d_recv_sms_Mean))
d_recv_sms_Mean <- as.data.frame(d_recv_sms_Mean)


#removal of outliers
b1 <- boxplot(t1$recv_sms_Mean)
getOUT <- which(t1$recv_sms_Mean %in% b1$out)
t1$recv_sms_Mean[getOUT] <- mean(t1$recv_sms_Mean)
summary(t1$recv_sms_Mean)

quantile(t1$recv_sms_Mean,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  rev_Mean  ..........................

summary(t1$rev_Mean)



d_rev_Mean <- t1 %>% mutate(dec = ntile(rev_Mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_rev_Mean$N <- unclass(t1 %>% mutate(dec = ntile(rev_Mean,10)) %>% count(dec) %>% 
                          unname())[[2]]
d_rev_Mean$churn_perc <- d_rev_Mean$n/d_rev_Mean$N
d_rev_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(rev_Mean,10)) %>% 
                                    group_by(dec) %>% summarize(min(rev_Mean)))[[2]]
d_rev_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(rev_Mean,10)) %>% 
                                 group_by(dec) %>% summarize(max(rev_Mean)))[[2]]
d_rev_Mean$varName <- rep("rev_Mean",nrow(d_rev_Mean))
d_rev_Mean <- as.data.frame(d_rev_Mean)


#removal of outliers
b1 <- boxplot(t1$rev_Mean)
getOUT <- which(t1$rev_Mean %in% b1$out)
t1$rev_Mean[getOUT] <- mean(t1$rev_Mean)
summary(t1$rev_Mean)

quantile(t1$rev_Mean,p = (0:100)/100,na.rm = TRUE)








# ........................... Variable :  rev_Range  ..........................

summary(t1$rev_Range)



d_rev_Range <- t1 %>% mutate(dec = ntile(rev_Range,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_rev_Range$N <- unclass(t1 %>% mutate(dec = ntile(rev_Range,10)) %>% count(dec) %>% 
                           unname())[[2]]
d_rev_Range$churn_perc <- d_rev_Range$n/d_rev_Range$N
d_rev_Range$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(rev_Range,10)) %>% 
                                     group_by(dec) %>% summarize(min(rev_Range)))[[2]]
d_rev_Range$LessThan <- unclass(t1 %>% mutate(dec = ntile(rev_Range,10)) %>% 
                                  group_by(dec) %>% summarize(max(rev_Range)))[[2]]
d_rev_Range$varName <- rep("rev_Range",nrow(d_rev_Range))
d_rev_Range <- as.data.frame(d_rev_Range)


#removal of outliers
b1 <- boxplot(t1$rev_Range)
getOUT <- which(t1$rev_Range %in% b1$out)
t1$rev_Range[getOUT] <- mean(t1$rev_Range)
summary(t1$rev_Range)

quantile(t1$rev_Range,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  roam_Mean  ..........................

summary(t1$roam_Mean)



d_roam_Mean <- t1 %>% mutate(dec = ntile(roam_Mean,2)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_roam_Mean$N <- unclass(t1 %>% mutate(dec = ntile(roam_Mean,2)) %>% count(dec) %>% 
                           unname())[[2]]
d_roam_Mean$churn_perc <- d_roam_Mean$n/d_roam_Mean$N
d_roam_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(roam_Mean,2)) %>% 
                                     group_by(dec) %>% summarize(min(roam_Mean)))[[2]]
d_roam_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(roam_Mean,2)) %>% 
                                  group_by(dec) %>% summarize(max(roam_Mean)))[[2]]
d_roam_Mean$varName <- rep("roam_Mean",nrow(d_roam_Mean))
d_roam_Mean <- as.data.frame(d_roam_Mean)


#removal of outliers
b1 <- boxplot(t1$roam_Mean)
getOUT <- which(t1$roam_Mean %in% b1$out)
t1$roam_Mean[getOUT] <- mean(t1$roam_Mean)
summary(t1$roam_Mean)

quantile(t1$roam_Mean,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  totcalls  ..........................

summary(t1$totcalls)



d_totcalls <- t1 %>% mutate(dec = ntile(totcalls,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_totcalls$N <- unclass(t1 %>% mutate(dec = ntile(totcalls,10)) %>% count(dec) %>% 
                          unname())[[2]]
d_totcalls$churn_perc <- d_totcalls$n/d_totcalls$N
d_totcalls$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(totcalls,10)) %>% 
                                    group_by(dec) %>% summarize(min(totcalls)))[[2]]
d_totcalls$LessThan <- unclass(t1 %>% mutate(dec = ntile(totcalls,10)) %>% 
                                 group_by(dec) %>% summarize(max(totcalls)))[[2]]
d_totcalls$varName <- rep("totcalls",nrow(d_totcalls))
d_roam_Mean <- as.data.frame(d_roam_Mean)


#removal of outliers
b1 <- boxplot(t1$totcalls)
getOUT <- which(t1$totcalls %in% b1$out)
t1$totcalls[getOUT] <- mean(t1$totcalls)
summary(t1$totcalls)

quantile(t1$totcalls,p = (0:100)/100,na.rm = TRUE)








# ........................... Variable :  totmrc_Mean  ..........................

summary(t1$totmrc_Mean)



d_totmrc_Mean <- t1 %>% mutate(dec = ntile(totmrc_Mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_totmrc_Mean$N <- unclass(t1 %>% mutate(dec = ntile(totmrc_Mean,10)) %>% count(dec) %>% 
                             unname())[[2]]
d_totmrc_Mean$churn_perc <- d_totmrc_Mean$n/d_totmrc_Mean$N
d_totmrc_Mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(totmrc_Mean,10)) %>% 
                                       group_by(dec) %>% summarize(min(totmrc_Mean)))[[2]]
d_totmrc_Mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(totmrc_Mean,10)) %>% 
                                    group_by(dec) %>% summarize(max(totmrc_Mean)))[[2]]
d_totmrc_Mean$varName <- rep("totmrc_Mean",nrow(d_totmrc_Mean))
d_totmrc_Mean <- as.data.frame(d_totmrc_Mean)


#removal of outliers
b1 <- boxplot(t1$totmrc_Mean)
getOUT <- which(t1$totmrc_Mean %in% b1$out)
t1$totmrc_Mean[getOUT] <- mean(t1$totmrc_Mean)
summary(t1$totmrc_Mean)

quantile(t1$totmrc_Mean,p = (0:100)/100,na.rm = TRUE)









# ........................... Variable :  totrev  ..........................

summary(t1$totrev)



d_totrev <- t1 %>% mutate(dec = ntile(totrev,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_totrev$N <- unclass(t1 %>% mutate(dec = ntile(totrev,10)) %>% count(dec) %>% 
                        unname())[[2]]
d_totrev$churn_perc <- d_totrev$n/d_totrev$N
d_totrev$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(totrev,10)) %>% 
                                  group_by(dec) %>% summarize(min(totrev)))[[2]]
d_totrev$LessThan <- unclass(t1 %>% mutate(dec = ntile(totrev,10)) %>% 
                               group_by(dec) %>% summarize(max(totrev)))[[2]]
d_totrev$varName <- rep("totrev",nrow(d_totrev))
d_totrev <- as.data.frame(d_totrev)


#removal of outliers
b1 <- boxplot(t1$totrev)
getOUT <- which(t1$totrev %in% b1$out)
t1$totrev[getOUT] <- mean(t1$totrev)
summary(t1$totrev)

quantile(t1$totrev,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  complete_mean  ..........................

summary(t1$complete_mean)



d_complete_mean <- t1 %>% mutate(dec = ntile(complete_mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_complete_mean$N <- unclass(t1 %>% mutate(dec = ntile(complete_mean,10)) %>% count(dec) %>% 
                               unname())[[2]]
d_complete_mean$churn_perc <- d_complete_mean$n/d_complete_mean$N
d_complete_mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(complete_mean,10)) %>% 
                                         group_by(dec) %>% summarize(min(complete_mean)))[[2]]
d_complete_mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(complete_mean,10)) %>% 
                                      group_by(dec) %>% summarize(max(complete_mean)))[[2]]
d_complete_mean$varName <- rep("complete_mean",nrow(d_complete_mean))
d_complete_mean <- as.data.frame(d_complete_mean)


#removal of outliers
b1 <- boxplot(t1$complete_mean)
getOUT <- which(t1$complete_mean %in% b1$out)
t1$complete_mean[getOUT] <- mean(t1$complete_mean)
summary(t1$complete_mean)

quantile(t1$complete_mean,p = (0:100)/100,na.rm = TRUE)







# ........................... Variable :  attempt_mean  ..........................

summary(t1$attempt_mean)



d_attempt_mean <- t1 %>% mutate(dec = ntile(attempt_mean,10)) %>% count(dec,churn) %>%
  filter(churn == 1)
d_attempt_mean$N <- unclass(t1 %>% mutate(dec = ntile(attempt_mean,10)) %>% count(dec) %>% 
                              unname())[[2]]
d_attempt_mean$churn_perc <- d_attempt_mean$n/d_attempt_mean$N
d_attempt_mean$GreaterThan <- unclass(t1 %>% mutate(dec = ntile(attempt_mean,10)) %>% 
                                        group_by(dec) %>% summarize(min(attempt_mean)))[[2]]
d_attempt_mean$LessThan <- unclass(t1 %>% mutate(dec = ntile(attempt_mean,10)) %>% 
                                     group_by(dec) %>% summarize(max(attempt_mean)))[[2]]
d_attempt_mean$varName <- rep("attempt_mean",nrow(d_attempt_mean))
d_attempt_mean <- as.data.frame(d_attempt_mean)


#removal of outliers
b1 <- boxplot(t1$attempt_mean)
getOUT <- which(t1$attempt_mean %in% b1$out)
t1$attempt_mean[getOUT] <- mean(t1$attempt_mean)
summary(t1$attempt_mean)

quantile(t1$attempt_mean,p = (0:100)/100,na.rm = TRUE)







#variables have negative minimum :
# change_mou
# totmrc_Mean
# rev_Mean
# eqpdays




t1$age1_N <- as.factor(t1$age1_N)
t1$age2_N <- as.factor(t1$age2_N)

t2 <- t1

t2$completed_perc <- round(t2$complete_mean/t2$totcalls,6)
t2$drp_blk_perc <- round(t2$drop_blk_Mean/t2$totcalls,6)
t2$attempted_call_perc <- round(t2$attempt_mean/t2$totcalls,6)
t2$opk_dat_perc <- round(t2$opk_dat_Mean/t2$totcalls,6)
t2$iwylis_vce_perc <- round(t2$iwylis_vce_Mean/t2$totcalls,6)
t2$custcare_call_perc <- round(t2$custcare_Mean/t2$totcalls,6)

chkMissing <- data.frame(colSums(is.na(t1)))

cpIND <- which(is.na(t2$completed_perc))
t2$completed_perc[cpIND] <- 0

dpIND <- which(is.na(t2$drp_blk_perc))
t2$drp_blk_perc[dpIND] <- 0

aIND <- which(is.na(t2$attempted_call_perc))
t2$attempted_call_perc[aIND] <- 0

oIND <- which(is.na(t2$opk_dat_perc))
t2$opk_dat_perc[oIND] <- 0

iwIND <- which(is.na(t2$iwylis_vce_perc))
t2$iwylis_vce_perc[iwIND] <- 0

ccIND <- which(is.na(t2$custcare_call_perc))
t2$custcare_call_perc[ccIND] <- 0

#outliers
names(t2)
rowt2 <- c(72:77)

for(i in 72:77)
{
  b1 <- boxplot(t2[,i])
  getIND <- which(t2[,i] %in% b1$out)
  t2[,i][getIND] <- mean(t2[,i])
}

#summary(t1$comp_vce_Mean)







#the following variables are interaction variables.
# ATTEMPT_MEAN =	PLCD_DAT_MEAN + PLCD_VCE_MEAN
# COMPLETE_MEAN	= COMP_DAT_MEAN + COMP_VCE_MEAN
# DROP_BLK_MEAN	 = BLCK_DAT_MEAN + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN
# OVRREV_MEAN	= DATOVR_MEAN + VCEOVR_MEAN

#Hence removing the inidividual variables

getIND <- which(names(t1) %in% c("plcd_dat_Mean","plcd_vce_Mean","comp_dat_Mean","comp_vce_Mean",
                                 "blck_dat_Mean","blck_vce_Mean","drop_dat_Mean","drop_vce_Mean",
                                 "datovr_Mean","vceovr_Mean"))
t1 <- t1[,-getIND]
names(t1)





#....................... Partition of Data


str(t1)
set.seed(200)
sampleIND <- sample(nrow(t1), 0.70 * nrow(t1),replace = FALSE)
train <- t1[sampleIND,]
test <- t1[-sampleIND,]

nrow(train)
nrow(test)


names(train)

mod <- glm(churn~.,data = train[,-c(44,47)],family = "binomial")
summary(mod)


#creating dummy variables for train data
train$asl_flag_Y <- ifelse(train$asl_flag == "Y",1,0)
train$refurb_new_R <- ifelse(train$refurb_new == "R",1,0)
train$area1_NORTHEAST <- ifelse(train$area1 == "NORTHEAST AREA",1,0)
train$area1_SOUTH <- ifelse(train$area1 == "SOUTH AREA",1,0)
train$area1_SOUTHWEST <- ifelse(train$area1 == "SOUTHWEST AREA",1,0)
train$area1_WEST <- ifelse(train$area1 == "WEST AREA",1,0)
train$ethnic1_C <- ifelse(train$ethnic1 == "C",1,0)
train$ethnic1_F <- ifelse(train$ethnic1 == "F",1,0)
train$ethnic1_N <- ifelse(train$ethnic1 == "N",1,0)
train$ethnic1_S <- ifelse(train$ethnic1 == "S",1,0)
train$ethnic1_U <- ifelse(train$ethnic1 == "U",1,0)
train$ethnic1_Z <- ifelse(train$ethnic1 == "Z",1,0)
train$hnd_webcap1_WCMB <- ifelse(train$hnd_webcap1 == "WCMB",1,0)
train$prizm_social_one1_R <- ifelse(train$prizm_social_one1 == "R",1,0)
train$prizm_social_one1_T <- ifelse(train$prizm_social_one1 == "T",1,0)
train$age1_N_Middle <- ifelse(train$age1_N == "Middle",1,0)
train$age1_N_Old <- ifelse(train$age1_N == "Old",1,0)
train$hnd_price_189.9899 <- ifelse(train$hnd_price1 == "89.9899",1,0)
train$hnd_price_1199.9899902 <- ifelse(train$hnd_price1 == "199.9899902",1,0)
train$hnd_price_1239.9899902 <- ifelse(train$hnd_price1 == "239.9899902",1,0)
train$hnd_price_1249.9899902 <- ifelse(train$hnd_price1 == "249.9899902",1,0)
train$income1_2 <- ifelse(train$income1 == "2",1,0)
train$models1_4 <- ifelse(train$models1 == "4",1,0)
train$truck1_Yes <- ifelse(train$truck1 == "Yes",1,0)
train$uniqsubs_1_2 <- ifelse(train$uniqsubs1 == "2",1,0)
train$uniqsubs_1_3 <- ifelse(train$uniqsubs1 == "3",1,0)
train$uniqsubs_1_4 <- ifelse(train$uniqsubs1 == "4",1,0)
train$uniqsubs_1_5 <- ifelse(train$uniqsubs1 == "5",1,0)
train$uniqsubs_1_6 <- ifelse(train$uniqsubs1 == "6",1,0)
train$uniqsubs_1_7 <- ifelse(train$uniqsubs1 == "7",1,0)
train$uniqsubs_1_8 <- ifelse(train$uniqsubs1 == "8",1,0)
train$uniqsubs_1_9 <- ifelse(train$uniqsubs1 == "9",1,0)




#Test data dummy variables
test$asl_flag_Y <- ifelse(test$asl_flag == "Y",1,0)
test$refurb_new_R <- ifelse(test$refurb_new == "R",1,0)
test$area1_NORTHEAST <- ifelse(test$area1 == "NORTHEAST AREA",1,0)
test$area1_SOUTH <- ifelse(test$area1 == "SOUTH AREA",1,0)
test$area1_SOUTHWEST <- ifelse(test$area1 == "SOUTHWEST AREA",1,0)
test$area1_WEST <- ifelse(test$area1 == "WEST AREA",1,0)
test$ethnic1_C <- ifelse(test$ethnic1 == "C",1,0)
test$ethnic1_F <- ifelse(test$ethnic1 == "F",1,0)
test$ethnic1_N <- ifelse(test$ethnic1 == "N",1,0)
test$ethnic1_S <- ifelse(test$ethnic1 == "S",1,0)
test$ethnic1_U <- ifelse(test$ethnic1 == "U",1,0)
test$ethnic1_Z <- ifelse(test$ethnic1 == "Z",1,0)
test$hnd_webcap1_WCMB <- ifelse(test$hnd_webcap1 == "WCMB",1,0)
test$prizm_social_one1_R <- ifelse(test$prizm_social_one1 == "R",1,0)
test$prizm_social_one1_T <- ifelse(test$prizm_social_one1 == "T",1,0)
test$age1_N_Middle <- ifelse(test$age1_N == "Middle",1,0)
test$age1_N_Old <- ifelse(test$age1_N == "Old",1,0)
test$hnd_price_189.9899 <- ifelse(test$hnd_price1 == "89.9899",1,0)
test$hnd_price_1199.9899902 <- ifelse(test$hnd_price1 == "199.9899902",1,0)
test$hnd_price_1239.9899902 <- ifelse(test$hnd_price1 == "239.9899902",1,0)
test$hnd_price_1249.9899902 <- ifelse(test$hnd_price1 == "249.9899902",1,0)
test$income1_2 <- ifelse(test$income1 == "2",1,0)
test$models1_4 <- ifelse(test$models1 == "4",1,0)
test$truck1_Yes <- ifelse(test$truck1 == "Yes",1,0)
test$uniqsubs_1_2 <- ifelse(test$uniqsubs1 == "2",1,0)
test$uniqsubs_1_3 <- ifelse(test$uniqsubs1 == "3",1,0)
test$uniqsubs_1_4 <- ifelse(test$uniqsubs1 == "4",1,0)
test$uniqsubs_1_5 <- ifelse(test$uniqsubs1 == "5",1,0)
test$uniqsubs_1_6 <- ifelse(test$uniqsubs1 == "6",1,0)
test$uniqsubs_1_7 <- ifelse(test$uniqsubs1 == "7",1,0)
test$uniqsubs_1_8 <- ifelse(test$uniqsubs1 == "8",1,0)
test$uniqsubs_1_9 <- ifelse(test$uniqsubs1 == "9",1,0)






names(train)
mod1 <- glm(churn~mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou +
              drop_blk_Mean + drop_vce_Range + mou_opkv_Range + months +
              eqpdays + custcare_Mean + iwylis_vce_Mean + ovrrev_Mean +
              avgmou + avg3qty +  + avgqty + asl_flag_Y + refurb_new_R + retdays +
              adjmou + totrev + area1_NORTHEAST + area1_SOUTHWEST + area1_WEST +
              area1_SOUTH + ethnic1_C + ethnic1_Z + ethnic1_U + ethnic1_S +
              ethnic1_N + ethnic1_F + hnd_webcap1_WCMB + prizm_social_one1_T +
              prizm_social_one1_R + age1_N_Old + age1_N_Middle + income1_2 +
              hnd_price_1249.9899902 + hnd_price_1239.9899902 +
              hnd_price_1199.9899902 + hnd_price_189.9899 + models1_4 +
              truck1_Yes + uniqsubs_1_9 + uniqsubs_1_8 + uniqsubs_1_7 +
              uniqsubs_1_6 + uniqsubs_1_5 + uniqsubs_1_4 + uniqsubs_1_3 +
              uniqsubs_1_2 + complete_mean,
            data = train[,-c(44,47)],family = "binomial")


summary(mod1)



mod2 <- glm(churn~mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou +
              drop_blk_Mean + drop_vce_Range + mou_opkv_Range + months +
              eqpdays + custcare_Mean + iwylis_vce_Mean + ovrrev_Mean +
              avgmou + avg3qty +  + avgqty + asl_flag_Y + refurb_new_R + retdays +
              adjmou + totrev + area1_NORTHEAST + area1_SOUTHWEST + area1_WEST +
              area1_SOUTH + ethnic1_C + ethnic1_Z + ethnic1_U + ethnic1_S +
              ethnic1_N + hnd_webcap1_WCMB + prizm_social_one1_T +
              prizm_social_one1_R + age1_N_Old + age1_N_Middle + 
              hnd_price_1249.9899902 + hnd_price_1239.9899902 +
              hnd_price_1199.9899902 + hnd_price_189.9899 + models1_4 +
              truck1_Yes + uniqsubs_1_9 + uniqsubs_1_7 + uniqsubs_1_4 + 
              uniqsubs_1_3 + uniqsubs_1_2 + complete_mean,
            data = train[,-c(44,47)],family = "binomial")

summary(mod2)



###### ............. Validation  of model

plot(mod2$residuals) #............random

df_vif <- as.data.frame(vif(mod2))
#no correlations found, all vif's < 10


#Cohen Kappa's matrix:

length(names(train))
length(names(test))

pred <- predict(mod2,type = "response",newdata = test)


#check churn rate 
table(t1$churn)/nrow(t1)

predVal <- ifelse(pred >= 0.2380827,1,0)
head(predVal)

#Cohen's kappa and Confusion Matrix :
confusionMatrix(table(predVal,test$churn),positive="1")

#Accuracy : 0.5902 ... looks good
#kappa : 0.1404 ( Less than chance agreement)

#           Reference
# Prediction    0    1
#          0 8697 1879
#          1 6081 2765                 


#Concordants : 2765
#Discordants : 8697
tprVal <- 2765/(1790+2854)
fprVal <- 6081/(6081+8697)

#TPR > FPR


#The model looks good



library(ROCR)

test$prob <- predict(mod2,type = "response",newdata = test)
pr1 <- prediction(test$prob,test$churn)
perf <- performance(pr1,"tpr","fpr")

library(graphics)
plot(perf)
abline(0,1,lty=8,col = "red")

aucVal <- performance(pr1,"auc")

aucVal@y.values

#Area under curve(AUC) value is 62.6341% , which is above the Random Chance Line.


t1NAMES <- data.frame(names(t1))









## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>     QUESTIONS    >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# 1. What are the top five factors driving likelihood of churn at Mobicom?

chkTop5 <- data.frame(Coeff = sort(mod2$coefficients,decreasing = TRUE))
factorNames <- head(row.names(chkTop5),5)

chkTop5$CoeffName <- row.names(chkTop5)
row.names(chkTop5) <- NULL

chkTop5 %>% filter(min_rank(desc(Coeff))<=5)




# top5 <- data.frame(factorNames,coeffVal)
# 
# finalFactors <- data.frame(sort(row.names(chkTop5)))


#Below are the top five factors driving lieklihood of churn
# There are 9 no.'s of unique subscribers in the household
# Current handset price is 239.9899902
# There are 7 no.'s of unique subscribers in the household
# There are 4 no.'s of unique subscribers in the household
# Handset is refurbished

#print(top5)
#              factorNames     Coeff
# 1           uniqsubs_1_9 1.2864100
# 2 hnd_price_1239.9899902 1.0922820
# 3           uniqsubs_1_7 0.7334257
# 4           uniqsubs_1_4 0.2291406
# 5           refurb_new_R 0.1701710

# Since these factors have all positive coefficients, a 1 unit increase in 
# these factors may lead to an average increase in the churn rate







#-------------------------------------------------------------------------


# 2. Validation of survey findings. a) Whether "cost and billing" and 
#   "network and service quality" are important factors influencing churn 
#   behaviour. b) Are data usage connectivity issues turning out to be
#   costly? In other words, is it leading to churn?

#Answer :


# a) -----  Check : Is "cost and billing" is an important factor ??

# The variables that deals with cost and billing are :

# ------------------------------ Cost -------------------------------

#   TOTMRC_MEAN	 : Monthly Recurring Charge / base cost of the calling 
#                  plan regardless of actual minutes used.

# From the model, we deduce that "totmrc_Mean" has a negative impact with
# coefficient value of "-0.005065220"
# Hence, a unit increase in the cost and billing factor will result in a unit
# decrease in the churn rate

#------------------------------ Billing -------------------------------

# Factors of Billing earned through revenue : 

#   DATOVR_MEAN  :	Mean revenue of data overage
#  ..... Overage represents calls or minutes of use over the number of minutes 
#  allowed by that customer's calling plan.

#   DATOVR_RANGE :	Range of revenue of data overage
#   REV_MEAN	   :  Mean monthly revenue (charge amount)
#   REV_RANGE	   :  Range of revenue (charge amount)
#   RMREV	       :  Total revenue of roaming calls
#   OVRREV_MEAN	 :  Mean overage revenue
#   OVRREV_RANGE :  Range of overage revenue
#   TOTREV	     :  Total revenue
#   VCEOVR_MEAN	 :  Mean revenue of voice overage
#   VCEOVR_RANGE :  Range of revenue of voice overage



# The model gives us a positive coefficient "0.002260397" for the factor
# "REV_RANGE"
# ..........this implies it has a postitive impact,
# i.e. a unit increase in the revenue will have a unit increase in the churn 
# behaviour, thus leading to churn


# Factor "OVRREV_MEAN" gives us a coefficient of value "0.008319130"
# ..........this implies it has a postitive impact,
# i.e. a unit increase in the overage revenue will have a unit increase in the 
# churn behaviour, thus leading to churn


# Factor "TOTREV" gives us a coefficient of value "0.0001651900"
# ..........this implies it has a postitive impact,
# i.e. a unit increase in the total revenue earned,will have a unit increase in 
# the churn behaviour, thus leading to churn


# Conclusion : 
# Since the unit increase is very miniscule, the "cost and billing" is
# not a really important factor influencing churn behaviour.






# b) -----  Check : Is "network and service quality" is an important factor ??


# ---- Service related variables:
# CHANGE_MOU	Percentage change in monthly minutes of use vs previous three month average
# EQPDAYS Number of days (age) of current equipment
# MONTHS	Total number of months in service
# MOU_MEAN	Mean number of monthly minutes of use
# MOU_RANGE	Range of number of minutes of use
# IWYLIS_VCE_MEAN	Mean number of inbound wireless to wireless voice calls
# RETDAYS Number of days since last retention call

getIND <- which(toupper(row.names(chkTop5)) %in% c("CHANGE_MOU","EQPDAYS","MONTHS","MOU_MEAN",
                                                   "MOU_RANGE","IWYLIS_VCE_MEAN",
                                                   "RETDAYS"))

s1 <- data.frame(Factors = row.names(chkTop5)[getIND],Coefficient = chkTop5$Coeff[getIND])

print(s1)



#           Factors   Coefficient
#      .............................
# 1         retdays  0.0009504146
# 2         eqpdays  0.0008637115
# 3       mou_Range  0.0003390077
# 4      change_mou -0.0004522552
# 5        mou_Mean -0.0004975644
# 6 iwylis_vce_Mean -0.0157049474
# 7          months -0.0181583284

#retdays : Higher the number of days since last retention call, 
#          higher is the churn rate
#change_mou : Less % change in minutes of usage vs previous months, 
#             will lead to increase in churn rate
#mou_Mean : Less number of monthly minutes of usage higher is the churn rate
#iwylis_vce_Mean : Providing less service of wireless voice calls will attract 
#                  higher churn rate
#months : less no. of months the plan is in service, the higher is the churn rate


summary(t1$retdays)
quantile(t1$retdays,p=(0:100)/100,na.rm = TRUE)
#98% of the data shows over 3 months since last retention call

#Hence service is bad

summary(t2$attempted_call_perc * 100)
# only 19% of the total calls are placed data calls and placed voice calls.


summary(t1$change_mou)
quantile(t1$change_mou,p=(0:100)/100,na.rm = TRUE)
#Over 60% shows no change in MOU's were provided to the customer.



# ----- Network related variables:  
# COMPLETE_MEAN	Mean number of completed calls
# DROP_BLK_MEAN	Mean number of dropped or blocked calls
# DROP_VCE_RANGE	Range of number of dropped (failed) voice calls
# MOU_OPKV_RANGE	Range of unrounded minutes of use of off-peak voice calls  
# IWYLIS_VCE_MEAN	Mean number of inbound wireless to wireless voice calls

getIND <- which(toupper(row.names(chkTop5)) %in% c("COMPLETE_MEAN","DROP_BLK_MEAN","DROP_VCE_RANGE",
                                                   "MOU_OPKV_RANGE","IWYLIS_VCE_MEAN"))

s1 <- data.frame(Factors = row.names(chkTop5)[getIND],Coefficient = chkTop5$Coeff[getIND])

print(s1)

#           Factors   Coefficient
# ------------------------------------
# 1  drop_vce_Range  0.0113320803
# 2   drop_blk_Mean  0.0083291629
# 3  mou_opkv_Range -0.0008413187
# 4   complete_mean -0.0009764040
# 5 iwylis_vce_Mean -0.0157049474

# Increase in the number of dropped (failed) voice calls, results in high churn rate
# Increase in the number of dropped or blocked calls, results in high churn rate
# Decrease in providing use of off-peak voice calls, will increase the churn rate
# Decrease in the number of completed calls will increase the churn rate
# Decrease in the number of inbound wireless voice calls will increase churn rate

summary(t2$completed_perc * 100)
# only 14% of the total calls are completed calls, which is less

summary(t2$attempted_call_perc * 100)
# only 19% of the total calls are placed data calls and placed voice calls.

summary(t2$opk_dat_perc * 100)
#number of off-peak calls made are very less

summary(t2$iwylis_vce_perc,100)
#number of wireless calls made are very less



#Conclusion :

# Considering the below factors :
#1. 98% of the data shows over 3 months since last retention call
#2. only 19% of the total calls are placed data calls and placed voice calls.
#3. Over 60% shows no change in MOU's were provided to the customer.
#4. only 14% of the total calls are completed calls, which is less
#5. only 19% of the total calls are placed data calls and placed voice calls.
#6. number of off-peak calls made are very less
#7. number of wireless calls made are very less


# ,the company should provide better service and network quality to the customers
# to reduce the churn rate and increase the revenue







# c) -----  Are data usage connectivity issues turning out to be costly, 
#    i.e leading to churn ? 

# The variables that deals with data usage connectivity issues are :
# COMPLETE_MEAN	Mean number of completed calls
# ATTEMPT_MEAN	Mean number of dropped or blocked calls
# DROP_VCE_RANGE	Range of number of dropped (failed) voice calls
# DROP_BLK_MEAN	Mean number of dropped or blocked calls
# MOU_OPKV_RANGE	Range of unrounded minutes of use of off-peak voice calls  
# IWYLIS_VCE_MEAN	Mean number of inbound wireless to wireless voice calls

getIND <- which(toupper(row.names(chkTop5)) %in% c("COMPLETE_MEAN","ATTEMPT_MEAN","DROP_BLK_MEAN","DROP_VCE_RANGE",
                                                   "MOU_OPKV_RANGE","IWYLIS_VCE_MEAN"))

s1 <- data.frame(Factors = row.names(chkTop5)[getIND],Coefficient = chkTop5$Coeff[getIND])

print(s1)

summary(t2$completed_perc * 100)
# only 14% of the total calls are completed calls, which is less

summary(t2$attempted_call_perc * 100)
# only 19% of the total calls are placed data calls and placed voice calls.

summary(t2$opk_dat_perc * 100)
#number of off-peak calls made are very less

summary(t2$iwylis_vce_perc,100)
#number of wireless calls made are very less


# Conclusion : 
# The above factors imply that there might have been data connectivity issues,
# which has resulted in less number of completed calls,data and voice calls,
# off-peak calls and wireless calls.





#-------------------------------------------------------------------------

# 3. Would you recommend rate plan migration as a proactive retention 
#    strategy?

# rate plan migration is a factor that depends on the overage usage

# Specifically, customer's rate plan suitability is determined based on their 
# voice usage, the monthly rate of their selected plans, and the associated 
# overage charges.
# Given that rate plan is not available on the data file, a proxy for 
# non-optimal rate plan could be higher overage revenue as a percentage of 
# total revenue. 

# OVRREV_MEAN	DATOVR_MEAN + VCEOVR_MEAN 
#  ---> Mean overage revenue is the sum of data and voice overage revenues.

# Coefficient : 0.008319130

overage_rev_perc <- t1$ovrrev_Mean/t1$totrev * 100
summary(overage_rev_perc)

summary(t1$ovrrev_Mean)
quantile(overage_rev_perc,p = (0:100)/100, na.rm = TRUE)



# Conclusion : There is less overage revenue obtained.Hence, number of customers 
# having overage charges are less, i.e. the overage usage is less.
# In this case, rate plan migration cannot be used as a proactive retention 
#strategy.






#-------------------------------------------------------------------------

# 4. What would be your recommendation on how to use this churn model 
#    for prioritisation of customers for a proactive retention campaigns 
#    in the future?

#Gains Chart
library(gains)
gains(test$churn,predict(mod2,type = "response",newdata = test),groups = 10)

#64% of the customers show a probability to churn in the top 50% of 
#the probabilities

#Retrieving the probabilities :
quantile(test$prob,p =c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.0),na.rm = TRUE)

#Customer's who are likely to churn
getCustomerID <- test[(test$prob>=0.2287425 & test$prob<=0.7611956),]$Customer_ID

g1 <- data.frame(CustomerID = getCustomerID)

write.csv(g1,"F://Final Case Study//CustomerChurnPrediction.csv",
          row.names = FALSE)





#-------------------------------------------------------------------------

# 5. What would be the target segments for proactive retention campaigns? 
#    Falling ARPU forecast is also a concern and therefore, Mobicom would 
#    like to save their high revenue customers besides managing churn. 
#    Given a budget constraint of a contact list of 20% of the subscriber 
#    pool, which subscribers should prioritized if "revenue saves" is 
#    also a priority besides controlling churn. In other words, 
#    controlling churn is the primary objective and revenue saves is the 
#    secondary objective

#ARPU : Average revenue per user

# avgrev : Average monthly revenue over the life of the customer
# totrev : Total revenue

#Considering a contact list of 20% of the subscriber pool :
gains(test$churn,predict(mod2,type = "response",newdata = test),groups = 10)

#Retrieving the probabilties :
quantile(test$prob,p =c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.0),na.rm = TRUE)

#Probability of churn scores vs revenue :

test$score_segment <- ifelse(test$prob<=0.20,"Low",
                             ifelse((test$prob>0.20 & test$prob<=0.3019),"Medium ","High"))


summary(test$totrev)



quantile(test$totrev,p =c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.0),na.rm = TRUE)
test$revenue_segment <- ifelse(test$totrev<=666.550,"Low",
                               ifelse((test$totrev>666.550 & test$totrev<=1250.710),"Medium","High"))


table(test$score_segment,test$revenue_segment,dnn = c("Probability Churn Score","   Revenue"))

#                           Revenue
# Probability Churn Score High  Low Medium
#                 High    1177  902   1808
#                 Low     1112 3090   2536
#                 Medium  1596 3777   3424
# 
# There are 1112 customers with low churn rate and giving higher revenue 

getCustomerID <- test[(test$score_segment=="Low" & test$revenue_segment=="High"),]$Customer_ID

g1 <- data.frame(CustomerID = getCustomerID)

write.csv(g1,"F://Final Case Study//CustomersForRetention.csv",
          row.names = FALSE)



