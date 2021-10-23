#Load data
setwd("C:/Users/Seizal Pathania/Downloads/Rfiles")
patents <- read.csv("patents_10.csv", header = TRUE)
head(patents)
View(patents)

#EDA
sum(table(patents$patnum))
sum(table(patents$patnum_kpss))
patents$patnum_kpss = gsub("D","", as.character(patents$patnum))
View(patents)
sum(table(patents$ptype))
sum(table(patents$applnum))
sum(table(patents$ee_number))
sum(table(patents$ee_name))
sum(table(patents$ee_role))
sum(table(patents$ee_role_desc))
sum(table(patents$ee_state))
cor(patents$grantyear,patents$X)
anov=aov(patents$grantyear~patents$ee_ind_fname,data=patents)
summary(anov)
anov=aov(patents$grantyear~patents$ee_ind_lname,data=patents)
summary(anov)
patent=patents[-c(1,3,10,11)]
View(patents)

#count of patents granted by each country
library(dplyr)
patents_by_country = patents %>% group_by(ee_country) %>% summarize(count = n())
head(patents_by_country)
View(patents_by_country)

#Visualising the Patents by countries data
pie(patents_by_country$count,labels = patents_by_country$ee_country,main = "Patents By Country",col= rainbow(length(patents_by_country$count)))
legend("topright",patents_by_country$ee_country,cex=0.5,fill = rainbow(length(patents_by_country$count)))
d1<- density(patents$backward_cites)
plot(d1,main = "Desity of Backward Cities")
d2<- density(patents$forward_cites)
plot(d2,main = "Desity of Forward Cities")

#patents in the US count of type of patents granted by US cities
patents_us = subset(patents, ee_country == "US")
nrow(patents_us)
View(patents_us)
Us_p= patents_us%>% group_by(ee_city,ptype) %>% summarize(count = n())
View(Us_p)
Us_p_type= Us_p%>% group_by(ptype) %>% summarize(count = n())
head(Us_p_type)

#Visualising the data
hist(Us_p_type$count,main = "Types of Patents in US",xlab = "Patent Types Count",col="darkmagenta")

#patents in the India and count of type of patents granted by IN cities
patents_in =subset(patents, ee_country == "IN")
nrow(patents_in)
View(patents_in)
In_p= patents_in%>% group_by(ee_city,ptype) %>% summarize(count = n())
View(In_p)
In_p_type= In_p%>% group_by(ptype) %>% summarize(count = n())
head(In_p_type)

#Visualising the data
hist(In_p_type$count,main = "Types of Patents in In",xlab = "Patent Types Count",col="darkgreen")

#how long does it take to get granted in the US
patents_us$time_to_approval=patents_us$grantyear - patents_us$applyear
sort(table(patents_us$time_to_approval))
a<-aggregate(time_to_approval ~ ee_state,data = patents_us, FUN = mean)

#Visualising the time taken to approve
hist(a$time_to_approval,main = "Time taken to get granted in Us",xlab = "Time",col = "red")
pie(a$time_to_approval,main = "Time taken to get granted in Us")

#how long does it take to get granted in IN
patents_in$time_to_approval=patents_in$grantyear - patents_in$applyear
table(patents_in$time_to_approval)
b<-aggregate(time_to_approval ~ ee_city,data = patents_in, FUN = mean)

#Visualising the time taken to approve
hist(b$time_to_approval,main = "Time taken to get granted in In",xlab = "Time",col = "blue")
pie(b$time_to_approval,main = "Time taken to get granted in In")

#Counting how many patents are granted per type of patents in countries
table(patents$ptype)
patents_types = patents %>% group_by(ee_country,ptype) %>% summarize(count = n())
head(patents_types)
View(patents_types)


