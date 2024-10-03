Crime <- read.csv("Crime.csv")
View(Crime)

#Data Cleaning and Processing ####
####Crime Data Cleaning and Processing ####
#Removing Garbage Data
Crime_clean <- Crime
Crime_clean[,c(1,2,3,4,5,9,10,11,14,17,18,19,20,21,22,23,26,29,30)]<- NULL
Crime_clean
View(Crime_clean)

#Treating NULL values
library(dplyr)
Crime_clean <- Crime_clean%>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
colSums(is.na(Crime_clean))
Crime_clean <- na.omit(Crime_clean)
colSums(is.na(Crime_clean))
View(Crime_clean)

#Keeping Maryland state data only 
table(Crime_clean$State)
Crime_clean$State <- ifelse(Crime_clean$State!="MD", NA, Crime_clean$State)
Crime_clean <- na.omit(Crime_clean)
table(Crime_clean$State)
View(Crime_clean)

(nrow(Crime)-nrow(Crime_clean))/nrow(Crime)*100 #0.67% data is gone
table(Crime_clean$Place)

#Separating Place data
Crime_clean$Place <- sapply(strsplit(Crime_clean$Place, split='-', fixed=TRUE), function(x) (x[1]))
View(Crime_clean)

#Treating Date and Time
Crime_clean <- Crime_clean%>%
  mutate(Date=sapply(strsplit(Crime_clean$Start_Date_Time, split=' ', fixed=TRUE), function(x) (x[1])))
View(Crime_clean)
Crime_clean <- Crime_clean%>%
  mutate(Time=sapply(strsplit(Crime_clean$Start_Date_Time, split=' ', fixed=TRUE), function(x) (x[2])))
View(Crime_clean)

Crime_clean$Date <- as.Date(Crime_clean$Date, format = "%m/%d/%Y") #Changing data type to date
str(Crime_clean$Date)
View(Crime_clean)

install.packages("lubridate")
library(lubridate)

Crime_clean$Month <- strftime(Crime_clean$Date, format="%b")
Crime_clean$Year <- strftime(Crime_clean$Date, format="%Y")
Crime_clean$Day <- weekdays(as.Date(Crime_clean$Date))
View(Crime_clean)

Crime_clean$Start_Date_Time <- NULL
(nrow(Crime)-nrow(Crime_clean))/nrow(Crime)*100


####Violent Crime Data Processing ####
library(dplyr)
Violent_Crime <- read.csv("Violent_Crime___Property_Crime_by_County__1975_to_Present.csv")
Violent_Crime_Clean <- Violent_Crime%>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
colSums(is.na(Violent_Crime_Clean))
View(Violent_Crime_Clean)
Violent_Crime_Clean[, 12:38] <- NULL
colSums(is.na(Violent_Crime_Clean))
View(Violent_Crime_Clean)

Violent_Crime_Clean_temp<-Violent_Crime_Clean #For H-2 (temp)
View(Violent_Crime_Clean_temp)

Violent_Crime_Clean <- Violent_Crime_Clean%>%
  filter(YEAR==2020)%>%
  arrange(JURISDICTION)
View(Violent_Crime_Clean)
str(Violent_Crime_Clean)

####Maryland Counties Socioeconomic Characteristics ####
Maryland_Socioeconomic <- read.csv("Maryland_Counties_Socioeconomic_Characteristics.csv")
View(Maryland_Socioeconomic)


Maryland_Socioeconomic_clean <- Maryland_Socioeconomic%>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
colSums(is.na(Maryland_Socioeconomic_clean))
View(Maryland_Socioeconomic_clean)

Maryland_Socioeconomic_clean <- Maryland_Socioeconomic_clean%>%
  arrange(Jurisdictions)

Maryland_Socioeconomic_clean[, c(2,3, 11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 26, 27, 28, 29, 30, 37, 38, 39, 40)] <- NULL
View(Maryland_Socioeconomic_clean)

####Combining Maryland socioeconomic and violent crime data ####
Maryland_temp <- Maryland_Socioeconomic_clean[,-1]
Socioeconomic_ViolentCrime <- cbind(Violent_Crime_Clean,Maryland_temp)
View(Socioeconomic_ViolentCrime)

Socioeconomic_ViolentCrime$JURISDICTION<-as.factor(Socioeconomic_ViolentCrime$JURISDICTION)
Socioeconomic_ViolentCrime$YEAR<-as.factor(Socioeconomic_ViolentCrime$YEAR)

Socioeconomic_ViolentCrime<-Socioeconomic_ViolentCrime %>% 
  mutate_if(is.character,as.numeric)
str(Socioeconomic_ViolentCrime)


Socioeconomic_ViolentCrime <- Socioeconomic_ViolentCrime%>%
  mutate(Families.in.Poverty= round(Families*(Percent.Families.in.Poverty/100)))
View(Socioeconomic_ViolentCrime)

#Data Exploration ####

library(ggplot2)
Crime_clean <- Crime_clean%>%
  mutate(Hour=sapply(strsplit(Crime_clean$Time, split=':', fixed=TRUE), function(x) (x[1])))
Crime_clean$Hour<-as.numeric(Crime_clean$Hour)
Crime_clean <- Crime_clean%>%
  mutate(Day_night=ifelse(Crime_clean$Hour<8 | Crime_clean$Hour >20,"Night","Day"))

# calculate frequencies
countc1 <- table(Crime_clean$Crime.Name1)
countc2 <- table(Crime_clean$Crime.Name2)
countp <- table(Crime_clean$Place)

# sort
countc1s <- sort(countc1)
countc2s <- sort(countc2)
countps <- sort(countp)
# extract 10 most frequent nationalities
top10a <- tail(names(countc1s), 10)
top10b <- tail(names(countc2s), 10)
top10p <- tail(names(countps), 10)
# subset of data frame
d_s <- subset(Crime_clean, Crime.Name1 %in% top10a)
d_a <- subset(Crime_clean, Crime.Name2 %in% top10b)
d_p <- subset(Crime_clean, Place %in% top10p)
# order factor levels
d_a$Crime.Name2 <- factor(d_s$Crime.Name2, levels=rev(top10a))
d_s$Crime.Name1 <- factor(d_s$Crime.Name1, levels=rev(top10b))
d_p$Place <- factor(d_p$Place, levels = rev(top10p))

####Plots####

#Q1.What is the frequency of each type of crime (against person, property, society or other)?
Q1 <- ggplot(d_a, aes(x=fct_infreq(Crime.Name2)))+
  labs(title = "Top 10 Types of Crime") +
  xlab("Crime Classification") +
  ylab ("Frequency") +
  theme_minimal(base_size = 12) +
  geom_bar(aes(fill=Crime.Name1))+
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar(aes(fill=Crime.Name1))+
  coord_flip()
Q1

#Q2.Where in Montgomery were these crimes most likely to occur?
library(forcats)
Q2 <- ggplot(d_p, aes(x=fct_infreq(Place)))+
  labs(title = "Top 10 Crimes occuring at different Places") +
  xlab("Places of incident") +
  ylab ("Frequency") +
  theme_minimal(base_size = 12) +
  geom_bar(aes(fill=Crime.Name1))+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()

Q2

#View(Crime_clean)

Q3 <- ggplot(d_a, aes(x=fct_infreq(Crime.Name2)))+ 
  labs(title = "Top 10 Crimes occuring During Day and Night") +
  xlab("Crimes") +
  ylab ("Frequency") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip() +
  geom_bar(aes(fill=Day_night))

Q3

Q4 <- ggplot(d_s, aes(x=fct_infreq(Day_night)))+
  labs(title = "Crimes Classification During Day and Night") +
  xlab("Day/Night") +
  ylab ("Frequency") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_bar(aes(fill=Crime.Name1))
Q4

Q5 <- d_s%>%
  filter(Crime.Name1!="Not a Crime",Crime.Name1!="Other")%>%
  ggplot(aes(x=Hour))+
  geom_density(aes(fill=Crime.Name1), alpha=0.5)+
  labs(title = "DIstribution of types of crimes During Hours of the day") +
  xlab("Hours") +
  ylab ("Density") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90))
Q5

####Map####
install.packages("sf")
install.packages("mapview")
library(sf)
library(mapview)
md_sf <- st_as_sf(Crime_clean, coords = c("Longitude", "Latitude"),  crs = 4326)
mapview(md_sf, map.types = "Stamen.Toner", zcol="Crime.Name1")

mapview(map.types="Stamen.Toner")

#Hypothesis ####

####H-1 Logistic Regression ####

Crime_clean <- Crime_clean%>%
  mutate(Hour=sapply(strsplit(Crime_clean$Time, split=':', fixed=TRUE), function(x) (x[1])))
Crime_clean$Hour<-as.numeric(Crime_clean$Hour)
Crime_clean <- Crime_clean%>%
  mutate(Day_night=ifelse(Crime_clean$Hour<8 | Crime_clean$Hour >20,"Night","Day"))
set.seed(7)
library(dplyr)
temp_crime<-Crime_clean
temp_crime <- temp_crime %>% select(-c("City","Latitude","Longitude","Victims","Crime.Name2","State","Agency","Time","Date"))
temp_crime$Crime.Name1<-as.factor(temp_crime$Crime.Name1)
temp_crime$Place<-as.factor(temp_crime$Place)
temp_crime$Street.Type<-as.factor(temp_crime$Street.Type)
temp_crime$Month<-as.factor(temp_crime$Month)
temp_crime$Year<-as.factor(temp_crime$Year)
temp_crime$Day<-as.factor(temp_crime$Day)
temp_crime$Day_night<-as.factor(temp_crime$Day_night)
install.packages("caTools")
install.packages("ROCR")
install.packages("nnet")
install.packages("caret")
library(caTools)
library(ROCR)
library(nnet)
library(caret)

training.samples<-temp_crime$Crime.Name1 %>% createDataPartition(p=0.8,list=FALSE)
train.data <- temp_crime[training.samples,]
test.data<- temp_crime[-training.samples,]
model<- nnet::multinom(Crime.Name1 ~.,data=train.data,maxit=500)
head(model)
predictions<- model %>% predict(test.data)
mean(predictions==test.data$Crime.Name1)

#Confusion Matrix
cm<-confusionMatrix(test.data$Crime.Name1,predictions)
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
cm
ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="grey", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Against Person","Against Class","Against Property","Against Society","Not a Crime")) +
  scale_y_discrete(labels=c("Not a Crime","Against Society","Against Property","Against Class","Against Person"))

####H-2 #### Regression, Random Forest feature selection, H-clust, Kmeans, 

#Regression Analysis 

modcoef <- lm(GRAND.TOTAL ~Female+ POPULATION + Less.than.9th.Grade + High.School.no.Diploma + High.School.Diploma + Some.College.no.degree + Associates.degree + Bachelor.s.degree + Graduate.or.Professional + Employed + Unemployed + Median.Household.Income.... + Families +Voting.Age.Population+Male+Female+White.Alone+Black.Alone+Asian.Alone+Hispanic.or.Latino..of.any.race.+Families.in.Poverty, data= Socioeconomic_ViolentCrime)

modcoef <- summary(model)[["coefficients"]]
modcoef[order(modcoef[ , 4]), ]  

library(dplyr)
library(corrplot)

View(Socioeconomic_ViolentCrime)

temp_socio <- Socioeconomic_ViolentCrime %>% select(c(POPULATION,Less.than.9th.Grade , High.School.no.Diploma , High.School.Diploma , Some.College.no.degree , Associates.degree , Bachelor.s.degree , Graduate.or.Professional , Employed , Unemployed , Median.Household.Income.... , Families ,Voting.Age.Population,Male,Female,White.Alone,Black.Alone,Asian.Alone,Hispanic.or.Latino..of.any.race.,Families.in.Poverty))
View(temp)

View(Socioeconomic_ViolentCrime)
#Random forest for feature selection
xyz = Socioeconomic_ViolentCrime$GRAND.TOTAL
set.seed(7)
install.packages("mlbench")
install.packages("caret")
install.packages("randomForest")
library(randomForest)
library(mlbench)
library(caret)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results<-rfe(temp_socio,y=xyz,rfeControl=control)
results
install.packages("NbClust")
library("NbClust")

new_socio <- Socioeconomic_ViolentCrime %>% select(c("JURISDICTION","Families.in.Poverty","Black.Alone","Female","Less.than.9th.Grade",GRAND.TOTAL))
#View(new_socio)
str(new_socio)
new_socio<-new_socio %>% mutate_if(is.numeric,scale)
# NORMAL H CLUST
dis_spi<- dist(as.matrix(new_socio[,2:6]), method="euclidean")
dis_spi
par(mfrow=c(1,1))
hc_spi_com<-hclust(dis_spi, method = "complete") 
plot(hc_spi_com,hang=-1, labels = new_socio$JURISDICTION)
nb<- NbClust(new_socio[,2:6], distance = "euclidean", min.nc = 2,max.nc=10, method = "complete", index ="all")
par(mfrow=c(1,1))
plot(hc_spi_com,hang=-1, labels = new_socio$JURISDICTION)
rect.hclust(hc_spi_com,k=2,border = "green")

# K-Means

fviz_nbclust(new_socio[,2:6], kmeans, method = "wss")  #ELBOW METHOD

kout1<-kmeans(new_socio[,2:6],centers=2, iter.max = 10, nstart = 10) # KMEANS
kout1
max(kout1$centers[,1])-min(kout1$centers[,1]) # highest effect
max(kout1$centers[,2])-min(kout1$centers[,2]) 
max(kout1$centers[,3])-min(kout1$centers[,3])
max(kout1$centers[,4])-min(kout1$centers[,4]) # 2nd highest effect


cluster_plot<-ggplot(new_socio,aes(x=Families.in.Poverty,y=Less.than.9th.Grade))+
  labs(title = "Visualization of Clusters formed")+
  geom_point(aes(col=factor(kout1$cluster)),size=3)+
  geom_text(label=new_socio$JURISDICTION,size=2.5,vjust=2)
cluster_plot


poverty_centroid <- tapply(new_socio[,2:6]$Families.in.Poverty,kout1$cluster, mean)
grade_centroid <- tapply(new_socio[,2:6]$Less.than.9th.Grade,kout1$cluster, mean)
centers <- data.frame(poverty_centroid,grade_centroid) # centroids
centers
c<-cluster_plot+geom_point(data = centers,aes(poverty_centroid,grade_centroid),color="black",pch=4,size=4)
c

#heat map (1)

Violent_Crime <- Violent_Crime %>%
  filter(YEAR == "2020")


View(Violent_Crime)
socio2 <- Violent_Crime[,-1]
rownames(socio2) <- Violent_Crime[,1]
View(socio2)
socio2 <- socio2[,24:30]
str(socio2)

names(socio2)[1]<-"Murder"
names(socio2)[2]<-"Rape"
names(socio2)[3]<-"Robbery"
names(socio2)[4]<-"Agg Assault"
names(socio2)[5]<-"B/E"
names(socio2)[6]<-"Larceny"
names(socio2)[7]<-"M.V Theft"


socio2 <- as.matrix(socio2)
heatmap(socio2, scale = "column")
heatmap(socio2, Colv = NA, Rowv = NA, scale = "column" )


## (2)
Top_6 <- Violent_Crime_Clean_temp%>%
  select(JURISDICTION, GRAND.TOTAL)
View(Top_6)

Top_6 <- sort(GRAND.TOTAL, decreasing = TRUE)

library(ggrepel)
Violent_Crime_Clean_temp$YEAR <- as.numeric(Violent_Crime_Clean_temp$YEAR)
Violent_Crime_Clean_temp$GRAND.TOTAL <- as.numeric(Violent_Crime_Clean_temp$GRAND.TOTAL)

counties <- c("Baltimore City", "Prince George's County", "Baltimore County", "Montgomery County", "Anne Arundel County", "Howard County")

Violent_Crime_Clean_temp%>%
  filter(JURISDICTION %in% counties)%>%
  ggplot(aes(YEAR, GRAND.TOTAL))+
  geom_line(aes(color=as.factor(JURISDICTION)), size=1.5)+
  geom_hline(yintercept=mean(Violent_Crime_Clean_temp$GRAND.TOTAL),color="black")+
  labs(x="Year", y="Grand Total of Crimes")
## (5)
cop <- socio2
install.packages("DataExplorer")  
library(DataExplorer)
plot_correlation(cop)

## (4)


ggplot(Socioeconomic_ViolentCrime, aes(x=JURISDICTION, y=Percent.Families.in.Poverty))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))


####H-3 A-Rules ####

table(Crime_clean$Victims)
Crime_clean$Victims<-as.numeric(Crime_clean$Victims)
table(Crime_clean$Victims)

str(Crime_clean)
crime_arules<-Crime_clean[,c("Place","Street.Type","Day","Day_night","Crime.Name2","Victims")]
View(crime_arules)
crime_arules$Victims<-ifelse(crime_arules$Victims==1,"Single",ifelse(crime_arules$Victims==2,"Pair","Group"))
View(crime_arules)
table(crime_arules$Victims)
crime_arules<-crime_arules%>%
  mutate(Place=as.factor(Place),Street.Type=as.factor(Street.Type),Day=as.factor(Day),Day_night=as.factor(Day_night),Crime.Name2=as.factor(Crime.Name2),Victims=as.factor(Victims))
str(crime_arules)

library(arules)
library(arulesViz)
crime_arules<-as(crime_arules,"transactions")
class(crime_arules)
summary(crime_arules)
itemFrequency(crime_arules)
itemFrequencyPlot(crime_arules,topN=10)

Grules<-apriori(crime_arules,parameter = list(support=0.05,conf=0.5))
Grules
summary(Grules)
inspect(sort(Grules,by="lift")[1:15])

plot(Grules,method = "grouped",control = list(k=10))
plot(Grules)

####General rules####
####Arules for items with top 5 frequencies because after this there are 0 rules

itemFrequencyPlot(crime_arules,topN=10)

####Single victim arules ####
victim_single<-subset(Grules,subset=rhs%in%"Victims=Single"&lift>1)
victim_single
inspect(sort(victim_single,by="lift")[1:10])

####Day time arules ####
Day_rule<-subset(Grules,subset=rhs%in%"Day_night=Day"&lift>1)
Day_rule
inspect(sort(Day_rule,by="lift"))

#####Night time arules ####
Night_rule<-subset(Grules,subset=rhs%in%"Day_night=Night"&lift>1)
Night_rule
inspect(sort(Night_rule,by="lift"))

#####Street place arules ####
Street_rule<-subset(Grules,subset=rhs%in%"Place=Street "&lift>1)
Street_rule
inspect(sort(Street_rule,by="lift"))
