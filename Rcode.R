install.packages("readxl")
library(readxl)
maryland_history <- read_excel("D:/fall 2020/750 data analytics/group project/maryland-history.xlsx")


#pie chart deathcase
maryland_history$dataQualityGrade = NULL
maryland_history[is.na(maryland_history)] = 0
total_dth= sum(maryland_history$death)
postv_nodth = sum(maryland_history$positive) - total_dth
slices_dthcase = c(postv_nodth,total_dth)
lbls_dthcase = c("COVID Survivors","COVID Deaths")
pct = round(slices_dthcase/sum(slices_dthcase)*100)
lbls_dthcase = paste(lbls_dthcase, pct)
lbls_dthcase = paste(lbls_dthcase,"%",sep="")
cols= c("cornflowerblue","blue1","red")
pie(slices_dthcase, labels=lbls_dthcase, main="Percentage of Positive COVID cases who \n Survived and Died", col=cols)


#pie chart for hospitalized / died
maryland_history$dataQualityGrade = NULL
maryland_history[is.na(maryland_history)] = 0
sum_hosp= sum(maryland_history$hospitalized)
sum_dth= sum(maryland_history$deathConfirmed)
sum_pstv = sum(maryland_history$positive)
total_hosp= sum(maryland_history$hospitalized) - sum_dth
total_dth= sum(maryland_history$death)
total_pstv = sum(maryland_history$positive) - sum_dth - sum_hosp
slices1 = c(total_pstv,total_hosp,total_dth)
lbls1 = c("Total Positives", "Total Hospitalized", "Total Deaths")
pct = round(slices/sum(slices1)*100)
lbls1 = paste(lbls1, pct)
lbls1 = paste(lbls1,"%",sep="")
cols= c("cornflowerblue", "blue1","blue4")
pie(slices1, labels=lbls1, main="Percentage of Positives Who Were Hospitalized or Died", col=cols)


#pie chart for hospitalization / recovered
crrnt_hosp= maryland_history_10_20$hospitalizedCurrently[1]
total_hosp = maryland_history_10_20$hospitalizedCumulative[1] - crrnt_hosp
rcvrd = maryland_history_10_20$recovered[1]
slices_hosprecover = c(total_hosp,rcvrd,crrnt_hosp)
lbls_hosprecover = c("Hospitalized", "Recovered", "Currently in Hospital")
pct_h = round(slices_hosprecover/sum(slices_hosprecover)*100)
lbls_hosprecover = paste(lbls_hosprecover,pct_h)
lbls_hosprecover = paste(lbls_hosprecover ,"%", sep=" ")
cols2= c("cornflowerblue","blue1","blue4")
pie(slices_hosprecover, labels=lbls_hosprecover, main="Percentage of COVID Patients \n Hospitalized, Recovered, and Currently Hospitalized", col=cols2)


#plot the data using ggplot scatterplot
casefatalityratio = (maryland_history$deathConfirmed/(maryland_history$deathConfirmed+maryland_history$recovered))*100
print (casefatalityratio)
install.packages("ggplot2")
library(ggplot2)  # Data visualization
options(scipen = 999)
ggplot(data = maryland_history, aes(x = date, y = casefatalityratio)) +
  geom_point() +  
  geom_point(colour = "Red", size = 1) +  
  labs(x = "Month",
       y = "casefatalityratio",
       title = "casefatalityratio",
       subtitle = "Maryland USA")


# scatterplot death increase
ggplot(maryland_history, aes(x=date, y=deathIncrease)) +
  geom_point(shape=18, fill="green", color="green", size=2) + 
  xlab('Time') + ylab('Count') + labs(title='Death Increase') +
  geom_smooth(method= loess,linetype="longdash", color="red", fill="orange") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# scatterplot hospitalized increase
ggplot(maryland_history, aes(x=date, y=hospitalizedIncrease)) +
  geom_point(shape=18, fill="green", color="green", size=2) + 
  xlab('Time') + ylab('Count') + labs(title='Hospitalized Increase') +
  geom_smooth(method= loess,linetype="longdash", color="red", fill="orange") +
  theme(axis.text.x=element_text(angle=45, hjust=1))



# scatterplot positive increase
ggplot(maryland_history, aes(x=date, y=positiveIncrease)) +
  geom_point(shape=18, fill="green", color="green", size=2) + 
  xlab('Time') + ylab('Count') + labs(title='Positive Increase') +
  geom_smooth(method= loess,linetype="longdash", color="red", fill="orange") +
  theme(axis.text.x=element_text(angle=45, hjust=1))




#creating month vectors
install.packages("data.table")
library(data.table)
OCT_V = c(setDT(maryland_history)[date %between% c('2020-10-01', '2020-10-20')])
SEP_V = c(setDT(maryland_history)[date %between% c('2020-09-01', '2020-09-30')])
AUG_V = c(setDT(maryland_history)[date %between% c('2020-08-01', '2020-08-31')])
JUL_V = c(setDT(maryland_history)[date %between% c('2020-07-01', '2020-07-31')])
JUN_V = c(setDT(maryland_history)[date %between% c('2020-06-01', '2020-06-30')])
MAY_V = c(setDT(maryland_history)[date %between% c('2020-05-01', '2020-05-31')])
APR_V = c(setDT(maryland_history)[date %between% c('2020-04-01', '2020-04-30')])
MAR_V = c(setDT(maryland_history)[date %between% c('2020-03-01', '2020-03-31')])

# month dataframe
March    <- data.frame(MAR_V) 
April    <- data.frame(APR_V) 
May      <- data.frame(MAY_V)
June     <- data.frame(JUN_V)
July     <- data.frame(JUL_V)
August   <- data.frame(AUG_V)
September<- data.frame(SEP_V)
October  <- data.frame(OCT_V)

# line graph of positive increase
ggplot(mapping = aes(x = date, y = positiveIncrease)) +
  ggtitle("POSITIVE INCREASE") +
  geom_line(data = March, color = "red") +
  geom_line(data = April, color = "blue")+
  geom_line (data =May, color = "green") +
  geom_line(data = June, color = "black")+
  geom_line(data = July, color = "Orange") +
  geom_line(data = August, color = "violet")+
  geom_line(data = September, color = "yellow") +
  geom_line(data = October, color = "brown")


#2020-03-05: Hogan declares a state of emergency; first 3 confirmed positive cases
#2020-03-30: Governor issues stay-home order
#2020-05-13 : Hogan announces Stage One of reopening
#2020-07-29: Hogan expands mask order, issues out-of-state travel advisory
#2020-09-01: Maryland ready to enter into Stage Three of recovery plan
#2020-09-21:	The governor announced restaurants can expand indoor capacity from 50% to 75% beginning at 5 p.m. 		

# creating period vectors
P1 = c(setDT(maryland_history)[date %between% c('2020-03-05', '2020-03-30')])
P2 = c(setDT(maryland_history)[date %between% c('2020-03-30', '2020-05-13')])
P3 = c(setDT(maryland_history)[date %between% c('2020-05-13', '2020-07-29')])
P4 = c(setDT(maryland_history)[date %between% c('2020-07-29', '2020-09-01')])
P5 = c(setDT(maryland_history)[date %between% c('2020-09-01', '2020-09-21')])

#period dataframes
Period1<- data.frame(P1) 
Period2<- data.frame(P2) 
Period3<- data.frame(P3)
Period4<- data.frame(P4)
Period5<- data.frame(P5)

# grid scatterplot showing period positive increase
install.packages("gridExtra")
install.packages(grid)
library('gridExtra')
library('grid')
library('ggplot2')
options(scipen = 999)
plot1 = ggplot(Period1, aes(x=date, y=positiveIncrease)) + 
  ggtitle("Period1 - 2020-03-05 to 2020-03-30") +
  geom_point(shape=18, fill="Blue", color="blue", size=2)+
  geom_smooth(method=loess,  linetype="longdash",color="darkred", fill="yellow")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot2 = ggplot(Period2, aes(x=date, y=positiveIncrease)) + 
  ggtitle("Period2 - 2020-03-30 to 2020-05-13") +
  geom_point(shape=18, fill="Blue", color="blue", size=2)+
  geom_smooth(method=loess,  linetype="longdash",color="darkred", fill="yellow")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot3 = ggplot(Period3, aes(x=date, y=positiveIncrease)) + 
  ggtitle("Period3 - 2020-05-13 to 2020-07-29") +
  geom_point(shape=18, fill="Blue", color="blue", size=2)+
  geom_smooth(method=loess,  linetype="longdash",color="darkred", fill="yellow")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot4 = ggplot(Period4, aes(x=date, y=positiveIncrease)) + 
  ggtitle("Period4 - 2020-07-29 to 2020-09-01") +
  geom_point(shape=18, fill="Blue", color="blue", size=2)+
  geom_smooth(method=loess,  linetype="longdash",color="darkred", fill="yellow")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
plot5 = ggplot(Period5, aes(x=date, y=positiveIncrease)) + 
  ggtitle("Period5 - 2020-09-01 to 2020-09-21") +
  geom_point(shape=18, fill="Blue", color="blue", size=2)+
  geom_smooth(method=loess,  linetype="longdash",color="darkred", fill="yellow")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow=3)




date = as.Date(maryland_history$date,format="%m/%d/%y")
maryland_history["Month"] = months(date)
pos_mon = data.frame(maryland_history$positive,maryland_history$Month)

#creating new table positive, hosp, and deaths increase by month#
posinc_by_month = aggregate(maryland_history$positiveIncrease, by=list(month=maryland_history$Month),FUN=sum)
deathinc_by_month = aggregate(maryland_history$deathIncrease, by=list(month=maryland_history$Month),FUN=sum)
hospinc_by_month = aggregate(maryland_history$hospitalizedIncrease, by=list(month=maryland_history$Month),FUN=sum)
names(posinc_by_month)[names(posinc_by_month)=="x"] = "Positives"
names(deathinc_by_month)[names(deathinc_by_month)=="x"] = "deaths"
names(hospinc_by_month)[names(hospinc_by_month)=="x"] = "hospitalized"
Hospitalized = hospinc_by_month$hospitalized
Deaths = deathinc_by_month$deaths
data_by_month = cbind(posinc_by_month,Hospitalized,Deaths)



#bar charts#
# grouped bar of sum
barplot(data_by_month$Positives, xlab="Months", ylab="Total Number of Positive Cases", main = "Total Positive Cases Each Month", names.arg = data_by_month$month)
barplot(data_by_month$Hospitalized, xlab="Months", ylab="Total Number of Hospitalizations", main = "Total Hospitalizations Each Month", names.arg = data_by_month$month)
barplot(data_by_month$Deaths, xlab="Months", ylab="Total Number of Deaths", main = "Total Deaths Each Month", names.arg = data_by_month$month)
install.packages("tidyr") 
library(tidyr)  
data_by_month$month = factor(data_by_month$month)
data_long = gather(data_by_month, Cases, Total, Positives:Deaths, factor_key = TRUE)
data_long
ggplot(data=data_long, aes(x=month, y=Total, fill=Cases)) + geom_bar(stat="identity", position=position_dodge())


#mean and standard deviation of each month
barplot(pos_mean_by_month$Positives, xlab="Months", ylab="Mean of Positive Cases", main = "Mean Positive Cases Each Month", names.arg = pos_mean_by_month$month)
barplot(pos_sd_by_month$Positives, xlab="Months", ylab="SD of Positive Cases", main = "Standard Deviation of  Monthly Positive Cases", names.arg = pos_mean_by_month$month)


pos_mean_by_month = aggregate(maryland_history$positiveIncrease, by=list(month=maryland_history$Month),FUN=mean)
hosp_mean_by_month = aggregate(maryland_history$hospitalizedIncrease, by=list(month=maryland_history$Month),FUN=mean)
dth_mean_by_month = aggregate(maryland_history$deathIncrease, by=list(month=maryland_history$Month),FUN=mean)
names(pos_mean_by_month)[names(pos_mean_by_month)=="x"] = "PositivesM"
names(dth_mean_by_month)[names(dth_mean_by_month)=="x"] = "Deaths"
names(hosp_mean_by_month)[names(hosp_mean_by_month)=="x"] = "Hospitalized"
HospitalizedM = hosp_mean_by_month$Hospitalized
DeathsM = dth_mean_by_month$Deaths
means_by_month = cbind(pos_mean_by_month,HospitalizedM,DeathsM)


pos_sd_by_month = aggregate(maryland_history$positiveIncrease, by=list(month=maryland_history$Month),FUN=sd)
hosp_sd_by_month = aggregate(maryland_history$hospitalizedIncrease, by=list(month=maryland_history$Month),FUN=sd)
dth_sd_by_month =aggregate(maryland_history$deathIncrease, by=list(month=maryland_history$Month),FUN=sd)
names(pos_sd_by_month)[names(pos_sd_by_month)=="x"] = "PositivesSD"
names(dth_sd_by_month)[names(dth_sd_by_month)=="x"] = "Deaths"
names(hosp_sd_by_month)[names(hosp_sd_by_month)=="x"] = "Hospitalized"
HospitalizedSD = hosp_sd_by_month$Hospitalized
DeathsSD = dth_sd_by_month$Deaths
sd_by_month = cbind(pos_mean_by_month,HospitalizedSD,DeathsSD)


StandardDeviation = pos_sd_by_month$Positives
names(pos_mean_by_month)[names(pos_mean_by_month)== "Positives"] = "Mean"
pos_sd_mean = cbind(pos_mean_by_month,StandardDeviation)
pos_sd_mean
library(tidyr)
pos_sd_mean$month = factor(pos_sd_mean$month)


data_long2 = gather(pos_sd_mean , Stats, Value, PositivesM:StandardDeviation)
data_long2
ggplot(data=data_long2, aes(x=month, y=Value, fill=Stats)) + geom_bar(stat="identity", position=position_dodge())

#one way Anova - inferential
pos_mon = data.frame(maryland_history$positiveIncrease,maryland_history$Month)
View (pos_mon)
# Assumption: samples are independent and categorical
pos_mon$maryland_history.Month = factor(pos_mon$maryland_history.Month)
class(pos_mon$maryland_history.Month)


#Assumption Normal Distribution of each group
names(pos_mon)[names(pos_mon)=="maryland_history.Month"] = "Month"
names(pos_mon)[names(pos_mon)=="maryland_history.positiveIncrease"] = "PositiveCases"
p.oct=subset(pos_mon, Month== "October")
p.sep=subset(pos_mon, Month== "September")
p.aug=subset(pos_mon, Month== "August")
p.jul=subset(pos_mon, Month== "July")
p.jun=subset(pos_mon, Month== "June")
p.may=subset(pos_mon, Month== "May")
p.apr=subset(pos_mon, Month== "April")
p.mar=subset(pos_mon, Month== "March")


qqnorm(p.oct$PositiveCases)
qqline(p.oct$PositiveCases)
qqnorm(p.sep$PositiveCases)
qqline(p.sep$PositiveCases)
qqnorm(p.aug$PositiveCases)
qqline(p.aug$PositiveCases)
qqnorm(p.may$PositiveCases)
qqline(p.may$PositiveCases)

#Assumption: homogeneity of variances
bartlett.test(PositiveCases ~ Month, data = pos_mon)

#one-way ANOVA
model1 = lm(PositiveCases ~ Month, data = pos_mon)
anova(model1)
#post-hoc test
TukeyHSD(aov(model1))

library(ggplot2)
ggplot(pos_mon, aes(x=Month, y=PositiveCases, fill=Month)) + geom_boxplot(alpha=0.6) + scale_fill_brewer(palette="Dark2") + xlab("Months") + ylab("Positive Cases") + theme(legend.position="none")




