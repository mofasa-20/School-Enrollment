getwd()

setwd("C:\\Users\\dell\\Desktop\\R\\R Project") # Setting Path of Project Directory

library(readxl)  # Loading readxl Library to ready xl Files

dfxl = read_excel("enrolment_and_fte_2004_05-to-2019_20_updated.xlsx")

class(dfxl)

my_data_file = data.frame(dfxl)

class(my_data_file)

library("doBy") # finding meand and SD of all provinces and Districts.

#Question-1 : How many students enrolled in BC - District Level & Province Level from 2004 - 2019 ?.

summaryBy(TOTAL_ENROLMENT ~ SCHOOL_YEAR+ DATA_LEVEL + PUBLIC_OR_INDEPENDENT, data=my_data_file, FUN = c(sum, mean))




###############################################################################################################################


#Question: 2 Histogram of Province level Enrolment & District Level Enrolment.

myhist1 = summaryBy(TOTAL_ENROLMENT ~ SCHOOL_YEAR+ DATA_LEVEL + PUBLIC_OR_INDEPENDENT, data=my_data_file, FUN = c(sum, mean))

library("sqldf")

myhist1$TOTAL_ENROLMENT.sum

myhist2<- sqldf("select * from myhist1 where DATA_LEVEL like 'P%'")
myhist3<- sqldf("select * from myhist1 where DATA_LEVEL like 'D%'")

par(mfrow = c(1,2))
hist(myhist2$TOTAL_ENROLMENT.sum,
     main = "Independent School Enrolment of BC",
     xlab = "Sum Of Enrolments of Students from 2004 - 2019",
     ylab = "Enrolments Frequency from 2004 - 2019",
     border = "blue",
     col = "green")
hist(myhist3$TOTAL_ENROLMENT.sum,
     main = "Public School Enrolment of BC",
     xlab = "Sum Of Enrolments of Students from 2004 - 2019",
     ylab = "Enrolments Frequency from 2004 - 2019",
     border = "black",
     col = "yellow")


###############################################################################################################################



# Question:3 Which year had the lowest no. of Enrolments in School Level of BC Province.

total_sum_enrol = myhist2$TOTAL_ENROLMENT.sum + myhist3$TOTAL_ENROLMENT.sum

# 672676 666987 661196 666727 666485 673593 673936 664125 663557 660596 659400 654775 659320 665657 671451 679875

Provn_Enlorment = scan()

library(forecast)

Provn_Enlorment = ts(Provn_Enlorment, start = 2004) # Converting the raw data of Provn_Enlorment to Time Series

class(Provn_Enlorment) # Time Series.

# QUESTION Plot

dev.off()
plot(Provn_Enlorment,
     main = "Enrolments of Students in Province of BC",
     xlab = "Enrolments of Students from 2004 - 2019 ",
     ylab = "Sum of Enrolments ")


###############################################################################################################################

#Question:4 Which year had the Hightest no. of  Future Full Time Equivalent - FTE Enrolments in Schools of BC Province ?


T_FTE = summaryBy(TOTAL_FTE ~ SCHOOL_YEAR+ DATA_LEVEL + PUBLIC_OR_INDEPENDENT, data=my_data_file, FUN = c(sum, mean))

T_FTE_Prov <- sqldf("select * from T_FTE where DATA_LEVEL like 'P%'")
T_FTE_Dist <- sqldf("select * from T_FTE where DATA_LEVEL like 'D%'")

Total_Sum_FTE = T_FTE_Prov$TOTAL_FTE.sum + T_FTE_Dist$TOTAL_FTE.sum

# 639454.0 634690.3 624929.5 610715.7 616380.2 613044.4 617257.3 620292.9 615251.9 614342.1 616102.3 622249.9 630795.9 637570.3 644294.0 650169.9

FTE_Enlorment = scan()

FTE_Enlorment = ts(FTE_Enlorment, start = 2004) # Converting the raw data of Provn_Enlorment to Time Series

class(FTE_Enlorment) # Time Series.

dev.off()
plot(FTE_Enlorment,
     main = "Full Time Equivalent Enrolments of Students in Province of BC",
     xlab = "Enrolments of Students from 2004 - 2019 ",
     ylab = "Sum of Enrolments ")





#################################################################################################################




# Question 5 Using Pie chart display the Enrolment of Female & Male Student including FTE Students.


FE = summaryBy(FEMALE_ENROLMENT ~ SCHOOL_YEAR+ DATA_LEVEL, data=my_data_file, FUN = c(sum))
f1 = sum(FE$FEMALE_ENROLMENT.sum)

ME = summaryBy(MALE_ENROLMENT ~ SCHOOL_YEAR+ DATA_LEVEL, data=my_data_file, FUN = c(sum))
m1 = sum(ME$MALE_ENROLMENT.sum)

FFTE= summaryBy(FEMALE_FTE ~ SCHOOL_YEAR+ DATA_LEVEL, data=my_data_file, FUN = c(sum))
ff1 = sum(FFTE$FEMALE_FTE.sum)


MFTE = summaryBy(MALE_FTE ~ SCHOOL_YEAR+ DATA_LEVEL, data=my_data_file, FUN = c(sum))
mf1 = sum(MFTE$MALE_FTE.sum)

head(my_data_file)

library(ggplot2)

pie1 <- c(f1,m1,mf1,ff1)
pie_L <- c("Female Enrolment", "Male Enrolment","Male Full Time Eqvivalent.", "Female Full Time Eqvivalent.")
pct1 <- round(pie1/sum(pie1)*100)
lb1 <- paste(pie_L, pct1)
lb2 <- paste(lb1,"%",  sep="")
df = data.frame(pie1 = pie1,labels = lb2 )
ggplot(df,aes(x = factor(1),fill = labels))+
  geom_bar(width = 1)+
  coord_polar(theta = "y")+
  theme(axis.title = element_blank())

library(plotrix)

pie1 <- c(f1,m1,mf1,ff1)
pie_L <- c("Female Enrolment", "Male Enrolment","Male Full Time Eqv.", "Female Full Time Eqv.")
pct1 <- round(pie1/sum(pie1)*100)
lb1 <- paste(pie_L, pct1)
lb2 <- paste(lb1,"%",  sep="")
pie(pie1, labels = lb2, col=rainbow(length(lbls)),main="Male and Female Enrolment Pie Chart")



# par(mfrow = c(1,2))
# pie11 <- c(f1,m1,mf1,ff1)
# pie_L1 <- c("Female Enrol", "Male Enrol","Male FTE", "Female FTE")
# pct11 <- round(pie11/sum(pie11)*100)
# lb11 <- paste(pie_L1, pct11)
# lb22 <- paste(lb11,"%",  sep="")
# pie3D(pie1, labels = lb22,explode=0.1, main="Male and Female Enrolment Pie Chart")
#
# pie(pie1, labels = lb22, col=rainbow(length(lbls)),main="Male and Female Enrolment Pie Chart")




#################################################################################################################




#Question: 6 Display the Admission of BC - Province by each year independantly.


library(ggplot2)

# h1 = c(myhist2$TOTAL_ENROLMENT.sum)
# h2 =c(myhist3$TOTAL_ENROLMENT.sum)
# s1 = c(myhist1$SCHOOL_YEAR)
# h3 = h1 + h2
#
# bar1 = data.frame(School_Year = s1, Independent_School_Enrolment = h1, Public_School = h2, Total_Enrolment = h3)
#
# ggplot(data=bar1, aes(x=School_Year, y=Total_Enrolment)) +
#   geom_bar(stat="identity")

ggplot(data=myhist1, aes(x=SCHOOL_YEAR, y=TOTAL_ENROLMENT.sum)) +
  geom_bar(stat="identity")


#################################################################################################################




#Question 7 : Find the Co-relation between the Aborginal Enrolments and Aborginal FTE, Non-Aborginal and Non-Aborginal FTE?

#Lower Panel
lower.pan<-function(x, y){
  points(x,y, pch=19, col=c("red", "blue")[mycoll])
}
pairs(my_data_file[,14:17],lower.panel = lower.pan)


#upper panel
upper.pan<-function(x, y){
  points(x,y, pch=19, col=c("red", "blue")[mycoll])
}

pairs(my_data_file[,14:17],upper.panel = upper.pan)


pairs(my_data_file[,14:17], lower.panel = lower.pan, upper.panel = upper.pan)





#################################################################################################################



#QUESTION:8 How to forecast or precdict the Future Full Time Equivalent - FTE Enrolments?

par(mfrow=c(2,1))
holttrend_FTE = holt(FTE_Enlorment, h = 10)
plot(holttrend_FTE)


FTE_Enlorment_arima = auto.arima(FTE_Enlorment)
plot(forecast(FTE_Enlorment_arima, h = 10))
summary(FTE_Enlorment_arima)


#################################################################################################################



# QUESTION:9 How to forecast or precdict the Future Enrolments?

#using holt and arima model

par(mfrow=c(2,1))
holttrend = holt(Provn_Enlorment, h = 10)
plot(holttrend)


Provn_Enlormentarima = auto.arima(Provn_Enlorment)
plot(forecast(Provn_Enlormentarima, h = 10))
summary(Provn_Enlormentarima)


#################################################################################################################

#QUESTION:10 How to forecast using different model to make clear prediction?

holttrend1 = holt(Provn_Enlorment, h = 10)
arimafore = forecast(auto.arima(Provn_Enlorment), h = 10)
plot(holttrend1)
plot(arimafore)



holttrend_FTE1 = holt(FTE_Enlorment, h = 10)
arimafore_FTE1 = forecast(auto.arima(FTE_Enlorment), h = 10)
plot(holttrend_FTE1)
plot(arimafore_FTE1)



library(ggplot2)

par(mfrow=c(2,1))

# Forecast Lines as Comparison
autoplot(Provn_Enlorment) +
  forecast::autolayer(holttrend1$mean, series = "Holt Linear Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") +
  xlab("Years") + ylab("Enrolments of Students in Province of BC") +
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Forecast of Students Enrolment") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue",
                                                             face="bold", size=15))


autoplot(FTE_Enlorment) +
  forecast::autolayer(holttrend_FTE1$mean, series = "Holt Linear Trend") +
  forecast::autolayer(arimafore_FTE1$mean, series = "ARIMA") +
  xlab("Years") + ylab("Enrolments of Students in Province of BC") +
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Forecast of FTE Students Enrolment") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "red",
                                                                                face="bold", size=15))
