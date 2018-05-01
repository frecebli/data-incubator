library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(timetk)
library(forcats)
library(stringr)
library(plyr)
library(stringr)
library(gridExtra)
library(xlsx)


ILMN = Quandl("WIKI/ILMN",collapse="daily",start_date="2006-09-01",type="raw")
ROG = Quandl("WIKI/ROG",collapse="daily",start_date="2006-09-01",type="raw")
TMO = Quandl("WIKI/TMO",collapse="daily",start_date="2006-09-01",type="raw")


ILMN<-cbind(ILMN,Stock="")
ROG<-cbind(ROG,Stock="")
TMO<-cbind(TMO,Stock="")

ILMN$Stock<-paste(ILMN$Stock,"ILMN",sep="")
ROG$Stock<-paste(ROG$Stock,"ROG",sep="")
TMO$Stock<-paste(TMO$Stock,"TMO",sep="")


Master_Data<-rbind(ILMN,ROG,TMO)


Master_Data$Date<-as.character(Master_Data$Date)

Master_Data$Date = as.character(Master_Data$Date)
list<-strsplit(Master_Data$Date,"-")

Master_Date1<-ldply(list)
colnames(Master_Date1)<-c("Year","Month","Day")

Master_Data<-cbind(Master_Data,Master_Date1)
names(Master_Data)

Master_Data$`Total Trade Quantity`<-Master_Data$`Total Trade Quantity`/100000

Master_Data$Date<-as.Date(Master_Data$Date)

Master_Data<-Master_Data%>%
  tibble::as.tibble()%>%
  group_by(Stock)

Master_Data %>%
  ggplot(aes(x = Date, y = Close, color = Stock)) +
  geom_point() +
  labs(title = "Daily Close Price", x = "Month",y="Close Price") +
  facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
  scale_fill_tq(fill="green4",theme="light") +
  theme_tq() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"))+
  theme(legend.position="none")

