rm(list=ls())
library(plyr)
library(XML)
library(lubridate)
library(maps)
source("tables.R")

#Import of data

donnees_2007 <- xmlToList("C:/Users/wissem bbm/Documents/Stage-2A/Ressources/donnees/PrixCarburants_annuel_2007.xml")
donnees_2008 <- xmlToList("C:/Users/wissem bbm/Documents/Stage-2A/Ressources/donnees/PrixCarburants_annuel_2008.xml")
donnees_2011 <- xmlToList("C:/Users/wissem bbm/Documents/Stage-2A/Ressources/donnees/PrixCarburants_annuel_2011.xml")
donnees_2012 <- xmlToList("C:/Users/wissem bbm/Documents/Stage-2A/Ressources/donnees/PrixCarburants_annuel_2012.xml")
donnees_2018 <- xmlToList("C:/Users/wissem bbm/Documents/Stage-2A/Ressources/donnees/PrixCarburants_annuel_2018.xml")
donnees_2020 <- xmlToList("C:/Users/wissem bbm/Documents/Stage-2A/Ressources/donnees/PrixCarburants_annuel_2020.xml")

#Print of the tables
View(spatial_year(donnees_2007))
View(fermeture(donnees_2007))
View(stock(donnees_2007))
View(Price_update(donnees_2007,2))

#Visualization of the prices on the french map
#Red dots are the most expensive gas stations, that particular year.

B<- spatial_year(donnees_2007)
idx=which((B$lon>(-10))&(B$lon<20)&(B$lat>35)&(B$lat<55))
B=B[idx,]
Q=quantile(B$Prix,seq(0,1,by=.01),na.rm=TRUE)
Q[1]=0
x=as.numeric(cut(B$Prix,breaks=unique(Q)))
CL=c(rgb(0,0,1,seq(1,0,by=-.025)),rgb(1,0,0,seq(0,1,by=.025)))
plot(B$lon,B$lat,pch=19,col=CL[x],cex=0.05)

#With contours of the French regions
map("france")
points(B$lon,B$lat,pch =19,col=CL[x],cex=0.05)


