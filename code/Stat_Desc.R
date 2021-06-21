rm(list=ls())
library(readxl)

#Evolution annuelle

#Evolution des ventes 
ventes <- read_excel("~/Stage/Ressources/donnees/Annuelles/Ventes.xlsx")
plot(ventes$`Total carburants routiers`,type = 'l',xaxt="n",ylim = c(min(ventes$`Total carburants routiers`),max(ventes$`Total carburants routiers`)),col = 'purple',ylab = "volume vendus en m3",xlab = "")
axis(1,at = c(1:11),labels = ventes$Années)
View(ventes)

#Evolution du prix moyen des carburants 

#Annuelle
TTC <-read_excel("~/Stage/Ressources/donnees/Données mensuelles 2007-2021/Prix_TTC.xlsx")
HTT <-read_excel("~/Stage/Ressources/donnees/Données mensuelles 2007-2021/Prix_HTT.xlsx")

plot(TTC$Gazole,type = 'l',xaxt = "n",ylim = c(0.6,max(TTC$SP95)),col='Blue',ylab = 'Average fuel prices',xlab = "")
axis(1, at = (1:length(TTC$Date)),labels =TTC$Date)
lines(x = TTC$SP95,col= 'Green')
legend(x = "bottom",legend = c("Gazole","SP95"),col =  c("Blue","Green"),text.width = 20,bty= "n", lty = 1,cex = 0.8)

#Hebdomadaires
TTC_hebdo <- read_excel("~/Stage/Ressources/donnees/Données hebdomadaires 2017-2021/TTC.xlsx")
plot(TTC_hebdo$Gazole,type = 'l',xaxt = "n",ylim = c(0.4,max(TTC_hebdo$SP98)),col='Blue',ylab = 'Average fuel prices',xlab = "")
axis(1, at = (1:length(TTC_hebdo$Date)),labels =TTC_hebdo$Date)
lines(x = TTC_hebdo$SP95,col= 'Green')
lines(TTC_hebdo$SP98,col ="red")
lines(TTC_hebdo$`SP95-E10`,col ="yellow")
lines(TTC_hebdo$GPL,col ="purple")
legend(x = "bottomright",legend = c("Gazole","SP95","SP98","SP95-E10","GPL"),col =  c("Blue","Green","red","yellow","purple"),text.width = 25,bty= "n", lty = 1,cex = 0.5)

#Evolution of the price of a barel of Brent
brent <-read_excel("~/Stage/Ressources/donnees/Brent/Prix mensuel 2007-2021.xlsx")
plot(brent$`Cours du Brent daté`,type = 'l',xaxt = "n",col='Blue',ylab = 'Price of a barel of brent',xlab = "")
axis(1, at = (1:length(TTC$Date)),labels =TTC$Date)



