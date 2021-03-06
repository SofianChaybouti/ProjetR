library(zoo)
library(xts)
library(mgcv)

rm (list=objects())
setwd("~/GitHub/ProjetR")
      

#lecture du fichier 
dataCirculation <- read.csv("accidents-corporels-de-la-circulation-routiere.csv", header = T, sep = ";")
summary(dataCirculation)
names(dataCirculation)
length(dataCirculation$DATE)

#suppression des colonnes inutiles
dataCirculation <- data.frame (dataCirculation$DATE, dataCirculation$HEURE)

names(dataCirculation)<-c( "date", "heure")
summary(dataCirculation)
head(dataCirculation)
tail(dataCirculation)
str(dataCirculation)
#On travaille sur 3ans

Date = as.POSIXct(strptime(dataCirculation$date, "%Y-%m-%d"))
head(Date)
Date = Date [which(format(Date,"%Y") == 2013 | format(Date, "%Y") == 2014 | format(Date,"%Y") == 2015)]
head(Date)
tail(Date)
# On recupere le nombre d'accidents par jour

dataCirculation <- as.data.frame(table(Date))
head(dataCirculation)

Date = as.POSIXct(strptime(dataCirculation$Date, "%Y-%m-%d"))
Freq = as.numeric(dataCirculation$Freq)
dataCirculation = data.frame(Date , Freq)

# Affichage
plot(dataCirculation$Date,dataCirculation$Freq,type='l',xlab="Date",ylab="Nombre d'accident",main="Nombre d'accidents par jour de 2008 a 2015")

#statistiques de base 
summary(dataCirculation$Freq)
mean(dataCirculation$Freq)
sd(dataCirculation$Freq)

dataCirculation$Date[which.max(Freq)]
dataCirculation$Date[which.min(Freq)]

boxplot(dataCirculation$Freq,ylab="Nombre d'accidents par jour")

hist(dataCirculation$Freq,main="Histogramme",xlab="Nombre d'accidents par jour",ylab="Frequence")

#Moyenne par an 
annees <- as.factor (format(dataCirculation$Date, "%Y"))
moyenne_an <- tapply(dataCirculation$Freq,annees,mean)

# Moyenne par mois
mois <- as.factor(format(dataCirculation$Date,"%m"))
moyenne_mois <- tapply(dataCirculation$Freq,mois,mean)
plot(moyenne_mois,type='b',pch=20)
barplot(moyenne_mois,col="lightblue",main="Moyenne par mois",xlab="Mois",ylab="Nombre d'accidents par jour")

boxplot(dataCirculation$Freq ~ mois,col="lightblue",pch=20,cex=0.5,xlab="Mois",ylab="Nombre d'accidents par jour")

# Moyenne par jour (de la semaine)
dataCirculation.xts<-xts(dataCirculation$Freq,order.by = Date)
plot.xts (dataCirculation.xts)
jour<-as.factor(.indexwday(dataCirculation.xts))
moyenne_jour<-tapply(dataCirculation.xts,jour,mean)
barplot(moyenne_jour,names.arg=c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim"),main="Moyenne par jour de la semaine",xlab="Jour de la semaine",ylab="Nombre d'accidents par jour") 

# Moyenne par jour et mois
moyenne_jour_mois = tapply(dataCirculation$Freq,mois:jour,mean)
moyenne_jour_mois = matrix(moyenne_jour_mois,nrow=12,ncol=7,byrow=T)
matplot(t(moyenne_jour_mois),type='l',col=rainbow(12), main="Moyenne par mois et par jour", xlab="Jour de la semaine",ylab="Nombre d'accidents par jour")

#par moi et par an 
moyenne_mois_an = tapply(dataCirculation$Freq, annees:mois, mean)
moyenne_mois_an = matrix(moyenne_mois_an, nrow = 3, ncol = 12, byrow = T)
matplot(t(moyenne_mois_an), type = 'l', col = rainbow(3), main = "moyenne par moi et par an", xlab = "mois", ylab="nombre d'acccident par jour")
####### Estimation de la tendance ##########
n<- length(Date)
t<-c(1:n)
par(mfrow=c(1,1))

# Par regression lineaire
reg<-lm(Freq~t)
tend.lm <- reg$fitted
plot(Date,Freq,type='l',
     main="Par regression lineaire",
     xlab="",ylab="Nombre d'accidents")
lines(Date,tend.lm,col='red')

# On voit que la tendance reste constante au cours d'annee en annee
#Par moyenne mobile 

mb<- filter(dataCirculation$Freq,filter=array(1/30,dim=30),method=c('convolution'),sides=2,circular=T)
mb<- xts(mb,order.by=Date)
plot(dataCirculation,type='l')
lines(Date, mb, col = 'red')

#Par noyaux
noyau <-ksmooth(t, dataCirculation$Freq , kernel = c("normal"),bandwidth = 30)
par = (mfrow=c(1, 2))
plot(dataCirculation$Date, dataCirculation$Freq, type = "l", xlab = "", ylab ="nbe d'accidents", col = "blue")
lines(dataCirculation$Date, noyau$y, type = "l", xlab = "", ylab = "tendance", col = "black")

#Par polynomes locaux
lo <- loess (Freq ~ t, data = dataCirculation, degree = 2, span = 0.7)
plot(dataCirculation$Date, dataCirculation$Freq, type = "l", xlab = "", ylab = "nbr d'accidents", col = "blue")
lines (dataCirculation$Date, lo$fitted, col = "orangered2", lwd = 2)

#Par projection sur fonctions splines polynomiales par morceaux

g <-gam(Freq ~ s(t, k = 10), data = dataCirculation)
plot(dataCirculation$Date, dataCirculation$Freq, type = "l", xlab = "", ylab ="nbe d'accidents", col = "blue", lwd = 2)
lines (dataCirculation$Date, g$fitted, col = "red", lwd = 2)

#A voir : comment r�gler tous les param�tres

#Choix de l'estimation � prendre pour la suite 
tend<-tend.lm

#Notre mod�le est additif!

#m�thode de la moyenne mobile 
data_st<- Freq-est
par(mfrow=c(1,1))
plot(Date,data_st,type='l',main="Donn�es sans la tendance", xlab="",ylab="Nombre d'accidents")

saison.mb<-filter(data_st, filter=array(1/30,dim=30), method = c("convolution"),
                  sides = 2, circular = T)
saison.mb<-xts(saison.mb,order.by=Date)
#plot(Date,data_st,type='l', main="Par moyenne mobile",xlab="",ylab="Nombre d'accidents")
plot(Date,saison.mb,type='l',col='red')
#Pas mal du tout on voit bien la p�riodicit� d'un ans

# Par r�gression � noyau gaussien
h=30#comment choisir h??? quand il est tout petit c'est parfait...
x<-seq(1,max(t),length=n)
W<-matrix(unlist(lapply(x,function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))})),ncol=n,nrow=n,byrow=F)
saison.kernel<-colSums(as.numeric(data_st)*W)
saison.kernel<-xts(saison.kernel,order.by=Date)
#plot(Date,data_st,type='l',main="Par r�gression � noyau gaussien",xlab="",ylab="Nombre de tapages")
plot(Date,saison.kernel,type="l",col='red')


# Par r�gression sur s�rie de Fourier
w=2*pi/30 
fourier<-cbind(cos(w*t), sin(w*t))
K<-20
for(i in c(2:K))
{
  fourier<-cbind(fourier,cos(i*w*t), sin(i*w*t))
}
#matplot(fourier,type='l')
dim(fourier)
reg<-lm(data_st~fourier[,1:2])
saison.lm<-xts(as.numeric(reg$fitted),order.by=Date)
plot(Date,saison.lm,type='l',col='red')
#MDR 

# Estimation de la saisonnalit� retenue
saison=saison.kernel

# Estimation tendance + saisonnalit� 
par(mfrow=c(1,1))
plot(Date,Freq,type='l',main="Estimation de la tendance et de la saisonnalit�",
     xlab="",ylab="Nombre de tapages")
lines(Date,tend+saison,col='red')

