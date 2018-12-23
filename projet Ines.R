### PROJET STA202 ###

# Hugo Tocco - Ines Belghiti 

library(zoo)
library(xts)
library(mgcv)


rm (list=objects())
setwd("~/Etudes/2A/STA202/Projet NYC")

# Recuperation des donnees
dataNYC<-read.csv("party_in_nyc.csv",header=TRUE)
names(dataNYC)

# Mise sous forme de date en oubliant les heures
Date <- as.POSIXct(strptime(dataNYC$Created.Date, "%Y-%m-%d"))

# On récupère le nombre de tapages par jour
dataNYC <- as.data.frame(table(Date))
names(dataNYC)

# On remet sous forme de date
Date <- as.POSIXct(strptime(dataNYC$Date,"%Y-%m-%d"))

# On construit la suite chronologique à exploiter
Nombre <- as.numeric(dataNYC$Freq)
dataNYC <- data.frame(Date,Nombre)

###### Analyse decriptive #####

# Affichage
plot(dataNYC$Date,dataNYC$Nombre,type='l',xlab="Mois",ylab="Nombre de tapages",main="Tapages à NYC en 2016")

# Statistiques de base
summary(dataNYC$Nombre)
mean(dataNYC$Nombre)
sd(dataNYC$Nombre)

dataNYC$Date[which.max(Nombre)]
dataNYC$Date[which.min(Nombre)]

boxplot(dataNYC$Nombre,ylab="Nombre de tapages par jour")

hist(dataNYC$Nombre, breaks=20,main="Histogramme",xlab="Nombre de tapages par jour",ylab="Fréquence")


# Moyenne par mois
mois <- as.factor(format(dataNYC$Date,"%m"))
moyenne_mois <- tapply(dataNYC$Nombre,mois,mean)
plot(moyenne_mois,type='b',pch=20)
barplot(moyenne_mois,col="lightblue",main="Moyenne par mois en 2016",xlab="Mois",ylab="Nombre de tapages par jour")

boxplot(dataNYC$Nombre ~ mois,col="lightblue",pch=20,cex=0.5,xlab="Mois",ylab="Nombre de tapages par jour")

# Moyenne par jour (de la semaine)
dataNYC.xts<-xts(dataNYC$Nombre,order.by = Date)
jour<-as.factor(.indexwday(dataNYC.xts))
moyenne_jour<-tapply(dataNYC.xts,jour,mean)
barplot(moyenne_jour,names.arg=c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim"),main="Moyenne par jour de la semaine",xlab="Jour de la semaine",ylab="Nombre de tapages par jour") 

# Moyenne par jour et mois
moyenne_jour_mois = tapply(dataNYC$Nombre,mois:jour,mean)
moyenne_jour_mois = matrix(moyenne_jour_mois,nrow=12,ncol=7,byrow=T)
matplot(t(moyenne_jour_mois),type='l',col=rainbow(12),
        main="Moyenne par mois et par jour",
        xlab="Jour de la semaine",ylab="Nombre de tapages par jour")
#legend("right", 
#        legend=c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août",
       #          "Septembre","Octobre","Novembre","Décembre"),
       # colnames(t(moyenne_jour_mois)),col=rainbow(12),cex=0.8,fill=rainbow(12))


##### Estimation de la tendance #####

n<-length(Date)
t<-c(1:n)
par(mfrow=c(1,1))

# Par régression linéaire
reg<-lm(Nombre~t)
tend.lm <- reg$fitted
plot(Date,Nombre,type='l',
     main="Par régression linéaire",
     xlab="",ylab="Nombre de tapages")
lines(Date,tend.lm,col='red')
# Pas terrible du tout ...

# Par moyenne mobile
mb<-filter(Nombre, filter=array(1/100,dim=100), method = c("convolution"),
           sides = 2, circular = T)
tend.mb<-xts(mb,order.by=Date)
plot(Date,Nombre,type='l',
     main="Par moyenne mobile",
     xlab="",ylab="Nombre de tapages")
lines(Date,tend.mb,col='red')
# On voit des bonds hebdomadaires...

# Par régression à noyau gaussien
h=50 #comment choisir h???
x<-seq(1,max(t),length=n)
noyau <- function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))}
W<-matrix(unlist(lapply(x,noyau)),ncol=n,nrow=n,byrow=F)
#plot(W[,1])  A quoi ça sert d'afficher ça ? A quoi correspond W ?
tend.kernel<-colSums(as.numeric(Nombre)*W)
tend.kernel<-xts(tend.kernel,order.by=Date)
plot(Date,Nombre,type='l',main="Par régression à noyau gaussien, h=50",
     xlab="",ylab="Nombre de tapages ")
lines(Date,tend.kernel,col='red')
# h=100 #comment choisir h???
# x<-seq(1,max(t),length=n)
# noyau <- function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))}
# W<-matrix(unlist(lapply(x,noyau)),ncol=n,nrow=n,byrow=F)
# #plot(W[,1])  A quoi ça sert d'afficher ça ? A quoi correspond W ?
# tend.kernel<-colSums(as.numeric(Nombre)*W)
# tend.kernel<-xts(tend.kernel,order.by=Date)
# plot(Date,Nombre,type='l',
#      main="Par régression à noyau gaussien, h=100",
#      xlab="",ylab="Nombre de tapages")
# lines(Date,tend.kernel,col='red')
# Pas mal, c'est lisse et rend compte des fluctuations sur l'année en restant assez précis
# mais il reste à savoir quelle valeur de h on prend...

# noyau <- ksmooth(Date,Nombre,kernel=c("normal"),bandwidth = 100)
# tend.kernel <- noyau$y
# plot(Date,Nombre,type='l',
#      main="Par régression à noyau gaussien",
#      xlab="",ylab="Nombre de tapages")
# lines(Date,tend.kernel,col='red')

# Par polynomes locaux
lo<-loess(Nombre~t, degree=1,span=0.9)
tend.lo<-xts(lo$fitted,order.by=Date)
plot(Date,Nombre,type='l',
     main="Par polynomes locaux",
     xlab="",ylab="Nombre de tapages")
lines(Date,tend.lo,col='red')
# Mmh là c'est pas ouf... Un peu mieux que la régression linéaire mais moins bien que les autres

# Par régression sur une base de splines
g<-gam(Nombre~s(t,k=3))
tend.gam<-xts(g$fitted,order.by=Date)
plot(Date,Nombre,type='l',
     main="Par régression sur base de splines",
     xlab="",ylab="Nombre de tapages")
lines(Date,tend.gam,col='red')
# Mmmh au final gros doute, j'aurais tendance à dire que c'est pire que les polynomes locaux
#mais avec du recul en fait j'en sais rien...

# Estimation de la tendance retenue
tend=tend.kernel #à modifier ?


##### Estimation de la partie saisonnière #####

data.st<-Nombre/tend # st pour "sans tendance"
par(mfrow=c(1,1))
plot(Date,data.st,type='l',
     main="Données sans la tendance",
     xlab="",ylab="Nombre de tapages")

# Par régression sur série de Fourier
w=2*pi/7 
fourier<-cbind(cos(w*t), sin(w*t))
K<-20
for(i in c(2:K))
{
  fourier<-cbind(fourier,cos(i*w*t), sin(i*w*t))
}
#matplot(fourier,type='l')
dim(fourier)
reg<-lm(data.st~fourier[,1:2])
saison.lm<-xts(as.numeric(reg$fitted),order.by=Date)
plot(Date,data.st,type='l',
     main="Par régression sur série de Fourier",
     xlab="",ylab="Nombre de tapages")
lines(Date,saison.lm,col='red')
# On voit bien le cycle des semaines... Mais pas des pics et pas la bonne hauteur...

# Par moyenne mobile
saison.mb<-filter(data.st, filter=array(1/7,dim=7), method = c("convolution"),
                  sides = 2, circular = T)
saison.mb<-xts(saison.mb,order.by=Date)
plot(Date,data.st,type='l',
     main="Par moyenne mobile",
     xlab="",ylab="Nombre de tapages")
lines(Date,saison.mb,col='red')
# Pas top... même bien nul

# Par régression à noyau gaussien
h=7 #comment choisir h??? quand il est tout petit c'est parfait...
x<-seq(1,max(t),length=n)
W<-matrix(unlist(lapply(x,function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))})),ncol=n,nrow=n,byrow=F)
#plot(W[,10])
saison.kernel<-colSums(as.numeric(data.st)*W)
saison.kernel<-xts(saison.kernel,order.by=Date)
plot(Date,data.st,type='l',
     main="Par régression à noyau gaussien",
     xlab="",ylab="Nombre de tapages")
lines(Date,saison.kernel,col='red')
# Ca donne rien, mais peut-être est-ce à cause de la valeur de h.

# Par polynômes locaux
lo<-loess(data.st~t, degree=2,span=0.1)
saison.lo<-xts(lo$fitted,order.by=Date)
plot(Date,data.st,type='l',
     main="Par polynomes locaux",
     xlab="",ylab="Nombre de tapages")
lines(Date,saison.lo,col='red')
# Mais c'est que c'est bien nul !

# Par régression sur une base de splines cycliques aka dernier espoir...
cycle<-c(rep(c(1:7),52),1,2,3)
#plot(cycle)
g<-gam(data.st~s(cycle,k=6,bs='cc')) #comment choisir k?
saison.gam<-xts(g$fitted,order.by=Date) 
plot(Date,data.st,type='l',
     main="Par régression sur base de splines cycliques",
     xlab="",ylab="Nombre de tapages")
lines(Date,saison.gam,col='red')
# Pas trop mal on a bien les cycles sous forme de pics !

# Estimation de la saisonnalité retenue
saison=saison.gam

# Estimation tendance + saisonnalité 
par(mfrow=c(1,1))
plot(Date,Nombre,type='l',main="Estimation de la tendance et de la saisonnalité",
     xlab="",ylab="Nombre de tapages")
lines(Date,tend*saison,col='red')

# Estimation partie aléatoire 
plot(Date,Nombre-saison*tend,type='l', main="Partie aléatoire cas T*S+eps",
     xlab="",ylab="Nombre de tapages  ")

plot(Date,data.st/saison,type='l', main="Partie aléatoire dans le cas T*S*eps",
     xlab="",ylab="Nombre de tapages")

plot(Date,data.st-saison,type='l', main="Partie aléatoire dans le cas T*(S+eps)",
     xlab="",ylab="Nombre de tapages")

##### Lissages exponentiels #####

# Lissage : comme on a une saisonnalité, on sait d'avance qu'il faudra utiliser la méthode de Holt-Winters 
# pour en rendre compte 

# Lissage simple
data.smoothsimple<-HoltWinters(Nombre,alpha=NULL,beta=F,gamma=F,l.start=NULL,b.start=NULL,s.start=NULL,
                          optim.start=c(alpha=0),optim.control=list())
plot(Date,Nombre,type='l',main="Lissage exponentiel simple",
     xlab="",ylab="Nombre de tapages")
lines(Date,c(1,data.smoothsimple$fitted[,1]),col='red')
# Bien nul...

# Lissage double
# comment utiliser la fonction HoltWinters pour ce lissage ??
DoubleExpSmooth=function(x,alpha)
{
  xsmooth=x
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1],dim=length(x))
  
  for(i in c(2:length(x)))
  {
    l[i]<-xsmooth[i-1]+(1-(1-alpha)^2)*(x[i]-xsmooth[i-1])
    b[i]<-b[i-1]+alpha^2*(x[i]-xsmooth[i-1])
    xsmooth[i]<-l[i]+b[i]
  }
  
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  return(res)
}


alpha<-seq(0.05,0.95,length=100)
forecast<-lapply(alpha,DoubleExpSmooth,x=Nombre)
erreur<-unlist(
  lapply(forecast,
         function(x){mean((tail(Nombre,n-1)-head(x$smooth,n-1))^2)}))
plot(alpha,erreur,type='l')
data.smoothdouble<-DoubleExpSmooth(Nombre,alpha[which.min(erreur)])
plot(Date,Nombre,type='l',main="Lissage exponentiel double",
     xlab="",ylab="Nombre de tapages")
lines(Date,data.smoothdouble$smooth,col='red')
# Ca commence à être intéressant ...


# Lissage de Holt-Winters saisonnier

# grosse attente ça devrait être la folie pour rendre compte de la saisonnalité

# smoothHW<-HoltWinters(Nombre,alpha=NULL,beta=NULL,gamma=NULL,seasonal="multiplicative",
#                       start.periods=7,l.start=NULL,b.start=NULL,s.start=NULL,
#                       optim.start=c(alpha=0,beta=0,gamma=0),optim.control=list())

# Ca marche pas... 
# " Error in decompose(ts(x[1L:wind], start = start(x), frequency = f), seasonal) : 
# la série temporelle a moins de 2 périodes "
# Il faudrait 2 années de données ...


SeasonalDoubleExpSmooth=function(x,y,T)
{
  alpha <- y[1]
  beta <- y[2]
  delta <- y[3]
  xsmooth=x
  l<-array(x[2],dim=length(x))
  b<-array(x[2]-x[1],dim=length(x))
  s<-array(x[1],dim=length(x))
  
  for(i in c(2:length(x)))
  {
    l[i]<-alpha*(x[i]/s[max(i-T,1)])+(1-alpha)*(l[i-1]+b[i-1])
    b[i]<-beta*(l[i]-l[i-1])+(1-beta)*b[i-1]
    s[i]<-delta*(x[i]/l[i])+(1-delta)*s[max(i-T,1)]
    xsmooth[i]<-(l[i]+b[i])*s[i]
  }
  
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  res$s<-s
  return(res)
}


T <- 7

alpha <-seq(0.05,0.95,length=19)
beta <-seq(0.05,0.95,length=19)
delta <-seq(0.05,0.95,length=19)
parametres <- expand.grid(alpha,beta,delta)

l<- dim(parametres)[1]
par <- list()
for (j in seq(1,l))
  (par[[j]]<-c(parametres[j,1],parametres[j,2],parametres[j,3]))

  
forecast<-lapply(par,SeasonalDoubleExpSmooth,x=Nombre,T=T)
erreur<-unlist(
  lapply(forecast,
         function(x){mean((tail(Nombre,n-1)-head(x$smooth,n-1))^2)}))
parametres <- par[which.min(erreur)]
parametres <- parametres[[1]]

data.seasonalsmooth<-SeasonalDoubleExpSmooth(Nombre,parametres,T)
plot(Date,Nombre,type="l",main="Lissage exponentiel double saisonnier",
     xlab="",ylab="Nombre de tapages")
lines(Date,data.seasonalsmooth$smooth,col="red")

# ARIMA 
data.arima<-auto.arima(Nombre)
plot(Date,Nombre,type="l",main="Méthode ARIMA",
     xlab="",ylab="Nombre de tapages")
lines(Date,data.arima$fitted,col="red")


##### SIMULATION DE LA PREDICTION #####

# Lissage exponentiel double

predict.double <- function(Xsmooth,inst,horizon){
  n<-length(Xsmooth$smooth)
  prev<-c(Xsmooth$smooth[1:inst], Xsmooth$l[inst]+Xsmooth$b[inst]*c(1:horizon))
  return(prev)
}

prev.double<-predict.double(data.smoothdouble,
                          inst=339,horizon=28)
plot(Nombre,type='l',main="Prévision avec lissage exponentiel double",
     xlab="Jour",ylab="Nombre de tapages")
lines(prev.double,col='red',lwd=2)
abline(v=339,lty='dashed')

# Lissage exponentiel double de HW saisonnier

predict.seasonal <- function(Xsmooth,inst,horizon){
  n<-length(Xsmooth$smooth)
  prev<-c(Xsmooth$smooth[1:inst],Xsmooth$s[inst]*(Xsmooth$l[inst]+Xsmooth$b[inst]*c(1:horizon)))
  return(prev)
}

prev.seasonal<-predict.seasonal(data.seasonalsmooth,
                            inst=339,horizon=28)
plot(Nombre,type='l',main="Prévision avec lissage exponentiel saisonnier",
     xlab="Jour",ylab="Nombre de tapages")
lines(prev.seasonal,col='red',lwd=2)
abline(v=339,lty='dashed')

# ARIMA

data.arimaP<-auto.arima(Nombre[1:339])
prev.arimaP <- forecast(data.arimaP,h=28)
plot(prev.arimaP,main="Prévision avec ARIMA",xlab="Jour",ylab="Nombre de tapages")
lines(Nombre,col="red")
abline(v=339,lty='dashed')

lines(prev.arimaP$fitted)
