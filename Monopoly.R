
Feldnummer<-1:40
library(readxl)
Straßen <- read_excel("B:/Cloud/OneDrive/Monopoly/Straßen.xlsx")
Ekarte<-c(11,0,0,0,40,0,39,24,0,0,0,0,0,10,0,1)
Gkarte<-c(0,0,0,0,0,40,0,0,0,0,1,0,10,0,0,0,5)
E<-sample(Ekarte,16,replace = FALSE)
G<-sample(Gkarte,16,replace = FALSE)
e<-1
g<-1
Start<-40
N<-10000#Anzahl der Züge
x<-vector(length = N)
z<-vector(length=N)
k<-40
for (i in 1:N){
ifelse (k>40,k<-k-40,k)
Ergebnis<-sum(sample(1:6,2,replace = TRUE))
z[i]<-Ergebnis
ifelse (i==1,k<-Start+Ergebnis,k<-k+Ergebnis)
ifelse (k>40,k<-k-40,k)
ifelse(k==Feld$Feldnummer[Feld$Feldname=="Ereignisfeld"],e<-e+1,e)
ifelse(e>16,e<-(e-16),e)
ifelse(k==Feld$Feldnummer[Feld$Feldname=="Ereignisfeld"],     ifelse(E[e]>0,k<-E[e],k),k)
ifelse(k==Feld$Feldnummer[Feld$Feldname=="Gemeinschaftsfeld"],g<-g+1,g)
ifelse(g>16,g<-(g-16),g)
ifelse(k==Feld$Feldnummer[Feld$Feldname=="Gemeinschaftsfeld"],ifelse(G[g]>0,k<-G[g],k),k)
ifelse (k==Feld$Feldnummer[Feld$Feldname=="Gehe ins Gefängnis"],k<-Feld$Feldnummer[Feld$Feldname=="Gefängnis"],k)
x[i]<-k
}
####ES WERDEN EINIGE FELDER NICHT GEWÜRFELT WARSCHEINLICH WEIL ES EREIGNISFELDER SIND
Feld$Feldnummer[Feld$Feldname=="Gemeinschaftsfeld"]
Feld$Feldnummer[Feld$Feldname=="Ereignisfeld"]

table<-table(x)/N
par(las=2)
barplot(table,names.arg=Feld$Feldname[-30],col=c("Brown","Grey","Brown","Darkgrey","Black","Lightblue","Grey","Lightblue","Lightblue","Red","Pink","Grey","Pink","Pink","Black","Orange","Grey","Orange","Orange","Red","firebrick","Grey","firebrick","firebrick","Black","Yellow","Yellow","Grey","Yellow","Green","Green","Grey","Green","Black","Grey","Blue","Grey","Blue","Red"))
abline(1/40,0,col="red")
?
?abline
length(table(x))
?aggregate
cumsum(Spiel$Wurf)
cumsum<-ifelse(cumsum(Spiel$Wurf)>40,cumsum(Spiel$Wurf)-40,cumsum(Spiel$Wurf))
Spiel<-data.frame(x,z,cumsum)
colnames(Spiel)<-(c("Feld","Wurf","Cumsum"))
?barplot
?row.names
