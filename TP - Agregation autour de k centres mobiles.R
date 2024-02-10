    #Exemple introductif

  #question1
n <- 200
x1 <- rnorm(n/2)
y1 <- rnorm(n/2)
x2 <- rnorm(n/2, 4, 2)
y2 <- rnorm(n/2, 4, 2)
d <- data.frame(c(x1, x2), c(y1, y2))
names(d) <- c("x", "y")
#les données sont généréees de manières alétoires : rnorm
plot(d)
#on voit 2 groupes

  #question2 : identifier 2 groupes homogènes dans les données
km <- kmeans(d, 2) #k moyennes mobiles avec 2 classes
km
km$cluster

  #question3
plot(d, pch="")
text(d, label=km$cluster)
#confirme les 2 groupes qu'on a vu lors du nuage de points des données (d)
plot(d, col=km$cluster) #change la couleur du nuage des points en fonction des deux classes

  #question4
km$centers #centre de gravite des classes 
km$size #taille des classes 
IClasse <- km$withinss/km$size #inertie de chaque classe : on normalise les resultats de km$withinss
# 6.986326 2.920019 --> la 1er a la plus forte inertie 
#intraclasse : 
poids <- km$size/n;poids #pondération 
IIntra0 <- poids%*%IClasse;IIntra0
IIntra <- km$tot.withinss/n;IIntra
#4.770189

  #question5 
#Inertie interclasse : 
IInter <- km$betweenss/n
#7.270401
#Inertie totale : 
I <- IIntra + IInter;I
#12.04059

  #question6
km1 <- kmeans(d, 1)
intra1 <- km1$tot.withinss/n
inter1 <- km1$betweenss/n
tot1 <- intra1 + inter1

km2 <- kmeans(d, 2)
intra2 <- km2$tot.withinss/n
inter2 <- km2$betweenss/n
tot2 <- intra2 + inter2

km3 <- kmeans(d, 3)
intra3 <- km3$tot.withinss/n
inter3 <- km3$betweenss/n
tot3 <- intra3 + inter3

km4 <- kmeans(d, 4)
intra4 <- km4$tot.withinss/n
inter4<- km4$betweenss/n
tot4 <- intra4 + inter4

km5 <- kmeans(d, 5)
intra5 <- km5$tot.withinss/n
inter5 <- km5$betweenss/n
tot5 <- intra5 + inter5

plot(c(intra1,intra2,intra3,intra4,intra5),type="l")
points(c(inter1,inter2,inter3,inter4,inter5),type="l",lty=2)
points(c(tot1,tot2,tot3,tot4,tot5),type="l",lty=3)
#d'après le graphique il est préférable de prendre 2 classes (coude : changement significatif dans les données)
#inertie inter augmente
#inertie intra diminue 

  #question7
plot(d, col=km3$cluster)
#individus extremes assez proches mais dans des classes différentes

    #Les données USArrests

  #question1
data("USArrests")
U <- USArrests
n <- nrow(U)
Z <- sqrt((n-1)/n)*scale(U);Z

  #question2
i <- 6
inter <- numeric(i)
intra <- numeric(i)
tot <- numeric(i)
for (k in 1:i){
  km <- kmeans(Z, k)
  intra[k] <- km$tot.withinss/n
  inter[k] <- km$betweenss/n
  tot[k] <- intra[k] + inter[k]
}

intra
inter
inter+intra
tot

plot(intra,type="l",)
points(inter,type="l",lty=2)
points(tot,type="l",lty=3)
#d'après les graphique il est préférable de prendre 2 ou 4 classes (car 2 coudes) : choisit 4 classes
km <- kmeans(Z,4)

  #question4
library(FactoMineR)
acp <- PCA(Z)
acp$eig
#la composante 3 représente 1/3 des variables donc on prend les 2 premières composantes
plot(acp$ind$coord[,1:2],pch="")
text(acp$ind$coord[,1:2],label=rownames(Z),col=km$cluster)

#distance entre plan d'ou le alaska qui est dans le bleu (Dim3) : contribue le plus à la 3eme composante
acp$var$cor
#fortement corrélé avec taux d'urbanisation : alaska
#dans les deux premiers plan factoriel : alaska très mal représenté 
plot(acp,axe=c(1,3))
#confirme ce que dit algorithme des k moyennes mobiles 