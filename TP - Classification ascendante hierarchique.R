    #PARTIE1

  #question1
data(USArrests)
U <- USArrests
n <- nrow(U);n #50 états

  #question2
d <- as.matrix(sqrt(n/(n-1))*scale(U,center=TRUE,scale=TRUE))

  #question3
distances <- dist(d)^2/2;distances
dist(d) #dist calcule et renvoie la matrice de distance calculée en utilisant la mesure de distance spécifiée pour calculer les distances entre les lignes d’une matrice de données
#Saut de Ward : on met au carré (distance au carré) et on divise par 2 (facteur 1/2 : cardinal de chaque classe 1 : 1*1/1+1) (1 individu = 1 classe)
length(distances) #1225 = 50*49/2 (pour 1 individu 49 distances (le cas pour les 50 états) : enlever les doublants)

  #question4
h <- hclust(distances, method="ward.D");h
attributes(h)
h$height
plot(h$height) #graphe des hauteurs 
#2 hauteurs importantes qui apparaissent 
plot(h,hang=-1) #dendogramme & hang = -1 : esthétique (mettre tous les noms des individus au meme niveau)

  #question5
plot(h)
#distingue au moins deux classes ou quatre classe 
plot(h$height) #soit 2,3 ou 4 classes mais plot(h) enlève le fait de prendre 3 classes : trop compliqué à la coupure 

  #question6
classe <- cutree(h,2)
classe #voit dans quelle classe appartient chaque individu 
d <- cbind(d,classe)
head(d) #rajoute la colonne des classes
rect.hclust(h,k=2,border='blue') #apparaitre les classes sur le dendogramme 

  #question7
classe1 <- subset(d,classe==1);classe1

  #question8
n1 <- nrow(classe1[,1:4]);n1 #19
cg1 <- colMeans(classe1[,1:4]);cg1 #cente de gravité de la classe 1
#   Murder   Assault  UrbanPop      Rape 
#1.0537705 1.0734025 0.1937144 0.8610414 
nvar <- ncol(U);nvar #nombre de variable total : 4
inertie1 <- mean(rowSums(classe1[,1:4] - matrix(cg1,n1,nvar,byrow=T))^2);inertie1 #1.931782

  #question9
classe2 <- subset(d,classe==2);classe2
n2 <- nrow(classe2[,1:4]);n2 #31
cg2 <- colMeans(classe2[,1:4]);cg2 #cente de gravité de la classe 2
#    Murder    Assault   UrbanPop       Rape 
#-0.6458594 -0.6578919 -0.1187282 -0.5277351  
nvar <- ncol(U);nvar #nombre de variable total : 4
inertie2 <- mean(rowSums(classe2[,1:4] - matrix(cg2,n2,nvar,byrow=T))^2);inertie2 #4.195866

#Inertie1 = 1.931782
#Inertie2 = 4.195866
#la classe 2 est donc la plus dispersée 

  #question10
#classe1 : etats avec fort taux de crime et taux d'urbanisation élevé (exeption alaska : urbanisation faible mais taux de viole/criminalité TRES fort)
#classe2 : etats avec faible taux de crime et taux d'urbanisation faible 

    #PARTIE2
  #question1
library(FactoMineR)
acp <- PCA(d,quali.sup=5)
plot(acp)
attributes(acp)
#taux d'urbanisation ne joue pas pour la classe 1
acp$eig
#retient 2 composantes principales : 86.75
plot(acp$ind$coord[,c(1,2)],pch="")
USArrests1 <- U 
USArrests1 <- cbind(USArrests1,classe)
text(acp$ind$coord[,c(1,2)],label=rownames(U),col=classe)
attributes(acp$ind)
acp$ind$cos2

    #PARTIE3
d1 <- as.matrix(sqrt(n/(n-1))*scale(U,center=TRUE,scale=TRUE))
distances1 <- dist(d1)^2/2;distances1
length(distances1)
h1 <- hclust(distances1, method="single");h1
h1$height
plot(h1$height) #2 classes
plot(h1,hang=-1)
classeS <- cutree(h1,2)
#se retrouve avec un groupe demuserement grand
#gros et plusieurs petits groups saellites 

h2 <- hclust(distances1, method="complet");h2
h2$height
plot(h2$height) #2,4 ou 6 classes
plot(h2,hang=-1)
#tend a former des groupes de taille egale 
#tres sensible aux points aberrants 

h3 <- hclust(distances1, method="average");h3
h3$height
plot(h3$height) #2 ou 5 classes
plot(h3,hang=-1)

