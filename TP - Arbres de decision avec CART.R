    #--question1 : installation du package 
install.packages("rpart.plot")
library(rpart.plot) #le package a bien ete compile


    #--question2 : charger les donnees ptitanic
data(ptitanic)
? ptitanic
attach(ptitanic)

n <- nrow(ptitanic) #1309
#1046 observations d'apres l'aide mais 1309 sur en reelle sur R
#6 variables : 
  #pclass : classe des passagers : 1st 2nd 3rd
  #survived	: died or survived
  #sex	: male or female
  #age	: in years : min 0.167 max 80.0
  #sibsp : nombre de freres et soeurs ou conjoints a bord : 0...8
  #parch : nombre de parents ou d'enfants a bord : 0...6


    #--question3 : variable survived
attributes(survived)
#qualitatifs : died & survived 
#class : facteur 

sum(is.na(survived))
#il n'y a pas de valeur manquante


    #--question4 : arbre de decision
r <- rpart(survived~., data = ptitanic)
rpart.plot(r) #arbre de decision pour predire survived 

#selectionne les femmes des classes 1 ou 2 dans cette exemple (dans 'ex') : 
ex <- subset(ptitanic, sex != "male" & pclass != "3rd")
summary(ex$survived)
sum(ex$survived == "survived")/nrow(ex) #proportion des survivantes : 0.932
nrow(ex)/nrow(ptitanic) #pourcentage relatif a tous les exemples : 0.19


    #--question5 : nouveau passager
np <- data.frame("3rd", "female", 19, 3, 0)
names(np) <- c("pclass", "sex", "age", "sibsp", "parch")
np

  #nouveaux passager : predire si elle a survecu ou non 
#  pclass    sex age sibsp parch
#    3rd female  19     3     0

predict(r, newdata = np) #probabilites
predict(r, newdata = np, type = "class") #classe majoritaire

#       died  survived
# 0.8571429 0.1428571
#elle a une probabilite de 86% d'etre morte 

  #estimer erreur apprentissage de l'arbre : 
pred.surv <- predict(r, newdata = ptitanic, type = "class") #predire valeur de survived
mat.conf <- table(ptitanic$survived, pred.surv) 
#matrice de confusion -> tableau qui presentes des previsions et des valeurs reelles 
err.app <- (mat.conf[2,1]+mat.conf[1,2])/sum(mat.conf) ; err.app #0.1749427
#il y a environ 17.5% d'erreur dans l'arbre 


    #--question6 : erreur apprentissage 
test <- ptitanic[(80:380),]
app <- ptitanic[-(80:380),]

n <- nrow(ptitanic)
param.app <- 0.7 #proportion des exemples pour l apprentissage (fixe tout le temps)
set.seed(123) #on fixe la graine aleatoire/les donnees : tout le monde le meme echantillon/erreur 
permut.lignes <- sample(n) #on "melange" les indices des lignes : aleatoire (tout le meme a la meme permutation des lignes)
sel <- permut.lignes[1:(param.app * n)] #selection des lignes pour l apprentissage
app <- ptitanic[sel,] #ensemble d apprentissage (70%)
test <- ptitanic[-sel,] #ensemble de test (30%)
nrow(app) + nrow(test) == nrow(ptitanic) #verification

r2 <- rpart(survived~., data = app)
pred.surv2 <- predict(r2, newdata = test, type = "class") #predire valeur de survived
mat.conf2 <- table(test$survived, pred.surv2)#matrice de confusion
err.app2 <- (mat.conf2[2,1]+mat.conf2[1,2])/sum(mat.conf2) ; err.app2 #0.2010178
#il y a environ 20% d'erreur dans l'arbre 


    #--question7 : choix pour l'arbre de decision 
?rpart.control
#minsplit: taille minimal pour segmenter
#cp : parametre de complexite qui permet de gagner du temps de calcul en raccourcissant les divisions qui n'en valent pas la peine (exemple : avec la division anova, cela signifie que le R au carre global doit augmenter de cp a chaque etape)

r3 <- rpart(survived~., data = ptitanic, minsplit = 0, cp = 0)
rpart.plot(r3) #affiche arbre de decision 

minsplit <- 10 #on travaille a minsplit constant
mes.cp <- seq(0, 0.1, by = 0.001) #valeurs de cp a essayer
erreur.test <- numeric(length(mes.cp)) #les erreurs de test correspondantes

erreur <- function (x) {
  som <- 0
  NC <- ncol(x)
  NL <- nrow(x)
  for (i in 1:NL){
    for (k in 1:NC) {
      if (i != k) {
        som <- som + x[i,k]
      }
    }
  }
  return(som/sum(x))
}

for (i in 1:length(mes.cp)) {
  r4 <- rpart(survived~., data = app, cp = mes.cp[i], minsplit = minsplit)
  pred.surv4 <- predict(r4, newdata = test, type = "class")
  mat.conf4 <- table(test$survived, pred.surv4)
  erreur.test[i] <- erreur(mat.conf4)
}
plot(mes.cp, erreur.test, type = "l") #erreur de test en fonction de cp

#Lorsque minsplit = 10, cp = 0.009 semble un choix pertinent car c'est a ce moment que l'erreur de test est la plus petite (la plus proche de l'arbre) (moins de 0.1)
r5 <- rpart(survived~., data = ptitanic, cp = 0.009, minsplit = 10)
rpart.plot(r5)


    #--question8 : repproche 
#comme on a fixe les donnees avec set.seed le choix est par rapport a cette commande : decoupage arbitraire


    #--question9 : avec zoo
install.packages("mlbench")
library(mlbench) #le package a bien ete compile
data(Zoo)
Zoo
attach(Zoo)

sum(is.na(type))
#il n'y a pas de valeur manquante

n2 <- nrow(Zoo)
set.seed(123)
mes.cp2 <- seq(0, 0.1, by = 0.001) 
permut.lignes2 <- sample(n2) 
sel2 <- permut.lignes2[1:(param.app * n2)] ;sel2
test2 <- Zoo[-sel2,]
app2 <- Zoo[sel2,]
nrow(app2) + nrow(test2) == n2 #verification


  #on essaie avec minsplit qui vaut 4 car il y a 4 amphibiens et c'est le type avec le moins d'individus 
minsplit2 <- 4
erreur.test2 <- numeric(length(mes.cp2))
for (i in 1:length(mes.cp2)) {
  r6 <- rpart(type~., data = app2, cp = mes.cp2[i], minsplit = minsplit2)
  pred.surv6 <- predict(r6, newdata = test2, type = "class")
  mat.conf6 <- table(test2$type, pred.surv6)
  erreur.test2[i] <- erreur(mat.conf6)
}
plot(mes.cp2, erreur.test2, type = "l")
#cp vaudrait entre 0 et 0.069
#avec une erreur de test d'environ de 0.032

#test : on essaie avec minsplit qui vaut 7
erreur.test3 <- numeric(length(mes.cp2))
for (i in 1:length(mes.cp2)) {
  r7 <- rpart(type~., data = app2, cp = mes.cp2[i], minsplit = 7)
  pred.surv7 <- predict(r7, newdata = test2, type = "class")
  mat.conf7 <- table(test2$type, pred.surv7)
  erreur.test3[i] <- erreur(mat.conf7)
}
plot(mes.cp2, erreur.test3, type = "l")
#cp vaut pareil que precedemment 

#on peut prendre un minsplit qui vaut de 1 a 7 : les cp qu'on trouvera seront toujours les meme 


  #trouver le cp : 
#cp = 0
r8 <- rpart(type~., data = Zoo, cp = 0, minsplit = minsplit2)
rpart.plot(r8) 
#arbre convenable (peut etre un peu trop profond)

#cp = 0.03
r9 <- rpart(type~., data = Zoo, cp = 0.03, minsplit = minsplit2)
rpart.plot(r9) 
#correspond le plus : le plus detaille sans trop l'etre 

#cp = 0.069
r10 <- rpart(type~., data = Zoo, cp = 0.069, minsplit = minsplit2)
rpart.plot(r10) 
#tous les types ne sont pas traites (les amphibiens)


    #Le "meilleur" arbre de decision pour predire la variable type semble etre : 
r11 <- rpart(type~., data = Zoo, cp = 0.03, minsplit = 4)
rpart.plot(r11)
#de plus, il y a environ 3.2% d'erreur dans l'arbre 