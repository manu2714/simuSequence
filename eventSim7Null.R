library(simstudy)
library(markovchain)
library(MmgraphR)
library(DiagrammeR)
library(diagram)
library(descr)
library(gmodels)
library(gmodels)
library(descr)
library(MCTM)

# simstudy Generando secuencias

library(simstudy)
set.seed(3928398)

tmatrix8Null <- matrix(c(1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8), 
                       8, 8, 
                     byrow = T)
dd <- genMarkov(n = 5000, transMat = tmatrix8Null, 
                chainLen = 30, wide = TRUE)
# dd<- dd[,-1]
# 
# x<-as.numeric(dd[12,])
# 
# r<-TransMatrix(x,order = 1,probs=F)
# r

# CrossTable(r, prop.r= F,prop.c =F,prop.t = F,asresid=T)

# Cálculo de los residuales

resid<- function(x){
mt<- TransMatrix(x,order= 1,probs= F)
res<-CrossTable(mt, prop.r= F,prop.c =F,
                prop.t = F,asresid=T)
resid<-as.vector(res$asr)
return(resid)
}

residT<-as.matrix(apply(dd,1,resid))


# Cálculo de errores (modelo nulo)

err<- function(x)ifelse(abs(x)>1.96,1,0)
err2<- function(x) x/64
errores<-lapply(residT,err)
errores2<-lapply(errores,sum) 
errores3<- lapply(errores2,err2)
#errores3
errores4<- unlist(errores3)
summary(errores4)
#hist(errores3)
#boxplot(errores3)
var(errores4,na.rm =T)
sd(errores4,na.rm =T)
