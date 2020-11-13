library(ggplot2)
library(simstudy)
library(markovchain)
library(descr)
library(gmodels)
library(MCTM)
library(plyr)
library(ggpubr)
library(phia)
library(car)
library(ipfr)
library(cat)
library(MCTM)
library(hrbrthemes)
library(nortest)
library(e1071)
library(psych)

# Matrices de transición 

# Matriz Nula con 4 estados 

tmatrix4N <- matrix(c(1/4, 1/4, 1/4,1/4,
                         1/4, 1/4, 1/4,1/4,
                         1/4, 1/4, 1/4,1/4,
                         1/4, 1/4, 1/4,1/4), 4, 4, 
                       byrow = T)


# Matrices con p > esperada 

# Matriz A: p = 2/4 estados = 4

tmatrix4A <- matrix(c(1/4, 1/4, 1/4,1/4,
                         1/6, 1/6, 3/6,1/6,
                         1/4, 1/4, 1/4,1/4,
                         1/4, 1/4, 1/4,1/4), 4, 4, 
                       byrow = T)

# Matriz B p =3/4 estados = 4

tmatrix4B <- matrix(c(1/4, 1/4, 1/4,1/4,
                      1/12, 1/12, 9/12,1/12,
                      1/4, 1/4, 1/4,1/4,
                      1/4, 1/4, 1/4,1/4), 4, 4, 
                    byrow = T)


# Matrices con 8 estados 

# Matriz nula
tmatrix8N <- matrix(c(1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                         1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8), 
                       8, 8, 
                       byrow = T)

# Matriz A con p = 4/8, casilla significativa (6,3)


tmatrix8A <- matrix(c(1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/14, 1/14, 4/8,1/14,1/14, 1/14, 1/14,1/14,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8), 
                    8, 8, 
                    byrow = T)


# Matriz B con p = 6/8, casilla significativa (6,3)


tmatrix8B <- matrix(c(1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/28, 1/28, 6/8,1/28,1/28, 1/28, 1/28,1/28,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8,
                      1/8, 1/8, 1/8,1/8,1/8, 1/8, 1/8,1/8), 
                    8, 8, 
                    byrow = T)





# estados = 12

# Matriz nula con 12 estados

tmatrix12N <- matrix(c(1/12,1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12), 
                        12, 12, byrow = T)

# Matriz A p = 6/12 fila significativa =7, col= 9

tmatrix12A <- matrix(c(1/12,1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/22, 1/22, 1/22, 1/22, 1/22, 1/22, 1/22, 1/22, 6/12, 1/22, 1/22,1/22,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                          1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12), 
                        12, 12, byrow = T)


# Matriz A p = 9/12 fila significativa =7, col= 9

tmatrix12B <- matrix(c(1/12,1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/44, 1/44, 1/44, 1/44, 1/44, 1/44, 1/44, 1/44, 9/12, 1/44, 1/44,1/44,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12,
                       1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12,1/12), 
                     12, 12, byrow = T)


# Cálculo de la matriz de transición

transi<- function(x) {
  x<- as.numeric(x)
  TransMatrix(x,order= 1,probs= F)
}

# Calcula dimensiones de la matriz

dimen<- function(x){
  x<- matrix(x,4,4,byrow = F)
  res1<- is.matrix(x)
  dimen<- dim(x)
  resul<- list(res1,dimen)
  resul
}


# Cálculo de los residuales

resid<- function(x){
  x<- as.matrix(x)
  RS <- rowSums(x)
  CS <- colSums(x) 
  GT <- sum(x) 
  CST<- chisq.test(x)
  ASR <- (CST$observed - CST$expected) / sqrt(CST$expected * ((1 - RS / GT) %*% t(1 - CS / GT))) 
  ASR
 }

resid2<- function(x){
   RS <- rowSums(x)
   CS <- colSums(x) 
   GT <- sum(x) 
   CST<- chisq.test(x)
   ASR <- (CST$observed - CST$expected) / sqrt(CST$expected * ((1 - RS / GT) %*% t(1 - CS / GT))) 
   ASR
 }



# Cálculo de errores modelo nulo

erroresNull<- function(x){
  ifelse(abs(x) > 1.96,1,0)
}

# Cálculo porcentaje de errores modelo nulo

perror<- function(x){
  x<- as.vector(x)
  pe<- sum(x)/length(x)
  pe
}

erroresSigA<- function(x,k,fila,col){
  for( i in seq_along(1:k)){
    for( j in seq_along(1:k)){
      if(is.na(x[i,j])==TRUE)x[i,j]=0
      else if( i != fila){
        if(x[i,j]< -1.96)x[i,j]= 1
        else if(x[i,j]> 1.96)x[i,j]= 2
        else x[i,j]= 3
      }
      else if(i == fila && j != col){
        if(x[i,j]< -1.96) x[i,j]= 3
        else if(x[i,j]> 1.96) x[i,j]= 2
        else x[i,j] = 1
      }
      else if(i == fila && j == col){
        if(x[i,j] > 1.96)x[i,j]=3 
        else x[i,j]= 1
      }
    }
  }
  x
}


erroresSig2<-function(x,k,fila,col){
  for( i in seq_along(1:k)){
    for( j in seq_along(1:k)){
      if(is.na(x[i,j])==TRUE)x[i,j]=0
      else if( i != fila){
        if(abs(x[i,j])< 1.96)x[i,j]= 4
        else x[i,j]= 3
      }
      else if(i == fila && j != col){
        if(x[i,j]< -1.96) x[i,j]= 4
        else x[i,j] = 2
      }
      else if(i == fila && j == col){
        if(x[i,j] > 1.96)x[i,j]=4 
        else x[i,j]= 1
      }
    }
  }
  x
}

taberr<- function(x){
  v<- table(x)
  nas<- as.numeric(v['0'])
  ecs<- as.numeric(v['1']) #errores casilla significativa
  efs<- as.numeric(v['2']) # errores fila significativa
  efns<- as.numeric(v['3']) # errores filas no significativas
  corr<- as.numeric(v['4']) # correctas
  
  if(is.na(nas) == T) nas<-0
  if(is.na(ecs) == T) ecs<-0
  if(is.na(efns) == T) efns<-0
  if(is.na(corr) == T) corr<-0
   
  n<- length(x)- nas
  pet<- (ecs+efs+efns)/n
  pecs<- ecs/n
  pefs<- efs/n 
  pefns<- efns/n
  pcor<- corr/n
  res<- c(pet,pecs,pefs,pefns, pcor)
  return(res)
}

# Errores totales y sus tipos

et<- function(x){
  v<- table(x)
  nas<- as.numeric(v['0'])
  ecs<- as.numeric(v['1']) #errores casilla significativa
  efs<- as.numeric(v['2']) # errores fila significativa
  efns<- as.numeric(v['3']) # errores filas no significativas
  corr<- as.numeric(v['4']) # correctas
  
  if(is.na(nas) == T) nas<-0
  if(is.na(ecs) == T) ecs<-0
  if(is.na(efns) == T) efns<-0
  if(is.na(corr) == T) corr<-0
  
  n<- length(x)- nas
  pet<- (ecs+efs+efns)/n
  pecs<- ecs/n
  pefs<- efs/n
  pefns<- efns/n
  pcor<- corr/n
  res<- c(pet,pecs,pefs,pcor)
  pet
}

# Errores casilla significativa

ecs<- function(x){
  v<- table(x)
  nas<- as.numeric(v['0'])
  ecs<- as.numeric(v['1']) #errores casilla significativa
  efs<- as.numeric(v['2']) # errores fila significativa
  efns<- as.numeric(v['3']) # errores filas no significativas
  corr<- as.numeric(v['4']) # correctas
  
  if(is.na(nas) == T) nas<-0
  if(is.na(ecs) == T) ecs<-0
  if(is.na(efns) == T) efns<-0
  if(is.na(corr) == T) corr<-0
  
  n<- length(x)- nas
  pet<- (ecs+efs+efns)/n
  pecs<- ecs/n
  pefs<- efs/n 
  pcor<- corr/n
  res<- c(pet,pecs,pefs,pcor)
  pecs
}

# Errores fila significativa

efs<- function(x){
  v<- table(x)
  nas<- as.numeric(v['0'])
  ecs<- as.numeric(v['1']) #errores casilla significativa
  efs<- as.numeric(v['2']) # errores fila significativa
  efns<- as.numeric(v['3']) # errores filas no significativas
  corr<- as.numeric(v['4']) # correctas
  
  if(is.na(nas) == T) nas<-0
  if(is.na(ecs) == T) ecs<-0
  if(is.na(efns) == T) efns<-0
  if(is.na(corr) == T) corr<-0
  
  n<- length(x)- nas
  pet<- (ecs+efs+efns)/n
  pecs<- ecs/n
  pefs<- efs/n 
  pcor<- corr/n
  res<- c(pet,pecs,pefs,pcor)
  pefs
}
# Errores fila significativa

efs<- function(x){
  v<- table(x)
  nas<- as.numeric(v['0'])
  ecs<- as.numeric(v['1']) #errores casilla significativa
  efs<- as.numeric(v['2']) # errores fila significativa
  efns<- as.numeric(v['3']) # errores filas no significativas
  corr<- as.numeric(v['4']) # correctas
  
  if(is.na(nas) == T) nas<-0
  if(is.na(ecs) == T) ecs<-0
  if(is.na(efns) == T) efns<-0
  if(is.na(corr) == T) corr<-0
  
  n<- length(x)- nas
  pet<- (ecs+efs+efns)/n
  pecs<- ecs/n
  pefs<- efs/n 
  pcor<- corr/n
  res<- c(pet,pecs,pefs,pcor)
  pefs
}

# Errores fila NO significativa

efns<- function(x){
  v<- table(x)
  nas<- as.numeric(v['0'])
  ecs<- as.numeric(v['1']) #errores casilla significativa
  efs<- as.numeric(v['2']) # errores fila significativa
  efns<- as.numeric(v['3']) # errores filas no significativas
  corr<- as.numeric(v['4']) # correctas
  
  if(is.na(nas) == T) nas<-0
  if(is.na(ecs) == T) ecs<-0
  if(is.na(efns) == T) efns<-0
  if(is.na(corr) == T) corr<-0
  
  n<- length(x)- nas
  pet<- (ecs+efs+efns)/n
  pecs<- ecs/n
  pefs<- efs/n 
  pefns<- efns/n
  pcor<- corr/n
  res<- c(pet,pecs,pefs,pefns,pcor)
  pefns
}

# Errores totales y sus tipos

errores<- function(x,tipo){
  v<- table(x)
  nas<- as.numeric(v['0'])
  ecs<- as.numeric(v['1']) #errores casilla significativa
  efs<- as.numeric(v['2']) # errores fila significativa
  efns<- as.numeric(v['3']) # errores filas no significativas
  corr<- as.numeric(v['4']) # correctas
  
  if(is.na(nas) == T) nas<-0
  if(is.na(ecs) == T) ecs<-0
  if(is.na(efns) == T) efns<-0
  if(is.na(corr) == T) corr<-0
  
  n<- length(x)- nas
  pet<- (ecs+efs+efns)/n
  pecs<- ecs/n
  pefs<- efs/n
  pefns<- efns/n
  pcor<- corr/n
  res<- c(pet,pecs,pefs,pcor)
  if( tipo == 'pet') 
    return(pet)
  if( tipo == 'pecs') 
    return(pecs)
  if( tipo == 'pefs') 
    return(pefs)
  if( tipo == 'pefns') 
    return(pefns)
  if( tipo == 'pcor') 
    return(pcor)
}

# Matrices que no cumplen la dimension

excepciones<- function(x,dm){
  v1<- vector()
  for(i in 1:length(x)){
    if(dim(x[[i]])[1]<  dm|| dim(x[[i]])[2]<dm){
      v1<- c(v1,i)
    }
  }
   if(length(v1) ==0)
     v1<- NA
  else v1
}

#resumen estadístico

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
