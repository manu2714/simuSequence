


# simstudy Generando secuencias


set.seed(1234567)


dd4N <- genMarkov(n = 5132, transMat = tmatrix4N, 
                chainLen = 20, wide = TRUE)

dd4N<- dd4N[,-1]

# Cálculo de las matrices de transición

mt4N<- apply(dd4N,1,transi)

# Identificación de las matrices con dimensiones no válidas

dimension<- laply(mt4Nb, dim)
nocumplen<- which(dimension !=4)
mt4Nb<-mt4N[-nocumplen]
mt4N<- mt4Nb[1:5000]


# Cálculo de los residuales

residT4N<- lapply(mt4N,resid2)
residT4Nb<- sapply(mt4N,resid2)
residT4Nb <- as.vector(residT4Nb)
summary(residT4Nb)
ks.test(residT4Nb,rnorm(length(residT4Nb)))
nas<-which(is.na(residT4Nb))
resid4Nb<-residT4Nb[-nas] 
hist(resid4Nb)
ad.test(resid4Nb)
lillie.test(residT4Nb)
pearson.test(residT4Nb)
skewness(residT4Nb, na.rm=T)
kurtosis(residT4Nb,na.rm = T)
describe(residT4Nb)


# Histograma de los residuales

resid4N<- as.data.frame(resid4Nb)


hresid<- ggplot(resid4N) +
  geom_histogram(aes(x = resid4Nb),binwidth=.3,fill= "blue")+
  theme_ipsum()
  
hresid

hresidCat<- cut(resid4N$resid4Nb,breaks = c(-100000,-2,2,99999999))
table(hresidCat)/sum(table(hresidCat))

