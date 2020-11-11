# simstudy Generando secuencias

options(error=recover)

ini<- Sys.time()

set.seed(1234567)


#dd12B20 <- genMarkov(n = 5000, transMat = tmatrix12B, 
#               chainLen = 20, wide = TRUE)
# dd12B30 <- genMarkov(n = 5798, transMat = tmatrix12B, 
#                        chainLen = 30, wide = TRUE)
# dd12B40 <- genMarkov(n = 5207, transMat = tmatrix12B, 
#                        chainLen = 40, wide = TRUE)
# dd12B50 <- genMarkov(n = 5070, transMat = tmatrix12B, 
#                        chainLen = 50, wide = TRUE)
dd12B100 <- genMarkov(n = 5011, transMat = tmatrix12B, 
                       chainLen = 100, wide = TRUE)
dd12B200 <- genMarkov(n = 5000, transMat = tmatrix12B, 
                       chainLen = 200, wide = TRUE)
dd12B500 <- genMarkov(n = 5000, transMat = tmatrix12B, 
                     chainLen = 500, wide = TRUE)

#dd12B20<- dd12B20[,-1]
# dd12B30<- dd12B30[,-1]
# dd12B40<- dd12B40[,-1]
# dd12B50<- dd12B50[,-1]
dd12B100<- dd12B100[,-1]
dd12B200<- dd12B200[,-1]
dd12B500<- dd12B500[,-1]

# Cálculo de las matrices de transición

#mt12B20<- alply(dd12B20,1,transi)
# mt12B30<- alply(dd12B30,1,transi)
# mt12B40<- alply(dd12B40,1,transi)
# mt12B50<- alply(dd12B50,1,transi)
mt12B100<- alply(dd12B100,1,transi)
mt12B200<- alply(dd12B200,1,transi)
mt12B500<- alply(dd12B500,1,transi)


# Cálculo de los residuales

#residT12B20<- lapply(mt12B20,resid2)
# residT12B30<- lapply(mt12B30,resid2)
# residT12B40<- lapply(mt12B40,resid2)
# residT12B50<- lapply(mt12B50,resid2)
residT12B100<- lapply(mt12B100,resid2)
residT12B200<- lapply(mt12B200,resid2)
residT12B500<- lapply(mt12B500,resid2)


# Cálculo de errores modelo con efecto mayor que azar

#v12B20<- excepciones(residT12B20,12)
# v12B30<- excepciones(residT12B30,12)
# v12B40<- excepciones(residT12B40,12)
# v12B50<- excepciones(residT12B50,12)
v12B100<- excepciones(residT12B100,12)
v12B200<- excepciones(residT12B200,12)
v12B500<- excepciones(residT12B500,12)

#residT12B20<- residT12B20[-v12B20]
# residT12B30<- residT12B30[-v12B30]
# residT12B40<- residT12B30[-v12B40]
# residT12B50<- residT12B30[-v12B50]
residT12B100<- residT12B100[-v12B100]


#erroresT12B20<- lapply(residT12B20,FUN=erroresSigA,k=12, fila=7,col=9)
# erroresT12B30<- lapply(residT12B30,FUN=erroresSig2,k=12,fila=7,col=9)
# erroresT12B40<- lapply(residT12B40,FUN=erroresSig2,k=12,fila=7,col=9)
# erroresT12B50<- lapply(residT12B50,FUN=erroresSig2,k=12,fila=7,col=9)
erroresT12B100<- lapply(residT12B100,FUN=erroresSig2,k=12,fila=7,col=9)
erroresT12B200<- lapply(residT12B200,FUN=erroresSig2,k=12,fila=7,col=9)
erroresT12B500<- lapply(residT12B500,FUN=erroresSig2,k=12,fila=7,col=9)

 # Porcentaje de errores totales
 
#perroresT12B20<- sapply(erroresT12B20,et)
# perroresT12B30<- sapply(erroresT12B30,et)
# perroresT12B40<- sapply(erroresT12B40,et)
# perroresT12B50<- sapply(erroresT12B50,et)
perroresT12B100<- sapply(erroresT12B100,et)
perroresT12B200<- sapply(erroresT12B200,et)
perroresT12B500<- sapply(erroresT12B500,et)

# Media errores totales

#mediaet12B20<- mean(perroresT12B20,na.rm=T)

# mediaET12B30<- mean(perroresT12B30,na.rm=T)
# mediaET12B40<- mean(perroresT12B40,na.rm=T)
# mediaET12B50<- mean(perroresT12B50,na.rm=T)
mediaET12B100<- mean(perroresT12B100,na.rm=T)
mediaET12B200<- mean(perroresT12B200,na.rm=T)
mediaET12B500<- mean(perroresT12B500,na.rm=T)

# dt errores totales

#dtET12B20<- sd(perroresT12B20,na.rm=T)
# dtET12B30<- sd(perroresT12B30,na.rm=T)
# dtET12B40<- sd(perroresT12B40,na.rm=T)
# dtET12B50<- sd(perroresT12B50,na.rm=T)
dtET12B100<- sd(perroresT12B100,na.rm=T)
dtET12B200<- sd(perroresT12B200,na.rm=T)
dtET12B500<- sd(perroresT12B500,na.rm=T)

mediaET12B<- c(NA,NA,NA,mediaET12B100,mediaET12B200,mediaET12B500)

dtET12B<- c(NA,NA,NA,dtET12B100,dtET12B200,dtET12B500)

resET12B<- data.frame(mediaET12B, dtET12B)
row.names(resET12B)<-c('30','40','50','100','200','500')



# Porcentaje de errores casilla significativa

#perroresCS12B20<- sapply(erroresT12B20,ecs)
# perroresCS12B30<- sapply(erroresT12B30,ecs)
# perroresCS12B40<- sapply(erroresT12B40,ecs)
# perroresCS12B50<- sapply(erroresT12B50,ecs)
perroresCS12B100<- sapply(erroresT12B100,ecs)
perroresCS12B200<- sapply(erroresT12B200,ecs)
perroresCS12B500<- sapply(erroresT12B500,ecs)

# Media errores casilla significativa

#mediaECS12B20<- mean(perroresCS12B20,na.rm=T)
# mediaECS12B30<- mean(perroresCS12B30,na.rm=T)
# mediaECS12B40<- mean(perroresCS12B40,na.rm=T)
# mediaECS12B50<- mean(perroresCS12B50,na.rm=T)
mediaECS12B100<- mean(perroresCS12B100,na.rm=T)
mediaECS12B200<- mean(perroresCS12B200,na.rm=T)
mediaECS12B500<- mean(perroresCS12B500,na.rm=T)

# dt errores casilla significativa

#dtECS12B20<- sd(perroresCS12B20,na.rm=T)
# dtECS12B30<- sd(perroresCS12B30,na.rm=T)
# dtECS12B40<- sd(perroresCS12B40,na.rm=T)
# dtECS12B50<- sd(perroresCS12B50,na.rm=T)
dtECS12B100<- sd(perroresCS12B100,na.rm=T)
dtECS12B200<- sd(perroresCS12B200,na.rm=T)
dtECS12B500<- sd(perroresCS12B500,na.rm=T)

mediaECS12B<- c(NA,NA,NA,
              mediaECS12B100,mediaECS12B200,mediaECS12B500)

dtECS12B<- c(NA,NA,NA,dtECS12B100,
            dtECS12B200,dtECS12B500)

resECS12B<- data.frame(mediaECS12B, dtECS12B)
row.names(resECS12B)<-c('30','40','50','100','200','500')



# Porcentaje de errores fila significativa

#perroresFS12B20<- sapply(erroresT12B20,efs)
# perroresFS12B30<- sapply(erroresT12B30,efs)
# perroresFS12B40<- sapply(erroresT12B40,efs)
# perroresFS12B50<- sapply(erroresT12B50,efs)
perroresFS12B100<- sapply(erroresT12B100,efs)
perroresFS12B200<- sapply(erroresT12B200,efs)
perroresFS12B500<- sapply(erroresT12B500,efs)

# Media errores fila significativa

#mediaEFS12B20<- mean(perroreFS12B20,na.rm=T)
# mediaEFS12B30<- mean(perroresFS12B30,na.rm=T)
# mediaEFS12B40<- mean(perroresFS12B40,na.rm=T)
# mediaEFS12B50<- mean(perroresFS12B50,na.rm=T)
mediaEFS12B100<- mean(perroresFS12B100,na.rm=T)
mediaEFS12B200<- mean(perroresFS12B200,na.rm=T)
mediaEFS12B500<- mean(perroresFS12B500,na.rm=T)

# dt errores fila significativa

#dtEFS12B20<- sd(perroresFS12B20,na.rm=T)
# dtEFS12B30<- sd(perroresFS12B30,na.rm=T)
# dtEFS12B40<- sd(perroresFS12B40,na.rm=T)
# dtEFS12B50<- sd(perroresFS12B50,na.rm=T)
dtEFS12B100<- sd(perroresFS12B100,na.rm=T)
dtEFS12B200<- sd(perroresFS12B200,na.rm=T)
dtEFS12B500<- sd(perroresFS12B500,na.rm=T)

mediaEFS12B<- c(NA,NA,NA,
               mediaEFS12B100,mediaEFS12B200,mediaEFS12B500)

dtEFS12B<- c(NA,NA,NA,dtEFS12B100,
            dtEFS12B200,dtEFS12B500)

resEFS12B<- data.frame(mediaEFS12B, dtEFS12B)
row.names(resET12B)<-c('30','40','50','100','200','500')


# Porcentaje de errores fila NO significativa

#perroreFNS12B20<- sapply(erroresT12B20,efns)
# perroresFNS12B30<- sapply(erroresT12B30,efns)
# perroresFNS12B40<- sapply(erroresT12B40,efns)
# perroresFNS12B50<- sapply(erroresT12B50,efns)
perroresFNS12B100<- sapply(erroresT12B100,efns)
perroresFNS12B200<- sapply(erroresT12B200,efns)
perroresFNS12B500<- sapply(erroresT12B500,efns)

# Media errores fila NO significativa

#mediaEFNS12B20<- mean(perroreFNS12B20,na.rm=T)
# mediaEFNS12B30<- mean(perroresFNS12B30,na.rm=T)
# mediaEFNS12B40<- mean(perroresFNS12B40,na.rm=T)
# mediaEFNS12B50<- mean(perroresFNS12B50,na.rm=T)
mediaEFNS12B100<- mean(perroresFNS12B100,na.rm=T)
mediaEFNS12B200<- mean(perroresFNS12B200,na.rm=T)
mediaEFNS12B500<- mean(perroresFNS12B500,na.rm=T)

# dt errores fila NO significativa

#dtEFNS12B20<- sd(perroresFNS12B20,na.rm=T)
# dtEFNS12B30<- sd(perroresFNS12B30,na.rm=T)
# dtEFNS12B40<- sd(perroresFNS12B40,na.rm=T)
# dtEFNS12B50<- sd(perroresFNS12B50,na.rm=T)
dtEFNS12B100<- sd(perroresFNS12B100,na.rm=T)
dtEFNS12B200<- sd(perroresFNS12B200,na.rm=T)
dtEFNS12B500<- sd(perroresFNS12B500,na.rm=T)

mediaEFNS12B<- c(NA,NA,NA,
               mediaEFNS12B100,mediaEFNS12B200,mediaEFNS12B500)

dtEFNS12B<- c(NA,NA,NA,dtEFNS12B100,dtEFNS12B200,
             dtEFNS12B500)

resEFNS12B<- data.frame(mediaEFNS12B, dtEFNS12B)
row.names(resET12B)<-c('30','40','50','100','200','500')

round(resET12B,2)
round(resECS12B,2)
round(resEFS12B,2)
round(resEFNS12B,2)


fin<- Sys.time()

fin-ini

