# simstudy Generando secuencias

options(error=recover)

ini<- Sys.time()

set.seed(1234567)


#dd12A20 <- genMarkov(n = 5000, transMat = tmatrix12A, 
#               chainLen = 20, wide = TRUE)
# dd12A30 <- genMarkov(n = 5798, transMat = tmatrix12A, 
#                        chainLen = 30, wide = TRUE)
# dd12A40 <- genMarkov(n = 5207, transMat = tmatrix12A, 
#                        chainLen = 40, wide = TRUE)
# dd12A50 <- genMarkov(n = 5070, transMat = tmatrix12A, 
#                        chainLen = 50, wide = TRUE)
dd12A100 <- genMarkov(n = 5011, transMat = tmatrix12A, 
                       chainLen = 100, wide = TRUE)
dd12A200 <- genMarkov(n = 5000, transMat = tmatrix12A, 
                       chainLen = 200, wide = TRUE)
dd12A500 <- genMarkov(n = 5000, transMat = tmatrix12A, 
                     chainLen = 500, wide = TRUE)

#dd12A20<- dd12A20[,-1]
# dd12A30<- dd12A30[,-1]
# dd12A40<- dd12A40[,-1]
# dd12A50<- dd12A50[,-1]
dd12A100<- dd12A100[,-1]
dd12A200<- dd12A200[,-1]
dd12A500<- dd12A500[,-1]

# Cálculo de las matrices de transición

#mt12A20<- alply(dd12A20,1,transi)
# mt12A30<- alply(dd12A30,1,transi)
# mt12A40<- alply(dd12A40,1,transi)
# mt12A50<- alply(dd12A50,1,transi)
mt12A100<- alply(dd12A100,1,transi)
mt12A200<- alply(dd12A200,1,transi)
mt12A500<- alply(dd12A500,1,transi)


# Cálculo de los residuales

#residT12A20<- lapply(mt12A20,resid2)
# residT12A30<- lapply(mt12A30,resid2)
# residT12A40<- lapply(mt12A40,resid2)
# residT12A50<- lapply(mt12A50,resid2)
residT12A100<- lapply(mt12A100,resid2)
residT12A200<- lapply(mt12A200,resid2)
residT12A500<- lapply(mt12A500,resid2)


# Cálculo de errores modelo con efecto mayor que azar

#v12A20<- excepciones(residT12A20,12)
# v12A30<- excepciones(residT12A30,12)
# v12A40<- excepciones(residT12A40,12)
# v12A50<- excepciones(residT12A50,12)
v12A100<- excepciones(residT12A100,12)
v12A200<- excepciones(residT12A200,12)
v12A500<- excepciones(residT12A500,12)

#residT12A20<- residT12A20[-v12A20]
# residT12A30<- residT12A30[-v12A30]
# residT12A40<- residT12A30[-v12A40]
# residT12A50<- residT12A30[-v12A50]
residT12A100<- residT12A100[-v12A100]


#erroresT12A20<- lapply(residT12A20,FUN=erroresSigA,k=12, fila=7,col=9)
# erroresT12A30<- lapply(residT12A30,FUN=erroresSig2,k=12,fila=7,col=9)
# erroresT12A40<- lapply(residT12A40,FUN=erroresSig2,k=12,fila=7,col=9)
# erroresT12A50<- lapply(residT12A50,FUN=erroresSig2,k=12,fila=7,col=9)
erroresT12A100<- lapply(residT12A100,FUN=erroresSig2,k=12,fila=7,col=9)
erroresT12A200<- lapply(residT12A200,FUN=erroresSig2,k=12,fila=7,col=9)
erroresT12A500<- lapply(residT12A500,FUN=erroresSig2,k=12,fila=7,col=9)

 # Porcentaje de errores totales
 
#perroresT12A20<- sapply(erroresT12A20,et)
# perroresT12A30<- sapply(erroresT12A30,et)
# perroresT12A40<- sapply(erroresT12A40,et)
# perroresT12A50<- sapply(erroresT12A50,et)
perroresT12A100<- sapply(erroresT12A100,et)
perroresT12A200<- sapply(erroresT12A200,et)
perroresT12A500<- sapply(erroresT12A500,et)

# Media errores totales

#mediaet12A20<- mean(perroresT12A20,na.rm=T)

# mediaET12A30<- mean(perroresT12A30,na.rm=T)
# mediaET12A40<- mean(perroresT12A40,na.rm=T)
# mediaET12A50<- mean(perroresT12A50,na.rm=T)
mediaET12A100<- mean(perroresT12A100,na.rm=T)
mediaET12A200<- mean(perroresT12A200,na.rm=T)
mediaET12A500<- mean(perroresT12A500,na.rm=T)

# dt errores totales

#dtET12A20<- sd(perroresT12A20,na.rm=T)
# dtET12A30<- sd(perroresT12A30,na.rm=T)
# dtET12A40<- sd(perroresT12A40,na.rm=T)
# dtET12A50<- sd(perroresT12A50,na.rm=T)
dtET12A100<- sd(perroresT12A100,na.rm=T)
dtET12A200<- sd(perroresT12A200,na.rm=T)
dtET12A500<- sd(perroresT12A500,na.rm=T)

mediaET12A<- c(NA,NA,NA,mediaET12A100,mediaET12A200,mediaET12A500)

dtET12A<- c(NA,NA,NA,dtET12A100,dtET12A200,dtET12A500)

resET12A<- data.frame(mediaET12A, dtET12A)
row.names(resET12A)<-c('30','40','50','100','200','500')



# Porcentaje de errores casilla significativa

#perroresCS12A20<- sapply(erroresT12A20,ecs)
# perroresCS12A30<- sapply(erroresT12A30,ecs)
# perroresCS12A40<- sapply(erroresT12A40,ecs)
# perroresCS12A50<- sapply(erroresT12A50,ecs)
perroresCS12A100<- sapply(erroresT12A100,ecs)
perroresCS12A200<- sapply(erroresT12A200,ecs)
perroresCS12A500<- sapply(erroresT12A500,ecs)

# Media errores casilla significativa

#mediaECS12A20<- mean(perroresCS12A20,na.rm=T)
# mediaECS12A30<- mean(perroresCS12A30,na.rm=T)
# mediaECS12A40<- mean(perroresCS12A40,na.rm=T)
# mediaECS12A50<- mean(perroresCS12A50,na.rm=T)
mediaECS12A100<- mean(perroresCS12A100,na.rm=T)
mediaECS12A200<- mean(perroresCS12A200,na.rm=T)
mediaECS12A500<- mean(perroresCS12A500,na.rm=T)

# dt errores casilla significativa

#dtECS12A20<- sd(perroresCS12A20,na.rm=T)
# dtECS12A30<- sd(perroresCS12A30,na.rm=T)
# dtECS12A40<- sd(perroresCS12A40,na.rm=T)
# dtECS12A50<- sd(perroresCS12A50,na.rm=T)
dtECS12A100<- sd(perroresCS12A100,na.rm=T)
dtECS12A200<- sd(perroresCS12A200,na.rm=T)
dtECS12A500<- sd(perroresCS12A500,na.rm=T)

mediaECS12A<- c(NA,NA,NA,
              mediaECS12A100,mediaECS12A200,mediaECS12A500)

dtECS12A<- c(NA,NA,NA,dtECS12A100,
            dtECS12A200,dtECS12A500)

resECS12A<- data.frame(mediaECS12A, dtECS12A)
row.names(resECS12A)<-c('30','40','50','100','200','500')



# Porcentaje de errores fila significativa

#perroresFS12A20<- sapply(erroresT12A20,efs)
# perroresFS12A30<- sapply(erroresT12A30,efs)
# perroresFS12A40<- sapply(erroresT12A40,efs)
# perroresFS12A50<- sapply(erroresT12A50,efs)
perroresFS12A100<- sapply(erroresT12A100,efs)
perroresFS12A200<- sapply(erroresT12A200,efs)
perroresFS12A500<- sapply(erroresT12A500,efs)

# Media errores fila significativa

#mediaEFS12A20<- mean(perroreFS12A20,na.rm=T)
# mediaEFS12A30<- mean(perroresFS12A30,na.rm=T)
# mediaEFS12A40<- mean(perroresFS12A40,na.rm=T)
# mediaEFS12A50<- mean(perroresFS12A50,na.rm=T)
mediaEFS12A100<- mean(perroresFS12A100,na.rm=T)
mediaEFS12A200<- mean(perroresFS12A200,na.rm=T)
mediaEFS12A500<- mean(perroresFS12A500,na.rm=T)

# dt errores fila significativa

#dtEFS12A20<- sd(perroresFS12A20,na.rm=T)
# dtEFS12A30<- sd(perroresFS12A30,na.rm=T)
# dtEFS12A40<- sd(perroresFS12A40,na.rm=T)
# dtEFS12A50<- sd(perroresFS12A50,na.rm=T)
dtEFS12A100<- sd(perroresFS12A100,na.rm=T)
dtEFS12A200<- sd(perroresFS12A200,na.rm=T)
dtEFS12A500<- sd(perroresFS12A500,na.rm=T)

mediaEFS12A<- c(NA,NA,NA,
               mediaEFS12A100,mediaEFS12A200,mediaEFS12A500)

dtEFS12A<- c(NA,NA,NA,dtEFS12A100,
            dtEFS12A200,dtEFS12A500)

resEFS12A<- data.frame(mediaEFS12A, dtEFS12A)
row.names(resET12A)<-c('30','40','50','100','200','500')


# Porcentaje de errores fila NO significativa

#perroreFNS12A20<- sapply(erroresT12A20,efns)
# perroresFNS12A30<- sapply(erroresT12A30,efns)
# perroresFNS12A40<- sapply(erroresT12A40,efns)
# perroresFNS12A50<- sapply(erroresT12A50,efns)
perroresFNS12A100<- sapply(erroresT12A100,efns)
perroresFNS12A200<- sapply(erroresT12A200,efns)
perroresFNS12A500<- sapply(erroresT12A500,efns)

# Media errores fila NO significativa

#mediaEFNS12A20<- mean(perroreFNS12A20,na.rm=T)
# mediaEFNS12A30<- mean(perroresFNS12A30,na.rm=T)
# mediaEFNS12A40<- mean(perroresFNS12A40,na.rm=T)
# mediaEFNS12A50<- mean(perroresFNS12A50,na.rm=T)
mediaEFNS12A100<- mean(perroresFNS12A100,na.rm=T)
mediaEFNS12A200<- mean(perroresFNS12A200,na.rm=T)
mediaEFNS12A500<- mean(perroresFNS12A500,na.rm=T)

# dt errores fila NO significativa

#dtEFNS12A20<- sd(perroresFNS12A20,na.rm=T)
# dtEFNS12A30<- sd(perroresFNS12A30,na.rm=T)
# dtEFNS12A40<- sd(perroresFNS12A40,na.rm=T)
# dtEFNS12A50<- sd(perroresFNS12A50,na.rm=T)
dtEFNS12A100<- sd(perroresFNS12A100,na.rm=T)
dtEFNS12A200<- sd(perroresFNS12A200,na.rm=T)
dtEFNS12A500<- sd(perroresFNS12A500,na.rm=T)

mediaEFNS12A<- c(NA,NA,NA,
               mediaEFNS12A100,mediaEFNS12A200,mediaEFNS12A500)

dtEFNS12A<- c(NA,NA,NA,dtEFNS12A100,dtEFNS12A200,
             dtEFNS12A500)

resEFNS12A<- data.frame(mediaEFNS12A, dtEFNS12A)
row.names(resET12A)<-c('30','40','50','100','200','500')

round(resET12A,2)
round(resECS12A,2)
round(resEFS12A,2)
round(resEFNS12A,2)


fin<- Sys.time()

fin-ini

