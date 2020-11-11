# librerias

library(ez)
library(car)

# Datos a analizar

id18<- 1:18
id27<- 1:27
id12<- 1:12
datET30_50<- subset(grafET, long ==c('30','40','50'))
datos30_50<- complete.cases(datET30_50)
datET30_50<- datET30_50[datos30_50,]
datET30_50<- cbind(id18,datET30_50)

datET100_500<- subset(grafET, long ==c('100','200','500'))
datET100_500<- cbind(id27,datET100_500)

datOTE30_50<-subset(grafDat2, long ==c('30','40','50'))
datosOTE30_50<-complete.cases(datOTE30_50)
datOTE30_50<- datOTE30_50[datosOTE30_50,]
datOTE30_50<- cbind(id12,datOTE30_50)

datOTE100_500<- subset(grafDat2,long2 ==c('100','200','500') )
datOTE100_500<- cbind(id18,datOTE100_500)

# Análisis longitudes 30,40,50 ET


modET30_50ez<- ezANOVA(data=datET30_50,dv =ET,
                      wid= id18,
                      between = .(codes, condicion))
modET30_50ez

mod130_50<-lm(ET~codes*condicion,data=dat30_50)
summary(mod130_50)
#plot(mod130_50)
Anova(mod130_50)
mod1Means<- interactionMeans(mod130_50)
g1<- plot(mod1Means)
testInteractions(mod130_50,
                 pairwise='condicion',
                 across='codes')

mod230_50<- lm(ET~codes*long,data=dat30_50)
Anova(mod230_50)
mod2Means<- interactionMeans(mod230_50)
g2<- plot(mod2Means)
             
mod330_50<- lm(ET~condicion*long,data=dat30_50)
Anova(mod330_50)
mod3Means<- interactionMeans(mod330_50)
plot(mod3Means)
testInteractions(mod330_50,
                 fixed='long',
                 across='condicion')

# Análisis longitudes 100,200,500 ET

dat100_500<- subset(grafET, 
                  long ==c('100','200','500'))
datos<- complete.cases(dat100_500)
datET100_500<- dat100_500[datos,]



mod1100_500ez<- ezANOVA(data=datET100_500,dv =ET,
                      wid= id27,
                      between = .(codes, condicion))
mod1100_500ez

mod1100_500<-lm(ET~codes*condicion,data=dat100_500)
summary(mod1100_500)
#plot(mod1100_500)
Anova(mod1100_500)
mod1Means<- interactionMeans(mod1100_500)
g3<-plot(mod1Means)
testInteractions(mod1100_500,
                 pairwise='codes',
                 across='condicion')

mod2100_500<- lm(ET~codes*long,data=dat100_500)
Anova(mod2100_500)
mod2Means<- interactionMeans(mod2100_500)
plot(mod2Means)

mod3100_500<- lm(ET~condicion*long,data=dat100_500)
Anova(mod3100_500)
mod3Means<- interactionMeans(mod3100_500)
g4<- plot(mod3Means)
testInteractions(mod3100_500,
                 fixed='long',
                 across='condicion')

# Análisis casilla significativa 30,40, 50

datOTE30_50<- subset(grafDat2, 
                    long2 ==c('30','40','50'))
datos2<- complete.cases(datOTE30_50)
datOTE30_50<- datOTE30_50[datos2,]


modCS30_50ez<- ezANOVA(data=datOTE30_50,
                  dv = ECS,
                  wid= id12,
                between = .(codes2, condicion2))
modCS30_50ez



mod1CS30_50<-lm(ECS~codes2*condicion2,
                data=datOTE30_50)
#summary(mod1CS30_50)
#plot(modCS130_50)
Anova(mod1CS30_50)
mod1CSMeans<- interactionMeans(mod1CS30_50)
g5<- plot(mod1CSMeans)
testInteractions(mod1CS30_50,
                 pairwise='condicion2',
                 across='codes2')

mod2CS30_50<- lm(ECS~codes2*long2,data=datOTE30_50)
Anova(mod2CS30_50)
mod2CSMeans<- interactionMeans(mod2CS30_50)
g6<- plot(mod2CSMeans)
testInteractions(mod2CS30_50,
                 adjustment='none',
                 fixed='long2',
                 across='codes2')

mod3CS30_50<- lm(ECS~condicion2*long2,
                 data=datOTE30_50)
Anova(mod3CS30_50)
mod3CSMeans<- interactionMeans(mod3CS30_50)
g7<- plot(mod3CSMeans)
testInteractions(mod3CS30_50,
                 fixed='long2',
                 across='condicion2')


# Análisis filas significativas 30,40, 50

modEFS_50ez<- ezANOVA(data=datOTE30_50,dv =EFS,
                       wid= id12,
                       between = .(codes2,condicion2))
modEFS_50ez

mod1FS30_50<-lm(EFS~codes2*condicion2,
                data=datOTE30_50)
#summary(mod1FS30_50)
#plot(modFS130_50)
Anova(mod1FS30_50)
mod1FSMeans<- interactionMeans(mod1FS30_50)
g8<- plot(mod1FSMeans)
testInteractions(mod1FS30_50,
                 pairwise='condicion2',
                 across='codes2')

mod2FS30_50<- lm(EFS~codes2*long2,data=datOTE30_50)
Anova(mod2FS30_50)
mod2FSMeans<- interactionMeans(mod2FS30_50)
g9<- plot(mod2FSMeans)
testInteractions(mod2FS30_50,
                 adjustment='none',
                 fixed='long2',
                 across='codes2')

testInteractions(mod2FS30_50,
                 adjustment='none',
                 fixed='codes2',
                 across='long2')

mod3FS30_50<- lm(EFS~condicion2*long2,
                 data=datOTE30_50)
Anova(mod3FS30_50)
mod3FSMeans<- interactionMeans(mod3FS30_50)
g10<- plot(mod3FSMeans)
testInteractions(mod3FS30_50,
                 fixed='long2',
                 across='condicion2')

testInteractions(mod3FS30_50,
                 fixed='condicion2',
                 across='long2')

# Análisis filas NO significativas 30,40, 50

datOTE30_50<- subset(grafDat2, 
                     long2 ==c('30','40','50'))
datos2<- complete.cases(datOTE30_50)
datOTE30_50<- datOTE30_50[datos2,]

mod1FNS30_50<-lm(EFNS~codes2*condicion2,
                data=datOTE230_50)
#summary(mod1FNS30_50)
#plot(modFNS130_50)
Anova(mod1FNS30_50)
mod1FNSMeans<- interactionMeans(mod1FNS30_50)
g11<- plot(mod1FNSMeans)
testInteractions(mod1FNS30_50,
                 pairwise='condicion2',
                 across='codes2')

mod2FNS30_50<- lm(EFNS~codes2*long2,data=datOTE30_50)
Anova(mod2FNS30_50)
mod2FNSMeans<- interactionMeans(mod2FNS30_50)
g12<- plot(mod2FNSMeans)
testInteractions(mod2FNS30_50,
                 adjustment='none',
                 fixed='long2',
                 across='codes2')

testInteractions(mod2FNS30_50,
                 adjustment='none',
                 fixed='codes2',
                 across='long2')

mod3FNS30_50<- lm(EFNS~condicion2*long2,
                 data=datOTE30_50)
Anova(mod3FNS30_50)
mod3FNSMeans<- interactionMeans(mod3FNS30_50)
g13<- plot(mod3FNSMeans)
testInteractions(mod3FNS30_50,
                 fixed='long2',
                 across='condicion2')

testInteractions(mod3FNS30_50,
                 fixed='condicion2',
                 across='long2')

##########################################

# Análisis casilla significativa 100,200, 500

modEFS100_500ez<- ezANOVA(data=datOTE100_500,dv =EFS,
                      wid= id18,
                      between = .(codes2,condicion2))
modEFS100_500ez



mod1CS100_500<-lm(ECS~codes2*condicion2,
                data=datOTE100_500)
#summary(mod1FS100_500)
#plot(modFS1100_500)
Anova(mod1CS100_500)
mod1CSMeans<- interactionMeans(mod1CS100_500)
g14<- plot(mod1CSMeans)
testInteractions(mod1CS100_500,
                 fixed='condicion2',
                 across='codes2')

testInteractions(mod1CS100_500,
                 fixed='codes2',
                 across='condicion2')

mod2CS100_500<- lm(ECS~codes2*long2,data=datOTE100_500)
Anova(mod2CS100_500)
mod2FSMeans<- interactionMeans(mod2CS100_500)
g15<- plot(mod2CSMeans)
testInteractions(mod2CS100_500,
                 adjustment='none',
                 fixed='long2',
                 across='codes2')

testInteractions(mod2CS100_500,
                 adjustment='none',
                 fixed='codes2',
                 across='long2')

mod3CS100_500<- lm(ECS~condicion2*long2,
                 data=datOTE100_500)
Anova(mod3CS100_500)
mod3CSMeans<- interactionMeans(mod3CS100_500)
g16<- plot(mod3CSMeans)
testInteractions(mod3CS100_500,
                 fixed='long2',
                 across='condicion2')

testInteractions(mod3CS100_500,
                 fixed='condicion2',
                 across='long2')

# Análisis filas significativas 100,200, 500

mod1FS100_500<-lm(EFS~codes2*condicion2,
                   data=datOTE100_500)
#summary(mod1FS100_500)
#plot(modFS1100_500)
Anova(mod1FS100_500)
mod1FSMeans<- interactionMeans(mod1FS100_500)
g17<- plot(mod1FSMeans)
testInteractions(mod1FS100_500,
                 fixed='condicion2',
                 across='codes2')

mod2FS100_500<- lm(EFS~codes2*long2,data=datOTE100_500)
Anova(mod2FS100_500)
mod2FSMeans<- interactionMeans(mod2FS100_500)
g18<- plot(mod2FSMeans)
testInteractions(mod2FS100_500,
                 adjustment='none',
                 fixed='long2',
                 across='codes2')

testInteractions(mod2FS100_500,
                 adjustment='none',
                 fixed='codes2',
                 across='long2')

mod3FS100_500<- lm(EFS~condicion2*long2,
                    data=datOTE100_500)
Anova(mod3FS100_500)
mod3FSMeans<- interactionMeans(mod3FS100_500)
g19<- plot(mod3FSMeans)
testInteractions(mod3FS100_500,
                 fixed='long2',
                 across='condicion2')

testInteractions(mod3FS100_500,
                 fixed='condicion2',
                 across='long2')


# Análisis filas NO significativas 30,40, 50

modEFNS30_50ez<- ezANOVA(data=datOTE30_50,dv =EFNS,
                      wid= id12,
                      between = .(codes2,condicion2))
modEFNS30_50ez



mod1FNS30_50<-lm(EFNS~codes2*condicion2,
                 data=datOTE30_50)
#summary(mod1FNS100_500)
#plot(modFNS1100_500)
Anova(mod1FNS30_50)
mod1FNSMeans<- interactionMeans(mod1FNS100_500)
g20<- plot(mod1FNSMeans)
testInteractions(mod1FNS100_500,
                 pairwise='condicion2',
                 across='codes2')

modEFNS100_500ez<- ezANOVA(data=datOTE100_500,
                    dv =EFNS,
                    wid= id18,
                    between = .(codes2,condicion2))
modEFNS100_500ez


mod2FNS100_500<- lm(EFNS~codes2*long2,data=datOTE100_500)
Anova(mod2FNS100_500)
mod2FNSMeans<- interactionMeans(mod2FNS100_500)
g21<- plot(mod2FNSMeans)
testInteractions(mod2FNS100_500,
                 fixed='long2',
                 across='codes2')

testInteractions(mod2FNS100_500,
                 fixed='codes2',
                 across='long2')

mod3FNS100_500<- lm(EFNS~condicion2*long2,
                  data=datOTE100_500)
Anova(mod3FNS100_500)
mod3FNSMeans<- interactionMeans(mod3FNS100_500)
g22<- plot(mod3FNSMeans)
testInteractions(mod3FNS100_500,
                 fixed='long2',
                 across='condicion2')

testInteractions(mod3FNS100_500,
                 fixed='condicion2',
                 across='long2')
