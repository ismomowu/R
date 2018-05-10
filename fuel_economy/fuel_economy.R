raw<-read.csv('~/Documents/fuel_economy/data.csv')

unique(raw$fuel)
x<-raw$Fuel.Type.ii
f<-c('nature gas','Diesel hybrid','Petrol Hybrid','Electricity','Diesel','Petrol')
raw$fuel<-gsub(f[1],1,raw$Fuel.Type.ii)
raw$fuel<-gsub(f[2],2,raw$fuel)
raw$fuel<-gsub(f[3],3,raw$fuel)
raw$fuel<-gsub(f[4],4,raw$fuel)
raw$fuel<-gsub(f[5],5,raw$fuel)
raw$fuel<-gsub(f[6],6,raw$fuel)

anova(lm(raw$CO2.g.km~factor(raw$engine.displacement)))
summary(aov(raw$CO2.g.km~factor(raw$engine.displacement)))
anova(lm(raw$CO2.g.km~factor(raw$fuel)))
summary(aov(raw$CO2.g.km~factor(raw$fuel)))
anova(lm(raw$CO2.g.km~factor(raw$fuel)*factor(raw$engine.displacement)))

anova(lm(raw$CO2.g.km~factor(raw$fuel)*factor(raw$engine.displacement)*factor(raw$year)*factor(raw$Metric.Combined)*factor(raw$Metric.Extra.urban)*factor(raw$Metric.Urban..cold.)*factor(raw$Imperial..cold.)*factor(raw$Imperial.Extra.urban)*factor(raw$Emissions.CO)*factor(raw$Emissions.HC.NOx)*factor(raw$Emissions.HC)*factor(raw$Emissions.NOx)*factor(raw$Emissions.Particulates)))

raw$MPG.Imperial.Combined<-as.numeric(raw$MPG.Imperial.Combined)
anova(lm(raw$MPG.Imperial.Combined~factor(raw$engine.displacement)))
anova(lm(raw$MPG.Imperial.Combined~factor(raw$fuel)))
anova(lm(raw$MPG.Imperial.Combined~factor(raw$fuel)*factor(raw$engine.displacement)))




Xr<-raw[,c(1,7,11,12,13,14,15,16,17,18,20,21,22,23,24,25,27)] #26
Xr$year<-as.numeric(Xr$year)
Xr$Engine.Capacity<-as.numeric(Xr$Engine.Capacity)
Xr$Metric.Urban..cold.<-as.numeric(Xr$Metric.Urban..cold.)
Xr$Metric.Extra.urban<-as.numeric(Xr$Metric.Extra.urban)
Xr$Metric.Combined<-as.numeric(Xr$Metric.Combined)
Xr$Imperial..cold.<-as.numeric(Xr$Imperial..cold.)
Xr$Imperial.Extra.urban<-as.numeric(Xr$Imperial.Extra.urban)
Xr$MPG.Imperial.Combined<-as.numeric(Xr$MPG.Imperial.Combined)
Xr$Fuel.Cost.6000.Miles<-as.numeric(Xr$Fuel.Cost.6000.Miles)
Xr$Noise.Level.dB.A.<-as.numeric(Xr$Noise.Level.dB.A.)
Xr$Emissions.CO<-as.numeric(Xr$Emissions.CO)
Xr$Emissions.HC.NOx<-as.numeric(Xr$Emissions.HC.NOx)
Xr$Emissions.HC<-as.numeric(Xr$Emissions.HC)
Xr$Emissions.NOx<-as.numeric(Xr$Emissions.NOx)
Xr$fuel<-as.numeric(Xr$fuel)

Y1<-as.numeric(raw$CO2.g.km)
Y2<-as.numeric(raw$MPG.Imperial.Combined)

cor(Xr,Y1) #CO2跟引擎大小及燃料種類最相關
cor(Xr,Y2) #MPG跟引擎大小及燃料種類最相關


Xreg<-raw[,c(1,7,11,12,13,14,15,16,17,18,20,21,22,23,24,25,26,27)] 
unique(raw$engine.displacement)
e<-c('<=1000','1001-1300','1301-1500','1501-2000','2001-2500','2501-3500','>3501') 
code<-c(1,2,3,4,5,6,7)
Xr$engine.displacement<-gsub(e[1],code[1],Xreg$engine.displacement)
Xr$engine.displacement<-gsub(e[2],code[2],Xr$engine.displacement)
Xr$engine.displacement<-gsub(e[3],code[3],Xr$engine.displacement)
Xr$engine.displacement<-gsub(e[4],code[4],Xr$engine.displacement)
Xr$engine.displacement<-gsub(e[5],code[5],Xr$engine.displacement)
Xr$engine.displacement<-gsub(e[6],code[6],Xr$engine.displacement)
Xr$engine.displacement<-gsub(e[7],code[7],Xr$engine.displacement)


Xr$engine.displacement<-as.numeric(Xr$engine.displacement)

reg1<- lm(Xr$CO2.g.km ~ Xr$year + Xr$Engine.Capacity + Xr$Metric.Urban..cold. +Xr$Metric.Extra.urban+Xr$Metric.Combined+Xr$Imperial..cold.+Xr$Imperial.Extra.urban+Xr$MPG.Imperial.Combined+Xr$Fuel.Cost.6000.Miles+Xr$Noise.Level.dB.A.+Xr$Emissions.CO+Xr$Emissions.HC.NOx+Xr$Emissions.HC+Xr$Emissions.NOx+Xr$Emissions.Particulates+Xr$engine.displacement+Xr$fuel, data = Xr)
summary(reg1)
reg2<- lm(Xr$CO2.g.km ~ Xr$Engine.Capacity + Xr$Metric.Urban..cold. +Xr$Metric.Extra.urban+Xr$Metric.Combined+Xr$Imperial..cold.+Xr$MPG.Imperial.Combined+Xr$Fuel.Cost.6000.Miles+Xr$Noise.Level.dB.A.+Xr$Emissions.CO+Xr$Emissions.HC.NOx+Xr$Emissions.HC+Xr$Emissions.NOx+Xr$Emissions.Particulates+Xr$engine.displacement+Xr$fuel, data = Xr)
summary(reg2)

reg3<- lm(Xr$MPG.Imperial.Combined ~ Xr$year + Xr$Engine.Capacity + Xr$Metric.Urban..cold. +Xr$Metric.Extra.urban+Xr$Metric.Combined+Xr$Imperial..cold.+Xr$Imperial.Extra.urban+Xr$CO2.g.km+Xr$Fuel.Cost.6000.Miles+Xr$Noise.Level.dB.A.+Xr$Emissions.CO+Xr$Emissions.HC.NOx+Xr$Emissions.HC+Xr$Emissions.NOx+Xr$Emissions.Particulates+Xr$engine.displacement+Xr$fuel, data = Xr)
summary(reg3)
reg4<- lm(Xr$MPG.Imperial.Combined ~ Xr$year  + Xr$Metric.Urban..cold. +Xr$Metric.Extra.urban+Xr$Imperial..cold.+Xr$Imperial.Extra.urban+Xr$CO2.g.km+Xr$Fuel.Cost.6000.Miles+Xr$Emissions.CO+Xr$Emissions.HC.NOx+Xr$Emissions.HC+Xr$Emissions.NOx+Xr$engine.displacement+Xr$fuel, data = Xr)
summary(reg4)
