#final project stat 401
ev<-read.csv("EV_cars.csv",header = TRUE,sep = ",")
ev
MLR<-lm(ev$Price.DE.~ev$Efficiency+ev$Fast_charge+ev$Range+ev$Top_speed+
          ev$acceleration..0.100.+ev$Battery,data=ev)
summary(MLR)
plot(MLR)
trim<-ev[-c(330,148,68),]
trim
MLR2<-lm(trim$Price.DE.~trim$Efficiency+trim$Fast_charge+trim$Range+trim$Top_speed+
          trim$acceleration..0.100.+trim$Battery,data=trim)
summary(MLR2)
plot(MLR2)
MLR3<-lm(trim$Price.DE.~trim$Efficiency+trim$Fast_charge+trim$Top_speed+
           trim$acceleration..0.100.+trim$Battery,data=trim)
summary(MLR3)
plot(MLR3)
trim$batRange<-trim$Battery*trim$Range
MLR4<-lm(trim$Price.DE.~trim$Efficiency+trim$Fast_charge+trim$Range+trim$Top_speed+
           trim$acceleration..0.100.+trim$Battery+trim$batRange,data=trim)
summary(MLR4)
plot(MLR4)
MLR5<-lm(trim$Price.DE.~trim$Efficiency+trim$Fast_charge+trim$Range+trim$Top_speed+
           trim$Battery+trim$batRange,data=trim)
summary(MLR5)
plot(MLR5)
