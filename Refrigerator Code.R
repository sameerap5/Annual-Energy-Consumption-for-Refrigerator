setwd("D:/USF/SDM/module 10")
r<- read.csv("Refrigerator data_cleaned signature.csv")
ws<-read.csv("Wine Chiller signature.csv")
install.packages("lme4")
library("lme4")
#Simple OLS for wine chiller
Sm4<-lm(sqrt(AEC) ~ as.factor(Defrost) +RefVol+as.factor(Defrost)*RefVol, data = ws)
summary(Sm4) # R2: 32.62
# lmer(random effect of brands) for wine chillers
Sm5m <- lmer(sqrt(AEC) ~ as.factor(Brand)+ as.factor(Defrost)+as.factor(Defrost)*RefVol+
               RefVol+(1|Brand), data = ws)
summary(Sm5m)
AIC(Sm4);AIC(Sm5m) #1077,896


####################
r$FreezerLoc <- relevel(r$FreezerLoc,"No freezer")
# simple OLS
Sm2<-lm(AEC ~ as.factor(Type) + as.factor(Defrost) + as.factor(FreezerLoc)
        + Ratio , data = r)
summary(Sm2) # R2: 71.53
# lmer without interaction
Sm3m <- lmer(AEC ~ as.factor(Brand)+as.factor(Type) + as.factor(Defrost)
             + as.factor(FreezerLoc)+ Ratio +(1|Brand), data = r)
summary(Sm3m)
# lmer with interaction
im2 <- lmer(AEC ~ as.factor(Brand)+as.factor(Type) + as.factor(Defrost)
            + as.factor(FreezerLoc)+ Ratio+ r$FreezerVol*as.factor(Defrost) +as.factor(DoorIce)
            +(1|Brand), data = r)
summary(im2)
AIC(Sm2);AIC(Sm3m);AIC(im2) #14171,13127,12700
#----------------------------------------------------------------------------------------------

