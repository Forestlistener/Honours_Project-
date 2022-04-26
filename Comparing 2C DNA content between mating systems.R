#testing for normality of 2C DNA content
shapiro.test(flora$DNA2C)
#2C DNA content is not normally distributed
#testing whether variances in 2C DNA content are homogenous across mating system groups
library(car)
leveneTest(DNA2C~msystem, data=flora)
msystem<- as.factor()
flora$msystem <- as.factor(flora$msystem)
leveneTest(DNA2C~msystem, data=flora)
#variances between groups are homogenous
shapiro.test(log(flora$DNA2C))
#does not make variances normally distributed
#NOTE: try other transformations
kruskal.test(DNA2C~msystem, data=flora)                    
#p=0.1196>0.05, so medians are the same across groups
boxplot(DNA2C~msystem, data=flora, na.rm=TRUE)
boxplot(log(DNA2C)~msystem, data=flora, na.rm=TRUE)
mating_system = factor(flora$msystem, levels=c("generally cross", "mixed", "generally self"))
library(beeswarm)
beeswarm(DNA2C~mating_system, data=flora, log=TRUE, xlab="Mating System", ylab="2C DNA Content (pg)", method="swarm", corral="wrap", pch=10)
boxplot(DNA2C~mating_system, data=flora, add=TRUE, outline=FALSE, col="#00000022")
beeswarm(DNA2C~mating_system, data=flora, log=TRUE, xlab="Mating System", ylab="2C DNA Content (pg)", method="swarm", corral="wrap", pch=10)
