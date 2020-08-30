#Examining whether injuries differ from passangers to drivers and between different vehicle sizes/types
car2dat<-read.csv("C:/Users/gbyel/Downloads/Car2.csv")
car2dat$logchest<-log10(car2dat$Chest)
car2dat$loghead<-log10(car2dat$Head)
car2dat$logleftleg<-log10(car2dat$LeftLeg)
car2dat$logrtleg<-log10(car2dat$RightLeg)
car2dat$logwt<-log10(car2dat$Wt)
#We are using the log values as the distriubtion is more normal with less skew, making it more ideal for statistical analysis
head(car2dat)
aggregate(loghead ~ DP + Size, data= car2dat, mean)
aggregate(loghead ~ DP + Size, data= car2dat, sd)
library(ggplot2)
boxplot <- ggplot(car2dat, aes(loghead, DP, Size))
boxplot + geom_boxplot() + facet_wrap(~DP)+facet_wrap(~Size) + labs(x = "log mean head criterion based on size and DP", y = "DP")
#INterquartile spread in heavy and van vehicle types much bigger than others[mid and smaller vehicles] which is interesting.
library(car)
leveneTest(car2dat$loghead, car2dat$DP, center = median)
leveneTest(car2dat$loghead, car2dat$Size, center = median)
leveneTest(car2dat$loghead, interaction(car2dat$DP,car2dat$Size), center = median)
#High p values for DP, size and interaction term, so homeogeneity assumption is satisfied we can proceed with 2 way ANOVA
#Factorial ANOVA 2 way
modloghead <- aov(loghead ~ DP + Size + DP:Size, data = car2dat)
summary(modloghead)
is.factor(car2dat$Size)
line <- ggplot(car2dat, aes(loghead,Size,DP, colour = DP))
line + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group= DP)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "log mean head criterion based on size and DP", y = "DP", colour = "DP") 
#There is significant interaction between different vehicle types, so it does not make sense to interpret the main effects.