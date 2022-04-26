#check for normality of change index data
shapiro.test(flora$change_index)
shapiro.test(log(flora$change_index))
#non-normal, log transformation not applicable as data is positive and negative
#are variances homogenous between ms groups?
leveneTest(change_index~msystem, data=flora)
#p>0.05 so variances homogenous
#KW test
kruskal.test(change_index~msystem, data=clean_flora)
#how many spp have both change index and ms data?
paired_flora <- subset(flora, change_index[!(is.na(flora$change_index))])
change_flora <- flora[!(is.na(flora$change_index)), ]
cleaned_flora <- change_flora[!(is.na(change_flora$msystem)), ]
view(cleaned_flora)
length(cleaned_flora$species)
#915 entries with both change index and mating system information
kruskal.test(change_index~msystem, data=flora)
shapiro.test(clean_flora$change_index)
#summary of means and sds
library(dplyr)
flora %>%
  summarise(mean=mean((change_index), na.rm=TRUE),
sd=sd((change_index), na.rm=TRUE))
boxplot(change_index ~ msystem, data=flora)
beeswarm(change_index~msystem, data=flora, xlab="Mating System", ylab="Change Index", method="swarm", corral="wrap", pch=20, cex=0.3, cex.axis=0.4)
boxplot(change_index~mating_system, add=TRUE, outline=FALSE, col="#00000022", axes=FALSE, cex=0.1)
#exclude any outliers?
flora$GBM_change <- flora$GBM_hectdads_2010_19-flora$GBM_hectads_1987_99
view(flora)
flora$total_hectads_1987_99 <- flora$GBM_hectads_1987_99+flora$CI_hectads_1987_99+flora$Ire_hectads_1987_99
flora$total_hectads_2010_19 <- flora$GBM_hectdads_2010_19+flora$CI_hectads_2010_19+flora$Ire_hectads_2010_2019
flora$total_hectad_change <- flora$total_hectads_2010_19-flora$total_hectads_1987_99
flora$CI_change <- flora$CI_hectads_2010_19-flora$CI_hectads_1987_99
flora$Ire_change <- flora$Ire_hectads_2010_2019-flora$Ire_hectads_1987_99
shapiro.test(flora$total_hectad_change)
#non-normal so KW
kruskal.test(total_hectad_change~msystem, data=flora)
#p=0.282, chi-squared = 2.5315, df=2, no diff between medians
#plotting anyway
beeswarm(total_hectad_change~msystem, data=flora, xlab="Change in Hectad Records between 1987-99 and 2010-2019", ylab="Change Index", method="swarm", corral="wrap", pch=20, cex=0.2, cex.axis=0.5)
boxplot(total_hectad_change~msystem, data=flora, add=TRUE, outline=FALSE, col="#00000022", axes=FALSE, cex=0.1)
shapiro.test(flora$GBM_change)
shapiro.test(flora$Ire_change)
#do metrics of range change differ with mating system in natives vs non-natives?
#Including archaeophytes within natives
natives <- subset(flora, nativity_stace==c("N","Arch-colonist","Arch-denizen","Arch-cultd"))
non_natives <- subset(flora, nativity_stace ==c("Neo-casual","Neo-natd","Neo-surv"))
view(natives)           
natives_clean <- natives[!(is.na(natives$msystem)), ]
view(natives_clean)
view(non_natives)
non_natives_clean <-non_natives[!(is.na(non_natives$msystem)), ]
shapiro.test(natives_clean$change_index)                     
shapiro.test(natives_clean$total_hectad_change)
shapiro.test(non_natives_clean$change_index)
#normally distributed
shapiro.test(non_natives_clean$total_hectad_change)
leveneTest(change_index~msystem, data=natives_clean)
leveneTest(change_index~msystem, data=non_natives_clean)
leveneTest(total_hectad_change~msystem, data=natives_clean)
leveneTest(total_hectad_change~msystem, data=non_natives_clean)
#total_hectad change variance not homogenous between ms in natives and non-natives
kruskal.test(change_index~msystem, data=natives_clean)
#p=0.2439, chi-squared=2.8222, df=2, no diff between medians
kruskal.test(change_index~msystem, data=non_natives_clean)
#p=0.2071, chi-squared=2.6182, df=2, no sig diff between medians
kruskal.test(total_hectad_change~msystem, data=natives_clean)
#p=0.7874, chi-squared=0.47804, df=2, no sig diff
kruskal.test(total_hectad_change~msystem, data=non_natives_clean)
#p=0.04109, chi-squared=6.384, df=2; sig diff between medians-but variances not homogenous-problem?
#plot them
par(mfrow=c(1,2))
beeswarm(total_hectad_change~msystem, data=natives, xlab="Change in Hectad Records between 1987-99 and 2010-2019", ylab="Hectads", method="swarm", corral="wrap", pch=20, cex=0.2, cex.axis=0.5)+theme(axis.text.x=element_text(angle=90,face="italic", size=5))
library(ggbeeswarm)
plot1<- ggplot(natives_clean, aes(x=msystem, y=total_hectad_change))+geom_boxplot(cex=0.1)+ 
  xlab("Mating System (Natives)")+
  ylab("Change in Number of BI Hectads with Records")+
  geom_beeswarm(color="black", size=0.4)+
  expand_limits(y=c(-500, 1000))+
  theme(axis.title.y=element_text(size=7))+
  theme(axis.title.x=element_text(size=7))+
  theme(axis.text.x=element_text(angle=90, face="italic", size=5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background= element_blank(),
        axis.line= element_line(colour="black"),
        legend.text=element_text(face="italic"))
plot1
plot2<- ggplot(non_natives_clean, aes(x=msystem, y=total_hectad_change))+geom_boxplot(cex=0.1)+ 
  xlab("Mating System (Non-natives)")+
  ylab("Change in Number of BI Hectads with Records")+
  geom_beeswarm(color="black", size=0.4)+
  expand_limits(y=c(-500, 1000))+
  theme(axis.title.y=element_text(size=7))+
  theme(axis.title.x=element_text(size=7))+
  theme(axis.text.x=element_text(angle=90, face="italic", size=5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background= element_blank(),
        axis.line= element_line(colour="black"),
        legend.text=element_text(face="italic"))
plot2
#attempting to align plots
require(gridExtra)
grid.arrange(plot1, plot2, ncol=2)
library(cowplot)
aligned <- align_plots(plot1, plot2, align="h")
al_plot1<- ggdraw(aligned[[1]])
al_plot2<- ggdraw(aligned[[2]])
grid.arrange(al_plot1, al_plot2, ncol=2)
plot_grid(plot1, plot2, ncol=2, align="h")
save(natives_clean, file="natives_clean.Rda")
save(non_natives_clean, file="non_natives_clean.Rda")
