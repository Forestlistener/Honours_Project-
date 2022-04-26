library(dplyr)
families <- table(flora$family)  
view(families)     
families <- as.data.frame(families)
colnames(families)<- c("Family", "Species_no")
#making a subset of families that contain over 20 spp
large_families <- subset(families, Species_no > 20)
view(large_families)
length(large_families$Family)
#35 families contain over 20 species
#Amaranthaceae
amaranth_species <- flora$species[which(flora$family=="Amaranthaceae")]
amaranth_genus <- flora$genus[which(flora$family=="Amaranthaceae")]
amaranth_ms <- flora$msystem[which(flora$family=="Amaranthaceae")]
amaranth_2C <-flora$DNA2C[which(flora$family=="Amaranthaceae")]
Amaranthaceae <-data.frame (amaranth_species, amaranth_genus, amaranth_ms, amaranth_2C)
view(Amaranthaceae)
table(Amaranthaceae$amaranth_ms)
#Analysis
shapiro.test(Amaranthaceae$amaranth_2C)
#p<0.05 so non-normal
shapiro.test(log(Amaranthaceae$amaranth_2C))
#p=0.0817>0.05 therefore normal
Amaranth_anova <- aov(log(amaranth_2C) ~ amaranth_ms, data=Amaranthaceae)
summary(Amaranth_anova)
#p=0.0503>0.05 therefore no sig diff between means-but almost significant
# F=4.093, DF=12

#Amaryllidaceae
amaryll_species <- flora$species[which(flora$family=="Amaryllidaceae")]
amaryll_genus <- flora$genus[which(flora$family=="Amaryllidaceae")]
amaryll_ms <- flora$msystem[which(flora$family=="Amaryllidaceae")]
amaryll_2C <- flora$DNA2C[which(flora$family=="Amaryllidaceae")]
Amaryllidaceae <- data.frame(amaryll_species, amaryll_genus,amaryll_ms, amaryll_2C)
#Analysis
shapiro.test(Amaryllidaceae$amaryll_2C)
#p=0.06586>0.05 therefore normally distributed
Amaryll_anova <- aov(amaryll_2C~amaryll_ms, data=Amaryllidaceae)
summary(Amaryll_anova)
view(Amaryllidaceae)
#p>0.05 therefore no sig diff between means
#F=2.784, DF= 8
#Apiaceae
ap_species <- flora$species[which(flora$family=="Apiaceae")]
ap_genus <- flora$genus[which(flora$family=="Apiaceae")]
ap_ms <- flora$msystem[which(flora$family=="Apiaceae")]
ap_2c <- flora$DNA2C[which(flora$family=="Apiaceae")]
Apiaceae <- data.frame(ap_species, ap_genus, ap_ms, ap_2c)
#Analysis
shapiro.test(Apiaceae$ap_2c)
#p<0.05 therefore non-normal
shapiro.test (log(Apiaceae$ap_2c))
#p=0.09465>0.05 therefore normal
ap_anova <- aov(log(ap_2c) ~ ap_ms, data=Apiaceae)
summary(ap_anova)
#p>0.05 so no sig diff between means
#F=0.467, DF= 25

#Asparagaceae
asp_species <- flora$species[which(flora$family=="Asparagaceae")]
asp_genus <- flora$genus[which(flora$family=="Asparagaceae")]
asp_ms <- flora$msystem[which(flora$family=="Asparagaceae")]
asp_2c <- flora$DNA2C[which(flora$family=="Asparagaceae")]
Asparagaceae <- data.frame(asp_species, asp_genus,asp_ms, asp_2c)
shapiro.test(Asparagaceae$asp_2c)
#p=0.01017>0.05 so normal
asp_anova <- aov(asp_2c~asp_ms, data=Asparagaceae)
summary(asp_anova)
#p=0.0468<0.05 therefore significant difference between means
#p=0.0468, F=5.804, DF= 8 (23 samples NA)

#Aspleniaceae
aspl_species <- flora$species[which(flora$family=="Aspleniaceae")]
aspl_genus<- flora$genus[which(flora$family=="Aspleniaceae")]
aspl_ms <- flora$msystem[which(flora$family=="Aspleniaceae")]
aspl_2c <- flora$DNA2C[which(flora$family=="Aspleniaceae")]
Aspleniaceae <- data.frame(aspl_species, aspl_genus, aspl_ms, aspl_2c)
shapiro.test(Aspleniaceae$aspl_2c)
#p<0.05 therefore non-normal
shapiro.test(log(Aspleniaceae$aspl_2c))
#p>0.05 therefore normal
aspl_anova <- aov(log(aspl_2c)~ aspl_ms, data=Aspleniaceae)
view(Aspleniaceae)
#no mating system data for Aspleniaceae

#Asteraceae - rename Compositae
ast_species <- flora$species [which(flora$family=="Asteraceae")]
ast_genus <- flora$genus[which(flora$family=="Asteraceae")]
ast_ms <- flora$msystem[which(flora$family=="Asteraceae")]
ast_2c <- flora$DNA2C[which(flora$family=="Asteraceae")]
Asteraceae <- data.frame(ast_species, ast_genus, ast_ms, ast_2c)
shapiro.test(Asteraceae$ast_2c)
#p<0.05 therefore non-normal
shapiro.test(log(Asteraceae$ast_2c))
#p=0.04673<0.05 therefore non-normal
#non-parametric KW test
#check for homogeneity of variance
leveneTest(ast_2c~ast_ms, data=Asteraceae)
#p=0.7077>0.05 so variances are homogenous
kruskal.test(ast_2c~ast_ms, data=Asteraceae)
#p=0.7896>0.05 so median gs not sig different.
#chi-squared =0.47234, df=2

#Boraginaceae
bor_sp <- flora$species[which(flora$family=="Boraginaceae")]
bor_genus <- flora$genus[which(flora$family=="Boraginaceae")]
bor_ms <- flora$msystem[which(flora$family=="Boraginaceae")]
bor_2c <- flora$DNA2C[which(flora$family=="Boraginaceae")]
Boraginaceae <- data.frame(bor_sp, bor_genus, bor_ms, bor_2c)
shapiro.test(Boraginaceae$bor_2c)
#p<0.05 so non-normal
shapiro.test(log(Boraginaceae$bor_2c))
#p=0.6286>0.05 therefore normal
bor_anova <- aov(bor_2c~bor_ms, data=Boraginaceae)
summary(bor_anova)
#p=0.723>0.05 therefore no sig diff between means. F=0.334, DF=13

#Brassicaceae
br_sp<- flora$species[which(flora$family=="Brassicaceae")]
br_genus <- flora$genus[which(flora$family=="Brassicaceae")]
br_ms <- flora$msystem[which(flora$family=="Brassicaceae")]
br_2c<- flora$DNA2C[which(flora$family=="Brassicaceae")]
Brassicaceae <- data.frame(br_sp, br_genus, br_ms, br_2c)
#analysis

shapiro.test(Brassicaceae$br_2c)
#p=0.05 therefore non-normal
shapiro.test(log(Brassicaceae$br_2c))
#p<0.05 therefore non-normal
#test for homogeneity of variances
leveneTest(br_2c~br_ms, data=Brassicaceae)
#p>0.05 so varaiances are homogenous
kruskal.test(br_2c~br_ms, data=Brassicaceae)
#p=0.06492, chi-squared=5.4962, df=2, medians not sig diff

#Campanulaceae
cam_sp <- flora$species[which(flora$family=="Campanulaceae")]
cam_genus <- flora$genus[which(flora$family=="Campanulaceae")]
cam_ms <- flora$msystem[which(flora$family=="Campanulaceae")]
cam_2c <- flora$DNA2C[which(flora$family=="Campanulaceae")]
Campanulaceae <- data.frame (cam_sp, cam_genus, cam_ms, cam_2c)
#Analysis
shapiro.test(Campanulaceae$cam_2c)
#p<0.05 therefore non-normal
shapiro.test(log(Campanulaceae$cam_2c))
#p=0.2582>0.05 therefore normal
cam_anova <- aov(cam_2c~cam_ms, data=Campanulaceae)
summary(cam_anova)
#p=0.889, F=0.119, DF=10 so no sig diff between means

#Caprifoliaceae
cap_sp <- flora$species[which(flora$family=="Caprifoliaceae")]
cap_genus <- flora$genus[which(flora$family=="Caprifoliaceae")]
cap_ms <- flora$msystem[which(flora$family=="Caprifoliaceae")]
cap_2c <- flora$DNA2C[which(flora$family=="Caprifoliaceae")]
Caprifoliaceae <- data.frame(cap_sp, cap_genus, cap_ms, cap_2c)
#Analysis
shapiro.test(Caprifoliaceae$cap_2c)
#p<0.05 so non-normal
shapiro.test(log(Caprifoliaceae$cap_2c))
#p=0.3912>0.05 so normal
cap_anova <- aov(cap_2c~cap_ms, data = Caprifoliaceae)
summary(cap_anova)
#p=0.0695, F=3.358, DF= 14 so no sig diff between means

#Caryophyllaceae
cary_sp <- flora$species[which(flora$family=="Caryophyllaceae")]
cary_genus <- flora$genus[which(flora$family=="Caryophyllaceae")]
cary_ms <- flora$msystem[which(flora$family=="Caryophyllaceae")]
cary_2c <- flora$DNA2C[which(flora$family=="Caryophyllaceae")]
Caryophyllaceae <- data.frame(cary_sp, cary_genus, cary_ms, cary_2c)
#Analysis
shapiro.test(Caryophyllaceae$cary_2c)
#p<0.05 so non-normal
shapiro.test(log(Caryophyllaceae$cary_2c))
#p=0.3714>0.05 so normal
cary_anova <- aov(cary_2c ~ cary_ms, data=Caryophyllaceae)
summary(cary_anova)
#p=0.00148**, F=7.644, DF= 44 so significant difference between means

#Cyperaceae
cy_sp <- flora$species[which(flora$family=="Cyperaceae")]
cy_genus <- flora$genus[which(flora$family=="Cyperaceae")]
cy_ms <- flora$msystem[which(flora$family=="Cyperaceae")]
cy_2c <- flora$DNA2C[which(flora$family=="Cyperaceae")]
Cyperaceae <- data.frame(cy_sp, cy_genus, cy_ms, cy_2c)
#Analysis
shapiro.test(Cyperaceae$cy_2c)
#p<0.05 so non-normal
shapiro.test(log(Cyperaceae$cy_2c))
#p<0.05 so non-normal, np test
#test for homogeneity of variances
leveneTest(cy_2c~cy_ms, data=Cyperaceae)
#p>0.05 so variances are homogenous
kruskal.test(cy_2c~cy_ms, data=Cyperaceae)
#p=0.09059, chi-squared=2.8639, df=1 so no sig diff between medians
view(Cyperaceae)
#all Cyperaceae generally cross except C.fuscus which has mixed ms-worth including?

#Ericaceae
er_sp <- flora$species[which(flora$family=="Ericaceae")]
er_genus <- flora$genus [which(flora$family=="Ericaceae")]
er_ms <- flora$msystem[which(flora$family=="Ericaceae")]
er_2c <- flora$DNA2C[which(flora$family=="Ericaceae")]
Ericaceae <- data.frame(er_sp, er_genus, er_ms, er_2c)
view(Ericaceae)
#Analysis
shapiro.test(Ericaceae$er_2c)
#p<0.05 so non-normal
shapiro.test(log(Ericaceae$er_2c))
#p<0.05 so still non-normal, np test
#test for homogeneity of variances
leveneTest(er_2c~er_ms, data=Ericaceae)
#p>0.05 so variances homogenous
kruskal.test(er_2c~er_ms, data=Ericaceae)
#p=0.4548, chi-squared=1.5758, df=2; no sig diff between medians

#Euphorbiaceae
eu_sp <- flora$species[which(flora$family=="Euphorbiaceae")]
eu_genus <- flora$genus[which(flora$family=="Euphorbiaceae")]
eu_ms <- flora$msystem[which(flora$family=="Euphorbiaceae")]
eu_2c <- flora$DNA2C[which(flora$family=="Euphorbiaceae")]
Euphorbiaceae <- data.frame(eu_sp, eu_genus, eu_ms, eu_2c)
#Analysis
shapiro.test(Euphorbiaceae$eu_2c)
#p<0.05 so non-normal
shapiro.test(log(Euphorbiaceae$eu_2c))
#p>0.05 so normal
eu_anova <- aov(log(eu_2c)~eu_ms, data=Euphorbiaceae)
summary(eu_anova)
#p=0.462, F=0.596, DF=9, no sig diff between means

#Fabaceae
f_sp <- flora$species[which(flora$family=="Fabaceae")]
f_genus <- flora$genus[which(flora$family=="Fabaceae")]
f_ms <- flora$msystem[which(flora$family=="Fabaceae")]
f_2c <- flora$DNA2C[which(flora$family=="Fabaceae")]
Fabaceae<- data.frame(f_sp, f_genus, f_ms, f_2c)
#Analysis
shapiro.test(Fabaceae$f_2c)
#p<0.05 so non-normal
shapiro.test(log(Fabaceae$f_2c))
#p<0.05 so non-normal, np test
#test homogeneity of variances
leveneTest(f_2c~f_ms, data=Fabaceae)
#p>0.05 so homogenous
kruskal.test(f_2c~f_ms, data=Fabaceae)
#p>0.05, chi-squared=1.9531 so no sig diff between medians

#Geraniaceae
g_sp <- flora$species[which(flora$family=="Geraniaceae")]
g_genus <- flora$genus[which(flora$family=="Geraniaceae")]
g_ms <- flora$msystem[which(flora$family=="Geraniaceae")]
g_2c <- flora$DNA2C[which(flora$family=="Geraniaceae")]
Geraniaceae <- data.frame(g_sp, g_genus, g_ms, g_2c)
#Analysis
shapiro.test(Geraniaceae$g_2c)
#p<0.05 so non-normal
shapiro.test(log(Geraniaceae$g_2c))
#p>0.05 so normal
g_anova <- aov(log(g_2c)~g_ms, data= Geraniaceae)
summary(g_anova)
#p=0.118, F=2.609, DF=13, no sig diff between means

#Iridaceae
Irid_sp <- flora$species[which(flora$family=="Iridaceae")]
Irid_genus <-flora$genus[which(flora$family=="Iridaceae")]
Irid_ms <- flora$msystem[which(flora$family=="Iridaceae")]
Irid_2c <- flora$DNA2C[which(flora$family=="Iridaceae")]
Iridaceae <- data.frame(Irid_sp, Irid_genus, Irid_ms, Irid_2c)
#Analysis
shapiro.test(Iridaceae$Irid_2c)
#p<0.05 so non-normal
shapiro.test(log(Iridaceae$Irid_2c))
#p>0.05 so normal
Irid_anova <- aov(log(Irid_2c)~Irid_ms, data=Iridaceae)
summary(Irid_anova)
view(Iridaceae)
#ms data for only 3sp, 2 mixed 1 generally cross, worth including?
#p=2.53e-15, F6.534e+28, df=2

#Juncaceae
j_sp <-flora$species[which(flora$family=="Juncaceae")]
j_genus <-flora$genus[which(flora$family=="Juncaceae")]
j_ms <- flora$msystem[which(flora$family=="Juncaceae")]
j_2c <-flora$DNA2C[which(flora$family=="Juncaceae")]
Juncaceae <- data.frame(j_sp, j_genus, j_ms, j_2c)
view(Juncaceae)
#Analysis
shapiro.test(Juncaceae$j_2c)
shapiro.test(log(Juncaceae$j_2c))
#p>0.05 so normal
j_anova <- aov(log(j_2c)~j_ms, data=Juncaceae)
summary(j_anova)
#p=0.183, F=2.12, DF=9, no sig diff between means

#Onagraceae
on_sp <-flora$species[which(flora$family=="Onagraceae")]
on_genus <-flora$genus[which(flora$family=="Onagraceae")]
on_ms <- flora$msystem[which(flora$family=="Onagraceae")]
on_2c <- flora$DNA2C[which(flora$family=="Onagraceae")]
Onagraceae <- data.frame(on_sp, on_genus, on_ms, on_2c)
view(Onagraceae)
#Analysis
shapiro.test(Onagraceae$on_2c)
shapiro.test(log(Onagraceae$on_2c))
#p<0.05 so non-normal, np test
#variances homogenous?
leveneTest(on_2c~on_ms, data=Onagraceae)
#p>0.5 so variances homogenous
kruskal.test(on_2c~on_ms, data=Onagraceae)
#p=0.04816, chi-squared=6.0664, df=2, sig diff between medians

#Orchidaceae
or_sp <-flora$species[which(flora$family=="Orchidaceae")]
or_genus <-flora$genus[which(flora$family=="Orchidaceae")]
or_ms<-flora$msystem[which(flora$family=="Orchidaceae")]
or_2c<- flora$DNA2C[which(flora$family=="Orchidaceae")]
Orchidaceae <-data.frame(or_sp, or_genus, or_ms, or_2c)
view(Orchidaceae)
#Analysis
shapiro.test(Orchidaceae$or_2c)
shapiro.test(log(Orchidaceae$or_2c))
#p>0.05 so normal
or_anova <-aov(log(or_2c)~or_ms, data=Orchidaceae)
summary(or_anova)
#p=0.0273, F=4.707, DF=16, sig diff between means

#Papaveraceae
pap_sp <- flora$species[which(flora$family=="Papaveraceae")]
pap_genus <- flora$genus[which(flora$family=="Papaveraceae")]
pap_ms <- flora$msystem[which(flora$family=="Papaveraceae")]
pap_2c <-flora$DNA2C[which(flora$family=="Papaveraceae")]
Papaveraceae <- data.frame(pap_sp, pap_genus, pap_ms, pap_2c)
view(Papaveraceae)
#Analysis
shapiro.test(Papaveraceae$pap_2c)
shapiro.test(log(Papaveraceae$pap_2c))
#p>0.05 so normal
pap_anova <- aov(log(pap_2c)~pap_ms, data= Papaveraceae)
summary(pap_anova)
#p=0.07, F=3.627, DF=11, no sig diff between means

#Pinaceae
pin_sp<- flora$species[which(flora$family=="Pinaceae")]
pin_genus <- flora$genus[which(flora$family=="Pinaceae")]
pin_ms <- flora$msystem[which(flora$family=="Pinaceae")]
pin_2c <-flora$DNA2C[which(flora$family=="Pinaceae")]
Pinaceae <- data.frame(pin_sp, pin_genus, pin_ms, pin_2c)
view(Pinaceae)
#Only ms info for 1 species (but 2c for almost all spp)

#Plantaginaceae
pl_sp <- flora$species[which(flora$family=="Plantaginaceae")]
pl_genus <- flora$genus[which(flora$family=="Plantaginaceae")]
pl_ms<- flora$msystem[which(flora$family=="Plantaginaceae")]
pl_2c<-flora$DNA2C[which(flora$family=="Plantaginaceae")]
Plantaginaceae <-data.frame(pl_sp, pl_genus, pl_ms, pl_2c)
view(Plantaginaceae)
#ANalysis
shapiro.test(Plantaginaceae$pl_2c)
shapiro.test(log(Plantaginaceae$pl_2c))
#p>0.05 so normal
pl_anova <- aov(log(pl_2c)~pl_ms, data=Plantaginaceae)
summary(pl_anova)
#p=0.0198, F=4.41, df=36, sig diff between means

#Poaceae
poa_sp <- flora$species[which(flora$family=="Poaceae")]
poa_genus <-flora$genus[which(flora$family=="Poaceae")]
poa_ms <- flora$msystem[which(flora$family=="Poaceae")]
poa_2c <- flora$DNA2C[which(flora$family=="Poaceae")]
Poaceae<- data.frame(poa_sp, poa_genus, poa_ms, poa_2c)
view(Poaceae)
#Analysis
shapiro.test(Poaceae$poa_2c)
shapiro.test(log(Poaceae$poa_2c))
#p>0.05 so normal
poa_anova <- aov(log(poa_2c)~poa_ms, data=Poaceae)
summary(poa_anova)
#p=0.46, F=0.785, df= 65, no sig diff between means

#Polygonaceae
pol_sp <- flora$species[which(flora$family=="Polygonaceae")]
pol_genus <- flora$genus[which(flora$family=="Polygonaceae")]
pol_ms <- flora$msystem[which(flora$family=="Polygonaceae")]
pol_2c<- flora$DNA2C[which(flora$family=="Polygonaceae")]
Polygonaceae<- data.frame(pol_sp, pol_genus, pol_ms, pol_2c)
view(Polygonaceae)
#Analysis
shapiro.test(Polygonaceae$pol_2c)
shapiro.test(log(Polygonaceae$pol_2c))
#p>0.05 so normal
pol_anova <- aov(log(pol_2c)~pol_ms, data=Polygonaceae)
summary(pol_anova)
#p=0.0887, F=2.935, df= 15, no sig diff between means

#Potamogetonaceae
pot_sp <- flora$species[which(flora$family=="Potamogetonaceae")]
pot_genus <-flora$genus[which(flora$family=="Potamogetonaceae")]
pot_ms <- flora$msystem[which(flora$family=="Potamogetonaceae")]
pot_2c <- flora$DNA2C[which(flora$family=="Potamogetonaceae")]
Potamogetonaceae <- data.frame(pot_sp, pot_genus, pot_ms, pot_2c)
view(Potamogetonaceae)
#data only available for 5 species
#Analysis
shapiro.test(Potamogetonaceae$pot_2c)
#normal
pot_anova <- aov(pot_2c~pot_ms, data= Potamogetonaceae)
summary(pot_anova)
#p=0.298, F=1.578, df=4

#Ranunculaceae
ran_sp <- flora$species[which(flora$family=="Ranunculaceae")]
ran_genus <-flora$genus[which(flora$family=="Ranunculaceae")]
ran_ms<- flora$msystem[which(flora$family=="Ranunculaceae")]
ran_2c <-flora$DNA2C[which(flora$family=="Ranunculaceae")]
Ranunculaceae <- data.frame(ran_sp, ran_genus, ran_ms, ran_2c)
view(Ranunculaceae)
#Analysis
shapiro.test(Ranunculaceae$ran_2c)
shapiro.test(log(Ranunculaceae$ran_2c))
#non-normal so np test required
#variances homogenous?
leveneTest(ran_2c~ran_ms, data=Ranunculaceae)
#p>0.05 so homogenous
kruskal.test(ran_2c~ran_ms, data=Ranunculaceae)
#p=0.2279, chi-squared=2.9579, df=2, no sig diff between medians

#Rosaceae
ro_sp <- flora$species[which(flora$family=="Rosaceae")]
ro_genus <- flora$genus[which(flora$family=="Rosaceae")]
ro_ms <- flora$msystem[which(flora$family=="Rosaceae")]
ro_2c <- flora$DNA2C[which(flora$family=="Rosaceae")]
Rosaceae <- data.frame(ro_sp, ro_genus, ro_ms, ro_2c)
#Analysis
shapiro.test(Rosaceae$ro_2c)
shapiro.test(log(Rosaceae$ro_2c))
#non-normal so np test required
#variances homogenous? 
leveneTest(ro_2c~ro_ms, data=Rosaceae)
#variances not homogenous-which test/transformation to use?
view(Rosaceae)

#Rubiaceae
ru_sp <- flora$species[which(flora$family=="Rubiaceae")]
ru_genus <- flora$genus[which(flora$family=="Rubiaceae")]
ru_ms <- flora$msystem[which(flora$family=="Rubiaceae")]
ru_2c <-flora$DNA2C[which(flora$family=="Rubiaceae")]
Rubiaceae <- data.frame(ru_sp, ru_genus, ru_ms, ru_2c)
view(Rubiaceae)
#Analysis
shapiro.test(Rubiaceae$ru_2c)
#p>0.05 so normal
ru_anova <- aov(ru_2c~ru_ms, data=Rubiaceae)
summary(ru_anova)  
#p=0.452, F=0.618, df=10, no sig diff between means

#Salicaceae 
sal_sp <- flora$species[which(flora$family=="Salicaceae")]
sal_genus <- flora$genus[which(flora$family=="Salicaceae")]
sal_ms <- flora$msystem[which(flora$family=="Salicaceae")]
sal_2c <- flora$DNA2C[which(flora$family=="Salicaceae")]
Salicaceae <- data.frame(sal_sp, sal_genus, sal_ms, sal_2c)
view(Salicaceae)
#all salicaceae with known ms generally cross

#Saxifragaceae
sax_sp <- flora$species[which(flora$family=="Saxifragaceae")]
sax_genus <- flora$genus[which(flora$family=="Saxifragaceae")]
sax_ms <- flora$msystem[which(flora$family=="Saxifragaceae")]
sax_2c <- flora$DNA2C[which(flora$family=="Saxifragaceae")]
Saxifragaceae <- data.frame(sax_sp, sax_genus, sax_ms, sax_2c)
view(Saxifragaceae)
#Analysis
shapiro.test(Saxifragaceae$sax_2c)
#p>0.05 so normal
sax_anova <- aov(sax_2c~sax_ms, data=Saxifragaceae)
summary(sax_anova)
#p=0.825, F=0.212, df=4; no sig diff between means

#Scrophulariaceae 
scr_sp <- flora$species[which(flora$family=="Scrophulariaceae")]
scr_genus <- flora$genus[which(flora$family=="Scrophulariaceae")]
scr_ms <- flora$msystem[which(flora$family=="Scrophulariaceae")]
scr_2c <- flora$DNA2C[which(flora$family=="Scrophulariaceae")]
Scrophulariaceae <- data.frame(scr_sp, scr_genus, scr_ms, scr_2c)
view(Scrophulariaceae)
#only 3 paired data points available

#Solanaceae
sol_sp <- flora$species[which(flora$family=="Solanaceae")]
sol_genus <- flora$genus[which(flora$family=="Solanaceae")]
sol_ms<- flora$msystem[which(flora$family=="Solanaceae")]
sol_2c <- flora$DNA2C[which(flora$family=="Solanaceae")]
Solenaceae <- data.frame(sol_sp, sol_genus, sol_ms, sol_2c)
view(Solenaceae)
#only 4 paired data points available
#Analysis
shapiro.test(Solenaceae$sol_2c)
shapiro.test(log(Solenaceae$sol_2c))
#p>0.05 so normal
sol_anova <- aov(log(sol_2c)~sol_ms, data=Solenaceae)
summary(sol_anova)
#p=0.967, F=0.002, DF=3, no sig diff between means