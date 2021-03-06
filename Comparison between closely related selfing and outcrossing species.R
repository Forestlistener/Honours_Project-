self_cross_pairs_genome <- data.frame(self_species=c("Briza minor",
                                                     "Callitriche brutia",
                                                     "Cephalanthera damasonium",
                                                     "Juncus squarrosus",
                                                     "Luzula campestris",
                                                     "Orobanche minor",
                                                     "Papaver dubium",
                                                     "Senecio vulgaris",
                                                     "Trifolium dubium",
                                                     "Trifolium fragiferum",
                                                     "Trifolium glomeratum",
                                                     "Veronica polita"),
                                      cross_species=c("Briza media",
                                                      "Callitriche stagnalis",
                                                      "Cephalanthera longifolia",
                                                      "Juncus bufonius",
                                                      "Luzula forsteri",
                                                      "Orobanche caryophylacea",
                                                      "Papaver rhoeas",
                                                      "Senecio squalidus",
                                                      "Trifolium campestre",
                                                      "Trifolium hybridum",
                                                      "Trifolium repens",
                                                      "veronia chamaedrys"),
                                      genus=c("Briza",
                                              "Callitriche",
                                              "Cephalanthera",
                                              "Juncus",
                                              "Luzula",
                                              "Orobanche",
                                              "Papaver",
                                              "Senecio",
                                              "Trifolium",
                                              "Trifolium",
                                              "Trifolium",
                                              "Veronica"),
                                      self_2c=c(5.8,
                                                3.67,
                                                32.8,
                                                0.61,
                                                0.68,
                                                3.66,
                                                4.84,
                                                2.94,
                                                1.39,
                                                1.07,
                                                0.78,
                                                2.98),
                                      cross_2c=c(6.68,
                                                 2.99,
                                                 33.6,
                                                 1.51,
                                                 1.4,
                                                 6.56,
                                                 4.84,
                                                 1.0,
                                                 0.74,
                                                 0.99,
                                                 1.38,
                                                 0.67),
                                      pair=c("B. minor - B. media",
                                             "C. brutia - C. stagnalis",
                                             "C. damasonium - C. longifolia",
                                             "J. squarrosus - J. bufonius",
                                             "L. campestris - L. forsteri",
                                             "O. minor - O. caryophylaceae",
                                             "P. dubium - P. rhoeas",
                                             "S. vulgaris - S. squalidus",
                                             "T. dubium - T. campestre",
                                             "T. fragigerum - T. hybridum",
                                             "T. glomeratum - T. repens",
                                             "V. polita - V. chamaedrys"))
self_cross_pairs_genome$difference_2c <- self_cross_pairs_genome$self_2c-self_cross_pairs_genome$cross_2c
library(ggplot2)
library(ggplot2)
ggplot(data= self_cross_pairs_genome, aes(x=pair, y=difference_2c, fill=genus))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Difference in 2C DNA Content (pg)")+
  labs(fill="Genus")+
  theme(axis.text.x=element_text(angle=90, face="italic", size=5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background= element_blank(),
        axis.line= element_line(colour="black"),
        legend.text=element_text(face="italic"))+
        theme(legend.key.size=unit(0.2,'cm'))+theme(legend.text=element_text(size=5))+theme(legend.title=element_text(size=7))+theme(axis.title.y=element_text(size=7))
  
  
t.test(self_cross_pairs_genome$difference_2c, mu=0)
#p>0.05 so mean difference between pairs not sig diff from 0
