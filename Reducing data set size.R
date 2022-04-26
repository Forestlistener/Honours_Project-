rm(list=ls())
db_merged <- read.csv("C:/Users/Owner/Downloads/merged_db2 (1).csv", na.strings=c("","NA","nan", "NaN","Na"))
species <- db_merged$taxon_name_binom
genus <- db_merged$genus_y
family <- db_merged$family_y
order <- db_merged$order
msystem <-db_merged$myFertGen
DNA2C <- db_merged$GS_2C_pg
change_index <- db_merged$Change.Index_x
GBM_hectads_1987_99 <- db_merged$GB_Man_hectads_1987_1999
GBM_hectads_2000_09 <-db_merged$GB_Man_hectads_2000_2009
GBM_hectdads_2010_19 <-db_merged$GB_Man_hectads_2010_2019
CI_hectads_1987_99 <- db_merged$CI_hectads_1987_1999
CI_hectads_2000_09 <- db_merged$CI_hectads_2000_2009
CI_hectads_2010_19 <- db_merged$CI_hectads_2010_2019
Ire_hectads_1987_99 <- db_merged$Ire_hectads_1987_1999
Ire_hectads_2000_09 <- db_merged$Ire_hectads_2000_2009
Ire_hectads_2010_2019 <- db_merged$Ire_hectads_2010_2019
BI_DNA <- db_merged$from_BI_material
nativity_stace <- db_merged$StaceIV_nativity
nativity_atlas <- db_merged$Atlas_nativity_viaALIENATT_PLANTATT
nativity_stacecrawley_aliens <- db_merged$Stace_Crawley_nativity_aliens
flora <- data.frame(species, genus, family, order, msystem, DNA2C, data_source, BI_DNA, change_index, GBM_hectads_1987_99,GBM_hectads_2000_09,GBM_hectdads_2010_19, CI_hectads_1987_99, CI_hectads_2000_09, CI_hectads_2010_19, Ire_hectads_1987_99, Ire_hectads_2000_09, Ire_hectads_2010_2019, nativity_stace, nativity_atlas, nativity_stacecrawley_aliens)
view(flora)
save(flora, file = "flora.Rda")
