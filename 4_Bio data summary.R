#Biological data summarizaation
library(readxl)
library(tidyverse)
library(sf)
library(clipr)
library(ggpubr)

library(ggrepel)
library(ggpp)

########Import data from previous step########
sites_dates<-read_csv("NotForGit/1_Data import and assembly/sites_dates.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-"))


all_bio_data_usable2<-read_csv( "NotForGit/1_Data import and assembly/all_bio_data_usable2.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
all_hab_data<-read_csv( "NotForGit/1_Data import and assembly/all_hab_data.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
gis_df_socal<-read_csv( "NotForGit/1_Data import and assembly/gis_df_socal.csv") %>%
  filter(processed_by=="automated_tool") %>%
  filter(masterid %in% sites_dates$masterid)

#This metric is missing but should be -9999
gis_df_socal$gismetricresult[gis_df_socal$gismetric=="nrst_dam" & gis_df_socal$masterid=="SMC00028"] <- "-9999"

# select(-masterid)
lu_stations_df_socal<-read_csv( "NotForGit/1_Data import and assembly/lu_stations_df_socal.csv")  %>%
  filter(masterid %in% sites_dates$masterid)

bmi_df_socal<-read_csv( "NotForGit/1_Data import and assembly/bmi_df_socal.csv") %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, stationcode=stationid)) %>%
  mutate(MID_Date=paste(masterid, sampledate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
algae_df_socal<-read_csv( "NotForGit/1_Data import and assembly/algae_df_socal.csv")  %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, stationcode=stationid)) %>%
  mutate(MID_Date=paste(masterid, sampledate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)

bug_mets<-read_csv("NotForGit/1_Data import and assembly/bug_mets.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
bryo_mets<-read_csv("NotForGit/1_Data import and assembly/bryo_mets.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)


phab_metrics__df<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_04092024.csv") %>%
  mutate(StationCode = case_when(StationCode=="403R4SSCT"~"402R4SSCT",
                                 T~StationCode)) %>%
  filter(StationCode %in% sites_dates$StationCode) %>%
  select(-starts_with("na.rm")) %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, StationCode=stationid)) %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)

disturbance_screening_df<-read_csv("NotForGit/3_Ref screening/disturbance_screening_df.csv")
# disturbance_screening_df %>%
#   filter(str_detect(masterid, "911")) %>%
#   select(masterid, contains("Ref"),contains("prox"))
meta_arth_df<-  read_xlsx("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - RB9 SWAMP 2020/Reports/Arthropod STE/STE_Arthropods_Feb2024.xlsx",
                          sheet="Sheet1") %>%
  filter(Phylum=="Arthropoda") %>%
  mutate(BugType = case_when(
    Order=="Araneae"~"Araneae",
    Order=="Coleoptera"~"Coleoptera",
    Family=="Formicidae"~"Formicidae",
    Order=="Hymenoptera"~"Other Hymenoptera",
    Order=="Hemiptera"~"Hemiptera",
    Order=="Diptera"~"Diptera",
    Order=="Lepidoptera"~"Lepidoptera",
    Order=="Thysanoptera"~"Thysanoptera",
    Order=="Psocodea"~"Psocodea",
    Order=="Dermaptera"~"Dermaptera",
    Order=="Isopoda"~"Isopoda",
    Class=="Collembola"~"Collembola",
    Class=="Insecta"~"Other insects",
    Class!="Insecta"~"Other non-insects",
    T~"OTHER"
  ))


meta_bryo_df<-#Verify with Rachel
  read_xlsx("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - RB9 SWAMP 2020/Reports/Bryo STE/California Bryophyte STE_v3.xlsx",
            sheet="FULL LIST ADDED") %>% 
  mutate(across(where(is.character), ~na_if(.,"NA"))) %>% 
  mutate(across(BET_perennial:BET_ngd10, as.numeric))
meta_bryo_df2<-meta_bryo_df %>%
  select(FinalID, Class, Order, Family, Genus, Species, Variety,
         TaxonomicLevelCode,
         STE1, STE1_Level, STE1_Acheived,
         STE2, STE2_Level, STE2_Acheived,
         #Traits
         GrowthForm,
         starts_with("BET")
  ) %>%
  unique()

phab_met_lu<-read_csv("Metadata/phab_data_lu.csv")
gis_met_lu<-read_csv("Metadata/gis_data_lu.csv")
bryo_met_lu<-read_csv("Metadata/bryo_mets_lu.csv")
bug_met_lu<-read_csv("Metadata/bug_mets_lu.csv")



bio_mets<-c(bug_met_lu$Metric_Method[bug_met_lu$Use],
            bryo_met_lu$name[bryo_met_lu$Use]) %>% unique()

bug_met_lu %>%
  filter(Use) %>%
  group_by(MetricType) %>%
  tally()

bryo_met_lu %>%
  filter(Use) %>%
  # group_by(MetricType) %>%
  tally()

####Review of trait dominance####
getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

bug_met_lu$Mode<-sapply(1:nrow(bug_met_lu), function(i){
  met.i=bug_met_lu$Metric_Method[i]
  
  xdf = bug_mets %>%
    mutate(Metric_Method=case_when(CollectionMethodCode =="TerrInvert_Trap_DryStreams"~paste0(name, "_Trap"),
                                   CollectionMethodCode =="TerrInvert_Veg_DryStreams"~paste0(name, "_Veg"),
                                   T~"Other")) %>%
    filter(Metric_Method==met.i)
  getMode(xdf$value)
})

bug_met_lu$Pct_Dom<-sapply(1:nrow(bug_met_lu), function(i){
  met.i=bug_met_lu$Metric_Method[i]
  
  xdf = bug_mets %>%
    mutate(Metric_Method=case_when(CollectionMethodCode =="TerrInvert_Trap_DryStreams"~paste0(name, "_Trap"),
                                   CollectionMethodCode =="TerrInvert_Veg_DryStreams"~paste0(name, "_Veg"),
                                   T~"Other")) %>%
    filter(Metric_Method==met.i)
  x.mode=getMode(xdf$value)
  sum(xdf$value==x.mode, na.rm = T)/sum(!is.na(xdf$value))
})


bug_met_lu %>%filter(Pct_Dom>=0.95)
###########Bio data summary###########


#How many taxa in each method?
all_bio_data_usable2 %>%
  select(CollectionMethodName, FinalID) %>%
  distinct() %>%
  group_by(CollectionMethodName) %>%
  tally()

#How many taxa, combining the bug methods
all_bio_data_usable2 %>%
  transmute(FinalID, Assemblage= case_when(CollectionMethodName=="Bryo_DryStreams"~"Bryophytes",
                                           str_detect(CollectionMethodName, "TerrInvert")~"Arthropods",
                                           T~"OTHER")) %>%
  distinct() %>%
  group_by(Assemblage) %>%
  tally()

#How many bug taxa were shared among the two types of methods?
intersect(all_bio_data_usable2$FinalID[all_bio_data_usable2$CollectionMethodName=="TerrInvert_Trap_DryStreams"],
          all_bio_data_usable2$FinalID[all_bio_data_usable2$CollectionMethodName=="TerrInvert_Veg_DryStreams"]) %>%
  length()

#Most common type of bug found in each type of sample


all_bio_data_usable2_bugs<-all_bio_data_usable2 %>%
  filter(str_detect(CollectionMethodName, "TerrInvert")) %>%
  inner_join(meta_arth_df %>%
               select(
                 FinalID, 
                 BugType,
                 Order, Class, Family
               ))


taxa_freq_plot_dat <-
  all_bio_data_usable2_bugs  %>%
  select(StationCode, SampleDate, CollectionMethodName, Type=BugType, FinalID) %>%
  distinct() %>%
  bind_rows(all_bio_data_usable2 %>%
              filter(str_detect(CollectionMethodName,"Bryo")) %>%
              select(StationCode, SampleDate, CollectionMethodName, Type=Family, FinalID) %>%
              distinct())

# all_bio_data_usable2_bugs %>%
#   select(StationCode, SampleDate, CollectionMethodName) %>%
#   distinct() %>%
#   inner_join(all_bio_data_usable2_bugs %>%
#                select(StationCode, SampleDate, CollectionMethodName, Order) %>%
#                distinct()) %>%
#   group_by(CollectionMethodName, Order) %>%
#   tally()

#how many samples were each group found in?
taxa_freq_plot_dat2<-taxa_freq_plot_dat %>%
  select(CollectionMethodName, StationCode, SampleDate) %>%
  distinct() %>%
  group_by(CollectionMethodName) %>% tally(name="n_samples") %>%
  ungroup() %>%
  inner_join(
    taxa_freq_plot_dat %>%
      select(CollectionMethodName, StationCode, SampleDate,
             Type) %>%
      distinct()
  ) %>%
  group_by(CollectionMethodName, n_samples, Type) %>%
  tally() %>%
  mutate(pct_samples = 100*n/n_samples) %>%
  arrange(-pct_samples) %>%
  na.omit() %>%
  mutate(Type_Method=paste(Type, CollectionMethodName),
         
         Assemblage= case_when(CollectionMethodName=="Bryo_DryStreams"~"Bryophytes",
                               CollectionMethodName=="TerrInvert_Trap_DryStreams"~"Terrestrial arthropods\nfrom the streambed",
                               CollectionMethodName=="TerrInvert_Veg_DryStreams"~"Terrestrial arthropods\nfrom vegetation",
                               T~"OTHER")) 


taxa_freq_plot_dat2 %>% filter(CollectionMethodName=="TerrInvert_Veg_DryStreams")
taxa_freq_plot_dat2 %>% filter(Type=="Araneae")
# 
# 
# taxa_freq_plot_dat2<-taxa_freq_plot_dat %>%
#   select(CollectionMethodName, StationCode, SampleDate, Type, FinalID) %>%   distinct() %>%
#   # select(CollectionMethodName, StationCode, SampleDate, Type) %>%   distinct() %>%
#   group_by(CollectionMethodName, Type) %>%
#   tally() %>%
#   arrange(-n) %>%
#   na.omit() %>%
#   mutate(Type_Method=paste(Type, CollectionMethodName),
# 
#          Assemblage= case_when(CollectionMethodName=="Bryo_DryStreams"~"Bryophytes",
#                                CollectionMethodName=="TerrInvert_Trap_DryStreams"~"Terrestrial arthropods\nfrom the streambed",
#                                CollectionMethodName=="TerrInvert_Veg_DryStreams"~"Terrestrial arthropods\nfrom vegetation",
#                                T~"OTHER")) 


taxa_freq_plot_dat2$Type_Method=factor(taxa_freq_plot_dat2$Type_Method, levels=taxa_freq_plot_dat2$Type_Method %>% unique())



taxa_freq_plot_dat2$Type

taxa_freq_plot<-ggplot(taxa_freq_plot_dat2, aes(x=Type_Method, y=pct_samples))+
  geom_bar(stat="identity")+
  facet_wrap(~Assemblage, scales="free_y", nrow=1)+
  coord_flip()+
  scale_x_discrete(labels=taxa_freq_plot_dat2$Type %>% as.character(),
                   breaks=taxa_freq_plot_dat2$Type_Method)+
  xlab("")+
  ylab("% of samples")+
  theme_bw()
ggsave(taxa_freq_plot, filename="NotForGit/4_Bio data summary/taxa_freq_plot.png",
       width=8, height=5)


#Which groups are the most speciose?



# taxa_freq_plot_dat <-
# all_bio_data_usable2_bugs  %>%
# select(StationCode, SampleDate, CollectionMethodName, Type=BugType, FinalID) %>%
# distinct() %>%
# bind_rows(all_bio_data_usable2 %>%
# filter(str_detect(CollectionMethodName,"Bryo")) %>%
# select(StationCode, SampleDate, CollectionMethodName, Type=Family, FinalID) %>%
# distinct())

#Most speciose bryophytes? 
taxa_freq_plot_dat %>%
  select(CollectionMethodName, Type, FinalID) %>%
  distinct() %>%
  group_by(CollectionMethodName, Type) %>%
  tally() %>%
  arrange(-n) %>%
  filter(CollectionMethodName=="Bryo_DryStreams")

taxa_freq_plot_dat %>%
  filter(Type=="Pottiaceae") %>%
  select(FinalID) %>% unique() %>%
  arrange(FinalID)

#Most speciose bugs in traps? 
taxa_freq_plot_dat %>%
  select(CollectionMethodName, Type, FinalID) %>%
  distinct() %>%
  group_by(CollectionMethodName, Type) %>%
  tally() %>%
  arrange(-n) %>%
  # filter(CollectionMethodName=="TerrInvert_Trap_DryStreams")
  filter(CollectionMethodName=="TerrInvert_Veg_DryStreams")

taxa_freq_plot_dat %>%
  filter(Type=="Hemiptera", 
         CollectionMethodName=="TerrInvert_Veg_DryStreams") %>%
  select(FinalID) %>% unique() %>%
  inner_join(meta_arth_df %>%
               select(FinalID, Suborder,Superfamily, Family, Subfamily, Tribe, Genus)) %>%
  arrange(Suborder, Superfamily, Family, Subfamily, Tribe, Genus) %>%
  print(n=40)

####### Review of trait coverage in our datasets ######

arth_traits<-c( #Invasiveness
  "Nonnative","Nonnative_Synanthropic",
  #Habitat
  "Aquatic","Stratum_final",
  #Feeding
  "FFG_final_simple","FFG_final_detailed",
  #Body size
  # "BodySizeMin",
  "BodySizeMean",
  # "BodySizeMax",
  #Other
  # "AntWiki_ColonySizeBin",
  "Gossner_DispersalAbility",
  # "Ubick_HuntingStyle",
  #Temperature
  "TempMin_final","TempMean_final","TempMax_final")

arth_groups_trait_coverage<-all_bio_data_usable2_bugs %>%
  select(StationCode, FinalID) %>%
  distinct() %>%
  group_by(FinalID) %>%
  tally(name = "n_samples") %>%
  ungroup() %>%
  left_join(meta_arth_df %>%
              select(FinalID, BugType, 
                     all_of(arth_traits)) %>%
              distinct()
  ) %>%
  pivot_longer(cols=all_of(arth_traits),
               names_to = "trait",
               values_to = "val", 
               values_transform = list(val = as.character)) %>%
  group_by(trait, BugType) %>%
  summarize(n_taxa=length(val),
            n_trait=sum(!is.na(val))) %>%
  ungroup() %>%
  transmute(
    # Taxon = paste0(BugType," (", n_taxa," taxa)"),
    BugType, n_taxa,
    trait, pct_trait = 100*n_trait/n_taxa) %>%
  pivot_wider(names_from = trait, values_from = pct_trait) %>%
  arrange(-n_taxa)


meta_bryo_df_reformated<-meta_bryo_df %>%
  transmute(
    FinalID, ParentOrganismFinalID, 
    Phylum, Class, Subclass, Order, Family, Subfamily, Genus, Species, Variety,
    GrowthForm,
    BET_lstrat = #Life strategy
      case_when(BET_perennial==1~"Perennial",
                BET_short_lived_shuttle==1~"Short_lived_shuttle",
                BET_annual_shuttle==1~"Annual_shuttle",
                BET_long_lived_shuttle==1~"Long_lived_shuttle",
                BET_colonist==1~"Colonist",
                BET_fugitive==1~"Fugitive",
                T~NA_character_),
    BET_lform = #Life form
      case_when(BET_weft==1~"Weft",
                BET_mat==1~"Mat",
                BET_turf==1~"Turf",
                BET_annual==1~"Annual",
                BET_cushion==1~"Cushion",
                BET_rosette==1~"Rosette",
                BET_dendroid==1~"Dendroid",
                T~NA_character_),
    BET_gform =  #Growth form
      case_when(BET_acrocarpous==1~"Acrocarpous",
                BET_foliose==1~"Foliose",
                BET_pleurocarpous==1~"Pleurocarpous",
                BET_sphagnum==1~"Sphagnum",
                BET_thalloid==1~"Thalloid",
                T~NA_character_),
    BET_genl = #Generation length
      case_when(BET_LongGeneration==1~"Long",
                BET_MediumGeneration==1~"Medium",
                BET_ShortGeneration==1~"Short",
                T~NA_character_),
    BET_indL,	#Light indicator value
    BET_indT,	#Temperature indicator value
    BET_indF,	#Moisture indicator value
    BET_indR,	#Acidity indicator value
    BET_indN,	#Nutrients indicator value
    BET_indS,	#Salt indicator value
    BET_indHM, #Heavy metal indicator value
    
    BET_sub_so, #Soil substrate
    BET_sub_ro, #Rocky substrate
    BET_sub_ba, #Bark substrate,
    BET_sub_wo, #Dead wood substrate
    BET_sub_nw, #Epiphytic nonwoody substrate
    BET_sub_an, #Carcass or dung substrate
    BET_sub_sum, #Sum of substrate classes
    
    BET_aquatic, #Aquatic taxa
    
    BET_hab_we, #Habitat: Wetland
    BET_hab_fo, #Habitat: Forest
    BET_hab_sh, #Habitat: Shrubland
    BET_hab_gr, #Habitat: Grassland
    BET_hab_ro, #Habitat: Rocky areas
    BET_hab_ar, #Habitat: Artificial/terrestrial
    BET_hab_sum, #Habitat: Sum of habitat classes
    
    BET_forest, #Forest affinity; lower is more restrictred to closed forest
    BET_hemeroby, #Hemerobic environment affinity; higher is more hemerobic/human-dominated. Source: BryForTrait
    BET_hem_e, #Hemerobic environment affinity; higher is more hemerobic/human-dominated. Source: Flora indicativa
    
    BET_T_diurR, #Mean dirunal air temperature range
    BET_T_iso, #Isothermality
    BET_T_seas, #Temperature seasonality
    BET_Tmax_warmM, #Mean daily max air temp of the warmest month
    BET_Tmin_coldM, #Mean daily min air temp of the coldest moth
    BET_T_annualR, #Annual range of air temp
    BET_T_wetQ, #Mean daily mean air temp of the wettest quarter
    BET_T_dryQ, #Mean daily air temp of the driest quarter
    BET_T_warmQ, #Mean daily mean air temp of the warmest quarter
    BET_T_coldQ, #Mean daily mean air temp of the coldest quarter
    
    BET_MAP, #Annual precipitation amount
    BET_P_wetM, #Precipitation amout of the wettest month
    BET_P_dryM, #Precipitation amount of the driest month
    BET_P_seas, #Precipitation seasonality
    BET_P_wetQ, #Mean monthly precipitation amount of the wettest quarter
    BET_P_dryQ, #Mean monthly precipitation amount of the driest quarter
    BET_P_warmQ, #Mean monthly precipitation amount of the warmest quarter
    BET_P_coldQ, #Mean monthly precipitation amount of the coldest quarter
    
    BET_gdd0, #Growing degree days heat sum above 0 C.
    BET_gdd5, #Growing degree days heat sum above 5 C.
    BET_gdd10, #Growing degree days heat sum above 10 C.
    BET_ngd0, #Number of growing degree days above 0 C.
    BET_ngd5, #Number of growing degree days above 5 C.
    BET_ngd10 #Number of growing degree days above 10 C.
    
  )

bry_traits_reformatted = names(meta_bryo_df_reformated) %>%
  setdiff(c("FinalID", "ParentOrganismFinalID", 
            "Phylum", "Class", "Subclass", "Order", "Family", "Subfamily", "Genus", "Species", "Variety"))

bryo_groups_trait_coverage<-all_bio_data_usable2 %>%
  filter(CollectionMethodName=="Bryo_DryStreams") %>%
  select(StationCode, FinalID) %>%
  distinct() %>%
  group_by(FinalID) %>%
  tally(name = "n_samples") %>%
  ungroup() %>%
  left_join(meta_bryo_df_reformated %>%
              distinct() %>%
              mutate(#BryGroup = Family,
                BryGroup = case_when(Family %in% c("Pottiaceae", "Bryaceae", "Grimmiaceae",
                                                   "Brachytheciaceae", "Bartramiaceae","Funariaceae",
                                                   "Amblystegiaceae","Ditrichaceae")~Family,
                                     T~"Other Families")     )
  )%>%
  pivot_longer(cols=#all_of(bry_traits_reformatted),
                 c(GrowthForm, BET_lstrat, BET_lform, BET_gform, BET_genl,
                   contains("_ind"),
                   BET_sub_so, BET_aquatic, BET_hab_we,
                   BET_forest, BET_hemeroby, BET_hem_e,
                   BET_T_diurR, BET_MAP, BET_gdd0),
               names_to = "trait",
               values_to = "val", 
               values_transform = list(val = as.character)) %>%
  group_by(trait, BryGroup) %>%
  summarize(n_taxa=length(val),
            n_trait=sum(!is.na(val))) %>%
  ungroup() %>%
  transmute(
    # Taxon = paste0(BryGroup," (", n_taxa," taxa)"),
    BryGroup, n_taxa,
    trait, pct_trait = 100*n_trait/n_taxa) %>%
  pivot_wider(names_from = trait, values_from = pct_trait) %>%
  arrange(-n_taxa)


arth_groups_trait_coverage %>% write_clip()
bryo_groups_trait_coverage %>% write_clip()


########NMDS ordinations########
library(vegan)

#Run an overall ordination on the full dataset


arth_traps_df<-all_bio_data_usable2 %>%
  filter(CollectionMethodName=="TerrInvert_Trap_DryStreams") %>%
  select(masterid, SampleDate, MID_Date, FinalID, Counts) %>%
  group_by(masterid, SampleDate, MID_Date, FinalID) %>%
  summarise(Counts=sum(Counts, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = FinalID, values_from = Counts, values_fill = 0)

arth_traps_mat<-arth_traps_df %>%
  select(-c("masterid","SampleDate","MID_Date")) %>%
  as.matrix()
row.names(arth_traps_mat)<-arth_traps_df$MID_Date
arth_traps_mat_PA<-arth_traps_mat
arth_traps_mat_PA[arth_traps_mat_PA>0]<-1

arth_veg_df<-all_bio_data_usable2 %>%
  filter(CollectionMethodName=="TerrInvert_Veg_DryStreams") %>%
  select(masterid, SampleDate, MID_Date, FinalID, Counts) %>%
  group_by(masterid, SampleDate, MID_Date, FinalID) %>%
  summarise(Counts=sum(Counts, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = FinalID, values_from = Counts, values_fill = 0)

arth_veg_mat<-arth_veg_df %>%
  select(-c("masterid","SampleDate","MID_Date")) %>%
  as.matrix()
row.names(arth_veg_mat)<-arth_veg_df$MID_Date
arth_veg_mat_PA<-arth_veg_mat
arth_veg_mat_PA[arth_veg_mat_PA>0]<-1

bryo_df<-all_bio_data_usable2 %>%
  filter(CollectionMethodName=="Bryo_DryStreams") %>%
  transmute(masterid, SampleDate, MID_Date, FinalID, Counts=1) %>%
  group_by(masterid, SampleDate, MID_Date, FinalID) %>%
  summarise(Counts=sum(Counts, na.rm=T)) %>%
  ungroup() %>%
  mutate(Counts=case_when(Counts==0~0,T~1)) %>%
  pivot_wider(names_from = FinalID, values_from = Counts, values_fill = 0)

bryo_mat_PA<-bryo_df %>%
  select(-c("masterid","SampleDate","MID_Date")) %>%
  as.matrix()
row.names(bryo_mat_PA)<-bryo_df$MID_Date


#Run an overall ordination on the full dataset
set.seed(100)
arth_traps_NMDS<-metaMDS(arth_traps_mat, distance="bray", k=2, autotransform = T, wascores=T)
arth_traps_df<-bind_cols(arth_traps_df,
                         arth_traps_NMDS$points %>% 
                           as_tibble() %>%
                           rename(Traps_MDS1=MDS1, Traps_MDS2=MDS2))
set.seed(1000)
arth_traps_NMDS_PA<-metaMDS(arth_traps_mat_PA, distance="bray", k=2, autotransform = T, wascores=T)
arth_traps_df<-bind_cols(arth_traps_df,
                         arth_traps_NMDS_PA$points %>% 
                           as_tibble() %>%
                           rename(Traps_PA_MDS1=MDS1, Traps_PA_MDS2=MDS2))


set.seed(101)
arth_veg_NMDS<-metaMDS(arth_veg_mat, distance="bray", k=2, autotransform = T, wascores=T)
arth_veg_df<-bind_cols(arth_veg_df,
                       arth_veg_NMDS$points %>% 
                         as_tibble()%>%
                         rename(Veg_MDS1=MDS1, Veg_MDS2=MDS2))
set.seed(1001)
arth_veg_NMDS_PA<-metaMDS(arth_veg_mat_PA, distance="bray", k=2, autotransform = T, wascores=T)
arth_veg_df<-bind_cols(arth_veg_df,
                       arth_veg_NMDS_PA$points %>% 
                         as_tibble() %>%
                         rename(Veg_PA_MDS1=MDS1, Veg_PA_MDS2=MDS2))


set.seed(1020)
bryo_NMDS<-metaMDS(bryo_mat_PA, distance="bray", k=2, autotransform = T, wascores=T)
bryo_df<-bind_cols(bryo_df,
                   bryo_NMDS$points %>% 
                     as_tibble() %>%
                     rename(Bryo_PA_MDS1=MDS1, Bryo_PA_MDS2=MDS2))

ord_df<-
  arth_traps_df %>%
  select(masterid, SampleDate, MID_Date, contains("MDS")) %>%
  pivot_longer(cols=contains("MDS"), names_to = "Ordination_Axis", values_to = "Ordination_Axis_score") %>%
  mutate(Assemblage="Traps") %>%
  bind_rows(
    arth_veg_df %>%
      select(masterid, SampleDate, MID_Date, contains("MDS")) %>%
      pivot_longer(cols=contains("MDS"), names_to = "Ordination_Axis", values_to = "Ordination_Axis_score") %>%
      mutate(Assemblage="Veg") 
  ) %>%
  bind_rows(
    bryo_df %>%
      select(masterid, SampleDate, MID_Date, contains("MDS")) %>%
      pivot_longer(cols=contains("MDS"), names_to = "Ordination_Axis", values_to = "Ordination_Axis_score") %>%
      mutate(Assemblage="Bryo") 
  ) %>%
  mutate(MDS_Axis = case_when(str_detect(Ordination_Axis, "MDS1")~"MDS1",
                              str_detect(Ordination_Axis, "MDS2")~"MDS2",
                              T~"OTHER"),
         MDS_DataType = case_when(str_detect(Ordination_Axis, "_PA_")~"PA",
                                  T~"Abundance")) %>%
  select(-Ordination_Axis) %>%
  rename(MDS_Score=Ordination_Axis_score) %>%
  pivot_wider(names_from=MDS_Axis, values_from = MDS_Score) %>%
  inner_join(
    disturbance_screening_df %>%
      select(masterid, RefStatus, RefStatus_GIS, StressLevel)
  ) %>%
  mutate(Assemblage_pretty = case_when(Assemblage=="Bryo"~"Bryophytes",
                                       Assemblage=="Traps"~"Arthropods from\nthe streambed",
                                       Assemblage=="Veg"~"Arthropods from\nvegetation",
                                       T~"OTHER"))

#####Sites#####
nmds_all_plot<-ggplot(data=ord_df, aes(x=MDS1, y=MDS2))+
  geom_hline(linetype="dashed", yintercept=0)+geom_vline(linetype="dashed", xintercept=0)+
  geom_point(aes(color=StressLevel))+
  facet_grid(Assemblage_pretty~MDS_DataType)+
  scale_color_manual(values=c("#d7191c","#fecc5c","#2c7bb6"),
                     name="Disturbance level")+
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())
ggsave(nmds_all_plot, filename="NotForGit/4_Bio data summary/nmds_all_plot.png",
       width=6.5, height=6)

nmds_3ass_plot<-ggplot(data=ord_df %>%
                         filter(
                           (Assemblage=="Bryo" & MDS_DataType=="PA") |
                             MDS_DataType=="Abundance"
                         ), aes(x=MDS1, y=MDS2))+
  geom_hline(linetype="dashed", yintercept=0)+geom_vline(linetype="dashed", xintercept=0)+
  geom_point(aes(color=StressLevel))+
  facet_wrap(~Assemblage_pretty)+
  scale_color_manual(values=c("#d7191c","#fecc5c","#2c7bb6"),
                     name="Disturbance level")+
  theme_bw() +
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(nmds_3ass_plot, filename="NotForGit/4_Bio data summary/nmds_3ass_plot.png",
       width=6.5, height=4)


#####Species#####
meta_bryo_df2$Group = case_when(is.na(meta_bryo_df2$Family)~"Bryophyta",
                                T~meta_bryo_df2$Family)


mds_species_df<-
  arth_traps_NMDS$species %>%
  as_tibble() %>%
  transmute(MDS1, MDS2,
            FinalID = row.names(arth_traps_NMDS$species),
            Assemblage="Traps", DataType="Abundance") %>%
  bind_rows(
    arth_traps_NMDS_PA$species %>%
      as_tibble() %>%
      transmute(MDS1, MDS2,
                FinalID = row.names(arth_traps_NMDS_PA$species),
                Assemblage="Traps", DataType="PA")
  ) %>%
  bind_rows(
    arth_veg_NMDS$species %>%
      as_tibble() %>%
      transmute(MDS1, MDS2,
                FinalID = row.names(arth_veg_NMDS$species),
                Assemblage="Veg", DataType="Abundance")
  ) %>%
  bind_rows(
    arth_veg_NMDS_PA$species %>%
      as_tibble() %>%
      transmute(MDS1, MDS2,
                FinalID = row.names(arth_veg_NMDS_PA$species),
                Assemblage="Veg", DataType="PA")
  )  %>%
  mutate(
    Group = case_when(
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Araneae"]~"Araneae",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Coleoptera"]~"Coleoptera",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Family=="Formicidae"]~"Formicidae",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Hymenoptera"]~"Other Hymenoptera",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Hemiptera"]~"Hemiptera",
      # Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Diptera"]~"Diptera",
      # Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Lepidoptera"]~"Lepidoptera",
      # Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Thysanoptera"]~"Thysanoptera",
      # Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Psocodea"]~"Psocodea",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Dermaptera"]~"Dermaptera",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Isopoda"]~"Isopoda",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Order=="Collembola"]~"Collembola",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Class=="Insecta"]~"Other insects",
      Assemblage %in% c("Traps","Veg") & FinalID %in% meta_arth_df$FinalID[meta_arth_df$Class!="Insecta"]~"Other non-insects",
      Assemblage %in% "Bryo"~"BRYO",
      T~"OTHER"
    )
  ) %>%
  
  bind_rows(
    bryo_NMDS$species %>%
      as_tibble() %>%
      transmute(MDS1, MDS2,
                FinalID = row.names(bryo_NMDS$species),
                Assemblage="Bryo", DataType="PA") %>%
      inner_join(meta_bryo_df2 %>%
                   select(FinalID, Group) %>%
                   distinct) %>%
      mutate(Group=case_when(
        # Group %in% c("Aulacomniaceae", "Fissidentaceae","Meesiaceae","Orthotrichaceae","Mielichhoferiaceae")~"Other acrocarp",
        # Group %in% c("Fabroniaceae", "Fontinalaceae", "Leskeaceae")~"Other pleurocarp",
        T~Group))
  )

meta_bryo_df2 %>% filter(Family=="Orthotrichaceae") %>% select(BET_acrocarpous)
meta_bryo_df2 %>%
  select(FinalID, Group) %>% distinct()

mds_species_df %>%
  filter(Group=="OTHER")

mds_species_df %>%
  group_by( Group, Assemblage) %>% tally()

mds_species_df<-mds_species_df %>%
  left_join(meta_arth_df %>%
              select(FinalID, Nonnative_Synanthropic) %>% 
              distinct()
  ) %>%
  mutate(Nat=case_when(is.na(Nonnative_Synanthropic)~"No",
                       Nonnative_Synanthropic==0~"No",
                       Nonnative_Synanthropic==1~"Yes"
  )) %>%
  left_join(
    meta_bryo_df2 %>%
      select(FinalID, GrowthForm) %>%
      distinct()
  )


traps_abundance_species_scores_plot_facets<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="Abundance") %>%
           filter(Assemblage %in% c("Traps")),
         aes(x=MDS1, y=MDS2))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point( data = ord_df %>%
                filter(MDS_DataType=="Abundance") %>%
                filter(Assemblage %in% c("Traps")), 
              aes(size=StressLevel), color="gray")+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  geom_point(size=2, aes(fill=Nat), shape=22)+ 
  scale_fill_manual(values=c("red","yellow"), name="Nonnative or\nSynanthropic?")+
  # geom_point(data = . %>% filter(Nonnative_Synanthropic==1), size=2,
  #            aes(fill="Nonnative or\nSynanthropic"), shape=24)+ scale_fill_manual(values="yellow", name="")+
  # 
  # geom_point( size=2, fill="red", shape=22)+
  # geom_point(data = . %>% filter(FinalID=="Linepithema humile"), size=2, 
  #            aes(fill="Linepithema humile"), shape=22)+ scale_fill_manual(values="yellow", name="")+
  facet_wrap(vars(Group)) +
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "bottom", 
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.direction="vertical")
ggsave(traps_abundance_species_scores_plot_facets,
       filename="NotForGit/4_Bio data summary/traps_abundance_species_scores_plot_facets.png",
       height=6.5, width=6.5)

traps_abundance_species_scores_plot<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="Abundance") %>%
           filter(Assemblage %in% c("Traps")) %>%
           mutate(Group=case_when(Group=="Dermaptera"~"Other insects",
                                  T~Group)),
         aes(x=MDS1, y=MDS2))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point( size=2, shape=21, aes(fill=Group))+  # facet_wrap(vars(Group)) +
  scale_fill_brewer(palette="Set1", name="Taxon group")+
  theme_bw()+
  theme(panel.grid=element_blank())
traps_abundance_species_scores_plot

traps_pa_species_scores_plot_facets<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="PA") %>%
           filter(Assemblage %in% c("Traps")),
         aes(x=MDS1, y=MDS2))+
  geom_point( data = ord_df %>%
                filter(MDS_DataType=="PA") %>%
                filter(Assemblage %in% c("Traps")), 
              aes(size=StressLevel), color="gray")+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  geom_point( size=1, fill="red", shape=22)+  facet_wrap(vars(Group)) +
  theme_bw()

veg_abundance_species_scores_plot_facets<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="Abundance") %>%
           filter(Assemblage %in% c("Veg")),
         aes(x=MDS1, y=MDS2))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point( data = ord_df %>%
                filter(MDS_DataType=="Abundance") %>%
                filter(Assemblage %in% c("Veg")), 
              aes(size=StressLevel), color="gray")+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  geom_point(size=2, aes(fill=Nat), shape=22)+ 
  scale_fill_manual(values=c("red","yellow"), name="Nonnative or\nSynanthropic?")+
  
  # geom_point( size=2, fill="red", shape=22)+  
  # geom_point(data = . %>% filter(FinalID=="Linepithema humile"), size=2, 
  # aes(fill="Linepithema humile"), shape=22)+ scale_fill_manual(values="yellow", name="")+
  facet_wrap(vars(Group)) +
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "bottom", 
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.direction="vertical")
ggsave(veg_abundance_species_scores_plot_facets,
       filename="NotForGit/4_Bio data summary/veg_abundance_species_scores_plot_facets.png",
       height=6.5, width=6.5)

veg_abundance_species_scores_plot<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="Abundance") %>%
           filter(Assemblage %in% c("Veg")) %>%
           mutate(Group=case_when(Group=="Dermaptera"~"Other insects",
                                  T~Group)),
         aes(x=MDS1, y=MDS2))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point( size=2, shape=21, aes(fill=Group))+  # facet_wrap(vars(Group)) +
  scale_fill_brewer(palette="Set1", name="Taxon group")+
  theme_bw()+
  theme(panel.grid=element_blank())


veg_pa_species_scores_plot_facets<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="PA") %>%
           filter(Assemblage %in% c("Veg")),
         aes(x=MDS1, y=MDS2))+
  geom_point( data = ord_df %>%
                filter(MDS_DataType=="PA") %>%
                filter(Assemblage %in% c("Veg")), 
              aes(size=StressLevel), color="gray")+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  geom_point( size=1, fill="red", shape=22)+  facet_wrap(vars(Group)) +
  theme_bw()

bryo_pa_species_scores_plot_facets<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="PA") %>%
           filter(Assemblage %in% c("Bryo")) %>%
           mutate(Group=case_when(
             Group %in% c("Aulacomniaceae", "Fissidentaceae","Meesiaceae",
                          "Orthotrichaceae","Mielichhoferiaceae","Ditrichaceae")~
               "Other acrocarp",
             Group %in% c("Fabroniaceae", "Fontinalaceae", "Leskeaceae",
                          "Amblystegiaceae")~
               "Other pleurocarp",T~Group) %>%
               factor(levels=c("Bartramiaceae", "Brachytheciaceae", "Bryaceae",
                               "Funariaceae","Grimmiaceae","Pottiaceae", 
                               "Other acrocarp","Other pleurocarp",
                               "Bryophyta"))
           ) %>%
           filter(Group!="Bryophyta"),
         aes(x=MDS1, y=MDS2))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point( data = ord_df %>%
                filter(MDS_DataType=="PA") %>%
                filter(Assemblage %in% c("Bryo")), 
              aes(size=StressLevel), color="gray")+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  # geom_point( size=2, fill="red", shape=22)+  facet_wrap(vars(Group)) +
  geom_point(size=2, aes(fill=GrowthForm), shape=22)+ 
  scale_fill_manual(values=c("red","yellow"), 
                    name="Growth form",
                    labels=c("Acrocarpous","Pleurocarpous"))+
  facet_wrap(vars(Group)) +
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "bottom", 
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.direction="vertical") +
  guides(size=guide_legend(order=2),
         fill=guide_legend(order=1))

ggsave(bryo_pa_species_scores_plot_facets,
       filename="NotForGit/4_Bio data summary/bryo_pa_species_scores_plot_facets.png",
       height=7, width=6.5)

bryo_pa_species_scores_plot<-
  ggplot(data=mds_species_df %>%
           filter(DataType=="PA") %>%
           filter(Assemblage %in% c("Bryo")) %>%
           mutate(Group=case_when(
             Group %in% c("Aulacomniaceae", "Fissidentaceae","Meesiaceae",
                          "Orthotrichaceae","Mielichhoferiaceae","Ditrichaceae")~
               "Other acrocarp",
             Group %in% c("Fabroniaceae", "Fontinalaceae", "Leskeaceae",
                          "Amblystegiaceae")~
               "Other pleurocarp",T~Group) %>%
               factor(levels=c("Bartramiaceae", "Brachytheciaceae", "Bryaceae",
                               "Funariaceae","Grimmiaceae","Pottiaceae", 
                               "Other acrocarp","Other pleurocarp",
                               "Bryophyta"))
           ) %>%
           filter(Group!="Bryophyta"),
         aes(x=MDS1, y=MDS2))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point( size=2, shape=21, aes(fill=Group))+  # facet_wrap(vars(Group)) +
  scale_fill_brewer(palette="Set1", name="Taxon group")+
  theme_bw()+
  theme(panel.grid=element_blank())


traps_abundance_species_scores_plot+ ggtitle("Arthropods on the streambed")
veg_abundance_species_scores_plot+ ggtitle("Arthropods on vegetation")
bryo_pa_species_scores_plot+ ggtitle("Bryophytes")

nmds_3ass_species_plot<-ggarrange(plotlist=
                                    list(
                                      traps_abundance_species_scores_plot+ ggtitle("Arthropods on the streambed"),
                                      veg_abundance_species_scores_plot+ ggtitle("Arthropods on vegetation"),
                                      bryo_pa_species_scores_plot+ ggtitle("Bryophytes")),
                                  ncol=1)
ggsave(nmds_3ass_species_plot,
       filename="NotForGit/4_Bio data summary/nmds_3ass_species_plot.png",
       height=8.5, width=6.5)
#####Correlations#####
dist_df_mod<-disturbance_screening_df %>%
  select(-c("RefStatus_GIS", "RefStatus", "StressLevel")) %>%
  pivot_longer(cols= -masterid) %>%
  mutate(MetricType = "Geospatial",
         MetricSubtype = case_when(name %in% c(
           "developed_1k_16","developed_5k_16", "developed_ws_16",
           "ag_1k_16","ag_5k_16", "ag_ws_16",
           "urban_1k_16","urban_5k_16","urban_ws_16",
           "agur_1k_16","agur_5k_16","agur_ws_16",
           "code_21_1k_16","code_21_5k_16","code_21_ws_16",
           "roaddens_1k","roaddens_5k","roaddens_ws",
           "paved_int_1k","paved_int_5k","paved_int_ws",
           "nrst_dam", #Can't easily correlate due to Inf
           "cnl_pi_pct","mines",
           "Max_HumanActivity_prox") ~ "Disturbance",T~"Natural")
  ) %>%
  filter(!name %in% c("Max_HumanActivity_prox","nrst_dam")) %>%
  filter(!str_detect(name,"agur"))

gis_nat_df<-gis_df_socal %>%
  select(masterid, #new_lat, new_long, 
         name=gismetric, value=gismetricresult ) %>%
  filter(name %in% c("area_sqkm", "site_elev", "ppt_00_09", "temp_00_09", "lst32ave", 
                     "maxwd_ws", "meanp_ws", "minp_ws", "xwd_ws", "tmax_ws", "sumave_p", 
                     "cao_mean", "mgo_mean", "p_mean", "s_mean", "n_mean", "kfct_ave", 
                     "lprem_mean", "prmh_ave", "ucs_mean", "bdh_ave",
                     "max_elev","elev_range",
                     "condqr01","condqr10","condqr50","condqr90","condqr99")) %>%
  mutate(value = as.numeric(value)) %>%
  na.omit() %>%
  bind_rows(  gis_df_socal %>% transmute(masterid, name="lat", value=new_lat)  ) %>%
  bind_rows(  gis_df_socal %>% transmute(masterid, name="lon", value=new_long)  ) %>%
  distinct() %>%
  mutate(MetricType="Geospatial",
         MetricSubtype = 
           case_when(name %in% c("lat", "lon","site_elev")~"Location",
                     name %in% c("area_sqkm", "max_elev","elev_range")~"Watershed",
                     name %in% c("condqr01","condqr10", "condqr50", "condqr90", "condqr99")~"Conductivity",
                     name %in% c("ppt_00_09","temp_00_09","sumave_p", "lst32ave","maxwd_ws","meanp_ws","minp_ws", "xwd_ws","tmax_ws","sumave_p")~"Climate",
                     name %in% c("area_sqkm", "max_elev","elev_range")~"Watershed",
                     name %in% c("cao_mean","mgo_mean","p_mean","s_mean","n_mean",
                      "kfct_ave","lprem_mean","prmh_ave","ucs_mean","bdh_ave")~"Geology",
                     T~"OTHER"))

gis_nat_df$name[gis_nat_df$MetricSubtype=="OTHER"]


phab_df_mod<-phab_metrics__df %>%
  select(-StationCode, -SampleDate) %>%
  pivot_longer(cols=c(-masterid, -MID_Date)) %>%
  inner_join(
    phab_met_lu %>%
      filter(Use) %>%
      transmute(
        name=PHAB_Metric,
        MetricType="PHAB",
        MetricSubtype=Type
      )
  )  # %>%
# filter(! name %in% c("DebrisSilt_Prox_SWAMP ")) #Nearly invariant

bugmets_df_mod<-bug_mets %>%
  filter(!str_detect(name, "_K")) %>%
  mutate(name_method = case_when(
    CollectionMethodCode=="TerrInvert_Trap_DryStreams"~paste0(name, "_Trap"),
    CollectionMethodCode=="TerrInvert_Veg_DryStreams"~paste0(name, "_Veg"),
    T~"OTHER"
  )) %>%
  inner_join( 
    bug_met_lu %>%
      filter(Use) %>%
      transmute(
        name=Metric,
        MetricType,
        MetricSubtype, 
        name_method=Metric_Method,
        MetricForm
      )
  )   %>% 
  select(masterid, name, value, MetricType, MetricSubtype, MID_Date, name_method, MetricForm)

bryomets_df_mod<- bryo_mets %>%
  filter(!str_detect(name, "_K")) %>%
  mutate(name_method = paste0(name, "_Bryo")) %>%
  inner_join( 
    bryo_met_lu %>%
      filter(Use) %>%
      transmute(
        name,
        MetricType,
        MetricSubtype, 
        name_method=paste0(name, "_Bryo"),
        MetricForm
      )
  )



ord_metrics_df<-
  ord_df %>%
  inner_join(bind_rows(dist_df_mod, gis_nat_df) ,
             relationship = "many-to-many") %>%
  bind_rows(
    inner_join(ord_df, relationship = "many-to-many",
               bind_rows(#dist_df_mod,
                 phab_df_mod,
                 bugmets_df_mod,
                 bryomets_df_mod) %>%
                 select(-SampleDate)
    )
  )



ord_metrics_df$MetricType %>% unique()  


ord_metrics_correlations<-ord_metrics_df %>%
  pivot_longer(cols=c(MDS1, MDS2), names_to = "Axis", values_to = "AxisScore") %>%
  group_by(Assemblage, MDS_DataType,  MetricType, MetricSubtype, MetricForm, name, name_method, Axis) %>%
  summarise(Pearsons_r = cor(x=value, y=AxisScore, use="pairwise.complete", method="pearson"),
            Spearmans_rho = cor(x=value, y=AxisScore, use="pairwise.complete", method="spearman")) %>%
  ungroup() %>%
  mutate(Pearsons_rsq= Pearsons_r^2,
         Spearmans_rhosq= Spearmans_rho^2)


ord_metrics_correlations %>%
  group_by(Assemblage) %>%
  slice_max(order_by = Spearmans_rhosq, n=3) %>%
  as.data.frame()

ord_metrics_correlations %>%
  filter(MetricType=="PHAB") %>%
  arrange(Pearsons_rsq) %>% as.data.frame() %>% tail()

ord_metrics_correlations_wide<-ord_metrics_correlations %>%
  select(-Pearsons_r, -Pearsons_rsq, -Spearmans_rhosq) %>%
  pivot_wider(names_from = Axis, values_from = Spearmans_rho) %>%
  mutate(RhoSq1 = MDS1^2, 
         RhoSq2 = MDS2^2) %>%
  rowwise() %>%
  mutate(MaxRhoSq = max(RhoSq1, RhoSq2),
         MetricType2 = case_when(MetricType %in% c("Arth_Trap","Arth_Veg","Bryo")~"Biological",
                                 # MetricType == "PHAB"~"Habitat",
                                 T~MetricType),
         Metric = str_remove(name, "_16")) %>%
  ungroup() 



ord_metrics_correlations_wide %>%
  filter(Assemblage=="Bryo",
         MDS_DataType =="PA",
         MetricType2=="Geospatial") %>%
  group_by(MetricSubtype) %>%
  # filter(MaxRhoSq>=.5)
  slice_max(order_by=MaxRhoSq, n=3)

text_size<-2

trap_abund_ordination_vectors_plot<-ggplot(data=ord_df %>%
                                             filter(MDS_DataType=="Abundance") %>%
                                             filter(Assemblage %in% c("Traps")),
                                           aes(x=MDS1, y=MDS2))+
  geom_point(aes(size=StressLevel), color="gray")+
  geom_segment(data=ord_metrics_correlations_wide %>%
                 filter(MDS_DataType=="Abundance") %>%
                 filter(Assemblage %in% c("Traps") ) %>%
                 filter(!MetricType %in% c("Arth_Veg","Bryo") ) %>%
                 #Hand-pick metrics
                 filter(name %in% c(
                   #Bio
                   "LinEpi_RelAbund","NonnatSynanth_RelAbund","CAF_Nat_Rich",
                   # "Hemip_RelAbund",# "TempTolAverage",#"Arth_Rich",
                   "BodySizeAverage","Detritivore_Rich","Ground_RelRich",# "BodySizeLargest"
                   #Geospatial
                   "urban_5k_16","developed_5k_16",
                   "roaddens_5k",#"code_21_5k_16",
                   "site_elev","ppt_00_09",#"temp_00_09",
                   #Habitat
                   "HumanActivity_Prox_SWAMP","Pct_CbBlBr","Pct_FastGHab","Pct_VgRip"
                 )),
               aes(x=0, y=0, xend=MDS1, yend=MDS2))+
  geom_label_repel(data=ord_metrics_correlations_wide %>%
                     filter(MDS_DataType=="Abundance") %>%
                     filter(Assemblage %in% c("Traps") ) %>%
                     filter(!MetricType %in% c("Arth_Veg","Bryo") ) %>%
                     #Hand-pick metrics
                     filter(name %in% c(
                       #Bio
                       "LinEpi_RelAbund","NonnatSynanth_RelAbund","CAF_Nat_Rich",
                       # "Hemip_RelAbund",# "TempTolAverage",#"Arth_Rich",
                       "BodySizeAverage","Detritivore_Rich","Ground_RelRich",# "BodySizeLargest"
                       #Geospatial
                       "urban_5k_16","developed_5k_16",
                       "roaddens_5k",#"code_21_5k_16",
                       "site_elev","ppt_00_09",#"temp_00_09",
                       #Habitat
                       "HumanActivity_Prox_SWAMP","Pct_CbBlBr","Pct_FastGHab","Pct_VgRip"
                     )), 
                   min.segment.length=1000,
                   aes(label=Metric),
                   position = position_nudge_center(x = 0.05, y = 0.1,
                                                    center_x = 0, center_y = 0),
                   vjust = "outward", hjust = "outward",
                   box.padding = .1, label.padding = .1,
                   fill = rgb(red = 1, green = 1, blue = 1, alpha = 0.75), label.size=NA,
                   size=text_size)+
  facet_wrap(~MetricType2)+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  theme_bw()+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  xlab("Rho with Axis 1")+ylab("Rho with Axis 2")
trap_abund_ordination_vectors_plot
ggsave(trap_abund_ordination_vectors_plot+
         theme(legend.position = "bottom"), filename="NotForGit/4_Bio data summary/trap_abund_ordination_vectors_plot.png",
       width=10, height = 6)
ggsave(trap_abund_ordination_vectors_plot+
         theme(legend.position = "bottom"), filename="NotForGit/4_Bio data summary/trap_abund_ordination_vectors_plot.svg",
       width=10, height = 6)

veg_abund_ordination_vectors_plot<-ggplot(data=ord_df %>%
                                            filter(MDS_DataType=="Abundance") %>%
                                            filter(Assemblage %in% c("Veg")),
                                          aes(x=MDS1, y=MDS2))+
  geom_point(aes(size=StressLevel), color="gray")+
  geom_segment(data=ord_metrics_correlations_wide %>%
                 filter(MDS_DataType=="Abundance") %>%
                 filter(Assemblage %in% c("Veg") ) %>%
                 filter(!MetricType %in% c("Arth_Trap","Bryo") ) %>%
                 #Hand-pick metrics
                 filter(name %in% c(
                   #Bio
                   "Nonnat_RelRich","DisperserGood_RelAbund",
                   
                   "CAF_Nat_RelAbund","BodySizeAverage",
                   #Geospatial
                   "urban_5k_16","developed_5k_16",
                   "roaddens_5k",#"code_21_5k_16",
                   "site_elev","ppt_00_09",#"temp_00_09",
                   #Habitat
                   "HumanActivity_Int",#"Pct_CbBlBr",
                   "Pct_FastGHab","Pct_VgRip","Mn_MaxHydHght"
                 )),
               aes(x=0, y=0, xend=MDS1, yend=MDS2))+
  geom_label_repel(data=ord_metrics_correlations_wide %>%
                     filter(MDS_DataType=="Abundance") %>%
                     filter(Assemblage %in% c("Veg") ) %>%
                     filter(!MetricType %in% c("Arth_Trap","Bryo") ) %>%
                     #Hand-pick metrics
                     filter(name %in% c(
                       #Bio
                       "Nonnat_RelRich","DisperserGood_RelAbund",
                       
                       # "TempTolAverage",
                       "CAF_Nat_RelAbund","BodySizeAverage",
                       #Geospatial
                       "urban_5k_16","developed_5k_16",
                       "roaddens_5k",#"code_21_5k_16",
                       "site_elev","ppt_00_09",#"temp_00_09",
                       #Habitat
                       "HumanActivity_Int",#"Pct_CbBlBr",
                       "Pct_FastGHab","Pct_VgRip","Mn_MaxHydHght"
                     )), 
                   min.segment.length=1000,
                   aes(label=Metric),
                   position = position_nudge_center(x = 0.05, y = 0.05,
                                                    center_x = 0, center_y = 0),
                   vjust = "outward", hjust = "outward",
                   box.padding = .1, label.padding = .1,
                   fill = rgb(red = 1, green = 1, blue = 1, alpha = 0.75), label.size=NA,
                   size=text_size)+
  facet_wrap(~MetricType2)+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  theme_bw()+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  xlab("Rho with Axis 1")+ylab("Rho with Axis 2")
veg_abund_ordination_vectors_plot
ggsave(veg_abund_ordination_vectors_plot+
         theme(legend.position = "bottom"), filename="NotForGit/4_Bio data summary/veg_abund_ordination_vectors_plot.png",
       width=10, height = 6)
ggsave(veg_abund_ordination_vectors_plot+
         theme(legend.position = "bottom"), filename="NotForGit/4_Bio data summary/veg_abund_ordination_vectors_plot.svg",
       width=10, height = 6)


bryo_pa_ordination_vectors_plot<-ggplot(data=ord_df %>%
                                          filter(MDS_DataType=="PA") %>%
                                          filter(Assemblage %in% c("Bryo")),
                                        aes(x=MDS1, y=MDS2))+
  geom_point(aes(size=StressLevel), color="gray")+
  geom_segment(data=ord_metrics_correlations_wide %>%
                 filter(MDS_DataType=="PA") %>%
                 filter(Assemblage %in% c("Bryo") ) %>%
                 filter(!MetricType %in% c("Arth_Trap","Arth_Veg") ) %>%
                 #Hand-pick metrics
                 filter(name %in% c(
                   #Bio
                   "indL_Mean",#Low = more shade-prefering
                   "T_DiurR_Min",#Min diurnal temperature range
                   "P_wetQ_Max", #Max mean monthly precip in wettest quarter
                   # "hab_sum_Low_Rich",#Richness of taxa preferring only 2 or fewer habitat types
                   "sub_ro_Rich", #Richness of taxa preferring rocky habitats
                   "indR_High_RelRich", #Rel richness of taxa prefering high pH soils
                   "indHM_Low_Rich", #Richness of taxa with low heavy metal tolerance
                   "indS_Low_Rich", #Richness of taxa with low salt tolerance
                   "FCAS_Rich", #richness of fugitive, colonist, or annual shuttle life strategy 
                   "Hem_e_Undisturbed_Rich", #Richness of taxa prefering undisturbed 
                   
                   
                   #Geospatial
                   "urban_5k_16","developed_5k_16",
                   "roaddens_5k",#"code_21_5k_16",
                   "site_elev","ppt_00_09",#"temp_00_09",
                   #Habitat
                   "HumanActivity_Int",
                   "AlgalMats_Prox_SWAMP",
                   "AnimalBurrow_Ext"
                 )),
               aes(x=0, y=0, xend=MDS1, yend=MDS2))+
  geom_label_repel(data=ord_metrics_correlations_wide %>%
                     filter(MDS_DataType=="PA") %>%
                     filter(Assemblage %in% c("Bryo") ) %>%
                     filter(!MetricType %in% c("Arth_Trap","Arth_Veg") ) %>%
                     #Hand-pick metrics
                     filter(name %in% c(
                       #Bio
                       "indL_Mean",#Low = more shade-prefering
                       "T_DiurR_Min",#Min diurnal temperature range
                       "P_wetQ_Max", #Max mean monthly precip in wettest quarter
                       # "hab_sum_Low_Rich",#Richness of taxa preferring only 2 or fewer habitat types
                       "sub_ro_Rich", #Richness of taxa preferring rocky habitats
                       "indR_High_RelRich", #Rel richness of taxa prefering high pH soils
                       "indHM_Low_Rich", #Richness of taxa with low heavy metal tolerance
                       "indS_Low_Rich", #Richness of taxa with low salt tolerance
                       "FCAS_Rich", #richness of fugitive, colonist, or annual shuttle life strategy 
                       "Hem_e_Undisturbed_Rich", #Richness of taxa prefering undisturbed 
                       
                       
                       #Geospatial
                       "urban_5k_16","developed_5k_16",
                       "roaddens_5k",#"code_21_5k_16",
                       "site_elev","ppt_00_09",#"temp_00_09",
                       #Habitat
                       "HumanActivity_Int",
                       "AlgalMats_Prox_SWAMP",
                       "AnimalBurrow_Ext"
                     )), 
                   min.segment.length=1000,
                   aes(label=Metric),
                   position = position_nudge_center(x = 0.05, y = 0.05,
                                                    center_x = 0, center_y = 0),
                   vjust = "outward", hjust = "outward",
                   box.padding = .1, label.padding = .1,
                   fill = rgb(red = 1, green = 1, blue = 1, alpha = 0.75), label.size=NA,
                   size=text_size)+
  facet_wrap(~MetricType2)+
  scale_size_manual(values=c(0.5,1,1.5), name="Disturbance level")+
  theme_bw()+
  coord_cartesian(xlim=c(-1,1), ylim=c(-1,1))+
  xlab("Rho with Axis 1")+ylab("Rho with Axis 2")
bryo_pa_ordination_vectors_plot
ggsave(bryo_pa_ordination_vectors_plot+
         theme(legend.position = "bottom"), filename="NotForGit/4_Bio data summary/bryo_pa_ordination_vectors_plot.png",
       width=10, height = 6)
ggsave(bryo_pa_ordination_vectors_plot+
         theme(legend.position = "bottom"), filename="NotForGit/4_Bio data summary/bryo_pa_ordination_vectors_plot.svg",
       width=10, height = 6)

nmds_3ass_corr_plot<-ggarrange(plotlist=
                                 list(
                                   trap_abund_ordination_vectors_plot+ ggtitle("Arthropods on the streambed"),
                                   veg_abund_ordination_vectors_plot+ ggtitle("Arthropods on vegetation"),
                                   bryo_pa_ordination_vectors_plot+ ggtitle("Bryophytes")),
                               ncol=1, common.legend = T, legend = "bottom")
ggsave(nmds_3ass_corr_plot,
       filename="NotForGit/4_Bio data summary/nmds_3ass_corr_plot.png",
       height=8.5, width=6.5,
       dpi=600)

ggsave(nmds_3ass_corr_plot,
       filename="NotForGit/4_Bio data summary/nmds_3ass_corr_plot.svg",
       height=8.5, width=6.5,
       dpi=600)


write_csv(ord_metrics_correlations_wide, file="NotForGit/4_Bio data summary/ord_metrics_correlations_wide.csv")

##############Correlation between bio metrics and other metrics#########

bio_env_raw_cor_df<-bugmets_df_mod %>%
  select(masterid, MID_Date,
         BioMetric=name, 
         BioMetric_Method=name_method,
         BioMetric_value=value, 
         BioMetricType=MetricType, 
         BioMetricSubtype=MetricSubtype, 
         BioMetricForm=MetricForm 
  ) %>%
  bind_rows(bryomets_df_mod %>%
              select(masterid, MID_Date,
                     BioMetric=name, 
                     BioMetric_Method=name_method,
                     BioMetric_value=value, 
                     BioMetricType=MetricType, 
                     BioMetricSubtype=MetricSubtype, 
                     BioMetricForm=MetricForm 
              )) %>%
  inner_join(
    dist_df_mod %>%
      bind_rows(gis_nat_df) %>%
      select(masterid, 
             EnvMetric=name, 
             EnvMetric_value=value,
             EnvMetricType=MetricType)
  ) %>%
  bind_rows(
    
    bugmets_df_mod %>%
      select(masterid, MID_Date,
             BioMetric=name, 
             BioMetric_Method=name_method,
             BioMetric_value=value, 
             BioMetricType=MetricType, 
             BioMetricSubtype=MetricSubtype, 
             BioMetricForm=MetricForm 
      ) %>%
      bind_rows(bryomets_df_mod %>%
                  select(masterid, MID_Date,
                         BioMetric=name, 
                         BioMetric_Method=name_method,
                         BioMetric_value=value, 
                         BioMetricType=MetricType, 
                         BioMetricSubtype=MetricSubtype, 
                         BioMetricForm=MetricForm 
                  )) %>%
      inner_join(phab_df_mod %>%
                   select(masterid, 
                          MID_Date,
                          EnvMetric=name, 
                          EnvMetric_value=value,
                          EnvMetricType=MetricType)
      )
  )

bio_env_raw_cor_df_summary<-
  bio_env_raw_cor_df %>%
  group_by(BioMetric,	BioMetric_Method,	BioMetricType,BioMetricSubtype,	BioMetricForm,
           EnvMetric,	EnvMetricType) %>%
  summarise(Pearsons_r = cor(x=EnvMetric_value, y=BioMetric_value, use="pairwise.complete", method="pearson"),
            Spearmans_rho = cor(x=EnvMetric_value, y=BioMetric_value, use="pairwise.complete", method="spearman")) %>%
  ungroup() %>%
  mutate(Pearsons_rsq= Pearsons_r^2,
         Spearmans_rhosq= Spearmans_rho^2)

bio_env_raw_cor_df_summary$EnvMetricType %>% unique()

# ggplot(bio_env_raw_cor_df_summary %>%
#          filter(BioMetricType=="Arth_Trap"), 
#        aes(y=BioMetric, x=EnvMetric))+
#   geom_tile(aes(fill=Spearmans_rhosq))+
#   scale_fill_viridis_c(name="Rhos-sq", limits=c(0,.6), breaks=c(0,0.2,0.4,0.6))+
#   facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
#   theme_bw()+
#   xlab("")+ylab("")+
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))

trap_bio_env_heatmap<-ggplot(bio_env_raw_cor_df_summary %>%
                               filter(BioMetricType=="Arth_Trap") %>%
                               mutate(BioMetricSubtype=case_when(BioMetricSubtype=="Thermal tolerance"~"Thermal\ntolerance",
                                                                 T~BioMetricSubtype)), 
                             aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0))
trap_bio_env_heatmap
ggsave(trap_bio_env_heatmap,
       filename="NotForGit/4_Bio data summary/trap_bio_env_heatmap.png",
       height=15, width=8)

trap_bio_env_heatmap_richrelrichother<-ggplot(bio_env_raw_cor_df_summary %>%
                                                filter(BioMetricType=="Arth_Trap") %>%
                                                filter(BioMetricForm%in% c("Richness","Relative richness","Other")) %>%
                                                mutate(BioMetricSubtype=case_when(BioMetricSubtype=="Thermal tolerance"~"Thermal tol.",
                                                                                  T~BioMetricSubtype)), 
                                              aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75), breaks=c(-.6, -.3,0,.3,.6))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0), 
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),
        legend.position = "bottom")
ggsave(trap_bio_env_heatmap_richrelrichother,
       filename="NotForGit/4_Bio data summary/trap_bio_env_heatmap_richrelrichother.png",
       # height=15, width=8
       height=10, width=10       )

trap_bio_env_heatmap_abund<-ggplot(bio_env_raw_cor_df_summary %>%
                                     filter(BioMetricType=="Arth_Trap") %>%
                                     filter(!BioMetricForm%in% c("Richness","Relative richness","Other")) %>%
                                     mutate(BioMetricSubtype=case_when(BioMetricSubtype=="Thermal tolerance"~"Thermal tol.",
                                                                       T~BioMetricSubtype)), 
                                   aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75), breaks=c(-.6, -.3,0,.3,.6))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0), 
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),
        legend.position = "bottom")
ggsave(trap_bio_env_heatmap_abund,
       filename="NotForGit/4_Bio data summary/trap_bio_env_heatmap_abund.png",
       # height=15, width=8
       height=10, width=10       )



veg_bio_env_heatmap<-ggplot(bio_env_raw_cor_df_summary %>%
                              filter(BioMetricType=="Arth_Veg") %>%
                              mutate(BioMetricSubtype=case_when(BioMetricSubtype=="Thermal tolerance"~"Thermal\ntolerance",
                                                                T~BioMetricSubtype)), 
                            aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0))
ggsave(veg_bio_env_heatmap,
       filename="NotForGit/4_Bio data summary/veg_bio_env_heatmap.png",
       height=10, width=8)


veg_bio_env_heatmap_richrelrichother<-ggplot(bio_env_raw_cor_df_summary %>%
                                               filter(BioMetricType=="Arth_Veg") %>%
                                               filter(BioMetricForm%in% c("Richness","Relative richness","Other")) %>%
                                               mutate(BioMetricSubtype=case_when(BioMetricSubtype=="Thermal tolerance"~"Thermal tol.",
                                                                                 T~BioMetricSubtype)), 
                                             aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75), breaks=c(-.6, -.3,0,.3,.6))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),
        strip.text.y = element_text(angle=0))
ggsave(veg_bio_env_heatmap_richrelrichother,
       filename="NotForGit/4_Bio data summary/veg_bio_env_heatmap_richrelrichother.png",
       height=10, width=10)


veg_bio_env_heatmap_abund<-ggplot(bio_env_raw_cor_df_summary %>%
                                    filter(BioMetricType=="Arth_Veg") %>%
                                    filter(!BioMetricForm%in% c("Richness","Relative richness","Other")) %>%
                                    mutate(BioMetricSubtype=case_when(BioMetricSubtype=="Thermal tolerance"~"Thermal tol.",
                                                                      T~BioMetricSubtype)), 
                                  aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75), breaks=c(-.6, -.3,0,.3,.6))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0),
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),)
ggsave(veg_bio_env_heatmap_abund,
       filename="NotForGit/4_Bio data summary/veg_bio_env_heatmap_abund.png",
       height=10, width=10)

bryo_bio_env_heatmap<-ggplot(bio_env_raw_cor_df_summary %>%
                               filter(BioMetricType=="Bryo") %>%
                               mutate(BioMetricSubtype=
                                        case_when(
                                          BioMetricSubtype=="Growth form"~"Growth\nform",
                                          BioMetricSubtype=="Life strategy"~"Life\nstrategy",
                                          BioMetricSubtype=="Generation time"~"Generation\ntime",
                                          BioMetricSubtype=="Precipitation preference"~"Climate\npreference",
                                          BioMetricSubtype=="Temperature preference"~"Climate\npreference",
                                          BioMetricSubtype=="Moisture preference"~"Moist\npreference",
                                          BioMetricSubtype=="Heavy metal tolerance"~"Metal\ntolerance",
                                          BioMetricSubtype=="Light preference"~"Light\npreference",
                                          BioMetricSubtype=="Nutrient preference"~"Nutrirent\npreference",
                                          BioMetricSubtype=="Acidity preference"~"Acidity\npreference",
                                          BioMetricSubtype=="Salt preference"~"Salinity\npreference",
                                          BioMetricSubtype=="Substrate preference"~"Substrate\npreference",
                                          BioMetricSubtype=="Taxonomic"~"Taxonomic",
                                          T~BioMetricSubtype)), 
                             aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),
        strip.text.y = element_text(angle=0))
ggsave(bryo_bio_env_heatmap,
       filename="NotForGit/4_Bio data summary/bryo_bio_env_heatmap.png",
       height=15, width=8)

bryo_bio_env_heatmap_richness<-  ggplot(bio_env_raw_cor_df_summary %>%
                                          filter(BioMetricType=="Bryo") %>%
                                          filter(BioMetricForm=="Richness")%>%
                                          mutate(BioMetricSubtype=
                                                   case_when(
                                                     BioMetricSubtype=="Growth form"~"Growth\nform",
                                                     BioMetricSubtype=="Life strategy"~"Life\nstrategy",
                                                     BioMetricSubtype=="Generation time"~"Gen time",
                                                     BioMetricSubtype=="Precipitation preference"~"Climate pref.",
                                                     BioMetricSubtype=="Temperature preference"~"Climate pref.",
                                                     BioMetricSubtype=="Moisture preference"~"Moist. pref.",
                                                     BioMetricSubtype=="Heavy metal tolerance"~"Metal tol.",
                                                     BioMetricSubtype=="Light preference"~"Light pref.",
                                                     BioMetricSubtype=="Nutrient preference"~"Nutrirent pref.",
                                                     BioMetricSubtype=="Acidity preference"~"Acidity pref.",
                                                     BioMetricSubtype=="Salt preference"~"Salinity pref.",
                                                     BioMetricSubtype=="Substrate preference"~"Substrate pref.",
                                                     BioMetricSubtype=="Taxonomic"~"Taxonomic",
                                                     T~BioMetricSubtype)), 
                                        aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75), breaks=c(-.6,-.3,0,.3,.6))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0),
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),
        legend.position ="bottom")
ggsave(bryo_bio_env_heatmap_richness,
       filename="NotForGit/4_Bio data summary/bryo_bio_env_heatmap_richness.png",
       height=10, width=10)

bryo_bio_env_heatmap_relrichness<-  ggplot(bio_env_raw_cor_df_summary %>%
                                             filter(BioMetricType=="Bryo") %>%
                                             filter(BioMetricForm=="Relative richness")%>%
                                             mutate(BioMetricSubtype=
                                                      case_when(
                                                        BioMetricSubtype=="Growth form"~"Growth\nform",
                                                        BioMetricSubtype=="Life strategy"~"Life\nstrategy",
                                                        BioMetricSubtype=="Generation time"~"Gen time",
                                                        BioMetricSubtype=="Precipitation preference"~"Climate pref.",
                                                        BioMetricSubtype=="Temperature preference"~"Climate pref.",
                                                        BioMetricSubtype=="Moisture preference"~"Moist. pref.",
                                                        BioMetricSubtype=="Heavy metal tolerance"~"Metal tol.",
                                                        BioMetricSubtype=="Light preference"~"Light pref.",
                                                        BioMetricSubtype=="Nutrient preference"~"Nutrirent pref.",
                                                        BioMetricSubtype=="Acidity preference"~"Acidity pref.",
                                                        BioMetricSubtype=="Salt preference"~"Salinity pref.",
                                                        BioMetricSubtype=="Substrate preference"~"Substrate pref.",
                                                        BioMetricSubtype=="Taxonomic"~"Taxonomic",
                                                        T~BioMetricSubtype)), 
                                           aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75), breaks=c(-.6,-.3,0,.3,.6))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0),
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),
        legend.position ="bottom")
ggsave(bryo_bio_env_heatmap_relrichness,
       filename="NotForGit/4_Bio data summary/bryo_bio_env_heatmap_relrichness.png",
       height=10, width=10)


bryo_bio_env_heatmap_other<-  ggplot(bio_env_raw_cor_df_summary %>%
                                       filter(BioMetricType=="Bryo") %>%
                                       filter(BioMetricForm=="Other")%>%
                                       mutate(BioMetricSubtype=
                                                case_when(
                                                  BioMetricSubtype=="Growth form"~"Growth\nform",
                                                  BioMetricSubtype=="Life strategy"~"Life\nstrategy",
                                                  BioMetricSubtype=="Generation time"~"Gen time",
                                                  BioMetricSubtype=="Precipitation preference"~"Climate pref.",
                                                  BioMetricSubtype=="Temperature preference"~"Climate pref.",
                                                  BioMetricSubtype=="Moisture preference"~"Moist. pref.",
                                                  BioMetricSubtype=="Heavy metal tolerance"~"Metal tol.",
                                                  BioMetricSubtype=="Light preference"~"Light pref.",
                                                  BioMetricSubtype=="Nutrient preference"~"Nutrirent pref.",
                                                  BioMetricSubtype=="Acidity preference"~"Acidity pref.",
                                                  BioMetricSubtype=="Salt preference"~"Salinity pref.",
                                                  BioMetricSubtype=="Substrate preference"~"Substrate pref.",
                                                  BioMetricSubtype=="Taxonomic"~"Taxonomic",
                                                  T~BioMetricSubtype)), 
                                     aes(y=BioMetric, x=EnvMetric))+
  geom_tile(aes(fill=Spearmans_rho))+
  scale_fill_gradient2(name="Rho",
                       low="#e66101", mid="white", high="#5e3c99",
                       limits=c(-.75, .75), breaks=c(-.6,-.3,0,.3,.6))+
  facet_grid(BioMetricSubtype~EnvMetricType, scales="free",space="free", drop=T )+
  theme_bw()+
  xlab("")+ylab("")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0, size=6),
        axis.text.y=element_text(size=6),
        strip.text.y = element_text(angle=0),
        strip.clip ="off", 
        strip.background.y = element_blank(), strip.text.y.right = element_text(hjust=0),
        legend.position ="bottom")
ggsave(bryo_bio_env_heatmap_other,
       filename="NotForGit/4_Bio data summary/bryo_bio_env_heatmap_other.png",
       height=10, width=10)

bio_env_raw_cor_df_summary %>%
  filter(BioMetricType=="Bryo") %>% select(BioMetricSubtype) %>%
  distinct()

bio_env_raw_cor_df_summary %>%
  filter(BioMetricType=="Bryo", BioMetricSubtype=="Life strategy") %>%
  filter(BioMetric %in% c("FCAS_Rich","FCAS_RelRich")) %>%
  filter(EnvMetric=="HumanActivity_Prox_SWAMP") %>%
  as.data.frame()%>% head()

bio_env_raw_cor_df %>%
  filter(BioMetricType=="Bryo", BioMetricSubtype=="Life strategy") %>%
  filter(BioMetric %in% c("FCAS_Rich","FCAS_RelRich")) %>%
  filter(EnvMetric=="HumanActivity_Prox_SWAMP") %>%
  ggplot(aes(x=EnvMetric_value, y=BioMetric_value))+
  geom_point()+geom_smooth() + facet_wrap(vars(BioMetric), scales="free")


write_csv(
  bio_env_raw_cor_df_summary %>%
    left_join(
      phab_met_lu %>%
        select(EnvMetric=PHAB_Metric,
               PhabMetricSubtype=Type)
    )
  , file="NotForGit/4_Bio data summary/bio_env_raw_cor_df_summary.csv")




bio_env_raw_cor_df_summary$Spearmans_rhosq

#######


#########ANOVAS#########


bio_ref_df<-bugmets_df_mod %>%
  select(masterid, MID_Date,
         BioMetric=name, 
         BioMetric_Method=name_method,
         BioMetric_value=value, 
         BioMetricType=MetricType, 
         BioMetricSubtype=MetricSubtype, 
         BioMetricForm=MetricForm 
  ) %>%
  bind_rows(bryomets_df_mod %>%
              select(masterid, MID_Date,
                     BioMetric=name, 
                     BioMetric_Method=name_method,
                     BioMetric_value=value, 
                     BioMetricType=MetricType, 
                     BioMetricSubtype=MetricSubtype, 
                     BioMetricForm=MetricForm 
              )) %>%
  inner_join(
    disturbance_screening_df %>%
      transmute(masterid, 
                StressLevel=factor(StressLevel, levels=c("Low","Intermediate","High")))
  ) 


bio_ref_df_anovas_summary<- bio_ref_df %>%
  select(BioMetric, BioMetric_Method, BioMetricType, BioMetricSubtype, BioMetricForm) %>%
  distinct()

bio_ref_df_anovas<-lapply(bio_ref_df_anovas_summary$BioMetric_Method, function(x){
  xdf = bio_ref_df %>%
    filter(BioMetric_Method==x) %>%
    select(BioMetric_value, StressLevel) %>%
    na.omit()
  lm(BioMetric_value ~ StressLevel, data=xdf)
})




bio_ref_df_anovas_summary$adj_rsq<-sapply(bio_ref_df_anovas, function(x){
  xx=summary(x)
  xx$adj.r.squared
})
bio_ref_df_anovas_summary$F_stat<-sapply(bio_ref_df_anovas, function(x){
  xx=summary(x)
  xx$fstatistic[1]
})

bio_ref_df_anovas_summary$F_stat_numdf<-sapply(bio_ref_df_anovas, function(x){
  xx=summary(x)
  xx$fstatistic[2]
})

bio_ref_df_anovas_summary$F_stat_denomdf<-sapply(bio_ref_df_anovas, function(x){
  xx=summary(x)
  xx$fstatistic[3]
})

bio_ref_df_anovas_summary$F_stat_p<-
  1-pf(bio_ref_df_anovas_summary$F_stat,
       bio_ref_df_anovas_summary$F_stat_numdf,
       bio_ref_df_anovas_summary$F_stat_denomdf)

bio_ref_df_anovas_summary$t_stat_low_v_int<-
  sapply(bio_ref_df_anovas_summary$BioMetric_Method, function(x){
    xdf = bio_ref_df %>%
      filter(BioMetric_Method==x) %>%
      filter(StressLevel %in% c("Low","Intermediate")) %>%
      select(BioMetric_value, StressLevel) %>%
      na.omit()
    if (all(dist(xdf$BioMetric_value) == 0)) {
      return(NA)
    }
    else
      t_x=t.test(BioMetric_value ~ StressLevel, data=xdf)
    t_x$statistic
  })

bio_ref_df_anovas_summary$t_stat_low_v_int_p<-  
  sapply(bio_ref_df_anovas_summary$BioMetric_Method, function(x){
    xdf = bio_ref_df %>%
      filter(BioMetric_Method==x) %>%
      filter(StressLevel %in% c("Low","Intermediate")) %>%
      select(BioMetric_value, StressLevel) %>%
      na.omit()
    if (all(dist(xdf$BioMetric_value) == 0)) {
      return(NA)
    }
    else
      t_x=t.test(BioMetric_value ~ StressLevel, data=xdf)
    t_x$p.value
  })

bio_ref_df_anovas_summary$t_stat_lowint_v_high<-
  sapply(bio_ref_df_anovas_summary$BioMetric_Method, function(x){
    xdf = bio_ref_df %>%
      filter(BioMetric_Method==x) %>%
      mutate(StressLevel2 =case_when(StressLevel %in% c("Low","Intermediate")~"Low_Int",
                                     T~"High")) %>%
      select(BioMetric_value, StressLevel2) %>%
      na.omit()
    if (all(dist(xdf$BioMetric_value) == 0)) {
      return(NA)
    }
    else
      t_x=t.test(BioMetric_value ~ StressLevel2, data=xdf)
    t_x$statistic
  })  


bio_ref_df_anovas_summary$t_stat_lowint_v_high_p<-  
  sapply(bio_ref_df_anovas_summary$BioMetric_Method, function(x){
    xdf = bio_ref_df %>%
      filter(BioMetric_Method==x) %>%
      mutate(StressLevel2 =case_when(StressLevel %in% c("Low","Intermediate")~"Low_Int",
                                     T~"High")) %>%
      select(BioMetric_value, StressLevel2) %>%
      na.omit()
    if (all(dist(xdf$BioMetric_value) == 0)) {
      return(NA)
    }
    else
      t_x=t.test(BioMetric_value ~ StressLevel2, data=xdf)
    t_x$p.value
  })  


bio_ref_df_anovas_summary$t_stat_low_v_inthigh<-
  sapply(bio_ref_df_anovas_summary$BioMetric_Method, function(x){
    xdf = bio_ref_df %>%
      filter(BioMetric_Method==x) %>%
      mutate(StressLevel2 =case_when(StressLevel %in% c("High","Intermediate")~"IntHigh",
                                     T~"Low")) %>%
      select(BioMetric_value, StressLevel2) %>%
      na.omit()
    if (all(dist(xdf$BioMetric_value) == 0)) {
      return(NA)
    }
    else
      t_x=t.test(BioMetric_value ~ StressLevel2, data=xdf)
    t_x$statistic
  })  


bio_ref_df_anovas_summary$t_stat_low_v_inthigh_p<-  
  sapply(bio_ref_df_anovas_summary$BioMetric_Method, function(x){
    xdf = bio_ref_df %>%
      filter(BioMetric_Method==x) %>%
      mutate(StressLevel2 =case_when(StressLevel %in% c("High","Intermediate")~"IntHigh",
                                     T~"Low")) %>%
      select(BioMetric_value, StressLevel2) %>%
      na.omit()
    if (all(dist(xdf$BioMetric_value) == 0)) {
      return(NA)
    }
    else
      t_x=t.test(BioMetric_value ~ StressLevel2, data=xdf)
    t_x$p.value
  })  

# ggplot(bio_ref_df_anovas_summary, aes(x=abs(t_stat_lowint_v_high), y=abs(t_stat_low_v_int)))+
# geom_point()+
# geom_abline(slope=1, linteype="dotted")

#How many metrics had a significant (p<0.01) relationship with stress levels?
bio_ref_df_anovas_summary %>%
  mutate(F_stat_p = case_when(is.na(F_stat_p)~Inf,T~F_stat_p)) %>%
  group_by(
    F_stat_p  <0.01,
  ) %>%
  tally() 



group_by(BioMetricType, BioMetricForm) %>%
  summarize(n_tot=length(BioMetric),
            n_sig_01=sum(F_stat_p<0.01)) %>%
  mutate(PCT=n_sig_01/n_tot) %>%
  select(-n_tot, -n_sig_01) %>%
  pivot_wider(names_from=BioMetricForm, values_from = PCT) %>%
  write_clip()
#How many metrics had a significant (p<0.01) relationship with stress levels, by assemblage?
bio_ref_df_anovas_summary %>%
  mutate(F_stat_p = case_when(is.na(F_stat_p)~Inf,T~F_stat_p)) %>%
  group_by(BioMetricType 
           # F_stat_p  <0.01,
  ) %>%
  summarize(n_tot=length(BioMetric),
            n_sig_01=sum(F_stat_p<0.01)) %>%
  mutate(PCT=n_sig_01/n_tot) %>%
  select(-n_tot, -n_sig_01) #%>%
# pivot_wider(names_from=BioMetricForm, values_from = PCT) 

ggplot(data=bio_ref_df_anovas_summary %>%
         mutate(F_stat_p = case_when(is.na(F_stat_p)~Inf,T~F_stat_p)), 
       aes(x=F_stat))+
  geom_histogram(aes(fill=F_stat_p<0.01))+
  facet_wrap(vars(BioMetricType), ncol=1)



#How many metrics had different mean values at low vs intermediate?
bio_ref_df_anovas_summary %>%
  group_by(BioMetricType, t_stat_low_v_int_p<0.05  )  %>%
  tally()
bio_ref_df_anovas_summary %>%
  filter(t_stat_low_v_int_p<0.05)

#How many metrics had different mean values at low + int vs high
bio_ref_df_anovas_summary %>%
  group_by(t_stat_lowint_v_high_p<0.01  )  %>%
  tally()

bio_ref_df_anovas_summary %>%
  group_by(
    # BioMetricType,
    lowint_high_sig=t_stat_low_v_int_p <0.01,
    # low_int_sig=t_stat_lowint_v_high_p <0.01,
    # low_inthigh_sig=t_stat_low_v_inthigh_p <0.01
  ) %>%
  tally() %>%
  na.omit() #%>%
# pivot_wider(names_from=low_int_sig, values_from = n)

bio_ref_df_anovas_summary  %>% filter(t_stat_low_v_int_p <0.01)

# bio_ref_df %>%
#   filter(BioMetric_Method=="TempTolHigh_RelRich_Veg") %>%
#   filter(StressLevel %in% c("Low","Intermediate")) %>%
#   group_by(StressLevel) %>%
#   summarize(Min=min(BioMetric_value, na.rm=T),
#             Maxn=max(BioMetric_value, na.rm=T))



bio_ref_df_anovas_summary %>%
  group_by(BioMetricType) %>%
  slice_max(F_stat, n=3)

bio_ref_df_anovas_summary %>%
  group_by(BioMetricType) %>%
  summarize(n_tot=length(BioMetric),
            n_sig_01=sum(F_stat_p<0.01)) %>%
  mutate(n_sig_01/n_tot)

#Responsiveness of metrics to stress levels by assemblage
bio_ref_df_anovas_summary %>%
  mutate(F_stat_p = case_when(is.na(F_stat_p)~Inf,T~F_stat_p)) %>%
  group_by(BioMetricType, BioMetricForm) %>%
  summarize(n_tot=length(BioMetric),
            n_sig_01=sum(F_stat_p<0.01)) %>%
  mutate(PCT=n_sig_01/n_tot) %>%
  select(-n_tot, -n_sig_01) %>%
  pivot_wider(names_from=BioMetricForm, values_from = PCT) %>%
  write_clip()

#Responsiveness of metrics to stress levels by assemblage and metric subtype

bio_ref_df_anovas_summary %>%
  mutate(F_stat_p = case_when(is.na(F_stat_p)~Inf,T~F_stat_p)) %>%
  group_by(BioMetricType, BioMetricSubtype) %>%
  summarize(n_tot=length(BioMetric),
            n_sig_01=sum(F_stat_p<0.01)) %>%
  mutate(PCT=n_sig_01/n_tot) %>%
  select(-n_tot, -n_sig_01) %>%
  pivot_wider(names_from=BioMetricType, values_from = PCT) %>%
  write_clip()


junk %>% summary()
setdiff(bio_ref_df_anovas_summary$BioMetric[bio_ref_df_anovas_summary$BioMetricType=="Arth_Trap"],
        bio_ref_df_anovas_summary$BioMetric[bio_ref_df_anovas_summary$BioMetricType=="Arth_Veg"])

#Select these metrics to plot
bio_ref_df_anovas_summary %>%
  filter(F_stat>5) %>%
  group_by(BioMetricType,BioMetricSubtype ) %>%
  slice_max(F_stat, n=3) %>%
  # arrange(-F_stat) %>%
  filter(BioMetricType=="Arth_Veg") %>%
  as.data.frame()

sel_mets<-c("BodySizeAverage_Trap","Nonnat_RelRich_Trap","Ground_RelAbund_Trap",
            "BodySizeAverage_Veg","Nonnat_RelAbund_Veg","CAF_Nat_RelAbund_Veg",#"TempTolAverage_Veg",
            "FCAS_Rich_Bryo",
            "indHM_Low_Rich_Bryo",
            # "Hemeroby_Mean_Bryo",
            "hab_sum_Low_Rich_Bryo")
boxplot_plot_dat<-bio_ref_df %>%
  filter(BioMetric_Method %in% sel_mets) %>% 
  na.omit() %>%
  mutate(Assemblage = case_when(BioMetricType=="Arth_Trap"~"Arthropods on the streambed",
                                BioMetricType=="Arth_Veg"~"Arthropods on vegetation",
                                BioMetricType=="Bryo"~"Bryophytes"))



# 
# library(ggh4x)
# ggplot(data=boxplot_plot_dat ,
#        aes(x=StressLevel, y=BioMetric_value))+
#   geom_boxplot()+
#   # facet_wrap(vars(Assemblage, BioMetric), scales="free_y")+
#   # facet_nested(vars(Assemblage,BioMetric), scales="free_y")+
#   facet_nested(.~Assemblage+BioMetric, scales = "free", independent = "y")+
#   theme_bw()

metric_responsiveness_boxplot<-ggplot(data=boxplot_plot_dat ,
                                      aes(x=StressLevel, y=BioMetric_value))+
  geom_boxplot()+
  facet_wrap(vars(Assemblage, BioMetric), scales="free_y")+
  theme_bw()+
  xlab("Stress level") + ylab("Metric value")
ggsave(metric_responsiveness_boxplot, filename="NotForGit/4_Bio data summary/metric_responsiveness_boxplot.png",
       width=6.5, height=6)

scatterplot_dat<-
  bio_env_raw_cor_df %>%
  filter(BioMetric_Method %in% sel_mets) %>%
  filter(EnvMetric %in% c("HumanActivity_Prox_SWAMP",
                          # "HumanActivity_Ext","HumanActivity_Int",
                          # "developed_1k_16","developed_5k_16",
                          "developed_ws_16",
                          # "roaddens_1k","roaddens_5k",
                          "roaddens_ws")) %>%
  mutate(Assemblage = case_when(BioMetricType=="Arth_Trap"~"Arthropods on the streambed",
                                BioMetricType=="Arth_Veg"~"Arthropods on vegetation",
                                BioMetricType=="Bryo"~"Bryophytes")) %>%
  left_join(disturbance_screening_df %>% select(masterid, StressLevel))




metric_responsiveness_scatterplot_gis<-
  ggplot(data=scatterplot_dat %>% 
           filter(#BioMetricType=="Arth_Trap",
             EnvMetric=="developed_ws_16"),
         aes(x=EnvMetric_value, y=BioMetric_value))+
  geom_point(aes(color=StressLevel))+
  geom_smooth()+
  facet_wrap(vars(Assemblage,BioMetric), scales="free_y")+
  # facet_grid(BioMetric_Method~, scales="free", switch="both")+
  # facet_wrap(BioMetric~Method)
  scale_color_manual(values=c("#d7191c","#fecc5c","#2c7bb6"),
                     name="Disturbance level")+
  theme_bw() +  
  xlab("developed_ws_16")+ylab("")+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        legend.position = "bottom")
ggsave(metric_responsiveness_scatterplot_gis, filename="NotForGit/4_Bio data summary/metric_responsiveness_scatterplot_gis.png",
       width=6.5, height=6)


metric_responsiveness_scatterplot_phab<-
  ggplot(data=scatterplot_dat %>% 
           filter(#BioMetricType=="Arth_Trap",
             EnvMetric=="HumanActivity_Prox_SWAMP"),
         aes(x=EnvMetric_value, y=BioMetric_value))+
  geom_point(aes(color=StressLevel))+
  geom_smooth()+
  facet_wrap(vars(Assemblage,BioMetric), scales="free_y")+
  # facet_grid(BioMetric_Method~, scales="free", switch="both")+
  # facet_wrap(BioMetric~Method)
  scale_color_manual(values=c("#d7191c","#fecc5c","#2c7bb6"),
                     name="Disturbance level")+
  theme_bw() +  
  xlab("HumanActivity_Prox_SWAMP")+
  ylab("")+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        legend.position = "bottom")
ggsave(metric_responsiveness_scatterplot_phab, filename="NotForGit/4_Bio data summary/metric_responsiveness_scatterplot_phab.png",
       width=6.5, height=6)


#####Role of natural vars in metrics

gis_nat_vars<-c("area_sqkm","site_elev",# "max_elev","elev_range",
                "ppt_00_09","temp_00_09", "lst32ave","maxwd_ws","meanp_ws","minp_ws", "xwd_ws",
                "tmax_ws","sumave_p",
                "cao_mean","mgo_mean","p_mean","s_mean","n_mean",
                "kfct_ave","lprem_mean","prmh_ave","ucs_mean","bdh_ave")

rf_ref_dat<-bio_env_raw_cor_df<-bugmets_df_mod %>%
  select(masterid, MID_Date,
         BioMetric=name, 
         BioMetric_Method=name_method,
         BioMetric_value=value, 
         BioMetricType=MetricType, 
         BioMetricSubtype=MetricSubtype, 
         BioMetricForm=MetricForm 
  ) %>%
  bind_rows(bryomets_df_mod %>%
              select(masterid, MID_Date,
                     BioMetric=name, 
                     BioMetric_Method=name_method,
                     BioMetric_value=value, 
                     BioMetricType=MetricType, 
                     BioMetricSubtype=MetricSubtype, 
                     BioMetricForm=MetricForm 
              )) %>%
  left_join(
    gis_df_socal %>%
      select(masterid, gismetric, gismetricresult) %>%
      filter(gismetric %in% gis_nat_vars) %>%
      mutate(gismetricresult=as.numeric(gismetricresult)) %>%
      pivot_wider(names_from=gismetric, values_from=gismetricresult)
  ) %>%
  left_join(disturbance_screening_df %>% select(masterid, StressLevel))

library(randomForest)
my_nat_rfs<-lapply(bio_ref_df_anovas_summary$BioMetric_Method, function(met.i){
  mydf = rf_ref_dat %>%
    filter(BioMetric_Method==met.i,
           StressLevel %in% c("Low","Intermediate")) %>%
    select(BioMetric_value, all_of(gis_nat_vars)) %>%
    filter(!is.na(BioMetric_value))
  set.seed(100)
  randomForest(x=mydf %>% select(all_of(gis_nat_vars)),
               y=mydf$BioMetric_value,
               importance=T, proximity=T, ntree=1000)
})


bio_ref_df_anovas_summary$RF_pseudoRsq = sapply(1:nrow(bio_ref_df_anovas_summary), function(i){
  myrf=my_nat_rfs[[i]]
  myrf$rsq[1000]
})

bio_ref_df_anovas_summary %>% slice_max(order_by=RF_pseudoRsq, n=5) %>%
  as.data.frame()
