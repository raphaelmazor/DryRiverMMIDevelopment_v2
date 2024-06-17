library(tidyverse)
library(sf)
library(clipr)

########Import data from previous step########
all_bio_data_usable2<-read_csv( "NotForGit/1_Data import and assembly/all_bio_data_usable2.csv")
all_hab_data<-read_csv( "NotForGit/1_Data import and assembly/all_hab_data.csv")
gis_df_socal<-read_csv( "NotForGit/1_Data import and assembly/gis_df_socal.csv")
lu_stations_df_socal<-read_csv( "NotForGit/1_Data import and assembly/lu_stations_df_socal.csv")
bmi_df_socal<-read_csv( "NotForGit/1_Data import and assembly/bmi_df_socal.csv") %>%
  inner_join(lu_stations_df_socal %>%
               select(stationcode=stationid, masterid))
algae_df_socal<-read_csv( "NotForGit/1_Data import and assembly/algae_df_socal.csv")%>%
  inner_join(lu_stations_df_socal %>%
               select(stationcode=stationid, masterid))
sites_dates<-read_csv("NotForGit/1_Data import and assembly/sites_dates.csv")
bug_mets<-read_csv("Data/BioData/Metrics/arthropod_metrics_04242024.csv")  %>%
  inner_join(lu_stations_df_socal %>%
               select(StationCode=stationid, masterid))
bryo_mets<-read_csv("Data/BioData/Metrics/bryo_metrics_04122024.csv") %>%
  inner_join(lu_stations_df_socal %>%
               select(StationCode=stationid, masterid))
phab_mets<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_04092024.csv") %>%
  inner_join(lu_stations_df_socal %>%
               select(StationCode=stationid, masterid))

phab_mets %>%
  # filter(StationCode=="911GSCAPV")# %>%
  # filter(stationcode=="911GSCAPV")# %>%
  filter(masterid=="911M25305")




#Metric lookup tables
gis_lu<-read_csv("Metadata/gis_data_lu.csv")
#Format for report
gis_lu %>%
  mutate(gismetricname_short=factor(gismetricname_short, levels=gismetricname_short %>% unique())) %>%
  filter(Use) %>%
  mutate(gismetricdescription=str_replace_all(gismetricdescription,"\n"," ")) %>%
  # mutate(gismetricdescription=str_replace_all(gismetricdescription," .",".")) %>%
  select(Type, Subtype, Metric=gismetricname_short, Description=gismetricdescription, gismetric) %>%
  distinct() %>%
  group_by(Type, Subtype, Metric, Description) %>%
  tally() %>%
  mutate(Scales=case_when(n>1~"1k, 5k, ws",
                          Metric %in% c("MINES")~"5k",
                          Metric %in% c("PPT_00_09","TEMP_00_09","New_Lat","New_Long",
                                        "SITE_ELEV")~"point",
                          T~"ws")) %>%
  ungroup() %>%
  select(-n)# %>%
  write_clip()
  
  

#Make a table of gismetrics
# gis_df_socal %>%
#   select(gismetric, gismetricname_short, gismetricscale, gismetricdescription) %>%
#   mutate(gismetricdescription=str_replace_all(gismetricdescription,"\n"," ")) %>%
#   distinct() %>% 
#   group_by(gismetricname_short, gismetricdescription) %>% tally(name="n_scales") %>%
#   mutate(Type = case_when(gismetricname_short %in% 
#                             c("AGUR_01","AGUR_06","AGUR_11","AGUR_16",
#                               "AG_01","AG_06","AG_11","AG_16",
#                               "UR_01","UR_06","UR_11","UR_16",
#                               "CD21_01","CD21_06","CD21_11","CD21_16",
#                               "CNL_PI_PCT",
#                               "GRVL_DENS","GRVL_MINES","MINES","MINE_DENS",
#                               "NRST_DAM",
#                               "PVD_INT","RDRRDEN"
#                               )~"Human activity",
#                           gismetricname_short %in%  
#                             c("AREA_SQKM","ELEV_RANGE","EVI_MaxAve","MAX_ELEV","SITE_ELEV",
#                               "AtmCa","AtmMg","AtmSO4",
#                               "BDH_AVE","KFCT_AVE","LPREM_mean","PRMH_AVE",
#                               "LST32AVE","MAXWD_WS", "MEANP_WS","MINP_WS",
#                               "PPT_00_09","TEMP_00_09",
#                               "CaO_Mean","MgO_Mean","N_Mean", "P_MEAN",
#                               "CondQR01","CondQR10","CondQR50","CondQR90","CondQR99",
#                               "PSA6","PSA8")~"Natural",
#                           T~"OTHER"))
  # 
  # clipr::write_clip()
gis_df_socal<-gis_df_socal %>%
  filter(gismetricname_short %in% gis_lu$gismetricname_short[gis_lu$Use])


all_hab_data
##########Assemble data inventory##########



#How many sites total?
sites_dates$masterid %>% unique() %>% length()

#How many sites per region?
sites_dates %>% 
  select(StationCode, RB) %>%
  distinct() %>%
  group_by(RB) %>%
  tally()

#Add data presence vector

sites_dates_samples<-sites_dates %>%
  left_join(
    #Bryophytes
    all_bio_data_usable2 %>%
      filter(CollectionMethodName=="Bryo_DryStreams") %>%
      select(StationCode, SampleDate) %>%
      distinct() %>%
      mutate(Bryos=T)
  ) %>%
  left_join(
    #Terrestrial Arthropods on the Streambed
    all_bio_data_usable2 %>%
      filter(CollectionMethodName=="TerrInvert_Trap_DryStreams") %>%
      select(StationCode, SampleDate) %>%
      distinct() %>%
      mutate(TAS=T)
  ) %>%
  left_join(
    #Terrestrial Arthropods on vegetation
    all_bio_data_usable2 %>%
      filter(CollectionMethodName=="TerrInvert_Veg_DryStreams") %>%
      select(StationCode, SampleDate) %>%
      distinct() %>%
      mutate(TAV=T)
  ) %>%
  mutate(Bryos = case_when(is.na(Bryos)~F,T~T),
         TAS = case_when(is.na(TAS)~F,T~T),
         TAV = case_when(is.na(TAV)~F,T~T))

#How many SITES with each data type?
sites_dates_samples %>%
  pivot_longer(cols=c("Bryos","TAS","TAV")) %>%
  filter(value) %>%
  select(name, StationCode) %>%
  distinct() %>%
  group_by(name) %>%
  tally()

#How many SAMPLES with each data type?
sites_dates_samples %>%
  pivot_longer(cols=c("Bryos","TAS","TAV")) %>%
  filter(value) %>%
  group_by(name) %>%
  tally()

#How many REPLICATE SAMPLES with each data type?
sites_dates_samples %>%
  pivot_longer(cols=c("Bryos","TAS","TAV")) %>%
  filter(value) %>%
  group_by(name, StationCode) %>%
  tally() %>%
  filter(n>1) %>%
  group_by(name) %>%
  tally()


#How many SITES with each data type by RB?
sites_dates_samples %>%
  pivot_longer(cols=c("Bryos","TAS","TAV")) %>%
  filter(value) %>%
  select(RB, name, StationCode) %>%
  distinct() %>%
  group_by(RB, name) %>%
  tally() %>%
  pivot_wider(names_from = name, values_from = n, values_fill = 0)

#How many SAMPLES with each data type by RB?
sites_dates_samples %>%
  pivot_longer(cols=c("Bryos","TAS","TAV")) %>%
  filter(value) %>%
  group_by(RB, name) %>%
  tally() %>%
  pivot_wider(names_from = name, values_from = n, values_fill = 0)


##################Mapping##################

smc_sf<-st_read("NotForGit/Shapefiles/SMCSheds2009/SMCSheds2009.shp")
rb_sf<-st_read("NotForGit/Shapefiles/RWQCB/rwqcbnda.shp")
rb_socal_sf <-rb_sf %>%
  filter( RB %in% c(4, 8, 9))
huc8_sf<-st_read("NotForGit/Shapefiles/HUC08/CalWater_SoCal.shp")
counties_sf<-st_read("NotForGit//Shapefiles/SoCalCounties/SoCal_Counties.shp")

sites_sf<-sites_dates_samples %>%
  pivot_longer(cols=c("Bryos","TAS","TAV")) %>%
  filter(value) %>%
  st_as_sf(coords=c("Longitude","Latitude"),
           crs=4326) %>%
  mutate(name2=case_when(name=="Bryos"~"Bryophytes",
                         name=="TAS"~"Terrestrial Arthropods\nfrom the Streambed",
                         name=="TAV"~"Terrestrial Arthropods\nfrom Vegetation",
                         T~"OTHER"))

map_of_sites<-ggplot()+
  geom_sf(data=counties_sf %>%
            st_transform(crs=st_crs(smc_sf)), linewidth=.25)+
  geom_sf(data=rb_socal_sf %>%
            st_transform(crs=st_crs(smc_sf)), color="black", linewidth=.5,
          fill="gray25") +
  geom_sf(data=huc8_sf %>%
            st_transform(crs=st_crs(smc_sf)), color="gray75", linewidth=.25,
          fill=NA) +
  geom_sf(data=counties_sf %>%
            st_transform(crs=st_crs(smc_sf), linewidth=.25),
          fill=NA)+
  geom_sf(data=rb_socal_sf %>%
            st_transform(crs=st_crs(smc_sf)), color="black", linewidth=.5,
          fill=NA) +
  geom_sf(data=sites_sf, size=.5, color="red")+
  facet_wrap(~name2) +
  coord_sf(xlim =c(st_bbox(smc_sf)$xmin,
                   st_bbox(smc_sf)$xmax),
           ylim =c(st_bbox(smc_sf)$ymin,
                   st_bbox(smc_sf)$ymax),
  )+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

map_of_sites
ggsave(map_of_sites, filename="NotForGit/2_Data inventory/map_of_sites.png",
       height=3, width=6.5)

sites_sf2<-sites_sf %>%
  group_by(StationCode, RB) %>%
  summarise(Assemblages = name %>% unique() %>% paste(collapse = "_"))
ggplot()+
  geom_sf(data=smc_sf)+
  geom_sf(data=sites_sf2, aes(color=Assemblages))


###########Data Summaries#############
bryo_mets %>%
  bind_rows(bug_mets) %>%
  filter(name %in% c("Bryo_Rich", "Arth_Rich")) %>%
  group_by(CollectionMethodCode, name) %>%
  summarise(Min = min(value),
            Median = median(value),
            Max= max(value)) %>%
  ungroup() %>%
  transmute(Asseblage = case_when(name=="Bryo_Rich"~"Bryophytes",
                                  CollectionMethodCode=="TerrInvert_Trap_DryStreams"~"Streambed arthropods",
                                  CollectionMethodCode=="TerrInvert_Veg_DryStreams"~"Riparian arthropods",
                                  T~"OTHER"),
            Min, Median, Max)

all_bio_data_usable2<-read_csv("NotForGit/1_Data import and assembly/all_bio_data_usable2.csv")

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
ggsave(taxa_freq_plot, filename="NotForGit/2_Data inventory/taxa_freq_plot.png",
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

