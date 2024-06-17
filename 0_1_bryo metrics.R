library(tidyverse)
library(readxl)
library(clipr)

#Import metadata
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

#Prep metadata for publication
meta_bryo_df2 %>%
  select(-TaxonomicLevelCode, -starts_with("STE2")) %>% 
  write_clip()

# 
#Import sampledata
# sample_data<-
#   bind_rows(
#     #Bakersfield 2018
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/Bakers_2018_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     #EPA 2018
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/EPA_2018_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     #RB4 2018, 2019, 2021
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/RB4_2018_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/RB4_2019_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/RB4_2021_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     #RB9 2017, 2019, 2020, 2021 
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/RB9_2017_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/RB9_2019_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/RB9_2020_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults"),
#     read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bryophyte_Data/RB9_2021_Bryophyte_SWAMP_MASTER_Template.xls", sheet="BenthicResults")
#   ) %>%
#   #Standard format for sample ID generation for arthropod samples
#   mutate(SampleID=paste(StationCode, SampleDate, CollectionMethodCode, Replicate, sep="_"),
#          #Same, but with location code
#          SampleIDLoc=paste(StationCode, SampleDate, CollectionMethodCode,LocationCode, Replicate, sep="_")) 



sample_data<-
  read_csv("Data/BioData/bug_and_bry_data_ceden_01262024.csv") %>%
  mutate(CollectionMethodName = case_when(FinalID=="Eucladium"~"Bryo_DryStreams",
                                          T~CollectionMethodName)) %>%
  filter(str_detect(CollectionMethodName, "Bryo")) %>%
  rename(CollectionMethodCode=CollectionMethodName,
         Replicate=CollectionReplicate,
         Result=Counts) #CEDEN fields don't match
# setdiff(sample_data$FinalID, meta_arth_df$FinalID)

#Are all taxa accounted for?
setdiff(sample_data$FinalID, meta_bryo_df$FinalID) 


#################

bryo_tax<-c("STE1","TaxonomicLevelCode",
            "Order","Family","Genus","Species","Variety"
)

#Make a long-format dataframe of taxonomic data
bryo_taxa_long<-meta_bryo_df %>% 
  select(all_of(bryo_tax)) %>%
  pivot_longer(cols=c("Order","Family","Genus","Species","Variety")) %>%
  
  
  na.omit() %>%
  
  rename(Original_LevelCode = TaxonomicLevelCode) %>%
  mutate(Parent_LevelCode = case_when(name == "Phylum" ~ 10, #Standard SWAMP levels
                                      name == "Subphylum" ~13,
                                      name == "Superclass" ~ 16,
                                      name == "Class" ~ 20,
                                      name == "Subclass" ~ 23,
                                      name == "Superorder" ~ 26,
                                      name == "Order" ~ 30,
                                      name == "Suborder" ~ 33,
                                      name == "SuperFamily" ~ 36, name == "Superfamily" ~ 36, #SWAMP uses capital F, but our data doesn't.
                                      name == "Family" ~ 40,
                                      name == "Subfamily" ~ 42,
                                      name == "Tribe" ~ 45,
                                      name == "Genus" ~ 50,
                                      name == "Species" ~ 60,
                                      name == "Subspecies" ~ 63,
                                      name == "Variety" ~ 66,
                                      T~ -88
  ))

# sample_data %>% filter(LocationCode=="InterTran8") %>%
#   select(StationCode, SampleDate, CollectionMethodCode, FinalID)

generate_sample_id<-function(x, LocationCode=F){
  if(LocationCode)
    sample_data_x = x %>%
      mutate(SampleID=paste(StationCode, SampleDate, CollectionMethodCode, Replicate, LocationCode, sep="_"))
  else
    sample_data_x = x %>%
      mutate(SampleID=paste(StationCode, SampleDate, CollectionMethodCode, Replicate, sep="_"))
  sample_data_x
}



# sample_data %>%
#   filter(StationCode=="558BPS01") %>%
#   select(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, FinalID, Result) %>%
#   left_join(meta_bryo_df2) %>%
#   group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1) %>%
#   summarise(Result=max(Result)) %>%
#   ungroup()  %>%
# 
# left_join(bryo_taxa_long %>%
# select(STE1, value), 
# relationship = "many-to-many") %>%
#   filter(STE1!=value) %>%
#   group_by(SampleID) %>%
#   mutate(DistinctTaxon = !STE1 %in% value) %>%
#   select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1, Result, DistinctTaxon) %>%
#   unique() 

flag_distinct_bryo_taxa<-function(x) {
  sample_data_x = x %>%
    select(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, FinalID, Result) %>%
    left_join(meta_bryo_df2) %>%
    group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1) %>%
    summarise(Result=max(Result)) %>%
    ungroup()  %>%
    
    left_join(bryo_taxa_long, relationship = "many-to-many") %>%
    filter(STE1!=value) %>%
    group_by(SampleID) %>%
    mutate(DistinctTaxon = !STE1 %in% value) %>%
    select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1, Result, DistinctTaxon) %>%
    unique() 
  sample_data_x
}



# sample_data %>%
#   select(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, FinalID, Result) %>%
#   left_join(meta_bryo_df2) %>%
#   group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1) %>%
#   summarise(Result=sum(Result)) %>%
#   ungroup()  %>%
#   
#   left_join(bryo_taxa_long, relationship = "many-to-many") %>%
#   filter(STE1!=value) %>%
#   group_by(SampleID) %>%
#   mutate(DistinctTaxon = !STE1 %in% value) %>%
#   select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1, Result, DistinctTaxon) %>%
#   unique() 

# sample_data_aggregated<-sample_data %>%
#   # filter(StationCode=="558BPS01") %>%
#   generate_sample_id() %>%
#   flag_distinct_bryo_taxa()
# 
# sample_data_aggregated %>% write_clip()



# Trait aggregation 
bryo_traits<-c(
  "BET_perennial",  "BET_short_lived_shuttle",  "BET_annual_shuttle",       "BET_long_lived_shuttle"       ,  "BET_colonist",        "BET_No Life Strat",       
  "BET_fugitive",             "BET_weft"         ,        "BET_mat"      ,            "BET_No Life Form"   ,      "BET_turf"        ,        
  "BET_annual",               "BET_cushion"       ,       "BET_rosette"   ,           "BET_dendroid"       ,      "BET_pleurocarpous" ,      
  "BET_foliose" ,             "BET_acrocarpous"    ,      "BET_thalloid"    ,         "BET_sphagnum"        ,     "BET_LongGeneration"  ,    
  "BET_MediumGeneration",     "BET_ShortGeneration" ,     "BET_No Generation Length", "BET_indL"           ,      "BET_indT"            ,    
  "BET_indF"             ,    "BET_indR"             ,    "BET_indN"          ,       "BET_indS"           ,      "BET_indHM"            ,   
  "BET_sub_so"            ,   "BET_sub_ro"          ,     "BET_sub_ba"        ,       "BET_sub_wo"         ,      "BET_sub_nw"            ,  
  "BET_sub_an"             ,  "BET_sub_sum"         ,     "BET_aquatic"       ,       "BET_hab_we"         ,      "BET_hab_fo"             , 
  "BET_hab_sh"            ,   "BET_hab_gr"          ,     "BET_hab_ro"        ,       "BET_hab_ar"         ,      "BET_hab_sum"             ,
  "BET_forest"            ,   "BET_hemeroby"        ,     "BET_hem_e"         ,       "BET_T_diurR"        ,      "BET_T_iso"               ,
  "BET_T_seas"            ,   "BET_Tmax_warmM"      ,     "BET_Tmin_coldM"     ,      "BET_T_annualR"     ,       "BET_T_wetQ"              ,
  "BET_T_dryQ"            ,   "BET_T_warmQ"         ,     "BET_T_coldQ"        ,      "BET_MAP"           ,       "BET_P_wetM"              ,
  "BET_P_dryM"            ,   "BET_P_seas"          ,     "BET_P_wetQ"          ,     "BET_P_dryQ"        ,       "BET_P_warmQ"             ,
  "BET_P_coldQ"           ,   "BET_gdd0"            ,     "BET_gdd5"            ,     "BET_gdd10"         ,       "BET_ngd0"                ,
  "BET_ngd5"              ,   "BET_ngd10"               
  
)


#Create a function 
calculate_bryo_metrics<-function(x) {
  x %>%
    rename(FinalID=STE1) %>%
    left_join(meta_bryo_df %>%
                select(FinalID,
                       Class, Order, Family, Genus, Species,GrowthForm,
                       all_of(bryo_traits)
                ) %>%
                unique()
              
    ) %>%
    # mutate(Nonnative_Syn) %>%
    group_by(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate) %>%
    summarise(
      
      #Bryophytes
      Bryo_Rich = sum(DistinctTaxon),
      # Bryo_Abund= sum(Result),
      Grimmiaceae_Rich=sum(DistinctTaxon[Family %in% c("Grimmiaceae")]),
      Grimmiaceae_RelRich=Grimmiaceae_Rich/Bryo_Rich,
      Pottiaceae_Rich=sum(DistinctTaxon[Family %in% c("Pottiaceae")]),
      Pottiaceae_RelRich=Pottiaceae_Rich/Bryo_Rich,
      Bryaceae_Rich=sum(DistinctTaxon[Family %in% c("Bryaceae")]),
      Bryaceae_RelRich=Bryaceae_Rich/Bryo_Rich,
      LeftFams_Rich=sum(DistinctTaxon[Family %in% c("Brachytheciaceae","Grimmiaceae","Leskeaceae","Orthotrichaceae")]), #Families found on the "referencey" side of an NMDS
      LeftFams_RelRich=LeftFams_Rich/Bryo_Rich,
      RightFams_Rich=sum(DistinctTaxon[Family %in% c("Aulacomniaceae","Bryaceae","Fabroniaceae","Fissidentaceae","Fontinalaceae","Meesiaceae","Mielichhoferiaceae")]), #Families found mostly on the non-referencey side of an NMDS
      RightFams_RelRich=RightFams_Rich/Bryo_Rich,
      #Fugitive
      Fugitive_KRich = sum(DistinctTaxon & !is.na(BET_fugitive)),
      Fugitive_Rich=sum(DistinctTaxon & BET_fugitive == 1, na.rm = T),
      Fugitive_RelRich = Fugitive_Rich/Fugitive_KRich,
      # Fugitive_Abund = sum(Result[BET_fugitive == 1], na.rm = T),
      # Fugitive_RelAbund = Fugitive_Abund / Bryo_Abund,
      
      #Colonist
      Colonist_KRich = sum(DistinctTaxon & !is.na(BET_colonist)),
      Colonist_Rich =sum(DistinctTaxon & BET_colonist == 1, na.rm = T),
      Colonist_RelRich = Colonist_Rich /Colonist_KRich,
      # Colonist_Abund = sum(Result[BET_colonist == 1], na.rm = T),
      # Colonist_RelAbund = Colonist_Abund / Bryo_Abund,
      
      #Perennial
      Perennial_KRich = sum(DistinctTaxon & !is.na(BET_perennial)),
      Perennial_Rich =sum(DistinctTaxon & BET_perennial == 1, na.rm = T),
      Perennial_RelRich = Perennial_Rich / Perennial_KRich,
      # Perennial_Abund = sum(Result[BET_perennial == 1], na.rm = T),
      # Perennial_RelAbund = Perennial_Abund / Bryo_Abund,
      
      #Short lived shuttle
      Short_Lived_Shuttle_KRich = sum(DistinctTaxon & !is.na(BET_short_lived_shuttle)),
      Short_Lived_Shuttle_Rich =sum(DistinctTaxon & BET_short_lived_shuttle == 1, na.rm = T),
      Short_Lived_Shuttle_RelRich = Short_Lived_Shuttle_Rich / Short_Lived_Shuttle_KRich,
      # Short_Lived_Shuttle_Abund = sum(Result[BET_short_lived_shuttle == 1], na.rm = T),
      # Short_Lived_Shuttle_RelAbund = Short_Lived_Shuttle_Abund / Bryo_Abund,
      
      #Long lived shuttle
      Long_Lived_Shuttle_KRich = sum(DistinctTaxon & !is.na(BET_long_lived_shuttle)),
      Long_Lived_Shuttle_Rich =sum(DistinctTaxon & BET_long_lived_shuttle == 1, na.rm = T),
      Long_Lived_Shuttle_RelRich =  Long_Lived_Shuttle_Rich /  Long_Lived_Shuttle_KRich,
      # Long_Lived_Shuttle_Abund = sum(Result[BET_long_lived_shuttle == 1], na.rm = T),
      # Long_Lived_Shuttle_RelAbund = Long_Lived_Shuttle_Abund / Bryo_Abund,
      
      #Annual shuttle
      Annual_Shuttle_KRich = sum(DistinctTaxon & !is.na(BET_annual_shuttle)),
      Annual_Shuttle_Rich =sum(DistinctTaxon & BET_annual_shuttle == 1, na.rm = T),
      Annual_Shuttle_RelRich = Annual_Shuttle_Rich / Annual_Shuttle_KRich,
      # Annual_Shuttle_Abund = sum(Result[BET_annual_shuttle == 1], na.rm = T),
      # Annual_Shuttle_RelAbund =Annual_Shuttle_Abund / Bryo_Abund,
      
      #Fugitive + Colonist
      FC_Rich = Fugitive_Rich + Colonist_Rich,
      FC_RelRich = Fugitive_RelRich + Colonist_RelRich,
      # FC_Abund = Fugitive_Abund + Colonist_Abund,
      # FC_RelAbund = Fugitive_RelAbund + Colonist_RelAbund,
      
      #Fugitive + Colonist + Annual Shuttle
      FCA_Rich = Fugitive_Rich + Colonist_Rich + Annual_Shuttle_Rich,
      FCA_RelRich = Fugitive_RelRich + Colonist_RelRich + Annual_Shuttle_RelRich,
      # FCA_Abund = Fugitive_Abund + Colonist_Abund + Annual_Shuttle_Abund,
      # FCA_RelAbund = Fugitive_RelAbund + Colonist_RelAbund + Annual_Shuttle_RelAbund,
      
      #Fugitive + Colonist + Annual Shuttle + Short Lived Shuttle
      FCAS_Rich = Fugitive_Rich + Colonist_Rich + Annual_Shuttle_Rich + Short_Lived_Shuttle_Rich,
      FCAS_RelRich = Fugitive_RelRich + Colonist_RelRich + Annual_Shuttle_RelRich + Short_Lived_Shuttle_RelRich,
      # FCAS_Abund = Fugitive_Abund + Colonist_Abund + Annual_Shuttle_Abund + Short_Lived_Shuttle_Abund,
      # FCAS_RelAbund = Fugitive_RelAbund + Colonist_RelAbund + Annual_Shuttle_RelAbund + Short_Lived_Shuttle_RelAbund,
      
      #Perennial + Long Lived Shuttle
      PL_Rich = Perennial_Rich + Long_Lived_Shuttle_Rich,
      PL_RelRich = Perennial_RelRich + Long_Lived_Shuttle_RelRich,
      # PL_Abund = Perennial_Abund + Long_Lived_Shuttle_Abund,
      # PL_RelAbund = Perennial_RelAbund + Long_Lived_Shuttle_RelAbund,
      
      
      
      #Growth Form Acrocarps
      Acrocarp_KRich = sum(DistinctTaxon & !is.na(GrowthForm)),
      Acrocarp_Rich = sum(DistinctTaxon & grepl("Acrocarp", GrowthForm), na.rm = T),
      Acrocarp_RelRich = Acrocarp_Rich / Acrocarp_KRich,
      
      
      #Growth Form Pleurocarps
      Pleurocarp_KRich = sum(DistinctTaxon & !is.na(GrowthForm)),
      Pleurocarp_Rich = sum(DistinctTaxon & grepl("Pleurocarp", GrowthForm), na.rm = T),
      Pleurocarp_RelRich = Pleurocarp_Rich / Pleurocarp_KRich,
      
      #Long Generation Time
      Long_Generation_KRich = sum(DistinctTaxon & !is.na(BET_LongGeneration)),
      Long_Generation_Rich = sum(DistinctTaxon & BET_LongGeneration == 1, na.rm = T),
      Long_Generation_RelRich = Long_Generation_Rich / Long_Generation_KRich,
      # Long_Generation_Abund = sum(Result[BET_LongGeneration == 1], na.rm = T),
      # Long_Generation_RelAbund = Long_Generation_Abund / Bryo_Abund,
      
      #Medium Generation Time
      Medium_Generation_KRich = sum(DistinctTaxon & !is.na(BET_MediumGeneration)),
      Medium_Generation_Rich = sum(DistinctTaxon & BET_MediumGeneration == 1, na.rm = T),
      Medium_Generation_RelRich = Medium_Generation_Rich /  Medium_Generation_KRich,
      # Medium_Generation_Abund = sum(Result[BET_MediumGeneration == 1], na.rm = T),
      # Medium_Generation_RelAbund = Medium_Generation_Abund / Bryo_Abund,
      
      #Short Generation Time
      Short_Generation_KRich = sum(DistinctTaxon & !is.na(BET_ShortGeneration)),
      Short_Generation_Rich = sum(DistinctTaxon & BET_ShortGeneration == 1, na.rm = T),
      Short_Generation_RelRich = Short_Generation_Rich / Short_Generation_KRich,
      # Short_Generation_Abund = sum(Result[BET_ShortGeneration == 1], na.rm = T),
      # Short_Generation_RelAbund = Short_Generation_Abund / Bryo_Abund,
      
      
      #######Indicator Values
      
      #Moisture indF
      indF_KRich = sum(DistinctTaxon & !is.na(BET_indF)),
      indF_Max = case_when(indF_KRich==0~NA_real_,
                           T~max(BET_indF, na.rm = T)),
      indF_Min = case_when(indF_KRich==0~NA_real_,
                           T~min(BET_indF, na.rm = T)),
      indF_Mean = case_when(indF_KRich==0~NA_real_,
                            T~mean(BET_indF, na.rm = T)),
      indF_Low_Rich = sum(DistinctTaxon & BET_indF <= 2, na.rm = T),
      indF_Low_RelRich = indF_Low_Rich / indF_KRich,
      indF_High_Rich = sum(DistinctTaxon & BET_indF >= 8, na.rm = T),
      indF_High_RelRich = indF_High_Rich / indF_KRich,
      
      #Heavy metal tolerance indHM
      indHM_KRich = sum(DistinctTaxon & !is.na(BET_indHM)),
      indHM_Max = max(BET_indHM, na.rm = T),
      indHM_Min = min(BET_indHM, na.rm = T),
      indHM_Mean = mean(BET_indHM, na.rm = T),
      indHM_Low_Rich = sum(DistinctTaxon & BET_indHM <= 2, na.rm = T),
      indHM_Low_RelRich = indHM_Low_Rich / indHM_KRich,
      indHM_High_Rich = sum(DistinctTaxon & BET_indHM >= 8, na.rm = T),
      indHM_High_RelRich = indHM_High_Rich / indHM_KRich,
      
      #Light indL
      indL_KRich = sum(DistinctTaxon & !is.na(BET_indL)),
      indL_Max = max(BET_indL, na.rm = T),
      indL_Min = min(BET_indL, na.rm = T),
      indL_Mean = mean(BET_indL, na.rm = T),
      indL_Low_Rich = sum(DistinctTaxon & BET_indL <= 2, na.rm = T),
      indL_Low_RelRich = indL_Low_Rich / indL_KRich,
      indL_High_Rich = sum(DistinctTaxon & BET_indL >= 8, na.rm = T),
      indL_High_RelRich = indL_High_Rich / indL_KRich,
      
      #Nutrients indN
      indN_KRich = sum(DistinctTaxon & !is.na(BET_indN)),
      indN_Max = max(BET_indN, na.rm = T),
      indN_Min = min(BET_indN, na.rm = T),
      indN_Mean = mean(BET_indN, na.rm = T),
      indN_Low_Rich = sum(DistinctTaxon & BET_indN <= 2, na.rm = T),
      indN_Low_RelRich = indN_Low_Rich / indN_KRich,
      indN_High_Rich = sum(DistinctTaxon & BET_indN >= 8, na.rm = T),
      indN_High_RelRich = indN_High_Rich / indN_KRich,
      
      #Acidity indR
      indR_KRich = sum(DistinctTaxon & !is.na(BET_indR)),
      indR_Max = max(BET_indR, na.rm = T),
      indR_Min = min(BET_indR, na.rm = T),
      indR_Mean = mean(BET_indR, na.rm = T),
      indR_Low_Rich = sum(DistinctTaxon & BET_indR <= 2, na.rm = T),
      indR_Low_RelRich = indR_Low_Rich / indR_KRich,
      indR_High_Rich = sum(DistinctTaxon & BET_indR >= 8, na.rm = T),
      indR_High_RelRich = indR_High_Rich / indR_KRich,
      
      #Salt indS
      indS_KRich = sum(DistinctTaxon & !is.na(BET_indS)),
      indS_Max = max(BET_indS, na.rm = T),
      indS_Min = min(BET_indS, na.rm = T),
      indS_Mean = mean(BET_indS, na.rm = T),
      indS_Low_Rich = sum(DistinctTaxon & BET_indS <= 2, na.rm = T),
      indS_Low_RelRich = indS_Low_Rich / indS_KRich,
      indS_High_Rich = sum(DistinctTaxon & BET_indS >= 8, na.rm = T),
      indS_High_RelRich = indS_High_Rich / indS_KRich,
      
      #Temerature indT
      indT_KRich = sum(DistinctTaxon & !is.na(BET_indT)),
      indT_Max = max(BET_indT, na.rm = T),
      indT_Min = min(BET_indT, na.rm = T),
      indT_Mean = mean(BET_indT, na.rm = T),
      indT_Low_Rich = sum(DistinctTaxon & BET_indT <= 2, na.rm = T),
      indT_Low_RelRich = indT_Low_Rich / indT_KRich,
      indT_High_Rich = sum(DistinctTaxon & BET_indT >= 8, na.rm = T),
      indT_High_RelRich = indT_High_Rich / indT_KRich,
      
      ###Substrate Values
      
      #Animal Carcass or Dung
      sub_an_KRich = sum(DistinctTaxon & !is.na(BET_sub_an)),
      sub_an_Rich = sum(DistinctTaxon & BET_sub_an == 1, na.rm = T),
      sub_an_RelRich = sub_an_Rich / sub_an_KRich,
      # sub_an_Abund = sum(Result[BET_sub_an == 1], na.rm = T),
      # sub_an_RelAbund = sub_an_Abund / Bryo_Abund,
      
      #Bark
      sub_ba_KRich = sum(DistinctTaxon & !is.na(BET_sub_ba)),
      sub_ba_Rich = sum(DistinctTaxon & BET_sub_ba == 1, na.rm = T),
      sub_ba_RelRich = sub_ba_Rich /  sub_ba_KRich,
      # sub_ba_Abund = sum(Result[BET_sub_ba == 1], na.rm = T),
      # sub_ba_RelAbund = sub_ba_Abund / Bryo_Abund,
      
      #Non-woody Epiphyte
      sub_nw_KRich = sum(DistinctTaxon & !is.na(BET_sub_nw)),
      sub_nw_Rich = sum(DistinctTaxon & BET_sub_nw == 1, na.rm = T),
      sub_nw_RelRich = sub_nw_Rich /  sub_nw_KRich,
      # sub_nw_Abund = sum(Result[BET_sub_nw == 1], na.rm = T),
      # sub_nw_RelAbund = sub_nw_Abund / Bryo_Abund,
      
      #Rock
      sub_ro_KRich = sum(DistinctTaxon & !is.na(BET_sub_ro)),
      sub_ro_Rich = sum(DistinctTaxon & BET_sub_ro == 1, na.rm = T),
      sub_ro_RelRich = sub_ro_Rich / sub_ro_KRich,
      # sub_ro_Abund = sum(Result[BET_sub_ro == 1], na.rm = T),
      # sub_ro_RelAbund = sub_ro_Abund / Bryo_Abund,
      
      #Soil
      sub_so_KRich = sum(DistinctTaxon & !is.na(BET_sub_so)),
      sub_so_Rich = sum(DistinctTaxon & BET_sub_so == 1, na.rm = T),
      sub_so_RelRich = sub_so_Rich / sub_so_KRich,
      # sub_so_Abund = sum(Result[BET_sub_so == 1], na.rm = T),
      # sub_so_RelAbund = sub_so_Abund / Bryo_Abund,
      
      #Dead wood
      sub_wo_KRich = sum(DistinctTaxon & !is.na(BET_sub_wo)),
      sub_wo_Rich = sum(DistinctTaxon & BET_sub_wo == 1, na.rm = T),
      sub_wo_RelRich = sub_wo_Rich / sub_wo_KRich,
      # sub_wo_Abund = sum(Result[BET_sub_wo == 1], na.rm = T),
      # sub_wo_RelAbund = sub_wo_Abund / Bryo_Abund,
      
      #Sum of substrate values
      sub_sum_KRich = sum(DistinctTaxon & !is.na(BET_sub_sum)),
      sub_sum_Low_Rich = sum(DistinctTaxon & BET_sub_sum <= 2, na.rm = T),
      sub_sum_Low_RelRich = sub_sum_Low_Rich / sub_sum_KRich,
      sub_sum_High_Rich = sum(DistinctTaxon & BET_sub_sum >= 3, na.rm = T),
      sub_sum_High_RelRich = sub_sum_High_Rich / sub_sum_KRich,
      
      
      #Aquatic
      Aquatic_KRich = sum(DistinctTaxon & !is.na(BET_aquatic)),
      Aquatic_Rich=sum(DistinctTaxon & BET_aquatic == 1, na.rm = T),
      Aquatic_RelRich = Aquatic_Rich/Aquatic_KRich,
      # Aquatic_Abund = sum(Result[BET_aquatic == 1], na.rm = T),
      # Aquatic_RelAbund = Aquatic_Abund / Bryo_Abund,
      
      ####Habitat Metrics
      
      #Artificial bar_ar
      hab_ar_KRich = sum(DistinctTaxon & !is.na(BET_hab_ar)),
      hab_ar_Rich = sum(DistinctTaxon & BET_hab_ar == 1, na.rm = T),
      hab_ar_RelRich = hab_ar_Rich / hab_ar_KRich,
      
      #Forest
      hab_fo_KRich = sum(DistinctTaxon & !is.na(BET_hab_fo)),
      hab_fo_Rich = sum(DistinctTaxon & BET_hab_fo == 1, na.rm = T),
      hab_fo_RelRich = hab_fo_Rich / hab_fo_KRich,
      
      #Grassland
      hab_gr_KRich = sum(DistinctTaxon & !is.na(BET_hab_gr)),
      hab_gr_Rich = sum(DistinctTaxon & BET_hab_gr == 1, na.rm = T),
      hab_gr_RelRich = hab_gr_Rich / hab_gr_KRich,
      
      #Rocky areas and cliffs
      hab_ro_KRich = sum(DistinctTaxon & !is.na(BET_hab_ro)),
      hab_ro_Rich = sum(DistinctTaxon & BET_hab_ro == 1, na.rm = T),
      hab_ro_RelRich = hab_ro_Rich / hab_ro_KRich,
      
      # Shrublands
      hab_sh_KRich = sum(DistinctTaxon & !is.na(BET_hab_sh)),
      hab_sh_Rich = sum(DistinctTaxon & BET_hab_sh == 1, na.rm = T),
      hab_sh_RelRich = hab_sh_Rich / hab_sh_KRich,
      
      #Wetlands
      hab_we_KRich = sum(DistinctTaxon & !is.na(BET_hab_we)),
      hab_we_Rich = sum(DistinctTaxon & BET_hab_we == 1, na.rm = T),
      hab_we_RelRich = hab_we_Rich / hab_we_KRich,
      
      #Sum of habitat metrics
      hab_sum_KRich = sum(DistinctTaxon & !is.na(BET_hab_sum)),
      hab_sum_Low_Rich = sum(DistinctTaxon & BET_hab_sum <= 2, na.rm = T),
      hab_sum_Low_RelRich = hab_sum_Low_Rich / hab_sum_KRich,
      hab_sum_High_Rich = sum(DistinctTaxon & BET_hab_sum >= 3, na.rm = T),
      hab_sum_High_RelRich = hab_sum_High_Rich / hab_sum_KRich,
      
      ##Forest
      #Restricted to forest
      Forest_KRich = sum(DistinctTaxon & !is.na(BET_forest)),
      Forest_Only_Rich = sum(DistinctTaxon & BET_forest == 1, na.rm = T),
      Forest_Only_RelRich = Forest_Only_Rich / Forest_KRich,
      
      #Prefers forest edges
      Forest_Edge_Rich = sum(DistinctTaxon & BET_forest == 2, na.rm = T),
      Forest_Edge_RelRich = Forest_Edge_Rich / Forest_KRich,
      
      #Occurs in forest and open lands
      Forest_And_Open_Land_Rich = sum(DistinctTaxon & BET_forest == 3, na.rm = T),
      Forest_And_Open_Land_RelRich = Forest_And_Open_Land_Rich / Forest_KRich,
      
      #May occur in forest but prefers open land
      Forest_Open_land_Preference_Rich = sum(DistinctTaxon & BET_forest == 4, na.rm = T),
      Forest_Open_land_Preference_RelRich =  Forest_Open_land_Preference_Rich / Forest_KRich,
      
      #Restricted to forests + prefers forest edge
      #Perennial + Long Lived Shuttle
      Forest_Only_and_Edge_Rich = Forest_Only_Rich + Forest_Edge_Rich,
      Forest_Only_and_Edge_RelRich =  Forest_Only_RelRich + Forest_Edge_RelRich,
      
      
      #Hemeroby
      Hemeroby_KRich = sum(DistinctTaxon & !is.na(BET_hemeroby)),
      Hemeroby_Max = max(BET_hemeroby, na.rm = T),
      Hemeroby_Min = min(BET_hemeroby, na.rm = T),
      Hemeroby_Mean = mean(BET_hemeroby, na.rm = T),
      Hemeroby_Low_Rich = sum(DistinctTaxon & BET_hemeroby <= 2, na.rm = T),
      Hemeroby_Low_RelRich = Hemeroby_Low_Rich / Hemeroby_KRich,
      Hemeroby_High_Rich = sum(DistinctTaxon & BET_hemeroby >= 8, na.rm = T),
      Hemeroby_High_RelRich = Hemeroby_High_Rich / Hemeroby_KRich,
      
      ##Hem_e
      #Prefers undisturbed habitats
      Hem_e_KRich = sum(DistinctTaxon & !is.na(BET_hem_e)),
      Hem_e_Undisturbed_Rich = sum(DistinctTaxon & BET_hem_e == 1, na.rm = T),
      Hem_e_Undisturbed_RelRich =  Hem_e_Undisturbed_Rich / Hem_e_KRich,
      
      #Indifferent
      Hem_e_Indifferent_Rich = sum(DistinctTaxon & BET_hem_e == 3, na.rm = T),
      Hem_e_Indifferent_RelRich = Hem_e_Indifferent_Rich / Hem_e_KRich,
      
      # Prefers disturbed habitats
      Hem_e_Distubred_Rich = sum(DistinctTaxon & BET_hem_e == 5, na.rm = T),
      Hem_e_Distubred_RelRich =  Hem_e_Distubred_Rich / Hem_e_KRich,
      
      #Indifferent + Disturbed
      Hem_e_Indifferent_Disturbed_Rich = Hem_e_Indifferent_Rich +  Hem_e_Distubred_Rich,
      Hem_e_Indifferent_Disturbed_RelRich =  Hem_e_Indifferent_RelRich + Hem_e_Distubred_RelRich,
      
      ##Temp and Precipitation
      
      # Mean diurnal air temperature range
      T_DiurR_Max = max(BET_T_diurR, na.rm = T),
      T_DiurR_Min = min(BET_T_diurR, na.rm = T),
      T_DiurR_Mean = mean(BET_T_diurR, na.rm = T),
      
      # Isothermality
      Therm_iso_Max = max(BET_T_iso, na.rm = T),
      Therm_iso_Min = min(BET_T_iso, na.rm = T),
      Therm_iso_Mean = mean(BET_T_iso, na.rm = T),
      
      # temperature seasonality
      Temp_seas_Max = max(BET_T_seas, na.rm = T),
      Temp_seas_Min = min(BET_T_seas, na.rm = T),
      Temp_seas_Mean = mean(BET_T_seas, na.rm = T),
      
      # mean daily maximum air temperature of the warmest month
      Tmax_warmM_Max = max(BET_Tmax_warmM, na.rm = T),
      Tmax_warmM_Min = min(BET_Tmax_warmM, na.rm = T),
      Tmax_warmM_Mean = mean(BET_Tmax_warmM, na.rm = T),
      
      # mean daily minimum air temperature of the coldest month
      Tmin_coldM_Max = max(BET_Tmin_coldM, na.rm = T),
      Tmin_coldM_Min = min(BET_Tmin_coldM, na.rm = T),
      Tmin_coldM_Mean = mean(BET_Tmin_coldM, na.rm = T),
      
      #  annual range of air temperature
      T_annualR_Max = max(BET_T_annualR, na.rm = T),
      T_annualR_Min = min(BET_T_annualR, na.rm = T),
      T_annualR_Mean = mean(BET_T_annualR, na.rm = T),
      
      # mean daily mean air temperatures of the wettest quarter
      T_wetQ_Max = max(BET_T_wetQ, na.rm = T),
      T_wetQ_Min = min(BET_T_wetQ, na.rm = T),
      T_wetQ_Mean = mean(BET_T_wetQ, na.rm = T),
      
      # mean daily mean air temperatures of the driest quarter
      T_dryQ_Max = max(BET_T_dryQ, na.rm = T),
      T_dryQ_Min = min(BET_T_dryQ, na.rm = T),
      T_dryQ_Mean = mean(BET_T_dryQ, na.rm = T),
      
      #  mean daily mean air temperatures of the warmest quarter
      T_warmQ_Max = max(BET_T_warmQ, na.rm = T),
      T_warmQ_Min = min(BET_T_warmQ, na.rm = T),
      T_warmQ_Mean = mean(BET_T_warmQ, na.rm = T),
      
      # mean daily mean air temperatures of the coldest quarter
      T_coldQ_Max = max(BET_T_coldQ, na.rm = T),
      T_coldQ_Min = min(BET_T_coldQ, na.rm = T),
      T_coldQ_Mean = mean(BET_T_coldQ, na.rm = T),
      
      #  annual precipitation amount
      MAP_Max = max(BET_MAP, na.rm = T),
      MAP_Min = min(BET_MAP, na.rm = T),
      MAP_Mean = mean(BET_MAP, na.rm = T),
      
      # precipitation amount of the wettest month
      P_wetM_Max = max(BET_P_wetM, na.rm = T),
      P_wetM_Min = min(BET_P_wetM, na.rm = T),
      P_wetM_Mean = mean(BET_P_wetM, na.rm = T),
      
      # precipitation amount of the driest month
      P_dryM_Max = max(BET_P_dryM, na.rm = T),
      P_dryM_Min = min(BET_P_dryM, na.rm = T),
      P_dryM_Mean = mean(BET_P_dryM, na.rm = T),
      
      # precipitation seasonality
      Ppt_seas_Max = max(BET_P_seas, na.rm = T),
      Ppt_seas_Min = min(BET_P_seas, na.rm = T),
      Ppt_seas_Mean = mean(BET_P_seas, na.rm = T),
      
      # mean monthly precipitation amount of the wettest quarter
      P_wetQ_Max = max(BET_P_wetQ, na.rm = T),
      P_wetQ_Min = min(BET_P_wetQ, na.rm = T),
      P_wetQ_Mean = mean(BET_P_wetQ, na.rm = T),
      
      #  mean monthly precipitation amount of the driest quarter
      P_dryQ_Max = max(BET_P_dryQ, na.rm = T),
      P_dryQ_Min = min(BET_P_dryQ, na.rm = T),
      P_dryQ_Mean = mean(BET_P_dryQ, na.rm = T),
      
      # mean monthly precipitation amount of the warmest quarter
      P_warmQ_Max = max(BET_P_warmQ, na.rm = T),
      P_warmQ_Min = min(BET_P_warmQ, na.rm = T),
      P_warmQ_Mean = mean(BET_P_warmQ, na.rm = T),
      
      # mean monthly precipitation amount of the coldest quarter
      P_coldQ_Max = max(BET_P_coldQ, na.rm = T),
      P_coldQ_Min = min(BET_P_coldQ, na.rm = T),
      P_coldQ_Mean = mean(BET_P_coldQ, na.rm = T),
      
      # growing degree days heat sum above 0°C
      gdd0_Max = max(BET_gdd0, na.rm = T),
      gdd0_Min = min(BET_gdd0, na.rm = T),
      gdd0_Mean = mean(BET_gdd0, na.rm = T),
      
      # growing degree days heat sum above 5°C
      gdd5_Max = max(BET_gdd5, na.rm = T),
      gdd5_Min = min(BET_gdd5, na.rm = T),
      gdd5_Mean = mean(BET_gdd5, na.rm = T),
      
      # growing degree days heat sum above 10°C
      gdd10_Max = max(BET_gdd10, na.rm = T),
      gdd10_Min = min(BET_gdd10, na.rm = T),
      gdd10_Mean = mean(BET_gdd10, na.rm = T),
      
      # number of growing degree days > 0°C
      ngd0_Max = max(BET_ngd0, na.rm = T),
      ngd0_Min = min(BET_ngd0, na.rm = T),
      ngd0_Mean = mean(BET_ngd0, na.rm = T),
      
      # number of growing degree days > 5°C
      ngd5_Max = max(BET_ngd5, na.rm = T),
      ngd5_Min = min(BET_ngd5, na.rm = T),
      ngd5_Mean = mean(BET_ngd5, na.rm = T),
      
      # number of growing degree days > 10°C
      ngd10_Max = max(BET_ngd10, na.rm = T),
      ngd10_Min = min(BET_ngd10, na.rm = T),
      ngd10_Mean = mean(BET_ngd10, na.rm = T),
      
    ) %>%
    ungroup()
}


bryo_mets<-c("Bryo_Rich", 
             "Grimmiaceae_Rich", "Grimmiaceae_RelRich", 
             "Pottiaceae_Rich", "Pottiaceae_RelRich",
             "Bryaceae_Rich", "Bryaceae_RelRich", 
             "LeftFams_Rich", "LeftFams_RelRich",
             "RightFams_Rich", "RightFams_RelRich", 
             "Fugitive_Rich","Fugitive_RelRich",
             "Colonist_Rich", "Colonist_RelRich", 
             "Perennial_Rich", "Perennial_RelRich", 
             "Short_Lived_Shuttle_Rich", "Short_Lived_Shuttle_RelRich", 
             "Long_Lived_Shuttle_Rich", "Long_Lived_Shuttle_RelRich", 
             "Annual_Shuttle_Rich", "Annual_Shuttle_RelRich", 
             "FC_Rich", "FC_RelRich",
             "FCA_Rich", "FCA_RelRich", 
             "FCAS_Rich", "FCAS_RelRich", 
             "PL_Rich", "PL_RelRich", 
             "Acrocarp_Rich", "Acrocarp_RelRich", 
             "Pleurocarp_Rich", "Pleurocarp_RelRich", 
             "Long_Generation_Rich", "Long_Generation_RelRich", 
             "Medium_Generation_Rich", "Medium_Generation_RelRich", 
             "Short_Generation_Rich", "Short_Generation_RelRich", 
             "indF_Max", "indF_Min", "indF_Mean", 
             "indF_Low_Rich", "indF_Low_RelRich", 
             "indF_High_Rich", "indF_High_RelRich", 
             "indHM_Max", "indHM_Min", "indHM_Mean", 
             "indHM_Low_Rich", "indHM_Low_RelRich", 
             "indHM_High_Rich", "indHM_High_RelRich", 
             "indL_Max", "indL_Min", "indL_Mean", 
             "indL_Low_Rich", "indL_Low_RelRich", 
             "indL_High_Rich", "indL_High_RelRich", 
             "indN_Max", "indN_Min", "indN_Mean", 
             "indN_Low_Rich", "indN_Low_RelRich", 
             "indN_High_Rich", "indN_High_RelRich",
             "indR_Max", "indR_Min", "indR_Mean", 
             "indR_Low_Rich", "indR_Low_RelRich", 
             "indR_High_Rich", "indR_High_RelRich", 
             "indS_Max", "indS_Min", "indS_Mean", 
             "indS_Low_Rich", "indS_Low_RelRich", 
             "indS_High_Rich", "indS_High_RelRich",
             "indT_Max", "indT_Min", "indT_Mean", 
             "indT_Low_Rich", "indT_Low_RelRich", 
             "indT_High_Rich", "indT_High_RelRich",
             "sub_an_Rich", "sub_an_RelRich", 
             "sub_ba_Rich", "sub_ba_RelRich", 
             "sub_nw_Rich", "sub_nw_RelRich",
             "sub_ro_Rich", "sub_ro_RelRich",
             "sub_so_Rich", "sub_so_RelRich", 
             "sub_wo_Rich", "sub_wo_RelRich", 
             "sub_sum_Low_Rich", "sub_sum_Low_RelRich", 
             "sub_sum_High_Rich", "sub_sum_High_RelRich", 
             "Aquatic_Rich", "Aquatic_RelRich", 
             "hab_ar_Rich", "hab_ar_RelRich", 
             "hab_fo_Rich", "hab_fo_RelRich", 
             "hab_gr_Rich", "hab_gr_RelRich", 
             "hab_ro_Rich", "hab_ro_RelRich", 
             "hab_sh_Rich", "hab_sh_RelRich",
             "hab_we_Rich", "hab_we_RelRich", 
             "hab_sum_Low_Rich", "hab_sum_Low_RelRich", 
             "hab_sum_High_Rich", "hab_sum_High_RelRich", 
             "Forest_Only_Rich", "Forest_Only_RelRich", 
             "Forest_Edge_Rich", "Forest_Edge_RelRich", 
             "Forest_And_Open_Land_Rich", "Forest_And_Open_Land_RelRich", 
             "Forest_Open_land_Preference_Rich", "Forest_Open_land_Preference_RelRich", 
             "Forest_Only_and_Edge_Rich", "Forest_Only_and_Edge_RelRich", 
             "Hemeroby_Max", "Hemeroby_Min", "Hemeroby_Mean", 
             "Hemeroby_Low_Rich", "Hemeroby_Low_RelRich", 
             "Hemeroby_High_Rich", "Hemeroby_High_RelRich", 
             "Hem_e_Undisturbed_Rich", "Hem_e_Undisturbed_RelRich", 
             "Hem_e_Indifferent_Rich", "Hem_e_Indifferent_RelRich", 
             "Hem_e_Distubred_Rich", "Hem_e_Distubred_RelRich", 
             "Hem_e_Indifferent_Disturbed_Rich", "Hem_e_Indifferent_Disturbed_RelRich", 
             "T_DiurR_Max", "T_DiurR_Min", "T_DiurR_Mean", 
             "Therm_iso_Max", "Therm_iso_Min", "Therm_iso_Mean", 
             "Temp_seas_Max", "Temp_seas_Min", "Temp_seas_Mean", 
             "Tmax_warmM_Max", "Tmax_warmM_Min", "Tmax_warmM_Mean",
             "Tmin_coldM_Max", "Tmin_coldM_Min", "Tmin_coldM_Mean", 
             "T_annualR_Max", "T_annualR_Min", "T_annualR_Mean", 
             "T_wetQ_Max", "T_wetQ_Min", "T_wetQ_Mean", 
             "T_dryQ_Max", "T_dryQ_Min", "T_dryQ_Mean", 
             "T_warmQ_Max", "T_warmQ_Min", "T_warmQ_Mean", 
             "T_coldQ_Max","T_coldQ_Min", "T_coldQ_Mean", 
             "MAP_Max", "MAP_Min", "MAP_Mean", 
             "P_wetM_Max", "P_wetM_Min", "P_wetM_Mean",
             "P_dryM_Max", "P_dryM_Min", "P_dryM_Mean", 
             "Ppt_seas_Max", "Ppt_seas_Min", "Ppt_seas_Mean",
             "P_wetQ_Max", "P_wetQ_Min", "P_wetQ_Mean",
             "P_dryQ_Max", "P_dryQ_Min", "P_dryQ_Mean", 
             "P_warmQ_Max", "P_warmQ_Min", "P_warmQ_Mean",
             "P_coldQ_Max", "P_coldQ_Min", "P_coldQ_Mean", 
             "gdd0_Max", "gdd0_Min", "gdd0_Mean", 
             "gdd5_Max", "gdd5_Min", "gdd5_Mean",
             "gdd10_Max", "gdd10_Min", "gdd10_Mean", 
             "ngd0_Max", "ngd0_Min", "ngd0_Mean", 
             "ngd5_Max", "ngd5_Min", "ngd5_Mean", 
             "ngd10_Max", "ngd10_Min", "ngd10_Mean")

metrics_output<-sample_data %>%
  # filter(StationCode=="558BPS01")%>%
  generate_sample_id() %>%
  flag_distinct_bryo_taxa() %>%
  calculate_bryo_metrics() %>%
  select(-contains("_K")) %>%
  pivot_longer(cols=c(all_of(bryo_mets))) %>%
  mutate(value=case_when(value==Inf~NA_real_,
                         value==-Inf~NA_real_,
                         T~value))

# metrics_output %>%# filter(indF_KRich==0) %>%
  # select(starts_with("IndF"))

# metrics_output_location<-sample_data %>%
#   # filter(StationCode=="558BPS01")%>%
#   generate_sample_id(LocationCode = T) %>%
#   flag_distinct_bryo_taxa() %>%
#   calculate_bryo_metrics() %>%
#   select(-contains("_K")) %>%
#   pivot_longer(cols=all_of(bryo_mets)) %>%
#   mutate(value=case_when(value==Inf~NA_real_,
#                          value==-Inf~NA_real_,
#                          T~value),
#          Location=case_when(str_detect(SampleID,"_Left_")~"Left",
#                             str_detect(SampleID,"_Right_")~"Right",
#                             str_detect(SampleID,"_Channel_")~"_Channel",
#                             T~"OTHER"))


setdiff(metrics_output$name,
        metrics_output_location$name
)
metrics_output_location %>%
  filter(value==Inf)


sample_data %>%
  filter(StationCode=="558BPS01") %>%
  select(FinalID)
meta_bryo_df %>%
  filter(FinalID %in% c("Didymodon tectorum","Tortula","Didymodon brachyphyllus","Didymodon australasiae")) %>%
  select(FinalID, STE1)

names(metrics_output) %>% dput()



# ggplot(data=metrics_output, aes(x=Bryo_Rich %>% as.factor()))+geom_histogram(stat="count")
# ggplot(data=metrics_output_location, aes(x=Bryo_Rich %>% as.factor()))+geom_histogram(stat="count")
metrics_output

metrics_output %>%
  write_csv("Data/BioData/Metrics/bryo_metrics_04122024.csv")

# select(StationCode, SampleDate, CollectionMethodCode, Replicate, SampleID,
#        all_of(bryo_mets)) %>%
# pivot_longer(cols=all_of(bryo_metrics)) %>%
# mutate(value=case_when(value==  Inf~NA_real_,
#                        value== -Inf~NA_real_,
#                        T~value)) %>%
# pivot_wider(names_from = name, values_from = value)


#############


# Reformat bryophyte metadata
# Combine binary columns (Life strategy, life form, growth form, generation time)

meta_bryo_df_reformated<-meta_bryo_df %>%
  transmute(
    FinalID, ParentOrganismFinalID, 
    Phylum, Class, Subclass, Order, Family, Subfamily, Genus, Species, Variety,
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
