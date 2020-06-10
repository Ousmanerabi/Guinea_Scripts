######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with ANC data for 1999, 2005, 2012, 2018 (IR)

ANC.list <- GNIR


# recoding ANC 
look_for(ANC.list[[4]], "antenatal")
table(ANC.list[[1]]$m14_3)

ANC.list  <- map(ANC.list, funEnv$recoder.ml4_1)


# key datasets and dhs/mis datasets are joined  revoir ce code
ANC.list <- map2(ANC.list, key_list, left_join) #PR datasets


#####################################################################################################
# ANC coverage 
####################################################################################################

# 2018
ANC.list[[4]] <-funEnv$dataclean(ANC.list[[4]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd18 <- funEnv$svydesign.fun(ANC.list[[4]])


DS_ANC_pre_18 <- funEnv$result.fun('ANC', 'NAME_2','num_p', design=ANC.svyd18)
head(DS_ANC_pre_18)
summary(DS_ANC_pre_18$ANC)

write.csv(DS_ANC_pre_18, "results/ANC/DS_ANC_pre_18.csv")


# 2012
ANC.list[[3]] <-funEnv$dataclean(ANC.list[[3]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd12 <- funEnv$svydesign.fun(ANC.list[[3]])


DS_ANC_pre_12 <- funEnv$result.fun('ANC', 'NAME_2','num_p', design=ANC.svyd12)
head(DS_ANC_pre_12)

write.csv(DS_ANC_pre_12, "results/ANC/DS_ANC_pre_12.csv")


# 2005 
ANC.list[[2]] <-funEnv$dataclean(ANC.list[[2]], m14_1, v005,'m14_1', 'ANC') 
ANC.svyd05 <- funEnv$svydesign.fun(ANC.list[[2]])

DS_ANC_pre_05 <- funEnv$result.fun('ANC', 'NAME_2','num_p', design=ANC.svyd05)
head(DS_ANC_pre_05)

write.csv(DS_ANC_pre_05, "results/ANC/DS_ANC_pre_05.csv")



# 1999 
ANC.list[[1]] <-funEnv$dataclean(ANC.list[[1]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd99 <- funEnv$svydesign.fun(ANC.list[[1]])

DS_ANC_pre_99 <- funEnv$result.fun('ANC', 'NAME_2','num_p', design=ANC.svyd99)
head(DS_ANC_pre_99)

write.csv(DS_ANC_pre_99, "results/ANC/DS_ANC_pre_99.csv")


# cluster-level estimates

# 2018

clu_ANC_pre_18 <- funEnv$result.clu.fun('ANC', 'v001', design=ANC.svyd18, ANC.list[[4]], "v007")
head(clu_ANC_pre_18)
summary(clu_ANC_pre_18$ANC)

write.csv(clu_ANC_pre_14, "results/ANC/clu_ANC_pre_14.csv")



# 2012

clu_ANC_pre_12 <- funEnv$result.clu.fun('ANC', 'v001', design=ANC.svyd12,ANC.list[[3]], "v007")
head(clu_ANC_pre_12)

write.csv(clu_ANC_pre_12, "results/ANC/clu_ANC_pre_10.csv")




# 2005

clu_ANC_pre_05 <- funEnv$result.clu.fun('ANC', 'v001', design=ANC.svyd05,ANC.list[[2]], "v007")
head(clu_ANC_pre_05)

write.csv(clu_ANC_pre_05, "results/ANC/clu_ANC_pre_03.csv")



# 1999 

clu_ANC_pre_99 <- funEnv$result.clu.fun('ANC', 'v001', design=ANC.svyd99,ANC.list[[1]], "v007")
head(clu_ANC_pre_99)

write.csv(clu_ANC_pre_99, "results/ANC/clu_ANC_pre_98.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf %>% left_join(DS_ANC_pre_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_ANC_pre_18)



# 2012 transformations 
DS_file_12 <- HD_sf %>% left_join(DS_ANC_pre_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_ANC_pre_12)


# 2005 transformations 
DS_file_05 <- HD_sf %>% left_join(DS_ANC_pre_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_ANC_pre_05)


# 1999 transformations 
DS_file_99 <- HD_sf %>% left_join(DS_ANC_pre_99)

pts_file_99 <- GNshplist_sf[[1]] %>% left_join(clu_ANC_pre_99)


# 2018 map 
GN_ANC18 <- funEnv$tmap.fun3(DS_file, colname="ANC", legtitle="ANC coverage (most recent child)", 
                              maintitle="ANC coverage by HD (2018)", ptsfile=pts_file, "Number of Participants",
                              "ANC") 



CK_ANC18 <- funEnv$tmap.fun3(DS_cona, colname="ANC", legtitle="ANC coverage (most recent child)", 
                                maintitle="ANC coverage by HD (2018)", ptsfile=cky_pts_file, "Number of Participants",
                                "ANC") 

GN_all_ANC18 <- tmap_arrange(GN_ANC18, CK_ANC18)

# 2012 map 
GN_ANC12 <- funEnv$tmap.fun3(DS_file_12, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by District (2012)", ptsfile=pts_file_12, "Number of Participants",
                      "ANC") 


CK_ANC12 <- funEnv$tmap.fun3(DS_cona, colname="ANC", legtitle="ANC coverage (most recent child)", 
                                maintitle="ANC coverage by HD (2012)", ptsfile=cky_pts12, "Number of Participants",
                                "ANC") 


GN_all_ANC12 <- tmap_arrange(GN_ANC12, CK_ANC12)
# 2005 map 
GN_ANC05 <- funEnv$tmap.fun3(DS_file_05, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by District (2005)", ptsfile=pts_file_05, "Number of Participants",
                      "ANC") 


CK_ANC05 <- funEnv$tmap.fun3(DS_cona, colname="ANC", legtitle="ANC coverage (most recent child)", 
                                maintitle="ANC coverage by HD (2005)", ptsfile=cky_pts_file, "Number of Participants",
                                "ANC") 
GN_all_ANC05<- tmap_arrange(GN_ANC05, CK_ANC05)

# 1999 map 
GN_ANC99 <- tmap.fun3(DS_file_99, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by District (1999)", ptsfile=pts_file_99, "Number of Participants",
                      "ANC") 

## Fusion des cartes sur une page

all_ANC <- tmap_arrange(GN_ANC05, GN_ANC12, GN_ANC18)
## Export des cartes
tmap_save(tm = GN_all_ANC05, filename = "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/ANC/2005_all_ANC.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

tmap_save(all_ANC, "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/ANC/all_ANC.png", width=1920, height=1080, asp=0)
