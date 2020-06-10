#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with ANC data for 1993, 2003, 2008, 2013, 2018
ANC.list <- GNIR


look_for(GNIR[[1]], "antenatal")
#table(NGAfiles[[10]]$s226a) #2010
#table(ANC.list[[1]]$m14_1)
# table(NGAfiles[[19]]$m14_1)


# recoding ANC 
ANC.list  <- map(ANC.list, funEnv$recoder.ml4_1_v2)


# key list for ANC (1993, 2003, 2008, 2013, 2018)
keys.ANC <- list(key_list[[1]], key_list[[2]], key_list[[3]], key_list[[5]], key_list[[7]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
ANC.list <- map2(ANC.list, key_list, left_join) #PR datasets



#####################################################################################################
# ANC coverage 
####################################################################################################

# 2018
ANC.list[[4]] <-funEnv$dataclean(ANC.list[[4]], m14_1, v005,'m14_1', 'ANC_3')  
ANC.svyd18 <- funEnv$svydesign.fun(ANC.list[[4]])


DS_ANC_pre_18 <- funEnv$result.fun('ANC_3', 'NAME_2','num_p', design=ANC.svyd18)
head(DS_ANC_pre_18)

#write.csv(DS_ANC_pre_18, "master/results/ANC_3/DS_ANC_pre_18.csv")


# 2012
ANC.list[[3]] <-funEnv$dataclean(ANC.list[[3]], m14_1, v005,'m14_1', 'ANC_3')  
ANC.svyd13 <- funEnv$svydesign.fun(ANC.list[[3]])


DS_ANC_pre_12 <- funEnv$result.fun('ANC_3', 'NAME_2','num_p', design=ANC.svyd13)
head(DS_ANC_pre_12)

#write.csv(DS_ANC_pre_12, "master/results/ANC_3/DS_ANC_pre_12.csv")



# 2005
ANC.list[[2]] <-funEnv$dataclean(ANC.list[[2]], m14_1, v005,'m14_1', 'ANC_3')  
ANC.svyd05 <- funEnv$svydesign.fun(ANC.list[[2]])

DS_ANC_pre_05 <- funEnv$result.fun('ANC_3', 'NAME_2','num_p', design=ANC.svyd05)
head(DS_ANC_pre_05)

#write.csv(DS_ANC_pre_05, "master/results/ANC_3/DS_ANC_pre_05.csv")



# 1999
ANC.list[[1]] <-funEnv$dataclean(ANC.list[[1]], m14_1, v005,'m14_1', 'ANC_3')  
ANC.svyd99 <- funEnv$svydesign.fun(ANC.list[[1]])


DS_ANC_pre_99 <- funEnv$result.fun('ANC_3', 'NAME_2','num_p', design=ANC.svyd99)
head(DS_ANC_pre_99)

#write.csv(DS_ANC_pre_03, "results/ANC_3_Nigeria/DS_ANC_pre_03.csv")






# cluster-level estimates

# 2018

clu_ANC_pre_18 <- funEnv$result.clu.fun('ANC_3', 'v001', design=ANC.svyd18,ANC.list[[4]], "v007")
head(clu_ANC_pre_18)

write.csv(clu_ANC_pre_18, "master/results/ANC_3/clu_ANC_pre_18.csv")



# 2012

clu_ANC_pre_12 <- funEnv$result.clu.fun('ANC_3', 'v001', design=ANC.svyd13,ANC.list[[3]], "v007")
head(clu_ANC_pre_12)

write.csv(clu_ANC_pre_12, "master/results/ANC_3/clu_ANC_pre_12.csv")




# 2005

clu_ANC_pre_05 <- funEnv$result.clu.fun('ANC_3', 'v001', design=ANC.svyd05,ANC.list[[2]], "v007")
head(clu_ANC_pre_05)

write.csv(clu_ANC_pre_05, "master/results/ANC_3/clu_ANC_pre_05.csv")



# 1999

clu_ANC_pre_99 <- funEnv$result.clu.fun('ANC_3', 'v001', design=ANC.svyd99,ANC.list[[1]], "v007")
head(clu_ANC_pre_03)

write.csv(clu_ANC_pre_99, "master/results/ANC_3/clu_ANC_pre_99.csv")



#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf%>% left_join(DS_ANC_pre_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_ANC_pre_18)


# 2012 transformations 
DS_file_12 <- HD_sf%>% left_join(DS_ANC_pre_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_ANC_pre_12)



# 2005 transformations 
DS_file_05 <- HD_sf%>% left_join(DS_ANC_pre_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_ANC_pre_05)



# 1999 transformations 
DS_file_ <- LGAshp_sf%>% left_join(DS_ANC_pre_03)

pts_file_03 <- NGAshplist_sf[[2]] %>% left_join(clu_ANC_pre_03)


# 2018 map 
GN_ANC18 <- funEnv$tmap.fun3(DS_file, colname="ANC_3", legtitle="ANC coverage 3 or more vists", 
                      maintitle="ANC coverage by HD (2018)", ptsfile=pts_file, "Number of Participants",
                      "ANC_3") 



CK_ANC18 <- funEnv$tmap.fun3(DS_cona, colname="ANC_3", legtitle="ANC coverage 3 or more vists", 
                             maintitle="ANC coverage by HD (2018)", ptsfile=cky_pts18, "Number of Participants",
                             "ANC_3") 

GN_all_ANC18<- tmap_arrange(GN_ANC18, CK_ANC18)

# 2012 map 
GN_ANC12 <- funEnv$tmap.fun3(DS_file_12, colname="ANC_3", legtitle="ANC coverage 3 or more vists (most recent child)", 
                      maintitle="ANC coverage by Health District (2012)", ptsfile=pts_file_12, "Number of Participants",
                      "ANC_3") 

CK_ANC12 <- funEnv$tmap.fun3(DS_cona, colname="ANC_3", legtitle="ANC coverage 3 or more vists", 
                             maintitle="ANC coverage by HD (2012)", ptsfile=cky_pts_file12, "Number of Participants",
                             "ANC_3") 

GN_all_ANC12<-tmap_arrange(GN_ANC12, CK_ANC12)

# 2005 map 
GN_ANC05 <- funEnv$tmap.fun3(DS_file_05, colname="ANC_3", legtitle="ANC coverage 3 or more vists (most recent child)", 
                      maintitle="ANC coverage by HD (2005)", ptsfile=pts_file_05, "Number of Participants",
                      "ANC_3") 


CK_ANC05 <- funEnv$tmap.fun3(DS_cona, colname="ANC_3", legtitle="ANC coverage 3 or more vists", 
                             maintitle="ANC coverage by HD (2005)", ptsfile=cky_pts_file05, "Number of Participants",
                             "ANC_3") 


GN_all_ANC05<-tmap_arrange(GN_ANC05,CK_ANC05)
# 1999 map 
NG_ANC99 <- tmap.fun3(DS_file_99, colname="ANC_3", legtitle="ANC coverage 3 or more vists (most recent child)", 
                      maintitle="ANC coverage by HD (1999)", ptsfile=pts_file_99, "Number of Participants",
                      "ANC_3") 

### Merge maps
all_ANC_3 <- tmap_arrange(GN_ANC05, GN_ANC12,GN_ANC18)


## Export maps
tmap_save(tm = CK_ANC05, filename = "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/ANC_3/CK_ANC3.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


tmap_save(all_ANC_3, "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/ANC_3/all_ANC3.png", width=1920, height=1080, asp=0)


