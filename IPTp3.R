# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with IPTP3 data for 2010, 2014, 2017/18
IPTP3.list <- list(GNIR[[2]], GNIR[[3]], GNIR[[4]])

table(GNIR[[4]]$ml1_1)

# recoding IPTP3 

# var_label(IPTP3.list[[4]]$ml1_1)
#table(IPTP3.list[[2]]$IPTP3)
# table(BFfiles[[16]]$ml1_1)


IPTP3.list  <- map(IPTP3.list, funEnv$recoder.ml1)


# key list for IPTP3
keys.IPTP3 <- list(key_list[[2]], key_list[[3]],key_list[[4]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
IPTP3.list <- map2(IPTP3.list, keys.IPTP3, left_join) #PR datasets



#####################################################################################################
# IPTP3 estimates 
####################################################################################################

# 2018
IPTP3.list[[3]] <-funEnv$dataclean(IPTP3.list[[3]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd17_18 <- funEnv$svydesign.fun(IPTP3.list[[3]])


DS_IPTP3_pre_18 <- funEnv$result.fun('IPTP3', 'NAME_2','num_p', design=IPTP3.svyd17_18)
head(DS_IPTP3_pre_18)

write.csv(DS_IPTP3_pre_18, "master/results/IPTP3/DS_IPTP3_pre_18.csv")


# 2012
IPTP3.list[[2]] <-funEnv$dataclean(IPTP3.list[[2]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd12 <- funEnv$svydesign.fun(IPTP3.list[[2]])


DS_IPTP3_pre_12 <- funEnv$result.fun('IPTP3', 'NAME_2','num_p', design=IPTP3.svyd12)
head(DS_IPTP3_pre_12)

write.csv(DS_IPTP3_pre_12, "master/results/IPTP3/DS_IPTP3_pre_12.csv")


# 2005
IPTP3.list[[1]] <-funEnv$dataclean(IPTP3.list[[1]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd05 <- funEnv$svydesign.fun(IPTP3.list[[1]])


DS_IPTP3_pre_05 <- funEnv$result.fun('IPTP3', 'NAME_2','num_p', design=IPTP3.svyd05)
head(DS_IPTP3_pre_05)

write.csv(DS_IPTP3_pre_05, "master/results/IPTP3/DS_IPTP3_pre_05.csv")



# cluster-level estimates

# 2018

clu_IPTP3_pre_18 <- funEnv$result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd17_18,IPTP3.list[[3]])
head(clu_IPTP3_pre_18)

write.csv(clu_IPTP3_pre_18, "master/results/IPTP3/clu_IPTP3_pre_18.csv")



# 2012

clu_IPTP3_pre_12 <- funEnv$result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd12,IPTP3.list[[2]])
head(clu_IPTP3_pre_12)

write.csv(clu_IPTP3_pre_12, "master/results/IPTP3/clu_IPTP3_pre_12.csv")



# 2005

clu_IPTP3_pre_05 <- funEnv$result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd05,IPTP3.list[[1]])
head(clu_IPTP3_pre_05)

write.csv(clu_IPTP3_pre_05, "master/results/IPTP3/clu_IPTP3_pre_05.csv")



#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf %>% left_join(DS_IPTP3_pre_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_IPTP3_pre_18)


# 2012 transformations 
DS_file_12 <- HD_sf %>% left_join(DS_IPTP3_pre_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_IPTP3_pre_12)


# 2005 transformations 
DS_file_05 <- HD_sf %>% left_join(DS_IPTP3_pre_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_IPTP3_pre_05)


# Map 2018 
GN_IPTP18 <- funEnv$tmap.fun3(DS_file, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                       maintitle="IPTP coverage by District (2018)", ptsfile=pts_file, "Number of Participants",
                       "IPTP3") 

CK_IPTP18 <- funEnv$tmap.fun3(DS_cona, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                              maintitle="IPTP coverage in conakry (2018)", ptsfile=cky_pt18, "Number of Participants",
                              "IPTP3") 
IPTp18 <- tmap_arrange(GN_IPTP18, CK_IPTP18)
# Map 2012
GN_IPTP12 <- funEnv$tmap.fun3(DS_file_12, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                       maintitle="IPTP coverage by District (2012)", ptsfile=pts_file_12, "Number of Participants",
                       "IPTP3") 

CK_IPTP12 <- funEnv$tmap.fun3(DS_cona, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                              maintitle="IPTP coverage in conakry (2012)", ptsfile=cky_pt12, "Number of Participants",
                              "IPTP3")
IPTp12 <- tmap_arrange(GN_IPTP12, CK_IPTP12)

# Map 2005
GN_IPTP05 <- funEnv$tmap.fun3(DS_file_05, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                       maintitle="IPTP coverage by District (2005)", ptsfile=pts_file_05, "Number of Participants",
                       "IPTP3") 

CK_IPTP05 <- funEnv$tmap.fun3(DS_cona, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                              maintitle="IPTP coverage in conakry (2005)", ptsfile=cky_pt05, "Number of Participants",
                              "IPTP3")

IPTp05 <- tmap_arrange(GN_IPTP05, CK_IPTP05)
# Fusion des maps sur une même page
all_IPTP <- tmap_arrange(GN_IPTP05, GN_IPTP12, GN_IPTP18)
## Exporter les maps sous pdf
tmap_save(tm =IPTp05,filename = "master/results/IPTP3/IPTp05.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
