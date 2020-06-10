#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with IPTP data for 1999, 2005, 2012, 2018 
IPTP.list <- list(GNIR[[2]],GNIR[[3]], GNIR[[4]])

# recoding IPTP 


table(IPTP.list[[3]]$m49a_1)

IPTP.list  <- map(IPTP.list, funEnv$recoder.m49a)


# key list for IPTP
keys.IPTP <- list(key_list[[2]],key_list[[3]], key_list[[4]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
IPTP.list <- map2(IPTP.list, keys.IPTP, left_join) #PR datasets


#####################################################################################################
# IPTP estimates 
####################################################################################################

# 2018
IPTP.list[[3]] <-funEnv$dataclean(IPTP.list[[3]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd18 <- funEnv$svydesign.fun(IPTP.list[[3]])


DS_IPTP_pre_18 <- funEnv$result.fun('IPTP', 'NAME_2','num_p', design=IPTP.svyd18)
head(DS_IPTP_pre_18)

write.csv(DS_IPTP_pre_18, "master/results/IPTP/DS_IPTP_pre_18.csv")


# 2012
IPTP.list[[2]] <-funEnv$dataclean(IPTP.list[[2]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd12 <- funEnv$svydesign.fun(IPTP.list[[2]])


DS_IPTP_pre_12 <- funEnv$result.fun('IPTP', 'NAME_2','num_p', design=IPTP.svyd12)
head(DS_IPTP_pre_12)

write.csv(DS_IPTP_pre_12, "master/results/IPTP/DS_IPTP_pre_12.csv")


# 2005
IPTP.list[[1]] <-funEnv$dataclean(IPTP.list[[1]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd05 <- funEnv$svydesign.fun(IPTP.list[[1]])


DS_IPTP_pre_05 <- funEnv$result.fun('IPTP', 'NAME_2','num_p', design=IPTP.svyd05)
head(DS_IPTP_pre_05)

write.csv(DS_IPTP_pre_05, "master/results/IPTP/DS_IPTP_pre_05.csv")



# cluster-level estimates

# 2018

clu_IPTP_pre_18 <- funEnv$result.clu.fun('IPTP', 'v001', design=IPTP.svyd18,IPTP.list[[3]])
head(clu_IPTP_pre_18)

write.csv(clu_IPTP_pre_18, "master/results/IPTP/clu_IPTP_pre_18.csv")



# 2012

clu_IPTP_pre_12 <- funEnv$result.clu.fun('IPTP', 'v001', design=IPTP.svyd12,IPTP.list[[2]])
head(clu_IPTP_pre_12)

write.csv(clu_IPTP_pre_12, "master/results/IPTP/clu_IPTP_pre_12.csv")


# 2005

clu_IPTP_pre_05 <- funEnv$result.clu.fun('IPTP', 'v001', design=IPTP.svyd05,IPTP.list[[1]])
head(clu_IPTP_pre_05)

write.csv(clu_IPTP_pre_05, "master/results/IPTP/clu_IPTP_pre_05.csv")

#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf %>% left_join(DS_IPTP_pre_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_IPTP_pre_18)


# 2012 transformations 
DS_file_12 <- HD_sf %>% left_join(DS_IPTP_pre_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_IPTP_pre_12)


# 2005 transformations 
DS_file_05 <- HD_sf %>% left_join(DS_IPTP_pre_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_IPTP_pre_05)


# 2018 map 
GN_IPTP18 <- funEnv$tmap.fun3(DS_file, colname="IPTP", legtitle="IPTP coverage", 
                        maintitle="IPTP coverage (2018)", ptsfile=pts_file, "Number of Participants",
                        "IPTP") 


CK_IPTP18 <- funEnv$tmap.fun3(DS_cona, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                              maintitle="IPTP coverage in conakry (2018)", ptsfile=cky_pt18, "Number of Participants",
                              "IPTP") 
IPTP18<- tmap_arrange(GN_IPTP18, CK_IPTP18)
# 2012 map 
GN_IPTP12 <- funEnv$tmap.fun3(DS_file_12, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (2012)", ptsfile=pts_file_12, "Number of Participants",
                        "IPTP") 

CK_IPTP12 <- funEnv$tmap.fun3(DS_cona_12, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                              maintitle="IPTP coverage in conakry (2012)", ptsfile=cky_pt12, "Number of Participants",
                              "IPTP") 
IPTP2012<-tmap_arrange(GN_IPTP12,CK_IPTP12)

# 2005 map 
GN_IPTP05 <- funEnv$tmap.fun3(DS_file_05, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (2005)", ptsfile=pts_file_05, "Number of Participants",
                        "IPTP") 


CK_IPTP05 <- funEnv$tmap.fun3(DS_cona, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                              maintitle="IPTP coverage in conakry (2005)", ptsfile=cky_pts_file05, "Number of Participants",
                              "IPTP") 

IPTP05<- tmap_arrange(GN_IPTP05, CK_IPTP05)

# 1999 map 
GN_IPTP99 <- tmap.fun3(DS_file_99, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (1999)", ptsfile=pts_file_99, "Number of Participants",
                        "IPTP") 

CK_IPTP99 <- funEnv$tmap.fun3(DS_cona_99, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                              maintitle="IPTP coverage in conakry (1999)", ptsfile=cky_pt99, "Number of Participants",
                              "IPTP") 




all_IPTP <- tmap_arrange(GN_IPTP05,GN_IPTP12,GN_IPTP18)


tmap_save(tm = IPTP05,filename = "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/IPTP/IPTP05.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

