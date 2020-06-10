#U5 bed net list includes 2005, 2012, 2018

U5_ITN_access <- list(GNIR[[2]],GNIR[[3]], GNIR[[4]])

#recoding variables 
U5_ITN_access  <- map(U5_ITN_access, funEnv$recoder.hv227)

table(U5_ITN_access[[1]]$hv227)
var_label(U5_ITN_access[[3]]$hv228)


# key list for ANC
keys.U5_ITN_access <- list(key_list[[2]],key_list[[3]], key_list[[4]]) 
#changing to a list of keys 

U5_ITN_access <- map(U5_ITN_access, funEnv$survey.month.fun)


# key datasets and dhs/mis datasets are joined  
U5_ITN_access <- map2(U5_ITN_access, keys.U5_ITN_access, left_join) #PR datasets

#####################################################################################################
# U5 ITN coverage 
####################################################################################################

# 2018
U5_ITN_access[[3]] <- U5_ITN_access[[3]] %>% filter(hv105<=5)

table(U5_ITN_access[[3]]$hv227)

U5_ITN_access[[3]] <-funEnv$dataclean.HH(U5_ITN_access[[3]], hv227, hv005,'hv227', 'U5_ITN_access') 

U5_ITN_access.svyd18 <- funEnv$svydesign.fun(U5_ITN_access[[3]])


DS_U5a_pre_18 <- funEnv$result.fun('U5_ITN_access', 'NAME_2','num_p', design=U5_ITN_access.svyd18)
head(DS_U5a_pre_18)

write.csv(DS_U5a_pre_18, "master/results/U5_ITN_access/DS_U5a_pre_18.csv")


# 2012
U5_ITN_access[[2]] <- U5_ITN_access[[2]] %>% filter(hv105<=5)

U5_ITN_access[[2]] <-funEnv$dataclean.HH(U5_ITN_access[[2]], hv227, hv005,'hv227', 'U5_ITN_access')  
U5_ITN_access.svyd12 <- funEnv$svydesign.fun(U5_ITN_access[[2]])


DS_U5a_pre_12 <- funEnv$result.fun('U5_ITN_access', 'NAME_2','num_p', design=U5_ITN_access.svyd12)
head(DS_U5a_pre_12)

write.csv(DS_U5a_pre_12, "master/results/U5_ITN_access/DS_U5a_pre_12.csv")


# 2005
U5_ITN_access[[1]] <- U5_ITN_access[[1]] %>% filter(hv105<=5)

U5_ITN_access[[1]] <-funEnv$dataclean.HH(U5_ITN_access[[1]], hv227, hv005,'hv227', 'U5_ITN_access')  
U5_ITN_access.svyd05 <- funEnv$svydesign.fun(U5_ITN_access[[1]])


DS_U5a_pre_05 <- funEnv$result.fun('U5_ITN_access', 'NAME_2','num_p', design=U5_ITN_access.svyd05)
head(DS_U5a_pre_05)

write.csv(DS_U5a_pre_05, "master/results/U5_ITN_access/DS_U5a_pre_05.csv")



#cluster-level estimates 

# 2018

clu_U5a_pre_18 <- funEnv$result.clu.fun('U5_ITN_access', 'v001', design=U5_ITN_access.svyd18,
                                 U5_ITN_access[[3]])
head(clu_U5a_pre_18)

write.csv(clu_U5a_pre_18, "master/results/U5_ITN_access/clu_U5a_pre_18.csv")



# 2012

clu_U5a_pre_12 <- funEnv$result.clu.fun('U5_ITN_access', 'v001', design=U5_ITN_access.svyd12,
                                 U5_ITN_access[[2]])
head(clu_U5a_pre_12)

write.csv(clu_U5a_pre_12, "master/results/U5_ITN_access/clu_U5a_pre_12.csv")



# 2005

clu_U5a_pre_05 <- funEnv$result.clu.fun('U5_ITN_access', 'v001', design=U5_ITN_access.svyd05,
                                 U5_ITN_access[[1]])
head(clu_U5a_pre_05)

write.csv(clu_U5a_pre_05, "master/results/U5_ITN_access/clu_U5a_pre_05.csv")




#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf %>% left_join(DS_U5a_pre_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_U5a_pre_18)


# 2012 transformations 
DS_file_12 <- HD_sf %>% left_join(DS_U5a_pre_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_U5a_pre_12)


# 2005 transformations 
DS_file_05 <- HD_sf %>% left_join(DS_U5a_pre_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_U5a_pre_05)




# 2018 map 
GN_U5a18 <- funEnv$tmap.fun3(DS_file, colname="U5_ITN_access", legtitle="U5 ITN access", 
                      maintitle="Insecticide Treated Net Access among Under-fives by District (2018)", ptsfile=pts_file, "Number of Participants",
                      "U5_ITN_access") 

CK_U5a18 <- funEnv$tmap.fun3(DS_cona, colname="U5_ITN_access", legtitle="U5 ITN access", 
                             maintitle="Insecticide Treated Net Access among Under-fives by District (2018)", ptsfile=pts_file, "Number of Participants",
                             "U5_ITN_access") 

GN_all_U5a18 <- tmap_arrange(GN_U5a18, CK_U5a18)
# 2012 map 
GN_U5a12 <- funEnv$tmap.fun3(DS_file_12, colname="U5_ITN_access", legtitle="U5 ITN access", 
                      maintitle="Insecticide Treated Net Access among Under-fives by District (2012)", ptsfile=pts_file_12, "Number of Participants",
                      "U5_ITN_access") 

CK_U5a12 <- funEnv$tmap.fun3(DS_cona, colname="U5_ITN_access", legtitle="U5 ITN access", 
                             maintitle="Insecticide Treated Net Access among Under-fives by District (2012)", ptsfile=pts_file, "Number of Participants",
                             "U5_ITN_access") 

GN_all_U5a12 <- tmap_arrange(GN_U5a12, CK_U5a12)
# 2005 map 
GN_U5a05 <- funEnv$tmap.fun3(DS_file_05, colname="U5_ITN_access", legtitle="U5 ITN access", 
                      maintitle="Insecticide Treated Net Access among Under-fives by District (2005)", ptsfile=pts_file_05, "Number of Participants",
                      "U5_ITN_access") 


CK_U5a05 <- funEnv$tmap.fun3(DS_cona, colname="U5_ITN_access", legtitle="U5 ITN access", 
                             maintitle="Insecticide Treated Net Access among Under-fives by District (2005)", ptsfile=pts_file, "Number of Participants",
                             "U5_ITN_access") 


GN_al_U5a05 <- tmap_arrange(GN_U5a05, CK_U5a05)


all_U5a <- tmap_arrange(GN_U5a05,GN_U5a12, GN_U5a18)

tmap_save(tm = GN_al_U5a05, filename = "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/U5_ITN_access/GN_all_U5a05.pdf",width=18, height=18, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
