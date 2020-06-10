# creating list of files with ANC data for 2005, 2012, 2018 
U5_ITN.list <- list(GNIR[[2]], GNIR[[3]], GNIR[[4]])

val_labels(U5_ITN.list[[3]]$ml0)

U5_ITN.list  <- map(U5_ITN.list, funEnv$recoder.ml0)


# key list for ANC
keys.U5_ITN <- list(key_list[[2]],key_list[[3]], key_list[[4]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
U5_ITN.list <- map2(U5_ITN.list, keys.U5_ITN, left_join) #PR datasets



#####################################################################################################
# U5  coverage 
####################################################################################################

# 2018
U5_ITN.list[[3]] <-funEnv$dataclean(U5_ITN.list[[3]], ml0, v005,'ml0', 'ITN')  
U5_ITN.svyd18 <- funEnv$svydesign.fun(U5_ITN.list[[3]])


DS_U5_ITN_18 <- funEnv$result.fun('ITN', 'NAME_2','num_p', design=U5_ITN.svyd18)
head(DS_U5_ITN_18)

write.csv(DS_U5_ITN_18, "master/results/U5_ITN/DS_U5_ITN_18.csv")


# 2012
U5_ITN.list[[2]] <-funEnv$dataclean(U5_ITN.list[[2]], ml0, v005,'ml0', 'ITN')  
U5_ITN.svyd14 <- funEnv$svydesign.fun(U5_ITN.list[[3]])


DS_U5_ITN_12 <- funEnv$result.fun('ITN', 'NAME_2','num_p', design=U5_ITN.svyd14)
head(DS_U5_ITN_12)

write.csv(DS_U5_ITN_12, "master/results/U5_ITN/DS_U5_ITN_12.csv")



# 2005
U5_ITN.list[[1]] <-funEnv$dataclean(U5_ITN.list[[1]], ml0, v005,'ml0', 'ITN')  
U5_ITN.svyd05 <- funEnv$svydesign.fun(U5_ITN.list[[1]])


DS_U5_ITN_05 <- funEnv$result.fun('ITN', 'NAME_2','num_p', design=U5_ITN.svyd05)
head(DS_U5_ITN_05)

write.csv(DS_U5_ITN_05, "master/results/U5_ITN/DS_U5_ITN_05.csv")





# cluster-level estimates

# 2018

clu_U5_ITN_18 <- funEnv$result.clu.fun('ITN', 'v001', design=U5_ITN.svyd18,U5_ITN.list[[3]])
head(clu_U5_ITN_18)

write.csv(clu_U5_ITN_18, "master/results/U5_ITN/clu_U5_ITN_18.csv")


# 2012

clu_U5_ITN_12 <- funEnv$result.clu.fun('ITN', 'v001', design=U5_ITN.svyd14,U5_ITN.list[[2]])
head(clu_U5_ITN_12)

write.csv(clu_U5_ITN_12, "master/results/U5_ITN/clu_U5_ITN_12.csv")


# 2005

clu_U5_ITN_05 <- funEnv$result.clu.fun('ITN', 'v001', design=U5_ITN.svyd05,U5_ITN.list[[1]])
head(clu_U5_ITN_05)

write.csv(clu_U5_ITN_05, "results/U5_ITN/clu_U5_ITN_05.csv")





#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf %>% left_join(DS_U5_ITN_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_U5_ITN_18)


# 2012 transformations 
DS_file_12 <- HD_sf %>% left_join(DS_U5_ITN_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_U5_ITN_12)


# 2005 transformations 
DS_file_05 <- HD_sf %>% left_join(DS_U5_ITN_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_U5_ITN_05)



# 2018 map 
GN_ITN18 <- funEnv$tmap.fun3(DS_file, colname="ITN", legtitle="ITN use among under-fives", 
                      maintitle="ITN use among under-fives by District (2018)", ptsfile=pts_file, "Number of Participants",
                      "ITN") 


CK_ITN18 <- funEnv$tmap.fun3(DS_cona, colname="ITN", legtitle="ITN use among under-fives", 
                             maintitle="ITN use among under-fives by District (2018)", ptsfile=cky_pt, "Number of Participants",
                             "ITN")

GN_all_ITN_18<-tmap_arrange(GN_ITN18, CK_ITN18)
# 2012 map 
GN_ITN12 <- funEnv$tmap.fun3(DS_file_12, colname="ITN", legtitle="ITN use among under-fives", 
                      maintitle="ITN use among under-fives by District (2012)", ptsfile=pts_file_12, "Number of Participants",
                      "ITN") 


CK_ITN12 <- funEnv$tmap.fun3(DS_cona, colname="ITN", legtitle="ITN use among under-fives", 
                             maintitle="ITN use among under-fives by District (2012)", ptsfile=cky_pt, "Number of Participants",
                             "ITN")

GN_all_ITN12 <- tmap_arrange(GN_ITN12, CK_ITN12)

# 2005 map 
GN_ITN05 <- funEnv$tmap.fun3(DS_file_05, colname="ITN", legtitle="ITN use among under-fives", 
                      maintitle="ITN use among under-fives by District (2005)", ptsfile=pts_file_05, "Number of Participants",
                      "ITN") 



CK_ITN05 <- funEnv$tmap.fun3(DS_cona, colname="ITN", legtitle="ITN use among under-fives", 
                             maintitle="ITN use among under-fives by District (2005)", ptsfile=cky_pt, "Number of Participants",
                             "ITN")

GN_all_ITN_05 <- tmap_arrange(GN_ITN05, CK_ITN05)
## Merge des maps

all_U5_ITN <- tmap_arrange(GN_ITN05, GN_ITN12, GN_ITN18)

## Export des cartes sous format pdf
tmap_save(tm = GN_all_ITN12, filename = "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/U5_ITN/GGN_all_ITN12.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
