# creating list of files with PR data for 2005, 2012, 2018
PR.list <- list(GNIR[[2]], GNIR[[3]], GNIR[[4]])

PR.list[[2]]$hv103 <- as.factor(PR.list[[2]]$hv103)

PR.list <- lapply(PR.list, subset, hv103 == "1")

PR.list[[3]]$hml12 <- as.factor(PR.list[[3]]$hml12)


PR.list <- map(PR.list, funEnv$recode_itn)

PR.list <- map(PR.list, funEnv$survey.month.fun)

# key list for ITN (2005, 2012, 2018)
keys.hh.itn <- list(key_list[[2]], key_list[[3]], key_list[[4]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
hh.itn.list <- map2(PR.list, keys.hh.itn, left_join) #PR datasets

table(PR.list[[1]]$time2)

#####################################################################################################
# ANC coverage 
####################################################################################################

# 2018
hh.itn.list[[3]] <-funEnv$dataclean.HH(hh.itn.list[[3]], hml12, hv005,'hh_itn', 'hh.itn')  
hh.itn.svyd18 <- funEnv$svydesign.fun(hh.itn.list[[3]])


DS_hh_itn_18 <- funEnv$result.fun('hh.itn', 'NAME_2','num_p', design=hh.itn.svyd18)
head(DS_hh_itn_18)

summary(DS_hh_itn_18$hh.itn)

write.csv(DS_hh_itn_18, "master/results/DHS_HH_ITN/DS_hh_itn_18.csv")



# 2012
hh.itn.list[[2]] <-funEnv$dataclean.HH(hh.itn.list[[2]], hml12, hv005,'hh_itn', 'hh.itn')  
hh.itn.svyd12 <- funEnv$svydesign.fun(hh.itn.list[[2]])


DS_hh_itn_12 <- funEnv$result.fun('hh.itn', 'NAME_2','num_p', design=hh.itn.svyd12)
head(DS_hh_itn_12)

summary(DS_hh_itn_12$hh.itn)

write.csv(DS_hh_itn_12, "master/results/DHS_HH_ITN/DS_hh_itn_12.csv")



# 2005
hh.itn.list[[1]] <-funEnv$dataclean.HH(hh.itn.list[[1]], hml12, hv005,'hh_itn', 'hh.itn')  
hh.itn.svyd05 <- funEnv$svydesign.fun(hh.itn.list[[1]])


DS_hh_itn_05 <- funEnv$result.fun('hh.itn', 'NAME_2','num_p', design=hh.itn.svyd05)
head(DS_hh_itn_05)

summary(DS_hh_itn_05$hh.itn)

write.csv(DS_hh_itn_05, "master/results/DHS_HH_ITN/DS_hh_itn_05.csv")



# cluster-level estimates

# 2018

clu_HH_ITN_18 <- funEnv$result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd18,hh.itn.list[[3]])
head(clu_HH_ITN_18)

write.csv(clu_HH_ITN_18, "master/results/DHS_HH_ITN/clu_hh_itn_18.csv")

clu_ptest_num_15 <- result.clu.fun('p_test', 'v001', design=hml32.svyd15,pfpr.list[[2]])
head(clu_ptest_num_15)


# 2012
clu_HH_ITN_12 <- funEnv$result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd12,hh.itn.list[[2]])
head(clu_HH_ITN_12)

write.csv(clu_HH_ITN_12, "master/results/DHS_HH_ITN/clu_hh_itn_12.csv")


# 2005
clu_HH_ITN_05 <- funEnv$result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd05,hh.itn.list[[1]])
head(clu_HH_ITN_05)

write.csv(clu_HH_ITN_05, "master/results/DHS_HH_ITN/clu_hh_itn_05.csv")



#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf %>% left_join(DS_hh_itn_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_HH_ITN_18)


# 2012 transformations 
DS_file_12 <- HD_sf %>% left_join(DS_hh_itn_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_HH_ITN_12)


# 2005 transformations 
DS_file_05 <- HD_sf %>% left_join(DS_hh_itn_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_HH_ITN_05)



# 2018 map 
GN_HH_ITN18 <- funEnv$tmap.fun3(DS_file, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                         maintitle="Household (HH) ITN use by District (2018)", ptsfile=pts_file, "Number of Participants",
                         "hh.itn")



CK_HH_ITN18 <- funEnv$tmap.fun3(DS_cona, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                             maintitle="Household (HH) ITN use by District (2018)", ptsfile=cky_pt, "Number of Participants",
                             "hh.itn")

GN_all_HH_ITN18 <- tmap_arrange(GN_HH_ITN18, CK_HH_ITN18)

# 2012 map 
GN_HH_ITN12 <- funEnv$tmap.fun3(DS_file_12, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                         maintitle="Household (HH) ITN use by District (2012)", ptsfile=pts_file_12, "Number of Participants",
                         "hh.itn")



CK_HH_ITN12 <- funEnv$tmap.fun3(DS_cona, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                             maintitle="Household (HH) ITN use by District (2012)", ptsfile=cky_pt, "Number of Participants",
                             "hh.itn")


GN_all_HH_ITN12 <- tmap_arrange(GN_HH_ITN12, CK_HH_ITN12)


# 2010 map 
GN_HH_ITN05 <- funEnv$tmap.fun3(DS_file_05, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                         maintitle="Household (HH) ITN use by District (2010)", ptsfile=pts_file_05, "Number of Participants",
                         "hh.itn")


CK_HH_ITN05 <- funEnv$tmap.fun3(DS_cona, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                                maintitle="Household (HH) ITN use by District (2005)", ptsfile=cky_pt, "Number of Participants",
                                "hh.itn")

GN_all_HH_ITN05 <- tmap_arrange(GN_HH_ITN05, CK_HH_ITN05)

all_hh.itn <- tmap_arrange(GN_HH_ITN05,GN_HH_ITN12,GN_HH_ITN18)


tmap_save(tm = GN_all_HH_ITN18, filename = "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/DHS_HH_ITN/GN_all_HH_ITN18.pdf",width=18, height=18, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



