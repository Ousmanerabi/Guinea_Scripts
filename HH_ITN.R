# creating list of files with ANC data for 1999, 2005, 2012, 2018 
HH_ITN.list <- list(GNIR[[2]],GNIR[[3]], GNIR[[4]])

look_for(GNIR, "hml1")

# creates two new variables named hh_net (number of household members per net) and net_ratio 
#(categorical variable- if > 2 then access is 0, If 2 or less than access is 1)
HH_ITN.list  <- map(HH_ITN.list, funEnv$recoder.nets)
table(HH_ITN.list[[3]]$net_ratio)

HH_ITN.list <- map(HH_ITN.list, funEnv$survey.month.fun)#creates survey month and changes hv001 to v001 to enable left_join 

# key list for ANC
keys.HH_ITN <- list(key_list[[2]],key_list[[3]], key_list[[4]]) 
#changing to a list of keys 



# key datasets and dhs/mis datasets are joined  
HH_ITN.list <- map2(HH_ITN.list, keys.HH_ITN, left_join) #PR datasets


#####################################################################################################
# HH_ITN_access
####################################################################################################

# 2018
HH_ITN.list[[3]] <-funEnv$dataclean.HH(HH_ITN.list[[3]], net_ratio, hv005,'net_ratio', 'net_ratio')  
HH_ITN.svyd18 <- funEnv$svydesign.fun(HH_ITN.list[[3]])

table(HH_ITN.list[[3]]$net_ratio)

DS_HH_ITN_18 <- result.fun.HH('net_ratio', 'NAME_2','num_p', design=HH_ITN.svyd18)
head(DS_HH_ITN_18)

write.csv(DS_HH_ITN_18, "master/results/HH_ITN/DS_HH_ITN_18.csv")


# 2012
HH_ITN.list[[2]] <-funEnv$dataclean.HH(HH_ITN.list[[2]], net_ratio, hv005,'net_ratio', 'net_ratio')  
HH_ITN.svyd12 <- funEnv$svydesign.fun(HH_ITN.list[[2]])


DS_HH_ITN_12 <- result.fun.HH('net_ratio', 'NAME_2','num_p', design=HH_ITN.svyd12)
head(DS_HH_ITN_12)

write.csv(DS_HH_ITN_12, "master/results/HH_ITN/DS_HH_ITN_12.csv")


# 2005
HH_ITN.list[[1]] <-funEnv$dataclean.HH(HH_ITN.list[[1]], net_ratio, hv005,'net_ratio', 'net_ratio')  
HH_ITN.svyd05 <- funEnv$svydesign.fun(HH_ITN.list[[1]])


DS_HH_ITN_05 <- result.fun.HH('net_ratio', 'NAME_2','num_p', design=HH_ITN.svyd05)
head(DS_HH_ITN_05)

write.csv(DS_HH_ITN_05, "master/results/HH_ITN/DS_HH_ITN_05.csv")

# cluster-level estimates

# 2018

clu_HH_ITN_18 <- funEnv$result.clu.fun.HH('net_ratio', 'v001', design=HH_ITN.svyd18,HH_ITN.list[[3]])
head(clu_HH_ITN_18)

write.csv(clu_HH_ITN_18, "master/results/HH_ITN/clu_U5_ITN_18.csv")


# 2012

clu_HH_ITN_12 <- funEnv$result.clu.fun.HH('net_ratio', 'v001', design=HH_ITN.svyd12,HH_ITN.list[[2]])
head(clu_HH_ITN_12)

write.csv(clu_HH_ITN_12, "master/results/HH_ITN/clu_U5_ITN_12.csv")


# 2005

clu_HH_ITN_05 <- funEnv$result.clu.fun.HH('net_ratio', 'v001', design=HH_ITN.svyd05,HH_ITN.list[[1]])
head(clu_HH_ITN_05)

write.csv(clu_HH_ITN_05, "master/results/HH_ITN/clu_U5_ITN_05.csv")



#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- HD_sf %>% left_join(DS_HH_ITN_18)

pts_file <- GNshplist_sf[[4]] %>% left_join(clu_HH_ITN_18)



# 2012 transformations 
DS_file_12 <- HD_sf %>% left_join(DS_HH_ITN_12)

pts_file_12 <- GNshplist_sf[[3]] %>% left_join(clu_HH_ITN_12)


# 2005 transformations 
DS_file_05 <- HD_sf %>% left_join(DS_HH_ITN_05)

pts_file_05 <- GNshplist_sf[[2]] %>% left_join(clu_HH_ITN_05)


# 2018 map 
GN_HH_ITN18 <- funEnv$tmap.fun3(DS_file, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                         maintitle="Household (HH) ITN access by District (2018)", ptsfile=pts_file, "Number of Households",
                         "net_ratio")


CK_HH_ITN18 <- funEnv$tmap.fun3(DS_cona, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                              maintitle="Household (HH) ITN access by District (2018)", ptsfile=cky_pt18, "Number of Households",
                              "net_ratio") 

GN_all_HH_ITN18<-tmap_arrange(GN_HH_ITN18, CK_HH_ITN18)  

# 2012 map 
GN_HH_ITN12 <- funEnv$tmap.fun3(DS_file_12, colname="net_ratio", legtitle="HH with 2 or fewer  persons per net", 
                         maintitle="Household (HH) ITN access by District (2012)", ptsfile=pts_file_12, "Number of Households",
                         "net_ratio") 

CK_HH_ITN12 <- funEnv$tmap.fun3(DS_cona, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                                maintitle="Household (HH) ITN access by District (2012)", ptsfile=cky_pt18, "Number of Households",
                                "net_ratio") 

GN_all_HH_ITN12 <- tmap_arrange(GN_HH_ITN12, CK_HH_ITN12)
# 2005 map 
GN_HH_ITN05 <- funEnv$tmap.fun3(DS_file_05, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                         maintitle="Household (HH) ITN access by District (2005)", ptsfile=pts_file_05, "Number of Households",
                         "net_ratio")



CK_HH_ITN05 <- funEnv$tmap.fun3(DS_cona, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                                maintitle="Household (HH) ITN access by District (2005)", ptsfile=cky_pts05, "Number of Households",
                                "net_ratio") 

GN_all_HH_ITN05 <- tmap_arrange(GN_HH_ITN05, CK_HH_ITN05)

##

all_HH_ITN <- tmap_arrange(GN_HH_ITN05,GN_HH_ITN12,GN_HH_ITN18)

###
tmap_save(tm = CK_HH_ITN05, filename = "/Users/ousmanediallo/Box/NU-malaria-team/projects/hbhi_guinea/maps/HH_ITN/CK_HH_ITN05.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
