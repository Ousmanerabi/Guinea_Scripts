# #Make sure we have the right packages
# list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
#                       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
#                       "fuzzyjoin", "splitstackshape")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

# Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("master", "src", "Fonctions_nettoyage.R"), envir = funEnv, toplevel.env = funEnv)

source("/Users/ousmanediallo/Box/NU-malaria-team/data/guinea_dhs/data_analysis/master/src/Fonctions_nettoyage.R")

# we read in files 
GNIR <-funEnv$read.files("*GNIR.*\\.DTA", "master/data")

# head(ACTwatch[[1]])
# look_for(ACTwatch[[4]], "longitude")
# val_labels(ACTwatch[[4]]$st_anyACT)
# table(ACTwatch[[4]]$e5)

# summary(ACTwatch[[4]]$c5a)
# 
# Nas <-ACTwatch[[4]][ACTwatch[[4]]$c3 == "ENUGU",]

# cluster locations 
GNshpfiles <- list.files(pattern="*FL.*\\.shp$", full.names=TRUE, recursive=T)

GNshplist <- sapply(GNshpfiles,shapefile, simplify = F)

GNshplist_sf <- map(GNshplist, st_as_sf)


# Health District shape file 

HDshp <- readOGR("master/data/guinea_shapefiles/Guinea_Health_District", layer ="GIN_adm2", use_iconv=TRUE, encoding= "UTF-8")
HD_sf <- st_as_sf(HDshp)
head(HD_sf)

# read in admin files 

admin1shp <- readOGR("master/data/guinea_shapefiles/GIN_adm_shp", layer ="GIN_adm1", use_iconv=TRUE, encoding= "UTF-8")
head(admin1shp)

admin1_sf <- st_as_sf(admin1shp)

#colnames(admin1_sf)[3] <- "State"

# Mapping points to inherit associated health district

key_list <- map(GNshplist, funEnv$over.fun)

key_list[[1]]$v001<-GNshplist[[1]]@data[,"DHSCLUST"] #1999
key_list[[2]]$v001<-GNshplist[[2]]@data[,"DHSCLUST"] #2005
key_list[[3]]$v001<-GNshplist[[3]]@data[,"DHSCLUST"] #2012 
key_list[[4]]$v001<-GNshplist[[4]]@data[,"DHSCLUST"] #2018



