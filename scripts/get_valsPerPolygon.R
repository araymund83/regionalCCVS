# Load libraries  ---------------------------------------------------------
library(pacman)
p_load (dplyr,exactextractr, foreign, ggplot2, glue,sf, stringr, terra, tidyverse)

#clean workspace 
rm(list = ls())

# Load data ---------------------------------------------------------------
# BCR shapefile  
bcr_all_2020 <- vect('inputs/BCR_terrestrial_2020/BCR_Terrestrial_master.shp') 
#select polygons outside CA

bcr_us <- vect('inputs/bcrUS/bcr_US.shp') 
bcr_mx <- vect('inputs/bcrMX/bcrMX.shp') 

crs <- '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs' #st_crs(4269) #ESPG 4269 == NAD83



#get the species code
# species <- str_sub(basename(files_fut), 
#                    start = 17, end = nchar(basename(files_fut)) - 71)
season <- 'breeding' #'nonbreeding'
country <- 'USA' #'CANADA', 'MEXICO'
yr <- '2055'
group <- 'generalists' # boreal forest, aridlands, arctic, urban, subtropical
rcp <-  '85'


#Audobon files
path_fut <- './ras_fut'
files_fut <- list.files(path_fut, pattern = group, full.names = TRUE)
#get the species available 

species <- str_extract(files_fut, '[A-Z]{4}')

#get bcr_id, name by country
bcr_class <- foreign::read.dbf('./inputs/BCR_terrestrial_2020/BCR_Terrestrial_master.dbf') %>%  
  dplyr::select(objectID = OBJECTID,
                bcr = BCR,
                bcrName = BCRNAME,
                country = COUNTRY)
 # to check the country code 
#country <- unique(bcr_class$country)

### function to create proportion table
sum_cover <- function(x){
  list(x %>%
         group_by(value) %>%
         summarize(total_area = sum(coverage_fraction)) %>%
         mutate(proportion = total_area/sum(total_area)))
  
}

getValPoly <- function(sp, group, season){
 # sp <- birdList[1] # Run and erase
  group <- group
  fle_fut <- grep(sp,files_fut, value = TRUE)
  #if (missing(country)){
  
  #TODO: ADD A CONDITIONAL FOR USING COUNTRY OR NOT!! 
    # if not filtering then use all BCR
   bcr <- bcr_all_2020 
  #} else {
   # bcr <- bcr_all_2020 %>% filter(COUNTRY == country) # IF FILTER BY COUNTRY
  #}
  message(crayon::blue('Getting proportions  for', sp))
  
  getVals<- map(.x = 1:length(yr), .f = function(yr){
    
    message(crayon::green('Loading files for', yr))
    fl <- grep(season, fle_fut, value = TRUE)
    rst_fut <- terra::rast(fl)
    names(rst_fut) <- 'change'
    #remove all values outside the raster
    rst_fut <- trim(rst_fut)
    
    #transform shp to crs from raster
    # Check if CRS match and transform if needed
    if (terra::crs(bcr) != terra::crs(rst_fut)) {
      bcr <- project(bcr, rst_fut)
    }
    
    #define classes in raster
    classes <- sort(terra::unique(rst_fut)[,1])  
    #create a look up table to keep track of bcr
    bcr_lu <- data.frame(objID = unique(bcr$OBJECTID),
                         bcrID  = bcr$BCR)
    bcr_lu$ID <- 1:length(bcr_lu$objID)
    val <- terra::extract(rst_fut, bcr)
    df<- data.frame(do.call(rbind,tapply(val[,2], val$ID, function(x) { 
     table(factor(x, levels=classes))})))
    df <- df %>% mutate(ID = 1:nrow(df),
                        species = sp)
    df_count <- left_join(bcr_lu, df, by = 'ID')
    
    #extract raster values to get the proportions per polygon
    df_prop <- data.frame(do.call(rbind,tapply(val[,2], val$ID, function(x) { 
     prop.table(table(factor(x, levels=classes)))})))
    
    unique_values <- sort(unique(val[,2]))
    col_names <- ifelse(unique_values == 0, "never",
                        ifelse(unique_values == 1, "gain",
                               ifelse(unique_values == 2, "loss",
                                      ifelse(unique_values == 3, "stable", NA))))
    colnames(df_prop) <- col_names
   # browser()
    df_prop <- df_prop %>% mutate(ID = 1:nrow(df_prop),
                        species = sp)
    df_prop <- left_join(bcr_lu, df_prop)
    
    #save both outputs 
    out <- glue('./outputs/{group}')
    ifelse(!dir.exists(out), dir.create(out, recursive = TRUE),'Folder already exists')
    write.csv(df_count,file= glue('{out}/df_count_allbcr_{yr}{rcp}_{group}_{sp}.csv'))
    write.csv(df_prop,file= glue('{out}/df_prop_allbcr_{yr}{rcp}_{group}_{sp}.csv'))
})
return(getVals)
}

# Apply the function ------------------------------------------------------

result <- map(species, ~getValPoly(sp = .x, group = group, season = season))
result <- future.apply::future_lapply(1:length(birdList), function(i){
  znl <- getValPoly(sp = species, group = group, season = season)
})
future::plan(future::multisession, workers = 16, gc = TRUE)
options(future.globals.maxSize= 4194304000) ## this option helps with  the error about global sizes 
znl_all <- furrr::future_map(.x = 1:length(species), .f = function(i){
  cat('Start\n')
  znl <- getValPoly(sp = species[i], group = group)
  cat('Done!\n')
  return(znl)
})

future:::ClusterRegistry('stop')    

# not using paralel
zonalDF <- map(.x = species, group = group, .f = getValPoly)

