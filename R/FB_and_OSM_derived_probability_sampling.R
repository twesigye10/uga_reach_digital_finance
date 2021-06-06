library(sf)
library(tidyverse)
library(raster)


# Read data ---------------------------------------------------------------

# osm data
osm<- st_read("inputs/hotosm_uga_buildings_polygons_shp","hotosm_uga_buildings_polygons", quiet=T)
# fb data - has been converted from pixels to pts
fb_points<-st_read("inputs/uga_sampling.gpkg","fb_pixels_to_points", quiet=T)
# sub counties
sub_county<- st_read("inputs/uga_sampling.gpkg","admin3_sub_county",quiet=T)

# settlement boundaries -- once zone layer is compiled it should replace this input
a1_sett<- st_read("inputs/uga_sampling.gpkg","a1_sett_uga_reach_unhcr",quiet=T)

# imvepi zone 4 point data- generated from field delineated polygon and UNHCR HH data
imvepi_z4<- st_read("inputs/uga_sampling.gpkg","Imvepi_Zone_4", quiet=T)


# Intregrate OSM & FB Data ------------------------------------------------

# first we read in the data run some preliminary calculations and cleaning on the data

# select sub counties with adjacent/touching refugee settlement
hosting_sub<-sub_county %>% 
  st_join(a1_sett) %>% 
  filter(!is.na(settlement_grp))


# create osm point centroids
osm_p<- st_centroid(osm)

# simplify features
osmp<-osm_p %>% 
  st_geometry() %>% 
  st_as_sf() %>% 
  mutate(
    uid=row_number()
  )

# simplify features
fbp<-fb_points %>% 
  st_geometry() %>% 
  st_as_sf() %>% 
  mutate(
    uid=row_number()
  )

# transform osm to UTM 36 N
osmp<-st_transform(x = osmp,crs = st_crs(fbp))



# To find FB points that are non-coincident with OSM data and therefore represents 
# newly detected populated area we measure the distance between every FB point and the closest OSM point.
# We can then inspect the distribution of shortest distances.
# just buffer the host community sub counties to help remove edge artefacts later when we perform 

# KDE density calculations
hosting_subs_buffered<-hosting_sub %>% 
  summarise() %>% # dissolve
  st_buffer(dist = 1000) # buffer

# spatially filter OSM and FB points to buffered hosting counties
osmp_subs<-osmp[hosting_subs_buffered,]
fbp_subs<-fbp[hosting_subs_buffered,]
   
# distance measurements
fbp_subs_dist<- fbp_subs %>% 
  butteR::mutate_nearest_feature(osmp_subs)

# turn distance numeric
fbp_subs_dist<- fbp_subs_dist %>%
  mutate(
    dist=as.numeric(distance)
  )


# Investigate distances ---------------------------------------------------

# make ecdf plot see if we can set a threshold
plot_ecdf<-fbp_subs_dist %>% 
  ggplot(aes(x=dist))+
  scale_x_log10(breaks=c(1,10,50,100,1000,10000))+
  stat_ecdf(geom="step")

plot_ecdf+
  geom_vline(xintercept = 50, linetype="dashed",lwd=1,color="red")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle=90)
  )


# Filter FB Data ----------------------------------------------------------

# grab values greater than threshold.
fb_gte50<-fbp_subs_dist %>% 
  filter(dist>=50) 

fb_gte50<- fb_gte50 %>% 
  mutate(
    source= "FB"
  ) %>% select(source)

osmp_subs<-osmp_subs %>% 
  mutate(
    source="OSM"
  ) %>% select(source)
imvepi_z4<-imvepi_z4 %>% 
  mutate(source="reach_unhcr") %>% 
  select(source)

all_pt_data<-bind_rows(osmp_subs,fb_gte50)
nrow(all_pt_data)

all_pt_data<-all_pt_data %>% 
  rename(geom="x")

all_pt_data<-bind_rows(all_pt_data,imvepi_z4)
inhab_mask<-st_buffer(all_pt_data,dist=100)
# st_write(all_pt_data,"osm_fb_pt_data.shp")


# Creating the sample -----------------------------------------------------

# read in density raster created in QGIS
# read in point buffered data (inhabited area mask)

inhab_mask<-st_read("outputs",'fb_osm_pt_buffered_data', quiet=T)
probraster<- raster("outputs/kde_fb_osm_1000_20.tif", quiet=T)


# Set up spatial sample frame ---------------------------------------------

# only concerned sub-counties which are overlapping/adjacent with settlements
hosting_sub<-sub_county %>% 
  st_join(a1_sett) %>% 
  filter(!is.na(settlement_grp))
# put hole in polygon
sub_county_no_sett<-st_difference(hosting_sub,st_union(a1_sett))


sframe<- sub_county_no_sett %>% 
  mutate(samplesize=100)


# Create Sampling Function & Sample ---------------------------------------

probsel<-function(sample_frame,
                  strata_label_col,
                  strata_num_col,
                  prob_raster ,
                  inhab_mask){
  sframe<-sample_frame %>% 
    st_zm() %>% 
    dplyr::select(all_of(c(strata_label_col, strata_num_col)))
  jitter_val<-res(prob_raster)/2
  
  crs_use<-st_crs(sframe)
  assertthat::are_equal(as.character(crs(sframe)),
                        as.character(crs(prob_raster)))
  
  
  assertthat::are_equal(as.character(crs(sframe)),
                        as.character(crs(inhab_mask)))
  
  sp_list<-list() # list to fill with sample point sets
  for(i in seq_along(sframe[[strata_label_col]])){
    print(i)
    sframe_temp<-sframe %>% slice(i) 
    samp_size_temp<- sframe_temp[[strata_num_col]]
    sframe_name_temp<- sframe_temp[[strata_label_col]]
    
    # crop both prob raster to sframe temp
    r_crop_temp<- crop(prob_raster, sframe_temp)
    #clip inhab mask to sframe
    inhab_mask_clip_temp<- sf::st_intersection(inhab_mask,sframe_temp)
    
    # clip cropped probraster to sframe temp
    r_clip_temp<- raster::mask(r_crop_temp,sframe_temp)
    
    # clip cropped/clipped probraster to inhab mask
    r_masked_temp<- raster::mask(r_clip_temp,inhab_mask_clip_temp)
    
    r_vals<-getValues(r_masked_temp)# extract raster values
    
    r_vals[is.na(r_vals)]<-0  # turn all NA to 0 probability
    
    samp<-sample(nrow(r_masked_temp)*ncol(r_masked_temp), size=samp_size_temp, prob=r_vals, replace=T)
    # get indices of all raster vals
    all_r_cell_indices<-1:(nrow(r_masked_temp)*ncol(r_masked_temp))
    
    # indices of all non sampled cells
    not_samp<- all_r_cell_indices[!all_r_cell_indices %in% samp] 
    
    # duplicate raster
    r_sampled_temp<- raster(r_masked_temp)
    
    # replace non sampled raster cells with value = 0
    r_sampled_temp[not_samp]<-0
    # replace sampled raster cells with value = 1
    r_sampled_temp[samp]<-1
    
    #return raster cells with value 1 and turn them to points
    sp_temp<-rasterToPoints(r_sampled_temp, fun=function(x){x==1}) %>% data.frame()
    sp_temp_sf<-st_as_sf(sp_temp,coords=c("x", "y"), crs=crs_use)
    
    # jitter final points by half cell resolution
    sp_temp_sf_jit<- st_jitter(x = sp_temp_sf,amount = jitter_val)
    
    #SET UP SAMPLE FOR KML/FIELD CONVENINCE
    sp_temp_arranged<-sp_temp_sf_jit %>% 
      cbind(st_coordinates(.)) %>% 
      arrange(X,Y) %>% 
      dplyr::select(-X,-Y) %>% 
      mutate(
        Description= paste0(sframe_name_temp,"_",1:nrow(.)),
        Name= Description
      )
    
    sp_list[[sframe_name_temp]]<- sp_temp_arranged
  }
  bind_rows(sp_list) # turn into data.frame
}



# sampling
sampledrawn<-probsel(sample_frame=sframe,
                     strata_label_col="Subcounty",
                     strata_num_col="samplesize",
                     prob_raster=probraster ,
                     inhab_mask = inhab_mask)
