library(sf)
library(tidyverse)
library(raster)


# Read data ---------------------------------------------------------------


# sub counties
sub_county<- st_read("inputs/UGS_admin03","UGA_Admin_3_Sub_Counties_2016",quiet=T)
host_sub_county<- st_read("inputs/UGA_Host_subcounty_samples_dfa","Selected_host_sub_county_samples_dfa",quiet=T)

# settlement boundaries -- once zone layer is compiled it should replace this input
a1_sett<- st_read("inputs/Settlement shapefiles_2019","Settlements_2019",quiet=T)
a1_sett <- a1_sett %>% 
  st_transform(crs = 32636 ) %>% 
  group_by(Name_setlm) %>% 
  summarise() %>% 
  mutate(settlement_grp = Name_setlm)

# settlements with zones
settlement_zone <- st_read("inputs/UGA_Refugee_settlement_samples","UGA_Refugee_settlement_samples_dfa",quiet=T)
  

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
    # print(i)
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
    # to reproduce same results
    set.seed(i)
    
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
    
    processing_pt_dec <- sframe_temp %>% pull(strata_label_col)
    processing_pt_sample_size <- sframe_temp %>% pull(strata_num_col)
    output_pt_nos <- nrow(sp_temp_sf)
    pt_no_differences <- processing_pt_sample_size - output_pt_nos
    
    print(paste(i, 
                ":  Desc: ", processing_pt_dec, 
                ":  SamplSize: ", processing_pt_sample_size, 
                ":  no_points: ", output_pt_nos, 
                ":  pt_no_diff: ", pt_no_differences
                )
          )
    
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



# Creating the sample -----------------------------------------------------

# read in density raster created in QGIS
# read in point buffered data (inhabited area mask)

inhab_mask<-st_read("inputs",'fb_osm_pt_data', quiet=T)
probraster<- raster("inputs/kde_fb_osm_1000_20.tif", quiet=T)


# Set up spatial sample frame HOST and Settlements ---------------------------------------------

# # only concerned sub-counties which are overlapping/adjacent with settlements
# hosting_sub<-sub_county %>% 
#   st_join(a1_sett) %>% 
#   filter(!is.na(settlement_grp))
# # put hole in polygon
# sub_county_no_sett<-st_difference(hosting_sub,st_union(a1_sett))
# 
# # HOST community
# sframe<- sub_county_no_sett %>% 
#   mutate(samplesize=100,
#          Subcounty = SNAME2016
#          )



# put hole in polygon using new dataset of host sub county with sample size
sub_county_no_sett<-st_difference(host_sub_county,st_union(a1_sett))


# HOST community ----------------------------------------------------------

host_modifier <- tibble::tribble(
                   ~"sb_cnt__1", ~"smpl_sz",
                        "arw",      27,
                        "bwe",      73,
                        "itl",     226,
                        "kbz",     61,
                        "kyg",     217,
                        "lob",     220,
                        "mpr",     91,
                        "nkm",      98,
                        "pgm",      86,
                        "rgb",     3264,
                        "rhc",     106,
                        "rom",      41
                   )

sframe_host<- sub_county_no_sett %>% 
  left_join(host_modifier, by=c("sb_cnt__1")) %>% 
  mutate(
    samplesize = as.integer(ifelse(!is.na(smpl_sz.y), smpl_sz.y, smpl_sz.x )),
         desc = sb_cnt__1
         )

# sampling host
sampledrawn_host <- probsel(sample_frame = sframe_host,
                     strata_label_col = "desc",
                     strata_num_col = "samplesize",
                     prob_raster = probraster ,
                     inhab_mask = inhab_mask)

st_write(sampledrawn_host, "outputs", "dfa_sample_host", driver = "ESRI Shapefile", append = FALSE)

# write out subcounty host features with challenges
problematic_host <- host_modifier %>% pull(sb_cnt__1)

st_write(sub_county_no_sett %>% filter(sb_cnt__1 %in% problematic_host), "outputs", "problematic_subcounty_no_settlement", driver = "ESRI Shapefile", append = FALSE)


# Settlements -------------------------------------------------------------

settlement_modifier <- tibble::tribble(
  ~"sttlmn__1", ~"smpl_sz",
  "alr",      7,
  "bse_cmp",     129,
  "bya",      39,
  "bwz",      31,
  "dong_west",      47,
  "imv_zon1",     102,
  "jur",     37,
  "kabo",     14,
  "kak",     19,
  "kya",     219,
  "lob_zon_a",     113,
  "lob_zon_b",     113,
  "mor",      62,
  "oru",     220,
  "pal_zon_vb",      56,
  "pal_zon_vii",      43,
  "ran_37",     104,
  "ran_i",     120,
  "rub",     58,
  "rwa",     215,
  "swe",      45
)

sframe_settlements <- settlement_zone %>% 
  left_join(settlement_modifier, by=c("sttlmn__1")) %>% 
  mutate(
    smpl_sz.x = ifelse(!is.na(smpl_sz.y), smpl_sz.y, smpl_sz.x ) ,
    samplesize = smpl_sz.x,
    desc = sttlmn__1  
  )


# sampling settlements
sampledrawn_settlements<-probsel(sample_frame=sframe_settlements,
                     strata_label_col="desc",
                     strata_num_col="samplesize",
                     prob_raster=probraster ,
                     inhab_mask = inhab_mask)

st_write(sampledrawn_settlements, "outputs", "dfa_sample_settlements", driver = "ESRI Shapefile", append = FALSE)

# write out settlement features with challenges
problematic_settlement <- settlement_modifier %>% pull(sttlmn__1)

st_write(settlement_zone %>% filter(sttlmn__1 %in% problematic_settlement), "outputs", "problematic_settlemets", driver = "ESRI Shapefile", append = FALSE)



