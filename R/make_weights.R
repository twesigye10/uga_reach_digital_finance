# refugee weights ---------------------------------------------------------

make_refugee_weight_table<- function(input_df_ref, input_refugee_pop){
  refugee_pop <- input_refugee_pop %>% 
    mutate(strata= paste0(strata, "_refugee"),
           pop_status= "refugee")
  
  refugee_weight_table <- input_df_ref %>% 
    group_by(strata) %>% 
    summarise(
      sample_strata=  n()
    ) %>% 
    mutate(
      sample_global= sum(sample_strata)) %>% 
    left_join(refugee_pop) %>% 
    rename(pop_strata= "total_pop") %>% 
    mutate(pop_global=sum(pop_strata) ,
           weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
}

# host weights ------------------------------------------------------------

make_host_weight_table<- function(input_df_host, input_host_pop){
  host_pop <- input_host_pop %>% 
    mutate(strata = paste0(strata, "_host"),
           pop_status= "host")
  
  host_weight_table <- input_df_host %>% 
    group_by(strata) %>% 
    summarise(
      sample_strata=  n()
    ) %>% 
    mutate(
      sample_global= sum(sample_strata)) %>% 
    left_join(host_pop) %>% 
    rename(pop_strata= "total_pop") %>% 
    mutate(pop_global=sum(pop_strata) ,
           weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
}

# overall weights --------------------------------------------------------

make_combined_weight_table <- function(input_df, input_ref_weight_table,
                                       input_host_weight_table) {
  weight_tables <- list(input_ref_weight_table, input_host_weight_table)
  
  weight_table <- map_dfr(weight_tables, function(x)x %>% 
                            select(pop_group = strata, sample_strata, pop_strata)) %>% 
    mutate(pop_global = sum(pop_strata),
           sample_global = sum(sample_strata),
           weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
}

