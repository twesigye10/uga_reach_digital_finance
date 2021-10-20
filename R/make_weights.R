# weights script

make_refugee_weight_table<- function(input_df_ref, input_refugee_pop){
  refugee_pop <- input_refugee_pop %>% 
    mutate(strata= paste0(strata, "_refugee"),
           pop_status= "refugee")
  
  refugee_weight_table <- input_df_ref %>% 
    group_by(strata) %>% 
    summarise(
      sample_strata=  n()
    )%>% 
    mutate(
      sample_global= sum(sample_strata)) %>% 
    left_join(refugee_pop) %>% 
    rename(pop_strata= "total_pop") %>% 
    mutate(pop_global=sum(pop_strata) ,
           weights = (pop_strata/pop_global)/(sample_strata/sample_global)
    )
}