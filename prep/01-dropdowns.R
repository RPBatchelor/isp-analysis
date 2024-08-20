


isp_source_list <- odp_table |> 
  pull(isp_source)

save(isp_source_list, file = "shiny-webtool/data/isp_source_list.rda")




source_scenario_pathway_list <- isp_generator_capacity |> 
  distinct(source, scenario, cdp, region, technology)

save(source_scenario_pathway_list, file = "shiny-webtool/data/source_scenario_pathway_list.rda")



region_list <- isp_generator_capacity |> 
  distinct(region) 

save(region_list, file = "shiny-webtool/data/region_list.rda")