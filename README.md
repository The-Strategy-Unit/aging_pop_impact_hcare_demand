# Modeling the impact of population aging on demand for healthcare

This project develops an approach to modeling the effects of demographic change on future demand for healthcare. 

## Running the code
There are 3 pipelines,

* population - handles population inputs
* activity - handles activity inputs
* results - builds and runs models using population and activity inputs

### Population pipeline
`read_historic_pop.r`  
read-in historic population estimates for 2000 to 2022  
`read_snpp_2018b.r`  
read-in 2018b sub-national population projections  
`read_npp_2018b.r`  
read-in 2018b national population projections  
`read_life_tables_2018b.r`  
read-in 2018b life tables from national population projections  
`read_life_tables_2020b.r`  
read-in 2020b life tables from national population projections  
`build_historic_pop.r`  
build full set of historic populations for app  
`build_snpp_2018b_custom_variants.r`  
create a set of x17 custom snpp variants  
`build_pop_90_inputs.r`  
create a set of variants for use in the app  
`build_pop_100_inputs.r`  
model the 90+ population for snpp variants  
`assemble_pop_inputs.r`  
assemble data files (JSON) for app

#### Activity pipeline
`read_aae_dat.r`  
read-in aae data (data created by proc_aae_grps.sql)  
`read_apc_dat.r`  
read-in apc data (data created by proc_apc_grps.sql)  
`read_opc_dat.r`  
read-in opcs data (data created by proc_opc_grps.sql)  
`clean_aae_dat.r`  
clean aae data - compile lads, ctys, icbs and England  
`clean_apc_dat.r`  
clean apc data - compile lads, ctys, icbs and England  
`clean_opc_dat.r`  
clean opc data - compile lads, ctys, icbs and England  
`assemble_activity_inputs.r`  
assemble data files (JSON) for app

#### Results pipeline
`hsa_build_gams.r`  
create GAMs needed for health status adjustment  
`hsa_review_gams.r`  
review GAMs  
`hsa_build_model.r`  
apply logic for health status adjustment  
`hsa_run_model.r`  
fns to run the models
`hsa_assemble_results.r`  
fns to assemble model results (single area)  
`assemble_results_inputs.r`  
assemble data files (JSON) for app

## License
Distributed under the MIT License. See `LICENSE` for more information.
