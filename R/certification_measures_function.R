# Source the general certification impact function

source("R/certification_impact.R")

# The model function estimating the outputs for every certification measure
# (i.e. impact of measures on farm productivity, cost associated to the measure,
# etc.) ####

certification_measures_function  <- function(){
  
  # use the sum of the 10 years of simulation to compare across interventions
  # the function is defined in the certification_impact.R file
  
  ### Relative impact of the energy measures:
  # 1. Land-use diversification ==== 
  # Intervention 1a: Buffer zone ####
  # 100m maximum width of Buffer zone. The maximum area is 5% of the area 
  certification_impact_buffer <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_buffer * base_diversify_cost,
                         # could be timber harvests and other benefits
                         yield_benefit = 0, 
                         additional_benefits = -yield_lost_for_buffer * base_diversify_cost,
                         # winds and storms are frequent
                         event_risk = wind_event_risk,
                         normal_damage = normal_wind_damage,
                         reduction_damage_measure = reduction_wind_damage_buffer,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = soil_quality_buffer, 
                         water_quality = water_quality_buffer,
                         biodiv_richness = biodiv_richness_buffer,
                         var_CV = var_CV, n_years = n_years)
  
  
  buffer_yield <- certification_impact_buffer$yield
  buffer_cost <- certification_impact_buffer$cost
  buffer_risk_increase <- certification_impact_buffer$risk_increase
  buffer_risk_reduction <- certification_impact_buffer$risk_reduction
  buffer_adaptation <- certification_impact_buffer$adaptation
  buffer_mitigation <- certification_impact_buffer$mitigation
  buffer_ecology <- certification_impact_buffer$ecology
  
  
  
  # Intervention 1b: Conversion of unproductive land ####
  certification_impact_conversion <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_conversion * base_diversify_cost,
                         # i.e. transform to agroforestry with bananas
                         yield_benefit = yield_conversion, 
                         #all the other crops/timber that harvested
                         additional_benefits = added_benefit_conversion * base_diversify_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = soil_quality_conversion, 
                         water_quality = 0,
                         biodiv_richness = biodiv_richness_conversion,
                         var_CV = var_CV, n_years = n_years)
  
  
  conversion_yield <- certification_impact_conversion$yield
  conversion_cost <- certification_impact_conversion$cost
  conversion_risk_increase <- certification_impact_conversion$risk_increase
  conversion_risk_reduction <- certification_impact_conversion$risk_reduction
  conversion_adaptation <- certification_impact_conversion$adaptation
  conversion_mitigation <- certification_impact_conversion$mitigation
  conversion_ecology <- certification_impact_conversion$ecology
  
  
  
  # 2. Energy use  ====
  # Intervention 2a: Energy use plan ####
  certification_impact_energy_use_plan <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = (cost_energy_use_plan * base_energy_cost),
                         yield_benefit = 0, 
                         additional_benefits = (energy_saved_use_plan * base_energy_cost),
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = increase_efficiency_energy_use_plan + 
                           reduced_fossil_fuel_consumption_energy_use_plan,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = 0,
                         var_CV = var_CV, n_years = n_years)
  
  energy_use_plan_yield <- certification_impact_energy_use_plan$yield
  energy_use_plan_cost <- certification_impact_energy_use_plan$cost
  energy_use_plan_risk_increase <- certification_impact_energy_use_plan$risk_increase
  energy_use_plan_risk_reduction <- certification_impact_energy_use_plan$risk_reduction
  energy_use_plan_adaptation <- certification_impact_energy_use_plan$adaptation
  energy_use_plan_mitigation <- certification_impact_energy_use_plan$mitigation
  energy_use_plan_ecology <- certification_impact_energy_use_plan$ecology
  
  # Intervention 2b: Energy equipment  ####
  certification_impact_energy_equipment <-
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_energy_equipment * base_energy_cost,
                         yield_benefit = 0, 
                         additional_benefits = energy_saved_energy_equipment * base_energy_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = increase_efficiency_energy_equipment +
                           reduced_fossil_fuel_consumption_energy_equipment,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = 0,
                         var_CV = var_CV, n_years = n_years)
  
  
  energy_equipment_yield <- certification_impact_energy_equipment$yield
  energy_equipment_cost <- certification_impact_energy_equipment$cost
  energy_equipment_risk_increase <- certification_impact_energy_equipment$risk_increase
  energy_equipment_risk_reduction <- certification_impact_energy_equipment$risk_reduction
  energy_equipment_adaptation <- certification_impact_energy_equipment$adaptation
  energy_equipment_mitigation <- certification_impact_energy_equipment$mitigation
  energy_equipment_ecology <- certification_impact_energy_equipment$ecology
  
  # Intervention 2c: Solar energy ####
  # some potential savings of going off the grid
  certification_impact_solar_energy <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         # cost_solar_energy - Considering initial investment 
                         # (over the lifetime of the equipment) and running costs
                         cost_measure = cost_solar_energy * base_energy_cost,  
                         yield_benefit = 0, 
                         # energy_saved_solar_energy - Assuming that the intervention 
                         # decreases fossil fuel use
                         additional_benefits = energy_saved_solar_energy * base_energy_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = increase_efficiency_solar_energy +
                           reduced_fossil_fuel_consumption_solar_energy,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = 0,
                         var_CV = var_CV, n_years = n_years)
  
  
  solar_energy_yield <- certification_impact_solar_energy$yield
  solar_energy_cost <- certification_impact_solar_energy$cost
  solar_energy_risk_increase <- certification_impact_solar_energy$risk_increase
  solar_energy_risk_reduction <- certification_impact_solar_energy$risk_reduction
  solar_energy_adaptation <- certification_impact_solar_energy$adaptation
  solar_energy_mitigation <- certification_impact_solar_energy$mitigation
  solar_energy_ecology <- certification_impact_solar_energy$ecology
  
  # Intervention 2d: Other renewable energy sources ####
  # some potential savings of going off the grid
  certification_impact_other_energy <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         # cost_other_energy - 
                         # Considering initial investment (over the lifetime of the equipment) 
                         # and running costs
                         cost_measure = cost_other_energy * base_energy_cost, 
                         yield_benefit = 0, 
                         # energy_saved_other_energy - Assuming that the intervention 
                         # decreases fossil fuel use
                         additional_benefits = energy_saved_other_energy * base_energy_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = increase_efficiency_other_energy +
                           reduced_fossil_fuel_consumption_other_energy,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = 0,
                         var_CV = var_CV, n_years = n_years)
  
  
  other_energy_yield <- certification_impact_other_energy$yield
  other_energy_cost <- certification_impact_other_energy$cost
  other_energy_risk_increase <- certification_impact_other_energy$risk_increase
  other_energy_risk_reduction <- certification_impact_other_energy$risk_reduction
  other_energy_adaptation <- certification_impact_other_energy$adaptation
  other_energy_mitigation <- certification_impact_other_energy$mitigation
  other_energy_ecology <- certification_impact_other_energy$ecology
  
  ### Relative impact of the water use measures:
  
  
  # 3. Water use  ====
  # Intervention 3a: Wastewater reuse ####
  certification_impact_waste_water <-  
    certification_impact(base_yield = prior_yield, # prior_yield (~ 3000 boxes 18-20kg each) 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_waste_water_use * base_water_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = dry_spells_risk,
                         normal_damage = normal_dry_spell_damage,
                         # packing is done daily during harvests and this uses a 
                         # lot of water
                         reduction_damage_measure = waste_water_deficit_reduction,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = salinity_risk,
                         normal_damage_not_from_measure = normal_salinity_damage,
                         increase_damage_by_measure = waste_water_salinity_damage,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = soil_quality_waste_water, 
                         water_quality = water_quality_waste_water,
                         biodiv_richness = biodiv_richness_waste_water,
                         var_CV = var_CV, n_years = n_years)
  
  
  waste_water_yield <- certification_impact_waste_water$yield
  waste_water_cost <- certification_impact_waste_water$cost
  waste_water_risk_increase <- certification_impact_waste_water$risk_increase
  waste_water_risk_reduction <- certification_impact_waste_water$risk_reduction
  waste_water_adaptation <- certification_impact_waste_water$adaptation
  waste_water_mitigation <- certification_impact_waste_water$mitigation
  waste_water_ecology <- certification_impact_waste_water$ecology
  
  
  # Intervention 3b: Water reservoir ####
  certification_impact_water_reservoir <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_water_reservoir * base_water_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = dry_spells_risk,
                         normal_damage = normal_dry_spell_damage,
                         reduction_damage_measure = reservoir_water_deficit_reduction,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = water_quality_water_reservoir,
                         biodiv_richness = biodiv_richness_water_reservoir,
                         var_CV = var_CV, n_years = n_years)
  

  water_reservoir_yield <- certification_impact_water_reservoir$yield
  water_reservoir_cost <- certification_impact_water_reservoir$cost
  water_reservoir_risk_increase <- certification_impact_water_reservoir$risk_increase
  water_reservoir_risk_reduction <- certification_impact_water_reservoir$risk_reduction
  water_reservoir_adaptation <- certification_impact_water_reservoir$adaptation
  water_reservoir_mitigation <- certification_impact_water_reservoir$mitigation
  water_reservoir_ecology <- certification_impact_water_reservoir$ecology
  
  # Intervention 3c: Anti-evapotranspiration spray ####
  certification_impact_a_evap_trans_spray <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_a_evap_trans_spray * base_water_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = dry_spells_risk,
                         normal_damage = normal_dry_spell_damage,
                         reduction_damage_measure = a_evap_trans_spray_water_deficit_reduction,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = water_quality_a_evap_trans_spray,
                         biodiv_richness = biodiv_richness_a_evap_trans_spray,
                         var_CV = var_CV, n_years = n_years)
  
  
  a_evap_trans_spray_yield <- certification_impact_a_evap_trans_spray$yield
  a_evap_trans_spray_cost <- certification_impact_a_evap_trans_spray$cost
  a_evap_trans_spray_risk_increase <- certification_impact_a_evap_trans_spray$risk_increase
  a_evap_trans_spray_risk_reduction <- certification_impact_a_evap_trans_spray$risk_reduction
  a_evap_trans_spray_adaptation <- certification_impact_a_evap_trans_spray$adaptation
  a_evap_trans_spray_mitigation <- certification_impact_a_evap_trans_spray$mitigation
  a_evap_trans_spray_ecology <- certification_impact_a_evap_trans_spray$ecology
 
  
  # Intervention 3d: Irrigation methods ####
  certification_impact_irrigation_methods <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_irrigation_methods * base_water_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = dry_spells_risk,
                         normal_damage = normal_dry_spell_damage,
                         reduction_damage_measure = irrigation_methods_water_deficit_reduction,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = soil_quality_irrigation_methods, 
                         water_quality = water_quality_irrigation_methods,
                         biodiv_richness = biodiv_richness_irrigation_methods,
                         var_CV = var_CV, n_years = n_years)
  
  
  irrigation_methods_yield <- certification_impact_irrigation_methods$yield
  irrigation_methods_cost <- certification_impact_irrigation_methods$cost
  irrigation_methods_risk_increase <- certification_impact_irrigation_methods$risk_increase
  irrigation_methods_risk_reduction <- certification_impact_irrigation_methods$risk_reduction
  irrigation_methods_adaptation <- certification_impact_irrigation_methods$adaptation
  irrigation_methods_mitigation <- certification_impact_irrigation_methods$mitigation
  irrigation_methods_ecology <- certification_impact_irrigation_methods$ecology
  
  # Intervention 3e: Irrigation scheduling ####
  certification_impact_irrigation_scheduling <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_irrigation_scheduling * base_water_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = dry_spells_risk,
                         normal_damage = normal_dry_spell_damage,
                         reduction_damage_measure = irrigation_scheduling_water_deficit_reduction,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = soil_quality_irrigation_scheduling, 
                         water_quality = water_quality_irrigation_scheduling,
                         biodiv_richness = biodiv_richness_irrigation_scheduling,
                         var_CV = var_CV, n_years = n_years)
  
  
  irrigation_scheduling_yield <- certification_impact_irrigation_scheduling$yield
  irrigation_scheduling_cost <- certification_impact_irrigation_scheduling$cost
  irrigation_scheduling_risk_increase <- certification_impact_irrigation_scheduling$risk_increase
  irrigation_scheduling_risk_reduction <- certification_impact_irrigation_scheduling$risk_reduction
  irrigation_scheduling_adaptation <- certification_impact_irrigation_scheduling$adaptation
  irrigation_scheduling_mitigation <- certification_impact_irrigation_scheduling$mitigation
  irrigation_scheduling_ecology <- certification_impact_irrigation_scheduling$ecology
  
  # Intervention 3f: Drainage management  ####
  certification_impact_drainage <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_drainage_mgmt * base_water_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = flood_event_risk,
                         normal_damage = normal_waterlog_damage,
                         reduction_damage_measure = reduction_waterlog_drainage_mgmt,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = soil_quality_drainage_mgmt, 
                         water_quality = water_quality_drainage_mgmt,
                         biodiv_richness = biodiv_richness_drainage_mgmt,
                         var_CV = var_CV, n_years = n_years)
  
  drainage_yield <- certification_impact_drainage$yield
  drainage_cost <- certification_impact_drainage$cost
  drainage_risk_increase <- certification_impact_drainage$risk_increase
  drainage_risk_reduction <- certification_impact_drainage$risk_reduction
  drainage_adaptation <- certification_impact_drainage$adaptation
  drainage_mitigation <- certification_impact_drainage$mitigation
  drainage_ecology <- certification_impact_drainage$ecology
  
 
  
  # 4. Plant nutrient and pest management ====
  # Intervention 4a: Composting  ####
  composting_adaptation_impact <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_composting * base_chemical_cost,
                         #yield can increase up to 30% with chemicals in compost
                         yield_benefit = yield_increase_composting, 
                         additional_benefits = fertilizer_reduction_composting * base_chemical_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = reduced_chemical_residue_composting + 
                           # not a linear relationship, demand may decrease 
                           # and production will still continue
                           # here we assume that the relationship is linear
                           reduced_fertilizer_production_composting,
                         # ecological impacts
                         soil_quality = soil_quality_composting, 
                         water_quality = water_quality_composting,
                         biodiv_richness = biodiv_richness_composting,
                         var_CV = var_CV, n_years = n_years)
  
  composting_yield <- composting_adaptation_impact$yield
  composting_cost <- composting_adaptation_impact$cost
  composting_risk_increase <- composting_adaptation_impact$risk_increase
  composting_risk_reduction <- composting_adaptation_impact$risk_reduction
  composting_adaptation <- composting_adaptation_impact$adaptation
  composting_mitigation <- composting_adaptation_impact$mitigation
  composting_ecology <- composting_adaptation_impact$ecology
  
  
  # Intervention 4b: Nutrient management ####
  nutrient_mgmt_adaptation_impact <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_nutrient_mgmt * base_chemical_cost,
                         yield_benefit = yield_increase_nutrient_mgmt, 
                         additional_benefits = fertilizer_reduction_nutrient_mgmt * base_chemical_cost,
                         # this refers to pests and disease
                         event_risk = pest_outbreak_risk,
                         normal_damage = normal_damage_pests,
                         reduction_damage_measure = reduction_damage_pest_nutrient_mgmt,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = reduced_chemical_residue_nutrient_mgmt + 
                           reduced_fertilizer_production_nutrient_mgmt,
                         # ecological impacts
                         soil_quality = soil_quality_nutrient_mgmt, 
                         water_quality = water_quality_nutrient_mgmt,
                         biodiv_richness = biodiv_richness_nutrient_mgmt,
                         var_CV = var_CV, n_years = n_years)
  
  nutrient_mgmt_yield <- nutrient_mgmt_adaptation_impact$yield
  nutrient_mgmt_cost <- nutrient_mgmt_adaptation_impact$cost
  nutrient_mgmt_risk_increase <- nutrient_mgmt_adaptation_impact$risk_increase
  nutrient_mgmt_risk_reduction <- nutrient_mgmt_adaptation_impact$risk_reduction
  nutrient_mgmt_adaptation <- nutrient_mgmt_adaptation_impact$adaptation
  nutrient_mgmt_mitigation <- nutrient_mgmt_adaptation_impact$mitigation
  nutrient_mgmt_ecology <- nutrient_mgmt_adaptation_impact$ecology

  
  # Intervention 4c: Integrated Pest Management (IPM) ####
  # we assume that the reduction in the outbreak risk by using ipm is 
  # directly transferred to yield increase in the same rate
  ipm_adaptation_impact <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_ipm_practice * base_chemical_cost,
                         yield_benefit = 0, 
                         additional_benefits = pesticide_reduction_ipm_practice * base_chemical_cost,
                         event_risk = pest_outbreak_risk,
                         normal_damage = normal_damage_pests,
                         reduction_damage_measure = reduction_damage_ipm_practice,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = reduced_pesticide_production_ipm_practice,
                         # ecological impacts
                         soil_quality = soil_quality_ipm_practice, 
                         water_quality = water_quality_ipm_practice,
                         biodiv_richness = biodiv_richness_ipm_practice,
                         var_CV = var_CV, n_years = n_years)
  
  
  ipm_practice_yield <- ipm_adaptation_impact$yield
  ipm_practice_cost <- ipm_adaptation_impact$cost
  ipm_practice_risk_increase <- ipm_adaptation_impact$risk_increase
  ipm_practice_risk_reduction <- ipm_adaptation_impact$risk_reduction
  ipm_practice_adaptation <- ipm_adaptation_impact$adaptation
  ipm_practice_mitigation <- ipm_adaptation_impact$mitigation
  ipm_practice_ecology <- ipm_adaptation_impact$ecology
  
  # Intervention 4d: Reincorporate crop residues  ####
  reincorporation_adaptation_impact <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_reincorporation * base_chemical_cost,
                         yield_benefit = yield_reincorporation, 
                         #mulches cover about 60% of ground
                         additional_benefits = (herbicide_reduction_reincorporation * base_chemical_cost) + 
                           (fertilizer_reduction_reincorporation * base_chemical_cost),
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = reduced_herbicide_production_reincorporation,
                         # ecological impacts
                         soil_quality = soil_quality_reincorporation, 
                         water_quality = water_quality_reincorporation,
                         biodiv_richness = biodiv_richness_reincorporation,
                         var_CV = var_CV, n_years = n_years)
  
  reincorporation_yield <- reincorporation_adaptation_impact$yield
  reincorporation_cost <- reincorporation_adaptation_impact$cost
  reincorporation_risk_increase <- reincorporation_adaptation_impact$risk_increase
  reincorporation_risk_reduction <- reincorporation_adaptation_impact$risk_reduction
  reincorporation_adaptation <- reincorporation_adaptation_impact$adaptation
  reincorporation_mitigation <- reincorporation_adaptation_impact$mitigation
  reincorporation_ecology <- reincorporation_adaptation_impact$ecology
  
  
  # Intervention 4e: Cover crops ####
  cover_crop_adaptation_impact <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = cost_cover_crop * base_chemical_cost,
                         yield_benefit = 0, 
                         additional_benefits = herbicide_reduction_cover_crop * base_chemical_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = competition_risk,
                         normal_damage_not_from_measure = normal_competition_damage,
                         increase_damage_by_measure = increased_damage_by_competition,
                         mitigation_factors = reduced_herbicide_production_cover_crop,
                         # ecological impacts
                         soil_quality = soil_quality_cover_crop, 
                         water_quality = water_quality_cover_crop,
                         biodiv_richness = biodiv_richness_cover_crop,
                         var_CV = var_CV, n_years = n_years)

  
  cover_crop_yield <- cover_crop_adaptation_impact$yield
  cover_crop_cost <- cover_crop_adaptation_impact$cost
  cover_crop_risk_increase <- cover_crop_adaptation_impact$risk_increase
  cover_crop_risk_reduction <- cover_crop_adaptation_impact$risk_reduction
  cover_crop_adaptation <- cover_crop_adaptation_impact$adaptation
  cover_crop_mitigation <- cover_crop_adaptation_impact$mitigation
  cover_crop_ecology <- cover_crop_adaptation_impact$ecology
  
  

  # 5. Waste management ====
  # Intervention 5a: Recycling plastic ####
  certification_impact_recycling <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = recycling_cost * base_plastic_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = reduced_plastic_production_recycling,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = biodiv_richness_recycling,
                         var_CV = var_CV, n_years = n_years)
  
  recycling_yield <- certification_impact_recycling$yield
  recycling_cost <- certification_impact_recycling$cost
  recycling_risk_increase <- certification_impact_recycling$risk_increase
  recycling_risk_reduction <- certification_impact_recycling$risk_reduction
  recycling_adaptation <- certification_impact_recycling$adaptation
  recycling_mitigation <- certification_impact_recycling$mitigation
  recycling_ecology <- certification_impact_recycling$ecology
  
  
  # Intervention 5b: Waste disposal plan ####
  certification_impact_waste_plan <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = waste_plan_cost * base_plastic_cost,
                         yield_benefit = 0, 
                         additional_benefits = 0,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         # This can have some mitigation effect but not major
                         mitigation_factors = 0,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = biodiv_richness_waste_plan,
                         var_CV = var_CV, n_years = n_years)
  
  waste_plan_yield <- certification_impact_waste_plan$yield
  waste_plan_cost <- certification_impact_waste_plan$cost
  waste_plan_risk_increase <- certification_impact_waste_plan$risk_increase
  waste_plan_risk_reduction <- certification_impact_waste_plan$risk_reduction
  waste_plan_adaptation <- certification_impact_waste_plan$adaptation
  waste_plan_mitigation <- certification_impact_waste_plan$mitigation
  waste_plan_ecology <- certification_impact_waste_plan$ecology
 
  
  # Intervention 5c: Plastic reduction ####
  # here we assume there is a cost of replacing plastics with other materials
  # impregnated plastic bags are used for fungicide application
  # using a fitted bag for the banana fruit rather than a bag that is too big 
  plastic_reduction_adaptation_impact <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = costs_plastic_wrapping_time * base_plastic_cost,
                         yield_benefit = 0, 
                         additional_benefits = savings_reduced_plastic * base_plastic_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = reduced_plastic_production_replacement,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = biodiv_richness_reduced_plastic,
                         var_CV = var_CV, n_years = n_years)
  
  plastic_reduction_yield <- plastic_reduction_adaptation_impact$yield
  plastic_reduction_cost <- plastic_reduction_adaptation_impact$cost
  plastic_reduction_risk_increase <- plastic_reduction_adaptation_impact$risk_increase
  plastic_reduction_risk_reduction <- plastic_reduction_adaptation_impact$risk_reduction
  plastic_reduction_adaptation <- plastic_reduction_adaptation_impact$adaptation
  plastic_reduction_mitigation <- plastic_reduction_adaptation_impact$mitigation
  plastic_reduction_ecology <- plastic_reduction_adaptation_impact$ecology
  
  # Intervention 5d: Plastic reuse ####
  # here we assume there is a cost of replacing plastics with other materials
  # re-using the bags for fungicide application 
  plastic_reuse_adaptation_impact <-  
    certification_impact(base_yield = prior_yield, 
                         base_market_price = prior_market_price,
                         base_cost = prior_cost,
                         cost_measure = costs_plastic_reuse * base_plastic_cost,
                         yield_benefit = 0, 
                         additional_benefits = savings_plastic_reuse * base_plastic_cost,
                         event_risk = 0,
                         normal_damage = 0,
                         reduction_damage_measure = 0,
                         # second risk (risk caused by measure)
                         negative_risk_event_measure = 0,
                         normal_damage_not_from_measure = 0,
                         increase_damage_by_measure = 0,
                         mitigation_factors = reduced_plastic_production_plastic_reuse,
                         # ecological impacts
                         soil_quality = 0, 
                         water_quality = 0,
                         biodiv_richness = biodiv_richness_reuse_plastic,
                         var_CV = var_CV, n_years = n_years)
  
  plastic_reuse_yield <- plastic_reuse_adaptation_impact$yield
  plastic_reuse_cost <- plastic_reuse_adaptation_impact$cost
  plastic_reuse_risk_increase <- plastic_reuse_adaptation_impact$risk_increase
  plastic_reuse_risk_reduction <- plastic_reuse_adaptation_impact$risk_reduction
  plastic_reuse_adaptation <- plastic_reuse_adaptation_impact$adaptation
  plastic_reuse_mitigation <- plastic_reuse_adaptation_impact$mitigation
  plastic_reuse_ecology <- plastic_reuse_adaptation_impact$ecology
  
  return(list(energy_use_plan_yield = energy_use_plan_yield,
              energy_use_plan_cost = energy_use_plan_cost,
              energy_use_plan_risk_increase = energy_use_plan_risk_increase,
              energy_use_plan_risk_reduction = energy_use_plan_risk_reduction,
              energy_use_plan_adaptation = energy_use_plan_adaptation,
              energy_use_plan_mitigation = energy_use_plan_mitigation,
              energy_use_plan_ecology = energy_use_plan_ecology,
              
              energy_equipment_yield = energy_equipment_yield,
              energy_equipment_cost = energy_equipment_cost,
              energy_equipment_risk_increase = energy_equipment_risk_increase,
              energy_equipment_risk_reduction = energy_equipment_risk_reduction,
              energy_equipment_adaptation = energy_equipment_adaptation,
              energy_equipment_mitigation = energy_equipment_mitigation,
              energy_equipment_ecology = energy_equipment_ecology,
              
              solar_energy_yield = solar_energy_yield,
              solar_energy_cost = solar_energy_cost,
              solar_energy_risk_increase = solar_energy_risk_increase,
              solar_energy_risk_reduction = solar_energy_risk_reduction,
              solar_energy_adaptation = solar_energy_adaptation,
              solar_energy_mitigation = solar_energy_mitigation,
              solar_energy_ecology = solar_energy_ecology,
              
              other_energy_yield = other_energy_yield,
              other_energy_cost = other_energy_cost,
              other_energy_risk_increase = other_energy_risk_increase,
              other_energy_risk_reduction = other_energy_risk_reduction,
              other_energy_adaptation = other_energy_adaptation,
              other_energy_mitigation = other_energy_mitigation,
              other_energy_ecology = other_energy_ecology,
              
              waste_water_yield = waste_water_yield,
              waste_water_cost = waste_water_cost,
              waste_water_risk_increase = waste_water_risk_increase,
              waste_water_risk_reduction = waste_water_risk_reduction,
              waste_water_adaptation = waste_water_adaptation,
              waste_water_mitigation = waste_water_mitigation,
              waste_water_ecology = waste_water_ecology,
              
              water_reservoir_yield = water_reservoir_yield,
              water_reservoir_cost = water_reservoir_cost,
              water_reservoir_risk_increase = water_reservoir_risk_increase,
              water_reservoir_risk_reduction = water_reservoir_risk_reduction,
              water_reservoir_adaptation = water_reservoir_adaptation,
              water_reservoir_mitigation = water_reservoir_mitigation,
              water_reservoir_ecology = water_reservoir_ecology,
              
              a_evap_trans_spray_yield = a_evap_trans_spray_yield,
              a_evap_trans_spray_cost = a_evap_trans_spray_cost,
              a_evap_trans_spray_risk_increase = a_evap_trans_spray_risk_increase,
              a_evap_trans_spray_risk_reduction = a_evap_trans_spray_risk_reduction,
              a_evap_trans_spray_adaptation = a_evap_trans_spray_adaptation,
              a_evap_trans_spray_mitigation = a_evap_trans_spray_mitigation,
              a_evap_trans_spray_ecology = a_evap_trans_spray_ecology,
              
              irrigation_methods_yield = irrigation_methods_yield,
              irrigation_methods_cost = irrigation_methods_cost,
              irrigation_methods_risk_increase = irrigation_methods_risk_increase,
              irrigation_methods_risk_reduction = irrigation_methods_risk_reduction,
              irrigation_methods_adaptation = irrigation_methods_adaptation,
              irrigation_methods_mitigation = irrigation_methods_mitigation,
              irrigation_methods_ecology = irrigation_methods_ecology,
              
              irrigation_scheduling_yield = irrigation_scheduling_yield,
              irrigation_scheduling_cost = irrigation_scheduling_cost,
              irrigation_scheduling_risk_increase = irrigation_scheduling_risk_increase,
              irrigation_scheduling_risk_reduction = irrigation_scheduling_risk_reduction,
              irrigation_scheduling_adaptation = irrigation_scheduling_adaptation,
              irrigation_scheduling_mitigation = irrigation_scheduling_mitigation,
              irrigation_scheduling_ecology = irrigation_scheduling_ecology,
              
              drainage_yield = drainage_yield,
              drainage_cost = drainage_cost,
              drainage_risk_increase = drainage_risk_increase,
              drainage_risk_reduction = drainage_risk_reduction,
              drainage_adaptation = drainage_adaptation,
              drainage_mitigation = drainage_mitigation,
              drainage_ecology = drainage_ecology,
              
              composting_yield = composting_yield,
              composting_cost = composting_cost,
              composting_risk_increase = composting_risk_increase,
              composting_risk_reduction = composting_risk_reduction,
              composting_adaptation = composting_adaptation,
              composting_mitigation = composting_mitigation,
              composting_ecology = composting_ecology,
              
              nutrient_mgmt_yield = nutrient_mgmt_yield,
              nutrient_mgmt_cost = nutrient_mgmt_cost,
              nutrient_mgmt_risk_increase = nutrient_mgmt_risk_increase,
              nutrient_mgmt_risk_reduction = nutrient_mgmt_risk_reduction,
              nutrient_mgmt_adaptation = nutrient_mgmt_adaptation,
              nutrient_mgmt_mitigation = nutrient_mgmt_mitigation,
              nutrient_mgmt_ecology = nutrient_mgmt_ecology,
              
              ipm_practice_yield = ipm_practice_yield,
              ipm_practice_cost = ipm_practice_cost,
              ipm_practice_risk_increase = ipm_practice_risk_increase,
              ipm_practice_risk_reduction = ipm_practice_risk_reduction,
              ipm_practice_adaptation = ipm_practice_adaptation,
              ipm_practice_mitigation = ipm_practice_mitigation,
              ipm_practice_ecology = ipm_practice_ecology,
              
              reincorporation_yield = reincorporation_yield,
              reincorporation_cost = reincorporation_cost,
              reincorporation_risk_increase = reincorporation_risk_increase,
              reincorporation_risk_reduction = reincorporation_risk_reduction,
              reincorporation_adaptation = reincorporation_adaptation,
              reincorporation_mitigation = reincorporation_mitigation,
              reincorporation_ecology = reincorporation_ecology,
              
              cover_crop_yield = cover_crop_yield,
              cover_crop_cost = cover_crop_cost,
              cover_crop_risk_increase = cover_crop_risk_increase,
              cover_crop_risk_reduction = cover_crop_risk_reduction,
              cover_crop_adaptation = cover_crop_adaptation,
              cover_crop_mitigation = cover_crop_mitigation,
              cover_crop_ecology = cover_crop_ecology,
              
              buffer_yield = buffer_yield,
              buffer_cost = buffer_cost,
              buffer_risk_increase = buffer_risk_increase,
              buffer_risk_reduction = buffer_risk_reduction,
              buffer_adaptation = buffer_adaptation,
              buffer_mitigation = buffer_mitigation,
              buffer_ecology = buffer_ecology,
              
              conversion_yield = conversion_yield,
              conversion_cost = conversion_cost,
              conversion_risk_increase = conversion_risk_increase,
              conversion_risk_reduction = conversion_risk_reduction,
              conversion_adaptation = conversion_adaptation,
              conversion_mitigation = conversion_mitigation,
              conversion_ecology = conversion_ecology,
              
              recycling_yield = recycling_yield,
              recycling_cost = recycling_cost,
              recycling_risk_increase = recycling_risk_increase,
              recycling_risk_reduction = recycling_risk_reduction,
              recycling_adaptation = recycling_adaptation,
              recycling_mitigation = recycling_mitigation,
              recycling_ecology = recycling_ecology,
              
              waste_plan_yield = waste_plan_yield,
              waste_plan_cost = waste_plan_cost,
              waste_plan_risk_increase = waste_plan_risk_increase,
              waste_plan_risk_reduction = waste_plan_risk_reduction,
              waste_plan_adaptation = waste_plan_adaptation,
              waste_plan_mitigation = waste_plan_mitigation,
              waste_plan_ecology = waste_plan_ecology,
              
              plastic_reduction_yield = plastic_reduction_yield,
              plastic_reduction_cost = plastic_reduction_cost,
              plastic_reduction_risk_increase = plastic_reduction_risk_increase,
              plastic_reduction_risk_reduction = plastic_reduction_risk_reduction,
              plastic_reduction_adaptation = plastic_reduction_adaptation,
              plastic_reduction_mitigation = plastic_reduction_mitigation,
              plastic_reduction_ecology = plastic_reduction_ecology,
              
              plastic_reuse_yield = plastic_reuse_yield,
              plastic_reuse_cost = plastic_reuse_cost,
              plastic_reuse_risk_increase = plastic_reuse_risk_increase,
              plastic_reuse_risk_reduction = plastic_reuse_risk_reduction,
              plastic_reuse_adaptation = plastic_reuse_adaptation,
              plastic_reuse_mitigation = plastic_reuse_mitigation,
              plastic_reuse_ecology = plastic_reuse_ecology))
  
}



#script for the decision function (easier to follow) 

# Temporary function for running model parts ####
# 
# make_variables <- function(est, n = 1){
# 
#   x <- decisionSupport::random(rho = est, n = n)
# 
#   for (i in colnames(x))
# 
#     assign(x = i, value = as.numeric(x[1, i]), envir = .GlobalEnv)}
# 
# make_variables(decisionSupport::estimate_read_csv("certification_measures_input_table.csv"))
# 
