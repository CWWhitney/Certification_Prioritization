# function to calculate the 5 outputs of certification measures

certification_impact <- function(base_yield = prior_yield, 
                                 base_market_price = prior_market_price,
                                 base_cost = prior_cost, 
                                 cost_measure,
                                 # general yield benefit (without risk)
                                 yield_benefit, 
                                 additional_benefits,
                                 event_risk,
                                 normal_damage,
                                 reduction_damage_measure,
                                 # second risk (risk caused by measure)
                                 negative_risk_event_measure,
                                 normal_damage_not_from_measure,
                                 increase_damage_by_measure,
                                 mitigation_factors,
                                 soil_quality, 
                                 water_quality,
                                 biodiv_richness,
                                 var_CV, n_years) {
  
  # Positive effect of measure on a risk ####
  
  # create a vector of years with any risk events
  # these are independent of the measure, they just happen, like major weather events
  # (hurricane)
  event_years <-  chance_event(event_risk, n = n_years)
  
  # calculate yield before intervention  
  # i.e. what is the yield before the measure decreases that risk
  yield_no_measure <- ifelse(event_years,
                             yes = 1 - normal_damage,
                             no = 1)
  
  # calculate yield with intervention  
  # i.e. what is the yield before the measure decreases that risk
  # 'reduction_damage_measure' is proportional to the total damage
  yield_measure <- ifelse(event_years,
                          yes = 1 - (normal_damage * (1 - reduction_damage_measure)),
                          no = 1)
  
  # relative mean annual yield with independent risk
  yield_positive_impact <- mean(yield_measure - yield_no_measure)
  
  # Negative effect of measure on risk ####
  
  #create a vector of years with risk events that the measure has a positive influence on
  #i.e. the measure will increase the risk (like waste water increases risk of salinity)
  negative_risk_event_measure_years <-  chance_event(negative_risk_event_measure,
                                                     n = n_years)
  
  # calculate yield before measure  
  # i.e. what is the yield before the measure increases risk
  yield_no_negative_risk_event_measure <- ifelse(negative_risk_event_measure_years,
                                                 yes = 1 - normal_damage_not_from_measure,
                                                 no = 1)

  # calculate yield with measure  
  # i.e. what is the yield after the measure when it increases risk
  yield_negative_risk_event_measure <- ifelse(negative_risk_event_measure_years,
                                              yes = 1 - (normal_damage_not_from_measure * 
                                                        (1 + increase_damage_by_measure)),
                                              no = 1)
  
  # relative mean annual yield with a risk that is increased by the measure 
  # (i.e. waste water and salinity, competition with cover crops or buffer zones)
  yield_negative_impact <- mean(yield_negative_risk_event_measure - 
                                  yield_no_negative_risk_event_measure)
  
  # estimate one unique proportional yield value given interactions of the measure
  # use the yield_negative_impact and yield_positive_impact from above
  # use the '+' because yield_negative_impact should always be negative
  proportional_yield_after_intervention <- yield_positive_impact + yield_negative_impact + 
    # in the case that there is no risk event this is the yield benefit 
    mean(vv(yield_benefit, var_CV, n_years))
  
  # generate a value for the absolute yield (kg/ha/yr) after the measure
  # using the prior 'base_yield' from the input table
  yield <- base_yield * (1 + proportional_yield_after_intervention)
  
  # calculate the monetary value of bananas USD/ha/yr
  monetary_benefits_from_yield <- yield * base_market_price
  
  # use chance_event to calculate the relative mean risk reduction with measure
  risk_reduction <- mean(
    chance_event(
      event_risk,
      # normal damage vs. the percentage damage reduced by measure
      value_if = (normal_damage * (1 - reduction_damage_measure)),
      value_if_not = 0,
      CV_if = var_CV,
      CV_if_not = 0,
      n = n_years
    )
  )
  
 # use chance_event to calculate the relative mean risk increase from measure
  # the measure increases the risk
  risk_increase <- mean(
    chance_event(
      negative_risk_event_measure,
      # normal damage vs. the percentage damage reduced by measure
      value_if = (normal_damage_not_from_measure * (1 + increase_damage_by_measure)),
      value_if_not = 0,
      CV_if = var_CV,
      CV_if_not = 0,
    )
  )
    
  # relative mean benefits of measure
  # calculate yield to any additional benefits 
  # can also be manipulated to be 'negative benefits' in case of disadvantages
  benefits <- monetary_benefits_from_yield + mean(vv(additional_benefits, var_CV, n_years))
  
  # total costs over n_years
  # calculate the yield * base_cost to consider the economies of scale 
  cost <- base_cost + mean(vv(cost_measure, var_CV, n_years))
  
  # to eliminate the chance of negative costs
  if(cost < 0) cost <- 0 
    
  # relative mean adaptation impacts in USD per ha per yr
  adaptation <- benefits - cost
  
  # mitigation here is the mean of any reductions in parameters related to global
  # warming potential
  mitigation <- mean(vv(mitigation_factors, var_CV, n_years))
  
  # simple ecological impact based on soil, water, biodiversity effects
  
  # how will ecological parameters change if measure is applied
  soil_impact <- mean(vv(soil_quality, var_CV, n_years))
  
  water_impact <- mean(vv(water_quality, var_CV, n_years))
  
  biodiv_richness_impact <- mean(vv(biodiv_richness, var_CV, n_years))
  
  # sum all and divide by 100 because inputs are norm not t_norm_0_1
  ecology <- sum(soil_impact, water_impact, biodiv_richness_impact)/100
  
  # to reach proportional change in the final output for the model 
  # we subtract the change from the prior and then divide it by the prior
  proportional_cost_after_intervention <- (cost - base_cost) / 
                                                    base_cost 
  proportional_adaptation_after_intervention <- (adaptation - ((base_yield * base_market_price) - base_cost)) / 
                                            ((base_yield * base_market_price) - base_cost)
  
  # define the outputs of the general function
  return(list(yield = proportional_yield_after_intervention,
              cost = proportional_cost_after_intervention,
              risk_increase = risk_increase, 
              risk_reduction = risk_reduction,  
              # adaptation is just the ratio of the intervention 
              # income compare to base profit
              # use original base profit as baseline
              adaptation = proportional_adaptation_after_intervention, 
              mitigation = mitigation, 
              ecology = ecology
              ))
}
