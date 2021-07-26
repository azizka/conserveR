get_user_input <- function(){
  # collect input
  inp <- list()
  
  ##target
  # inp$target <- menu(c("Area", "Species", "Any"), title="(1/17) What do you want to prioritize?")
  
  ##scale
  inp$scale <- menu(c("Regional", "Continental", "Global", "Any"), title="(1/17) What is the planned geographic scale?")
  
  ##scope
  inp$scope <- menu(c("Terrestrial", "Marine", "Freshwater", "Any"), title="(2/17) Which Realm are you targeting?")
  
  ## phylogeny
  inp$phylogeny <- menu(c("Yes", "No"), title="(3/17) Do you aim to include evolutionary history into the prioritization (i.e. does your data include a phylogeny)?")
  
  ## distribution
  inp$distribution <- menu(c("Yes", "No"), title="(4/17) Do you aim to include geographic range into the prioritization (does your data include range polygons, modelled distributions, occurrence records or similar)?")
  
  ## Ecologic function
  inp$functional <- menu(c("Yes", "No"), title="(5/17) Do you aim to include ecological role into the prioritization (does your data include functional traits)?")
  
  ## Rarity
  inp$rarity <- menu(c("Yes", "No"), title="(6/17) Do you aim to include rarity into the prioritization (does your data include abundances)?")
  
  ## Population dynamics
  inp$pop_dynamics <- menu(c("Yes", "No"), title="(7/17) Do you aim to include population dynamics into the prioritization (does your data include abundances through time)?")
  
  ## Population dynamics
  inp$genetics <- menu(c("Yes", "No"), title="(8/17) Do you aim to include genetic aspects, for instance genetic diversity into the prioritization (does your data include DNA sequences)?")
  
  ## Ecosystem services
  inp$ecosystem_servics <- menu(c("Yes", "No"), title="(9/17) Do you aim to include ecosystem services into the prioritization (does your data include information on services provided)?")
  
  ## Socio-economic factors
  inp$socio_economic <- menu(c("Yes", "No"), title="(10/17) Do you aim to include socio-economic factors into the prioritization (does your data for instance include information on financial and social costs or benefits to conserve area)?")
  
  ## LAndscape connectivity
  inp$landscape_connectivity <- menu(c("Yes", "No"), title="(11/17) Do you aim to include landscape into the prioritization (does your data include for instance a suitability matrix or habitat preferences)?")
  
  ## Land-use
  inp$land_use <- menu(c("Yes", "No"), title="(12/17) Do you aim to include land use into the prioritization?")
  
  ## Protected area
  inp$protected_area <- menu(c("Yes", "No"), title="(13/17) Do you aim to include effects of protected areas into the prioritization?")
  
  ## Extinction risk
  inp$extinction_risk <- menu(c("Yes", "No"), title="(14/17) Do you aim to include extinction risk into the prioritization (does your data include for instance IUCN conservation assessments)?")
  
  ## Environmental factors
  inp$environment <- menu(c("Yes", "No"), title="(15/17) Do you aim to include environmental factors into the prioritization (does your data include for instance climate data)?")
  
  ## Vulnerability
  inp$vulnerability <- menu(c("Yes", "No"), title="(16/17) Do you aim to include vulnerability into the prioritization (does your data include for instance estimates of species' vulnerability against specific threats)?")
  
  ## Climate change
  inp$climate_change <- menu(c("Yes", "No"), title="(17/17) Do you aim to include climate change specifically into the prioritization?")
  
  # translate for output
  # ## target
  # lookup <- c("Area", "Species")
  # if(inp$target <3){
  #   inp$target <- lookup[inp$target]
  # }else{
  #   inp$target = lookup
  # }
  # 
  ## scale
  lookup <- c("regional", "continental", "global", "any")
  inp$scale <- lookup[inp$scale]
  
  # if(inp$scale <4){
  #   inp$scale <- lookup[inp$scale]
  # }else{
  #   inp$scale < lookup
  # }
  
  ## scope
  lookup <- c("Terrestrial", "Marine", "Limnic", "any")
  inp$scope <- lookup[inp$scope]
  
  # if(inp$scope <4){
  #   inp$scope <- lookup[inp$scope]
  # }else{
  #   inp$scope = lookup
  # }
  
  ## all others
  inp[3:length(inp)] <- lapply(inp[3:length(inp)], function(k){k <- ifelse(k == 1, 1, 0)})
  
  return(inp)
}