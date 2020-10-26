#' Identify Most Suitable Methods for Conservation Prioritization
#' 
#' Identifies suitable methods for conservation prioritization based on a user dialogue on conservation targets and data availability.
#' 
#' @param ranking character string. The methods used for ranking the methods. See details. One of "both", "strict", "inclusive". Default = "both". 
#' @param mca logical. If TRUE a multiple correspondence analysis is included, to plot the most suitable and related methods.
#' @param plot logical. IF TRUE the results of the mca are plotted. 
#' 
#' Based on the \code{ranking} argument, the conservation prioritization methods in the database are
#'  ranked according to the user-provided information. If ranking = "strict" methods receive 
#'  one point for each full agreement with user reply (yes and no), if ranking = "inclusive", 
#'  methods get one point when they include a feature confirmed by the user (but non for not including it).
#'  This means that ranking = "inclusive" will  likely return more general methods 
#'  that can include many different types of data and perspectives. If ranking = "both",
#'  methods are first ranked as in strict and then equal ranks split by the inclusive ranking.
#' 
#' @examples
#' \dontrun{
#' find_method()
#' }
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom utils menu



find_method <- function(ranking = "both",
                        mca = TRUE,
                        plot = TRUE){
  
  # Checka rguyments
  match.arg(arg = ranking,
            choices = c("both", "strict", "inclusive"))
  
  message("The answers to the following 17 questions will be used to
  find the most suitable conservation prioritization methods.
  Methods will be ranked, not filtered, which means that
  not all returned methods may fully fit your needs.
  a full literature list ist availble under data(literature).")
 
  # get the use input
  inp <- get_user_input()

  # Rank methods according to user input
  tr <- conserveR::traits
  
  ## set baseline
  tr$score <- 0
  
  ## test for scale
  tr$score[tr$scale %in% inp$scale] <- tr$score[tr$scale %in% inp$scale] + 1
  if(inp$scale == "any"){tr$score <- tr$score + 1}
  
  ## test for scope
  if(inp$scope == "Terrestrial"){tr$score[tr$scope_terrestrial == 1] <-  tr$score[tr$scope_terrestrial== 1] + 1}
  if(inp$scope == "Marine"){tr$score[tr$scope_marine == 1] <-  tr$score[tr$scope_marine == 1] + 1}
  if(inp$scope == "Limnic"){tr$score[tr$scope_limnic == 1] <-  tr$score[tr$scope_limnic == 1] + 1}
  
  # the others
  tr$score[tr$phylogeny == inp$phylogeny] <- tr$score[tr$phylogeny == inp$phylogeny] + inp$phylogeny
  tr$score[tr$distribution == inp$distribution] <- tr$score[tr$distribution == inp$distribution] + inp$distribution
  tr$score[tr$functional == inp$functional] <- tr$score[tr$functional == inp$functional] + inp$functional
  tr$score[tr$rarity == inp$rarity] <- tr$score[tr$rarity == inp$rarity] + inp$rarity
  tr$score[tr$pop_dynamics == inp$pop_dynamics] <- tr$score[tr$pop_dynamics == inp$pop_dynamics] + inp$pop_dynamics
  tr$score[tr$genetics == inp$genetics] <- tr$score[tr$genetics == inp$genetics] + inp$genetics
  tr$score[tr$ecosystem_servics == inp$ecosystem_servics] <- tr$score[tr$ecosystem_servics == inp$ecosystem_servics] + inp$ecosystem_servics
  tr$score[tr$socio_economic == inp$socio_economic] <- tr$score[tr$socio_economic == inp$socio_economic] + inp$socio_economic
  tr$score[tr$landscape_connectivity == inp$landscape_connectivity] <- tr$score[tr$landscape_connectivity == inp$landscape_connectivity] + inp$landscape_connectivity
  tr$score[tr$land_use == inp$land_use] <- tr$score[tr$land_use == inp$land_use] + inp$land_use
  tr$score[tr$protected_area == inp$protected_area] <- tr$score[tr$protected_area == inp$protected_area] + inp$protected_area
  tr$score[tr$extinction_risk == inp$extinction_risk] <- tr$score[tr$extinction_risk == inp$extinction_risk] + inp$extinction_risk
  tr$score[tr$environment == inp$environment] <- tr$score[tr$environment == inp$environment] + inp$environment
  tr$score[tr$vulnerability == inp$vulnerability] <- tr$score[tr$vulnerability == inp$vulnerability] + inp$vulnerability
  tr$score[tr$climate_change == inp$climate_change] <- tr$score[tr$climate_change== inp$climate_change] + inp$climate_change
  
  #detailed score_strict
  tr$score_strict <- 0
  
  tr$score_strict[tr$scale %in% inp$scale] <- tr$score_strict[tr$scale %in% inp$scale] + 1
  if(inp$scale == "any"){tr$score_strict <- tr$score_strict + 1}
  
  ## test for scope
  if(inp$scope == "Terrestrial"){tr$score_strict[tr$scope_terrestrial == 1] <-  tr$score_strict[tr$scope_terrestrial== 1] + 1}
  if(inp$scope == "Marine"){tr$score_strict[tr$scope_marine == 1] <-  tr$score_strict[tr$scope_marine == 1] + 1}
  if(inp$scope == "Limnic"){tr$score_strict[tr$scope_limnic == 1] <-  tr$score_strict[tr$scope_limnic == 1] + 1}
  
  # the others
  tr$score_strict[tr$phylogeny == inp$phylogeny] <- tr$score_strict[tr$phylogeny == inp$phylogeny] + 1
  tr$score_strict[tr$distribution == inp$distribution] <- tr$score_strict[tr$distribution == inp$distribution] + 1
  tr$score_strict[tr$functional == inp$functional] <- tr$score_strict[tr$functional == inp$functional] + 1
  tr$score_strict[tr$rarity == inp$rarity] <- tr$score_strict[tr$rarity == inp$rarity] + 1
  tr$score_strict[tr$pop_dynamics == inp$pop_dynamics] <- tr$score_strict[tr$pop_dynamics == inp$pop_dynamics] + 1
  tr$score_strict[tr$genetics == inp$genetics] <- tr$score_strict[tr$genetics == inp$genetics] + 1
  tr$score_strict[tr$ecosystem_servics == inp$ecosystem_servics] <- tr$score_strict[tr$ecosystem_servics == inp$ecosystem_servics] + 1
  tr$score_strict[tr$socio_economic == inp$socio_economic] <- tr$score_strict[tr$socio_economic == inp$socio_economic] + 1
  tr$score_strict[tr$landscape_connectivity == inp$landscape_connectivity] <- tr$score_strict[tr$landscape_connectivity == inp$landscape_connectivity] + 1
  tr$score_strict[tr$land_use == inp$land_use] <- tr$score_strict[tr$land_use == inp$land_use] + 1
  tr$score_strict[tr$protected_area == inp$protected_area] <- tr$score_strict[tr$protected_area == inp$protected_area] + 1
  tr$score_strict[tr$extinction_risk == inp$extinction_risk] <- tr$score_strict[tr$extinction_risk == inp$extinction_risk] + 1
  tr$score_strict[tr$environment == inp$environment] <- tr$score_strict[tr$environment == inp$environment] + 1
  tr$score_strict[tr$vulnerability == inp$vulnerability] <- tr$score_strict[tr$vulnerability == inp$vulnerability] + 1
  tr$score_strict[tr$climate_change == inp$climate_change] <- tr$score_strict[tr$climate_change== inp$climate_change] + 1
  
  
  # rank wmethods according to score
  tr$rank <- rank(-tr$score, ties.method = "min")
  tr$rank_strict <- rank(-tr$score_strict, ties.method = "min")
  
  # run MCA
  if(mca){
    # dat_min <- inp %>%
    #   select(target, scale, scope_terrestrial, scope_marine, scope_limnic, contains("includes")) %>%
    #   #select(contains("includes")) %>%
    #   mutate_if(is.numeric, as.factor)
    # 
    # res_mca <- MCA(dat_min, ncp = 5, graph = TRUE)
    
  }

  # plot a part of the MCA output
  

  
  
  # return data.frame
  ## from the trait data
  tr <- tr[, c("author", "akronym", "method_name", "free_text_description", "DOI/link", "score", "score_strict", "ID", "rank", "rank_strict")]
  tr$fit_strict <-  round(tr$score_strict / 17 *100, 1)
  tr$fit_inclusive <-  round(tr$score / 17 *100, 1)
  tr$ID <- tolower(tr$ID)
  
  ## from the literature data
  li <- conserveR::literature
  li <- li[c("BIBTEXKEY", "AUTHOR", "DATE", "TITLE","JOURNALTITLE")]
  
  out <- merge(li, tr, by.x ="BIBTEXKEY", by.y = "ID")
  
  if(ranking == "both"){
    out <- out[order(out$rank_strict, out$rank), ]
  }else if(ranking == "strict"){
    out <- out[order(out$rank_strict), ]
  }else if(ranking == "inclusive"){
    out <- out[order(out$rank), ]
  }
  

  # Identify top 3
  if(ranking == "both"){
    sel <- out[out$rank_strict < 4,]
    sel <- sel[sel$rank <= sel$rank[3],]
    sel_fit <- sel$fit_strict 
  }else if(ranking == "strict"){
    sel <- out[out$rank_strict < 4,]
    sel_fit <- sel$fit_strict 
  }else if(ranking == "inklusive"){
    sel <- out[out$rank < 4,]
    sel_fit <- sel$fit_inclusive
  }
  
  # Final output df
  out <- out[, c("akronym", "method_name", "fit_strict", "fit_inclusive","AUTHOR", "DATE", "TITLE", "JOURNALTITLE", "DOI/link")]
  names(out) <- c("akronym", "method_name", "fit_strict", "fit_inclusive","authors", "publication_date", "title", "joutnal", "doi_or_link")

    # Print top 3 to screen
  cat("The three most fitting methods are:\n")
  cat("\n")
  
  cat(paste(sel$akronym[1], ", ", sel$author[1], " (", sel$DATE[1], "), ", "FIT = ", sel_fit[1], "%\n", sep = ""))
  cat(sel$free_text_description[1])
  cat("\n")
  cat("\n")
  
  cat(paste(sel$akronym[2], ", ", sel$author[2], " (", sel$DATE[2], "), ", "FIT = ", sel_fit[2], "%\n", sep = ""))
  cat(sel$free_text_description[2])
  cat("\n")
  cat("\n")
  
  cat(paste(sel$akronym[3], ", ", sel$author[3], " (", sel$DATE[3], "), ", "FIT = ", sel_fit[3], "%\n", sep = ""))
  cat(sel$free_text_description[3])
  cat("\n")
  
  # sprintf("%s, %s (%s): %s. (FIT_strict = %s, FIT_inclusive = %s)", 
  #         sel$akronym, 
  #         sel$author, 
  #         sel$DATE, 
  #         sel$free_text_description,
  #         sel$fit_strict,
  #         sel$fit_inclusive)
  
  # return ranked data.frame
  return(out)
}
