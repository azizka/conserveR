#' Identify Most Suitable Methods for Conservation Prioritization
#' 
#' Identifies suitable methods for conservation prioritization based on a user dialogue on conservation targets and data availability.
#' 
#' @param ranking character string. The methods used for ranking the methods. See details. One of "both", "strict", "inclusive". Default = "both". 
#' @param weights named list. Provide numeric values to weight questions differently. See details.
#' 
#' Based on the \code{ranking} argument, the conservation prioritization methods in the database are
#'  ranked according to the user-provided information. If ranking = "strict" methods receive 
#'  one point for each full agreement with user reply (yes and no), if ranking = "inclusive", 
#'  methods get one point when they include a feature confirmed by the user (but non for not including it).
#'  This means that ranking = "inclusive" will  likely return more general methods 
#'  that can include many different types of data and perspectives. If ranking = "both",
#'  methods are first ranked as in strict and then equal ranks split by the inclusive ranking.
#'  
#'  The \code{weight} argument allows to change the weighting of individual questions relative to the others. 
#'   The names of the list follow \code{names(traits)}, from \dQuote{scale} (for question 1) to 
#'   \dQuote{includes_simulation} (for question 17). The weights may include any numbers of questions. 
#'   See examples 
#' 
#' @examples
#' \dontrun{
#' find_method()
#' #double weight to question 3 and 15
#' find_method(weights = list(phylogeny = 2, vulnerability = 2))
#' }
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom utils menu



find_method <- function(ranking = "both",
                        weights = NULL){
  
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
  
  # set weights
  ids <- c("scale", "scope", names(tr)[11:26])
  wei <- rep(1, length(ids))
  
  names(wei) <- ids
  
  weights <- unlist(weights)
  wei[names(weights)] <- weights

  ## test for scale
  tr$score[tr$scale %in% inp$scale] <- tr$score[tr$scale %in% inp$scale] + (1 * wei["scale"])
  if(inp$scale == "any"){tr$score <- tr$score + (1 * wei["scale"])}
  
  ## test for scope
  if(inp$scope == "Terrestrial"){tr$score[tr$scope_terrestrial == 1] <-  tr$score[tr$scope_terrestrial== 1] + (1 * wei["scope"])}
  if(inp$scope == "Marine"){tr$score[tr$scope_marine == 1] <-  tr$score[tr$scope_marine == 1] + (1 * wei["scope"])}
  if(inp$scope == "Limnic"){tr$score[tr$scope_limnic == 1] <-  tr$score[tr$scope_limnic == 1] + (1 * wei["scope"])}
  
  # the others
  tr$score[tr$phylogeny == inp$phylogeny] <- tr$score[tr$phylogeny == inp$phylogeny] + (inp$phylogeny * wei["phylogeny"])
  tr$score[tr$distribution == inp$distribution] <- tr$score[tr$distribution == inp$distribution] + (inp$distribution * wei["distribution"])
  tr$score[tr$functional == inp$functional] <- tr$score[tr$functional == inp$functional] + (inp$functional * wei["functional"])
  tr$score[tr$rarity == inp$rarity] <- tr$score[tr$rarity == inp$rarity] + (inp$rarity * wei["rarity"])
  tr$score[tr$pop_dynamics == inp$pop_dynamics] <- tr$score[tr$pop_dynamics == inp$pop_dynamics] + (inp$pop_dynamics * wei["pop_dynamics"])
  tr$score[tr$genetics == inp$genetics] <- tr$score[tr$genetics == inp$genetics] + (inp$genetics * wei["genetics"])
  tr$score[tr$ecosystem_services == inp$ecosystem_services] <- tr$score[tr$ecosystem_services == inp$ecosystem_services] + (inp$ecosystem_services * wei["ecosystem_services"])
  tr$score[tr$socio_economic == inp$socio_economic] <- tr$score[tr$socio_economic == inp$socio_economic] + (inp$socio_economic * wei["socio_economic"])
  tr$score[tr$landscape_connectivity == inp$landscape_connectivity] <- tr$score[tr$landscape_connectivity == inp$landscape_connectivity] + (inp$landscape_connectivity * wei["landscape_connectivity"])
  tr$score[tr$land_use == inp$land_use] <- tr$score[tr$land_use == inp$land_use] + (inp$land_use * wei["land_use"])
  tr$score[tr$protected_area == inp$protected_area] <- tr$score[tr$protected_area == inp$protected_area] + (inp$protected_area * wei["protected_area"])
  tr$score[tr$extinction_risk == inp$extinction_risk] <- tr$score[tr$extinction_risk == inp$extinction_risk] + (inp$extinction_risk * wei["extinction_risk"])
  tr$score[tr$environment == inp$environment] <- tr$score[tr$environment == inp$environment] + (inp$environment * wei["environment"])
  tr$score[tr$vulnerability == inp$vulnerability] <- tr$score[tr$vulnerability == inp$vulnerability] + (inp$vulnerability * wei["environment"])
  tr$score[tr$climate_change == inp$climate_change] <- tr$score[tr$climate_change== inp$climate_change] + (inp$climate_change * wei["climate_change"])
  
  #detailed score_strict
  tr$score_strict <- 0
  
  tr$score_strict[tr$scale %in% inp$scale] <- tr$score_strict[tr$scale %in% inp$scale] + (1 * wei["scale"])
  if(inp$scale == "any"){tr$score_strict <- tr$score_strict + (1 * wei["scale"])}
  
  ## test for scope
  if(inp$scope == "Terrestrial"){tr$score_strict[tr$scope_terrestrial == 1] <-  tr$score_strict[tr$scope_terrestrial== 1] + (1 * wei["scope"])}
  if(inp$scope == "Marine"){tr$score_strict[tr$scope_marine == 1] <-  tr$score_strict[tr$scope_marine == 1] + (1 * wei["scope"])}
  if(inp$scope == "Limnic"){tr$score_strict[tr$scope_limnic == 1] <-  tr$score_strict[tr$scope_limnic == 1] + (1 * wei["scope"])}
  
  # the others
  tr$score_strict[tr$phylogeny == inp$phylogeny] <- tr$score_strict[tr$phylogeny == inp$phylogeny] + (1 * wei["phylogeny"])
  tr$score_strict[tr$distribution == inp$distribution] <- tr$score_strict[tr$distribution == inp$distribution] + (1 * wei["distribution"])
  tr$score_strict[tr$functional == inp$functional] <- tr$score_strict[tr$functional == inp$functional] + (1 * wei["functional"])
  tr$score_strict[tr$rarity == inp$rarity] <- tr$score_strict[tr$rarity == inp$rarity] + (1 * wei["rarity"])
  tr$score_strict[tr$pop_dynamics == inp$pop_dynamics] <- tr$score_strict[tr$pop_dynamics == inp$pop_dynamics] + (1 * wei["pop_dynamics"])
  tr$score_strict[tr$genetics == inp$genetics] <- tr$score_strict[tr$genetics == inp$genetics] + (1 * wei["genetics"])
  tr$score_strict[tr$ecosystem_services == inp$ecosystem_services] <- tr$score_strict[tr$ecosystem_services == inp$ecosystem_services] + (1 * wei["ecosystem_services"])
  tr$score_strict[tr$socio_economic == inp$socio_economic] <- tr$score_strict[tr$socio_economic == inp$socio_economic] + (1 * wei["socio_economic"])
  tr$score_strict[tr$landscape_connectivity == inp$landscape_connectivity] <- tr$score_strict[tr$landscape_connectivity == inp$landscape_connectivity] + (1 * wei["landscape_connectivity"])
  tr$score_strict[tr$land_use == inp$land_use] <- tr$score_strict[tr$land_use == inp$land_use] + (1 * wei["land_use"])
  tr$score_strict[tr$protected_area == inp$protected_area] <- tr$score_strict[tr$protected_area == inp$protected_area] + (1 * wei["protected_area"])
  tr$score_strict[tr$extinction_risk == inp$extinction_risk] <- tr$score_strict[tr$extinction_risk == inp$extinction_risk] + (1 * wei["extinction_risk"])
  tr$score_strict[tr$environment == inp$environment] <- tr$score_strict[tr$environment == inp$environment] + (1 * wei["environment"])
  tr$score_strict[tr$vulnerability == inp$vulnerability] <- tr$score_strict[tr$vulnerability == inp$vulnerability] + (1 * wei["vulnerability"])
  tr$score_strict[tr$climate_change == inp$climate_change] <- tr$score_strict[tr$climate_change== inp$climate_change] + (1 * wei["climate_change"])
  
  
  # rank wmethods according to score
  tr$rank <- rank(-tr$score, ties.method = "min")
  tr$rank_strict <- rank(-tr$score_strict, ties.method = "min")
  
  # return data.frame
  ## from the trait data
  tr <- tr[, c("author", "acronym", "method_name", "free_text_description", "DOI/link", "score", "score_strict", "ID", "rank", "rank_strict", "scalability")]
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
  out <- out[, c("acronym", "method_name", "fit_strict", "fit_inclusive","AUTHOR", "DATE", "TITLE", "JOURNALTITLE", "DOI/link")]
  names(out) <- c("acronym", "method_name", "fit_strict", "fit_inclusive","authors", "publication_date", "title", "joutnal", "doi_or_link")

    # Print top 3 to screen
  cat("The three most fitting methods are:\n")
  cat("\n")
  
  cat(paste(sel$acronym[1], ", ", sel$author[1], " (", sel$DATE[1], "), ", "FIT = ", sel_fit[1], "%, ", "Scalability = ", sel$scalability[1], "\n", sep = ""))
  cat(sel$free_text_description[1])
  if(as.numeric(format(Sys.Date(), "%Y")) - as.numeric(strsplit(sel$DATE[1], split = "-")[[1]][1]) > 10){
    cat("\n")
    cat("Method older than 10 years!")
  }
  cat("\n")
  cat("\n")
  
  cat(paste(sel$acronym[2], ", ", sel$author[2], " (", sel$DATE[2], "), ", "FIT = ", sel_fit[2], "%, ", "Scalability = ", sel$scalability[2], "\n", sep = ""))
  cat(sel$free_text_description[2])
  if(as.numeric(format(Sys.Date(), "%Y")) - as.numeric(strsplit(sel$DATE[2], split = "-")[[1]][1]) > 10){
    cat("\n")
    cat("Method older than 10 years!")
  }
  cat("\n")
  cat("\n")
  
  cat(paste(sel$acronym[3], ", ", sel$author[3], " (", sel$DATE[3], "), ", "FIT = ", sel_fit[3], "%, ", "Scalability = ", sel$scalability[3], "\n", sep = ""))
  cat(sel$free_text_description[3])
  if(as.numeric(format(Sys.Date(), "%Y")) - as.numeric(strsplit(sel$DATE[3], split = "-")[[1]][1]) > 10){
    cat("\n")
    cat("Method older than 10 years!")
  }
  cat("\n")

  # return ranked data.frame
  return(out)
}
