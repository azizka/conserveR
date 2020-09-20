find_method <- function(mca = TRUE,
                        plot = TRUE){
  # collect input

  # match user input with table to create scoring

  # run MCA
  dat_min <- inp %>%
    select(target, scale, scope.terrestrial, scope.marine, scope.limnic, contains("includes")) %>%
    #select(contains("includes")) %>%
    mutate_if(is.numeric, as.factor)

  res_mca <- MCA(dat_min, ncp = 5, graph = TRUE)

  # print summary sentence for the 3 best matching methods to the screen

  # return data.frame with the paper references
}
