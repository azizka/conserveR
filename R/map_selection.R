#' Map Selected Methods to Context
#' 
#' Maps methods selected with \code{\link{find_method}} to the trait space 
#' and/or citation network of methods included in conserveR, 
#' to identify further similar methods.
#' 
#' @param x data.frame. As produced by \code{\link{find_method}}.
#' @param num numerical. The number of top ranking methods to highlight.
#' @param type = character. The type of plot, either \dQuote{mca} for the results of 
#' a multiple correspondence analyses of the trait space,
#'  \dQuote{citation} for the citation network, or \dQuote{both} for both.
#' 
#' @examples
#' \dontrun{
#' sel <- find_method()
#' map_selection(sel)
#' }
#' 
#' 
#' @export
#' @importFrom ggplot2 aes ggplot geom_hline geom_vline geom_point scale_colour_manual theme element_blank theme_bw ggtitle scale_colour_manual xlab ylab
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#' @import GGally
#' @import network
#' @importFrom cowplot plot_grid

map_selection <- function(x,
                          num = 3,
                          type = "both"){
  
  match.arg(arg = type, 
            choices = c("both", "mca", "citation"))
  
  if(type == "both" | type == "mca"){
   
    mca <- conserveR::mca_results
    mca$sel <- mca$acronym %in% x$acronym[1:num]

    a <- ggplot()+
      geom_hline(yintercept = 0, linetype="dashed", color = 'grey', size = 0.2)+
      geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 0.2)+
      geom_point(data = mca, aes(x = .data$X, 
                                     y = .data$Y,
                                     color = .data$sel))+
      ggrepel::geom_text_repel(data = mca, aes(x = .data$X,
                                               y = .data$Y,
                                               label = .data$acronym, 
                                               color = .data$sel), size = 1.5)+
      scale_colour_manual(values = c("black", "red"))+
      theme_bw()+
      xlab("Dimension 1")+
      ylab("Dimension 2")+
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            legend.position = "none")
  }
  
  if(type == "both" | type == "citation"){
    
    net <- conserveR::connectivity_network
    
    phono <- ifelse(grepl("WOS:", network.vertex.names(net)), "Reference", "Method")
    phono <- ifelse(network.vertex.names(net) %in% x$acronym[1:num], "Selected", phono)
    
    net %v% "phono" <- phono
    
    col <- c("Method" = "gold","Reference" = "grey","Selected" = "red")
    siz <- c("actor" = 10, "event" = 0.5)
    b <- ggnet2(net,
           color = phono, 
           palette = col,
           label.size = 3,
           label = network.vertex.names(net)[!grepl("WOS:", network.vertex.names(net))], 
           edge.size = "weights",
           node.size = "mode",
           edge.color = "grey90",
           size.palette = siz
    )+
      theme(legend.position = "none")
  }
  
  # return output
  
  if(type == "both"){
    out <- plot_grid(a, b, labels = c("A Multiple correspondance analysis",
                                      "B Citation network"), ncol = 1)
  }
  
  if(type == "mca"){
    out <- a+
      ggtitle("Multiple correspondance analysis")
  }
  
  if(type == "citation"){
    out <- b+
      ggtitle("Citation network")
  }
  
  print(out)
}



