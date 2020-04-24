plot_ggpairs_funct <- function(data, 
                               colour = NULL, 
                               density_alpha = 0.5) {
   
   colour_expr <- enquo(colour)
   
   if (rlang::quo_is_null(colour_expr)) {
      
      g <- data %>%
         ggpairs(lower = "blank") 
      
   } else {
      
      colour_name <- quo_name(colour_expr)
      
      g <- data %>%
         ggpairs(mapping = aes_string(colour = colour_name), 
                 lower = "blank", legend = 1,
                 diag = list(continuous = wrap("densityDiag", 
                                               alpha = density_alpha)),
                 cardinality_threshold = NULL) +
         theme(legend.position = "bottom")
   }
   
   return(g)
   
}