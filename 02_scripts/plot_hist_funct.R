plot_hist_funct <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, colour = "white", 
                            ncol = 5, scale = "free") {
   
   data_factored <- data %>%
      mutate_if(is.character, as.factor) %>%
      mutate_if(is.factor, as.numeric) %>%
      gather(key = key, value = value, factor_key = TRUE) 
   
   if (fct_reorder) {
      data_factored <- data_factored %>%
         mutate(key = as.character(key) %>% as.factor())
   }
   
   if (fct_rev) {
      data_factored <- data_factored %>%
         mutate(key = fct_rev(key))
   }
   
   g <- data_factored %>%
      ggplot(aes(x = value, group = key)) +
      geom_histogram(bins = bins, colour = colour) +
      facet_wrap(~ key, ncol = ncol, scale = scale) + 
      theme_light()
   
   return(g)
   
}