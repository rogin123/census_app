# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
percent_map <- function(var = "white", color = "darkgreen",
                        legend_title = "% White",
                        min = 0, max = 100, 
                        state_selection = "Contiguous 48 States, Counties") {
  
  # Enquoting input for tidyverse functions 
  var <- rlang::sym(var)
  
  # constrain gradient to percents that occur between min and max
  dat <- county_data %>% 
    mutate(!!var := case_when(!!var >= max ~ max, 
                              !!var <= min ~ min, 
                              TRUE ~ !!var))
  dat_state <- state_map
  boundary_col <- "white"
  
  # Make only state map or not
  if (state_selection != "Contiguous 48 States, Counties") {
    #filtering data for single state
    dat <- filter(dat, state == str_to_lower(state_selection))
    dat_state <- filter(dat_state, state == str_to_lower(state_selection))
    boundary_col <- "black"
  }
  
  # Creating breaks and labels 
  # inc is ismply a quarter step 
  inc <- (max - min) / 4
  legend_breaks <- seq(min, max, inc)
  legend_labels <- c(paste0(if_else(min ==0, "0%", " % or less")),
                     paste0(min + inc, "%"),
                     paste0(min + 2 * inc, "%"),
                     paste0(min + 3 * inc, "%"),
                     paste0(max, if_else(max == 100, "%", "% or more")))
  
  
  p <- dat %>% 
    ggplot() +
    geom_sf(aes( fill = !!var), color = NA) + 
    geom_sf(data = dat_state, color = boundary_col, fill = NA) +
    scale_fill_gradient(name = legend_title, 
                        low = "white", 
                        high = color, 
                        limits = c(min, max), 
                        breaks = legend_breaks, 
                        labels = legend_labels) +
    theme_void() +
    coord_sf(datum = NA) + 
    theme(legend.text = element_text(size = 16, vjust = 0.75), 
          legend.title = element_text(size = 20), 
          legend.key.height = grid::unit(1.5, "cm"))
  
  #if counties lower 48 change projection
  if(state_selection == "Contiguous 48 States, Counties"){
    p <- p +
      coord_sf(
        datum = NA, 
        crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100")
  }
  p 
}
