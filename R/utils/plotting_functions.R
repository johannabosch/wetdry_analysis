plotting_functions_welcome <- function() {
  cat("\n")
  cat("=================================================== \n")
  cat("             YOU LOADED plotting_functions          \n")
  cat("   view this function in wet-dry_analysis/R/utils   \n")
  cat("=================================================== \n")
  cat("\n")
  cat("this function loads ALL the functions you need to plot \n")
  cat("the proportions, switches, total and longest wet/dry periods \n") 
  cat("uses a custom color palette, depends on sample size (n_colors) \n") 
  
  
  cat("\n")
  cat("\n")
  cat("- Ensure dependencies in project_settings.R\n")
  
  
  cat("=============================================\n\n")}

plotting_functions_welcome()

suppressMessages({
#set a color palette 
custom_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                    "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62",
                    "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494",
                    "#B3B3B3")

### Plot Proportions

# For proportions, I use the down sampled data by 2hr bins (taking the **sum** of wets).

# Plot by 2hr bins

plot_props <- function(data, y_var, y_label, plot_title) {
  
  n_colors <- length(unique(data$bird_id))

  #rep to ensure that colors repeat if n_colors is greater than the number of colors in custom_palette (want to add error message eventually)
  palette <- rep(custom_palette, length.out = n_colors) 
  
  plot_ly(data = data,
          x = ~as.POSIXct(paste(date, start_time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          y= as.formula(paste0("~", y_var)), 
          type = 'scatter',
          mode = 'lines+markers',
          color = ~bird_id,
          colors = palette,
          line = list(width = 0.5),
          marker = list(size = 3)) %>%
    
    layout(
      
      title = list(text=plot_title, font=list(size=11)),
      
      xaxis = list(title = "Time (2-hour bins)", 
                   tickformat = "%d %b %Y", # show day. month, year
                   tickmode = "auto", #plotly adjusts the ticks based on zoom
                   showgrid = TRUE,
                   tickangle = 45),
      
      yaxis = list(title = y_label, rangemode = "tozero"), 
      
      legend = list(title = list(text = "Bird ID"), orientation = "v"),

      margin = list(t=100, b=100, l=100, r=100)) %>%
    config(responsive = TRUE)
}

#or plot by day
plot_daily_props <- function(data, y_var, y_label, plot_title) {
  
  n_colors <- length(unique(data$bird_id))
  palette <- rep(custom_palette, length.out = n_colors)
  
  plot_ly(data = data,
          x = ~date,
          y= as.formula(paste0("~", y_var)), 
          type = 'scatter',
          mode = 'lines+markers',
          color = ~bird_id,
          colors = palette,
          line = list(width = 0.5),
          marker = list(size = 3)) %>%
    layout(
      title = list(text=plot_title, font=list(size=11)),
      xaxis = list(title = "Time (2-hour bins)", 
                   tickformat = "%d %b %Y",
                   tickmode = "auto",
                   showgrid = TRUE,
                   tickangle = 45),
      yaxis = list(title = y_label, rangemode = "tozero"), 
      legend = list(title = list(text = "Bird ID"), orientation = "v"),
      margin = list(t=100, b=100, l=120, r=120))%>%
    config(responsive = TRUE)
}



### Plot Switches

# Plotting switches in **10min intervals** across a whole deployment period takes up a lot of mem and it's hard to visualise. Let's summarize the number of switches into a daily period to visualise, taking the sum of switches per day:

daily_switches <- deg_switches %>%
  group_by(bird_id, date, deployment_period, colony) %>%
  summarize(daily_switches = sum(switches, na.rm = TRUE), 
            .groups = "drop")

# plot by day
plot_daily_switches <- function(data, y_var, y_label, plot_title) {
  
  n_colors <- length(unique(data$bird_id))
  palette <- rep(custom_palette, length.out = n_colors)
  
  plot_ly(data = data,
          x = ~date,
          y= as.formula(paste0("~", y_var)), 
          type = 'scatter',
          mode = 'lines+markers',
          color = ~bird_id,
          colors = palette,
          line = list(width = 0.5),
          marker = list(size = 3)) %>%
    layout(
      title = list(text=plot_title, font=list(size=11)),
      xaxis = list(title = "Time (2-hour bins)", 
                   tickformat = "%d %b %Y",
                   tickmode = "auto",
                   showgrid = TRUE,
                   tickangle = 45),
      yaxis = list(title = y_label, rangemode = "tozero"), 
      legend = list(title = list(text = "Bird ID"), orientation = "v"),
      margin = list(t=100, b=100, l=120, r=120))%>%
    config(responsive = TRUE)
}




################################################
################################################

### Plot Longest Periods

# These are the longest continuous dry and wet periods on a timeline (geom_segment)

plot_longest_prds <- function(data, plot_title) {
  
  plot <- ggplot(data, aes(y=bird_id)) +
    geom_segment(aes(x = longest_dry_start, xend = longest_dry_end, yend = bird_id), 
                 size = 1, color = "lightblue") +  
    geom_segment(aes(x = longest_wet_start, xend = longest_wet_end, yend = bird_id), 
                 size = 1, color = "blue") +
    
    labs(title = plot_title,
         subtitle = "Light Blue: Dry | Blue: Wet",
         x = "Time", y = "Bird ID") +
    
    scale_x_datetime(breaks = scales::date_breaks("1 month"), labels = scales::date_format("%b %Y")) +
    
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  plotly_plot <- ggplotly(plot) %>%
    layout(margin = list(l = 100, r = 100, b = 120, t = 120),
           xaxis = list(tickangle = 45))%>%
    config(responsive = TRUE)
  
  return(plotly_plot)
}



### Plot Total Wets 

#	Total time when entire 10min sampling interval was dry (count 0s * 10min)
#	Total time when entire 10min sampling interval was wet (count >=1 * 10min)

plot_total_times <- function(data, plot_title) {
  plot_ly(data = data) %>%
    add_trace(
      x = ~bird_id,
      y = ~total_time_wet,
      type = 'bar',
      name = 'Total time wet (mins)',
      marker = list(color = 'blue')
    ) %>%
    add_trace(
      x = ~bird_id,
      y = ~total_time_dry,
      type = 'bar',
      name = 'Total time dry (mins)',
      marker = list(color = 'lightblue')
    ) %>%
    layout(
      title = list(text = plot_title, font = list(size = 10)),
      xaxis = list(title = "Bird ID", tickangle = 45),
      yaxis = list(title = "Total time (mins)", rangemode = "tozero"),
      barmode = 'group',
      legend = list(title = list(text = "Metric")),
      margin = list(t = 100, b = 100, l = 120, r = 120)%>%
        config(responsive = TRUE)
    )
}


################################################
################################################


# Plot RANDOM subsets (adds more info to legend and fixes dates)

plot_props_random <- function(data, y_var, y_label, plot_title) {
  
  n_colors <- length(unique(data$bird_id))
  
  #rep to ensure that colors repeat if n_colors is greater than the number of colors in custom_palette (want to add error message eventually)
  palette <- rep(custom_palette, length.out = n_colors) 
  
  data <- data %>%
    mutate(extra_info = paste(colony, deployment_period, sep = " | "))
  
  plot_ly(data = data,
          x = ~as.POSIXct(paste(fake_date, start_time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          y= as.formula(paste0("~", y_var)), 
          type = 'scatter',
          mode = 'lines+markers',
          color = ~interaction(bird_id, extra_info, sep = " | "),
          colors = palette,
          line = list(width = 0.5),
          marker = list(size = 3)) %>%
    
    layout(
      
      title = list(text=plot_title, font=list(size=11)),
      
      xaxis = list(title = "Time (2-hour bins)", 
                   tickformat = "%d %b %Y", # show day. month, year
                   tickmode = "auto", #plotly adjusts the ticks based on zoom
                   showgrid = TRUE,
                   tickangle = 45),
      
      yaxis = list(title = y_label, rangemode = "tozero"), 
      
      legend = list(title = list(text = "Bird ID | Deployment | Colony"), 
                    font = list(size = 9),
                    orientation = "v"),
      
      margin = list(t=80, b=100, l=120, r=120)) %>%
    
    config(responsive = TRUE)
}

#or plot by day
plot_daily_props_random <- function(data, y_var, y_label, plot_title) {
  
  data <- data %>%
    mutate(extra_info = paste(colony, deployment_period, sep = " | "))
  
  n_colors <- length(unique(data$bird_id))
  palette <- rep(custom_palette, length.out = n_colors)
  
  plot_ly(data = data,
          x = ~fake_date,
          y= as.formula(paste0("~", y_var)), 
          type = 'scatter',
          mode = 'lines+markers',
          color = ~interaction(bird_id, extra_info, sep = " | "),
          colors = palette,
          line = list(width = 0.5),
          marker = list(size = 3)) %>%
    layout(
      title = list(text=plot_title, font=list(size=11)),
      xaxis = list(title = "Time (2-hour bins)", 
                   tickformat = "%d %b %Y",
                   tickmode = "auto",
                   showgrid = TRUE,
                   tickangle = 45),
      yaxis = list(title = y_label, rangemode = "tozero"),
      
      legend = list(title = list(text = "Bird ID | Deployment | Colony"),  
                    font = list(size = 9),
                    orientation = "v"),
      
      margin = list(t=80, b=100, l=120, r=120))%>%
    config(responsive = TRUE)
}



### Plot Random Switches

plot_daily_switches_random <- function(data, y_var, y_label, plot_title) {
  
  data <- data %>%
    mutate(extra_info = paste(colony, deployment_period, sep = " | "))
  
  n_colors <- length(unique(data$bird_id))
  palette <- rep(custom_palette, length.out = n_colors)
  
  plot_ly(data = data,
          x = ~fake_date,
          y= as.formula(paste0("~", y_var)), 
          type = 'scatter',
          mode = 'lines+markers',
          color = ~interaction(bird_id, extra_info, sep = " | "),
          colors = palette,
          line = list(width = 0.5),
          marker = list(size = 3)) %>%
    
    layout(
      title = list(text=plot_title, font=list(size=11)),
      xaxis = list(title = "Time (2-hour bins)", 
                   tickformat = "%d %b %Y",
                   tickmode = "auto",
                   showgrid = TRUE,
                   tickangle = 45),
      
      yaxis = list(title = y_label, rangemode = "tozero"), 
      
      legend = list(title = list(text = "Bird ID | Colony | Deployment Period"),  
                    font = list(size = 9),
                    orientation = "v"),
      margin = list(t=80, b=100, l=120, r=120))%>%
    config(responsive = TRUE)}


})



