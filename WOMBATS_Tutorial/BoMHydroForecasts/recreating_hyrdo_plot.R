### Visualisation recreated from the Bureau of Meteorology: 
# http://www.bom.gov.au/water/7daystreamflow/#panel=advanced&product_type=hourly_barplot_ens

library(tidyverse)

# load our data
hydro_file = "../BoMHydroForecasts/403242A_hourly_barplot_ens.csv"
hydro_data_raw <- read_csv(hydro_file, skip = 23)
summary(hydro_data_raw)

# Naive first plot - streamflow
names(hydro_data)
ggplot(hydro_data_raw) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`))

# Something isn't right :(

# Notice that there are dashes
# R will assume character variables instead of numeric for these columns 
hydro_data_preprocess = hydro_data_raw
hydro_data_preprocess[hydro_data_raw == "-"] <- NA

# Convert to the time column to a date type
time_col = str_replace(hydro_data_preprocess$Time, 
                       pattern = " AEDT", replace = ":00") %>%
  lubridate::ymd_hms(tz = NULL)

# Convert the other character variables to numeric
hydro_data <- hydro_data_preprocess %>%
  select(-Time) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(Time = time_col) 

# Notice how I've named my dataset along the way so I know 
# the steps taken

# Streamflow 
names(hydro_data)
ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`)) 

# Better! 

# Let's add some more variables

# Streamflow  - add forecasted streamflow
ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  theme_bw()

# Streamflow  - update the axes
ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  ylab("Streamflow (ML/day)") + 
  theme_bw() 

# Streamflow - add horizontal line between obs and forecast
# View(hydro_data)
ind = which(is.na(hydro_data$`Observed Streamflow (ML/day)`))[1]

ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") + 
  ylab("Streamflow (ML/day)") + 
  theme_bw()

# Streamflow - let's add the ribbons
View(hydro_data)

names(hydro_data)
ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), col = "red") + 
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  ylab("Streamflow (ML/day)") + 
  theme_bw()

# Okay but we need to tweak the aesthetics
ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  ylab("Streamflow (ML/day)") + 
  theme_bw()
# Careful of Color vs fill
# Careful of layering

# Let's fix it 
names(hydro_data)
ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 25th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 75th PCTL (ML/day)`), fill = "red", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  ylab("Streamflow (ML/day)") + 
  theme_bw()

# What else ?? 

# Streamflow - let's add the text
?geom_label
label_obs_x = min(time_col) 
label_forc_x = time_col[ind]
label_obs_y = label_forc_y = max(hydro_data$`Observed Streamflow (ML/day)`,
                                 hydro_data$`Forecast Streamflow 95th PCTL (ML/day)`,
                                 na.rm = TRUE)

label_df <- data.frame(label = c("Observed", "Forecast"), 
           x = c(label_obs_x, label_forc_x),
           y = c(label_forc_y, label_forc_y),
           col = c("obs", "forc"))

ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 25th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 75th PCTL (ML/day)`), fill = "red", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  geom_label(data = label_df, aes(x = x, y = y, label = label, col = col)) + 
  ylab("Streamflow (ML/day)") + 
  theme_bw()

# Almost ... 
# Need to horiztonaly adjust them 
# Need to colour them differently

ggplot(hydro_data) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 25th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 75th PCTL (ML/day)`), fill = "red", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  geom_label(data = label_df, aes(x = x, y = y, label = label, col = col), hjust = -0.1) + 
  scale_color_manual(values = c("red", "blue")) +
  ylab("Streamflow (ML/day)") + 
  theme_bw() + 
  theme(legend.position = "none")

# Let's add the historical levels
names(hydro_data)

ggplot(hydro_data) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 5th PCTL (ML/day)`, 
                  ymax = `Historical Reference 95th PCTL (ML/day)`), fill = "blue", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 25th PCTL (ML/day)`, 
                  ymax = `Historical Reference 75th PCTL (ML/day)`), fill = "blue", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 25th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 75th PCTL (ML/day)`), fill = "red", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  geom_label(data = label_df, aes(x = x, y = y, label = label, col = col), hjust = -0.1) + 
  scale_color_manual(values = c("red", "blue")) +
  ylab("Streamflow (ML/day)") + 
  theme_bw() + 
  theme(legend.position = "none")

# Need title and legend
main_title = "Ovens River at Wangarattra (ID: 403242A)"
sub_title = "Forecast for 04 Dec 2022 to 11 Dec 2022 (10:00 AEDT)"
ggplot(hydro_data) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 5th PCTL (ML/day)`, 
                  ymax = `Historical Reference 95th PCTL (ML/day)`), fill = "blue", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 25th PCTL (ML/day)`, 
                  ymax = `Historical Reference 75th PCTL (ML/day)`), fill = "blue", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 25th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 75th PCTL (ML/day)`), fill = "red", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  geom_label(data = label_df, aes(x = x, y = y, label = label, col = col), hjust = -0.1) + 
  scale_color_manual(values = c("red", "blue")) +
  ylab("Streamflow (ML/day)") + 
  ggtitle(main_title, sub_title) + 
  theme_bw() + 
  theme(legend.position = "none")

# ?? Second axes ?? 
# https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

# What about the legend ??

# Rainfall 
ggplot(hydro_data) + 
  geom_col(aes(x = Time, y = `Observed Rainfall (mm/hour)`), fill = "blue") + 
  geom_col(aes(x = Time, y = `Forecast Rainfall Median (mm/hour)`), fill = "red") + 
  ylab("Rainfall (mm/hour)") + 
  ylim(c(0, 0.5)) +
  theme_bw()

# Combine the plots 
library(patchwork)

rainfall_plot <- ggplot(hydro_data) + 
  geom_col(aes(x = Time, y = `Observed Rainfall (mm/hour)`), fill = "blue") + 
  geom_col(aes(x = Time, y = `Forecast Rainfall Median (mm/hour)`), fill = "red") + 
  ylab("Rainfall (mm/hour)") + 
  ylim(c(0, 0.5)) +
  ggtitle(main_title, sub_title) + 
  theme_bw()

hydro_plot <- ggplot(hydro_data) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 5th PCTL (ML/day)`, 
                  ymax = `Historical Reference 95th PCTL (ML/day)`), fill = "blue", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 25th PCTL (ML/day)`, 
                  ymax = `Historical Reference 75th PCTL (ML/day)`), fill = "blue", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 25th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 75th PCTL (ML/day)`), fill = "red", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  geom_label(data = label_df, aes(x = x, y = y, label = label, col = col), hjust = -0.1) + 
  scale_color_manual(values = c("red", "blue")) +
  ylab("Streamflow (ML/day)") + 
  theme_bw() + 
  theme(legend.position = "none") 

rainfall_plot / hydro_plot +
  plot_layout(heights = unit(c(1, 4), c('cm', 'null')))

# Let's tweak it 

rainfall_plot <- ggplot(hydro_data) + 
  geom_col(aes(x = Time, y = `Observed Rainfall (mm/hour)`), fill = "blue") + 
  geom_col(aes(x = Time, y = `Forecast Rainfall Median (mm/hour)`), fill = "red") + 
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  ylab("Rainfall (mm/hr)") + 
  ggtitle(main_title, sub_title) + 
  ylim(c(0, 0.5)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

hydro_plot <- ggplot(hydro_data) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 5th PCTL (ML/day)`, 
                  ymax = `Historical Reference 95th PCTL (ML/day)`), fill = "blue", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Historical Reference 25th PCTL (ML/day)`, 
                  ymax = `Historical Reference 75th PCTL (ML/day)`), fill = "blue", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Observed Streamflow (ML/day)`), col = "blue") + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 5th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 95th PCTL (ML/day)`), fill = "red", alpha = 0.2) + 
  geom_ribbon(aes(x = Time, ymin = `Forecast Streamflow 25th PCTL (ML/day)`, 
                  ymax = `Forecast Streamflow 75th PCTL (ML/day)`), fill = "red", alpha = 0.5) + 
  geom_line(aes(x = Time, y = `Forecast Streamflow Median (ML/day)`), col = "red") +
  geom_vline(aes(xintercept = Time[ind]), linetype = "dashed") +
  geom_label(data = label_df, aes(x = x, y = y, label = label, col = col), hjust = -0.1) + 
  scale_color_manual(values = c("red", "blue")) +
  ylab("Streamflow (ML/day)") + 
  theme_bw() + 
  theme(legend.position = "none")

rainfall_plot / hydro_plot +
  plot_layout(heights = unit(c(2.25, 7), c('cm', 'null')))

# Redundancy in the legend 
# Does showing the historical reference seem useful 
# What about the forecast trajectories? Surely they matter
# We could find trajectory data from https://geoglows.ecmwf.int/

# Pretty close: some small tweaks - 
# Fix the y-axis ticks on the rainfall 
# Need a legend - but I don't love the Bureau legend

