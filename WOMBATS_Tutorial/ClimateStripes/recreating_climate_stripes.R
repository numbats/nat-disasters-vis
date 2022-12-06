## interactive lecture

# -----------------------------------------------------------------------------

# The R Package patchwork is a great way to combine multiple plots
# Check out: https://github.com/thomasp85/patchwork
install.packages("patchwork")
library(patchwork)

# Let's create a toy data frame to play around with patchwork
eg_df = data.frame(x = 1:10, y = 10:1, z  = -(1:10))

# Let's create some example plots
library(tidyverse)
point_plot <- ggplot(data = eg_df) + 
  geom_point(aes(x = x, y = y))
line_plot <- ggplot(data = eg_df) + 
  geom_line(aes(x = x, y = z))  
line_plot
box_plot <- ggplot(data = eg_df) +
  geom_boxplot(aes(x = x))

# We can combine these plot together using patchwork

# horizontal
point_plot + line_plot 

# vertical
point_plot / line_plot 

# combination plots
(point_plot + line_plot) / box_plot  

# combination plots with plot labels
(point_plot + line_plot) / box_plot  + 
  plot_annotation(tag_levels = 'A') 

# combination plots with plot labels and titles
(point_plot + line_plot) / box_plot  + 
  plot_annotation(tag_levels = 'A',
                  title = 'Some example plots',
                  subtitle = 'There are three',
                  caption = 'None of these plots are insightful') 

# -----------------------------------------------------------------------------

# Let's look at a data visualisation example
# Climate Stripes is a famous data visualisation to help communicate graphically 
# that our climate is changing
# https://showyourstripes.info/s/globe 

# Let's get some temperature data
library(tidyverse)
temp_file <- "Sydney_Temperature_Data.csv"
temp_data <- read_csv(temp_file)
View(temp_data)

# Can download the data from http://www.bom.gov.au/climate/data/stations/
# or the package rnoaa will provide the same data in a different format

# Rename the columns to make things easier
temp_data = rename(temp_data, 
                   max_temp = `Maximum temperature (Degree C)`)
View(temp_data)

# Get the average daily temperature
temp_data_year = group_by(temp_data, Year)
temp_data_average = summarise(temp_data_year, 
                              mean_daily_temp = mean(max_temp, na.rm = TRUE))
View(temp_data_average)
#Side note: Have a think about when it is okay to use na.rm when we estimate our mean #

# Visualise the data using ggplot2

# Line plot
ggplot(data = temp_data_average, aes(x = Year, y = mean_daily_temp)) + 
  geom_smooth() +
  geom_line() + 
  ylab("Average daily maximum temperature (oC)")

# Why doesn't this convince people of climate change???

# Why do the climate stripes do better as a data visualisation

# column plot
ggplot(temp_data_average, aes(x = Year, y = mean_daily_temp)) + 
  geom_col(aes(col = mean_daily_temp)) 

# add colours
ggplot(temp_data_average, aes(x = Year, y = mean_daily_temp)) + 
  geom_col(aes(col = mean_daily_temp)) +
  scale_color_gradient(low = "blue", high = "red")

# another way to change the colour
ggplot(temp_data_average, aes(x = Year, y = mean_daily_temp)) + 
  geom_col(aes(col = mean_daily_temp)) +
  scale_color_distiller(palette = "RdBu")

# actually we need to fill the cols
ggplot(temp_data_average, aes(x = Year, y = mean_daily_temp)) + 
  geom_col(aes(fill = mean_daily_temp)) +
  scale_fill_distiller(palette = "RdBu")

# but the column plot isn't quite right, there is no y values
# so we can make the y values all the same
vec_of_ones = rep(1, nrow(temp_data_average))
temp_data_average <- mutate(temp_data_average, ones = vec_of_ones)
ggplot(temp_data_average, aes(x = Year, y = ones)) + 
  geom_col(aes(fill = mean_daily_temp)) +
  scale_fill_distiller(palette = "RdBu") 

# add a title 
ggplot(temp_data_average, aes(x = Year, y = ones)) + 
  geom_col(aes(fill = mean_daily_temp)) +
  scale_fill_distiller(palette = "RdBu") + 
  ggtitle("Sydney Average Daily Temperature")

# get rid of the y axis label (don't need it now)
# This is extra 
ggplot(temp_data_average, aes(x = Year, y = ones)) + 
  geom_col(aes(fill = mean_daily_temp)) +
  scale_fill_distiller("Mean Daily Temperature", palette = "RdBu") +  # add a legend label
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  ggtitle("Sydney Average Daily Temperature")

# get rid of the y axis label (don't need it now)
# This is extra 
ggplot(temp_data_average, aes(x = Year, y = ones)) + 
  geom_col(aes(fill = mean_daily_temp)) +
  scale_fill_distiller("Mean Daily Temperature", palette = "RdBu") +  # add a legend label
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  ggtitle("Sydney Average Daily Temperature")

# Tufte - can't map variable to more than one aesthetics 

temp_data_average <- temp_data_average %>% 
  mutate(transformed = (mean_daily_temp  - mean(mean_daily_temp))/
           sd(mean_daily_temp))
  
ggplot(temp_data_average, aes(x = Year, 
                              y = transformed)) + 
  geom_col(aes(fill = mean_daily_temp)) +
  scale_fill_distiller("Mean Daily Temperature", palette = "RdBu") +  # add a legend label
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  ggtitle("Sydney Average Daily Temperature")

