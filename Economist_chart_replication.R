# Clear up data in global environment
rm(list=ls()) 

# Install Libraries (if required):
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('extrafont')
install.packages('ggthemes', dependencies = TRUE)
install.packages("ggtext")
install.packages("cowplot")
install.packages("dplyr")
install.packages('gsheet')
install.packages("magick")
install.packages("svglite")
install.packages('extrafont')

# Load libraries:
library(tidyverse)
library(extrafont)
windowsFonts()
library(ggthemes)
library(ggtext)
library(cowplot)
library(ggplot2)
library(dplyr)
library(gsheet)
library(magick)
library(svglite)
library(extrafont)


# Load dataset in raw format form my GitHub account
data_frame <- readr::read_csv("..data.csv")


# Review dataset
data_frame %>% glimpse()


# Data wrangling and cleanup
data_frame %>% 
    select(state_abb, total_evs_2016, probability = dem_probability_2016pred, probability_mand = dem_probability_mandatory) %>% # Select data
    mutate_at(vars(contains("prob")), ~.x * 100) %>%    # Change probability columns to %
    mutate(state_abb = reorder(state_abb, -probability_mand)) %>%   # Reorder bars (inverse)
    mutate(right_color = if_else(probability >= 50, "#e30613", "#0093b1")) %>%     # Set right color band
    mutate(left_color = if_else(probability_mand >= 50, "#e30613", "#0093b1")) %>%    # Set left color band
    
    
    # Plot
    ggplot(aes(x = probability, y = state_abb, label = state_abb)) + 
    geom_vline(xintercept = 50) +  # Setup vertical line at 50%
    geom_segment(aes(xend = probability_mand, yend = state_abb, size = log10(10*total_evs_2016 ) ), color = "#cfe1eb") + # Setup inner bar/band color section
    
    
    # Increase width of bars
    geom_segment(aes(xend = probability - 1, yend = state_abb, size = log10(10*total_evs_2016),color = right_color)) +
    geom_segment(aes(x = probability_mand, xend = probability_mand + 1, yend = state_abb, size = log10(10*total_evs_2016), color = left_color)) +
    geom_text(aes(x = probability, y = state_abb), color = "black", nudge_x = 2.2) + # State labels on the right side of the bars
    
    
    # Annotation
    geom_richtext(aes( x = 83, y = 2.7),label="<i style='font-size:20pt;color:#0093b1;'><b> < </b></i>Mandatory voting would further inflate <br> Democrats margins in completely <br> safe blue states like New York (NY)" , color = 'black',label.color = "white",size=5) +
    geom_richtext(aes( x = 89, y = 23),label="Trump trailed in the polls in Michigan (MI), <br> and won it largely because black turnout <br> fell from its 2012 level. Mandatory voting <br> would have given Clinton a larger cushion <i style='font-size:20pt;color:#0093b1;'><b> > </b></i>" , color = 'black',label.color = 'white',size=5) +
    geom_richtext(aes( x = 10, y = 2.3),label=" *Maine and Nebraska are treated as if they gave <br> all their electoral votes to the state wide winner" , color = 'black',label.color = 'white',size=4) +
    geom_richtext(aes( x = 12, y = 27),label="<i style='font-size:20pt;color:#e30613;'><b> ^ </b></i>Mandatory voting would help<br>Democrats more in Mississippi<br>(MS) than in any other state" , color = 'black',label.color = 'white',size=5) +
    geom_richtext(aes( x = 89, y = 47),label="<p style='font-size:13pt;'><b>Change in pre-election win probability if everyone voted</b></p><p style='font-size:12pt;'>US presidential election 2016, by state, %</p>" , color = 'black',label.color = 'white',size=5) +
    annotate("text", x = 55, y = 45, label = "Hillary Clinton \n more likely to win", fontface=2,family = "serif", color = "#0093b1", hjust = 1, size = 7) + 
    annotate("text", x = 45, y = 45, label = "Donald Trump \n more likely to win", fontface=2, family = "serif", color = "#e30613", hjust = 0, size = 7) + 
    
    
    # Due to lack of data and not having Adobe Illustrator I will draw the legends onto the plot
    draw_image("https://..legend.jpg",x = -30, y = 10, width = 18, height = 9.925449871)+
    draw_image("https://..legend2.jpg",x = -100, y = 32, width = 20, height = 13.59807461)+
    
    
    # Format (axis and titles)
    scale_x_reverse(breaks = c(0, 10, 20, 30, 40, 50, 60, 70 ,80, 90, 100), labels = c("100", "90", "80", "70", "60", "50", "60", "70", "80", "90", "100"), ) +
    theme_minimal(base_size=18) +   # White background
    scale_y_discrete(breaks = NULL) +  # Remove vertical line-breaks
    xlab("") +  # Remove axis labels
    ylab("") +  # Remove axis labels
    theme(legend.position = "None",     # Remove legend 
          plot.title = element_text(size = 15, color = "#e30613", hjust = .5),
          plot.subtitle = element_text(size = 30, family = "serif", hjust = .5),
          panel.grid.minor.x = element_blank()) + 
    labs(title = "The silent near-majority",
         subtitle = "If everyone had voted, Hillary Clinton \n would probably be president")   # Setup titles


# You can export image as "SVG" with "WIDTH:1900" and "HEIGHT:1060", or better yet, please run the code below
# Save plot as PDF and SVG with the dimensions specified below
# Images will be saved in the working directory setup on line 7 above (please make sure you change the path to your location)
ggsave("Jovan_Trajceski_HW1.pdf", path=getwd(), width=22, height=12, units = ("in"))
ggsave("Jovan_Trajceski_HW1.svg", path=getwd(), width=22, height=12, units = ("in"))
dev.off()

# The PDF/SVG files will be saved here:
getwd()

# This code is free to use for academic purposes only, provided that a proper reference is cited. 
# This code comes without the technical support of any kind. 
# Under no circumstances will the author be held responsible for any use of this code in any way.


