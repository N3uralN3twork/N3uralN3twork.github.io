library(tidyverse)
library(stringr)
library(scales)

windowsFonts("Arial" = windowsFont("Arial"))

library(readxl)
dfSkills <- read_excel("assets/graphs/Quinn-SkillsData.xlsx", 
                       sheet = "Skills")


# Data Pre-processing:
dfSkills$Skill <- as.factor(dfSkills$Skill)

base_plot <- ggplot(data = dfSkills) +
  geom_hline(aes(yintercept = y),
             data.frame(y = c(0:4) * 25),
             color = "lightgrey") +
  
  geom_col(aes(x = reorder(Skill, Strength),
               y = Strength,
               fill = Years),
           position = "dodge2",
           show.legend = TRUE,
           alpha = 0.90) +
    
  geom_point(aes(x = reorder(Skill, Strength),
                 y = Strength),
             size = 3,
             color = "gray12") +
  
  geom_segment(aes(x = reorder(Skill, Strength),
                   y = 0,
                   xend = reorder(Skill, Strength),
                   yend = 100),
               linetype = "dashed",
               color = "gray12") +
  
  # Adding labels to each section:
  geom_text(aes(x = reorder(Skill, Strength),
                y = Strength + 10,
                label = Strength
                ),
            colour = "blue") +
  
  coord_polar()

# Check out the base plot:
base_plot
  


### Adding Annotations:
add_plot <- base_plot +
  
  # Scaling the y-axis so the bars don't start in the center:
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    breaks = c(0, 50, 60, 70, 80, 90, 100)
  ) +
  
  # Creating a new fill and legend:
  scale_fill_gradient("Experience (Years)",
                      low = "yellow", high = "red", na.value = NA) +
  
  # Creating a guide for the filled-in pattern:
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = 0.5, title.position = "top", title.hjust = 0.5
    )
  ) +
  
  # Removing axes and moving the legend to the bottom:
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom"
  )

add_plot
  
  
final_plot <- add_plot + 
  # Add labels
  labs(
    title = "Breakdown of Skills by Comfort and Experience",
    subtitle = "General strengths per my perceptions.",
    x = NULL
    ) +
  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Arial"),
    
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 15, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )

final_plot


