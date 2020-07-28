
# Load packages
library(tidyverse)
library(palmerpenguins)
library(ggridges)
library(ggdark)
library(here)
# Inspect data
head(penguins)
# Plot!
PenguinPlot <- penguins %>%
na.omit() %>% 
mutate(BodyMass = body_mass_g/1000) %>% 
ggplot(aes(x = BodyMass, y = year, fill = year, colour = year)) +
     geom_density_ridges() +
     scale_fill_viridis_d(option = "C") +
     scale_colour_viridis_d(option = "C") +
     scale_x_continuous(breaks = c(3:6), labels = paste0(3:6, " kg")) +
     labs(x = NULL, y = NULL,
     title = "\n How has the body mass (kg) of Palmer Penguins, \n by species and sex, changed over time? \n",
     caption = "\n By @alicesweeting | #TidyTuesday | Source: Gorman, Williams and Fraser, 2014") +
     dark_theme_gray()   + 
     theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none") +
     facet_wrap(species~sex, ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  ggsave(here(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')


