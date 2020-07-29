
# Load packages
library(tidyverse)
library(palmerpenguins)
library(ggridges)
library(ggdark)
library(here)
library(gghalves)
library(viridis)
library(cowplot)

# Inspect data
head(penguins)
# Plot!
PenguinPlot1 <- penguins %>%
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

# Import artwork
url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/man/figures/lter_penguins.png"
img <- png::readPNG(RCurl::getURLContent(url))
cute_penguins <- grid::rasterGrob(img, interpolate = T)

# Penguin pal
pal <- c("#FF8C00", "#A034F0", "#159090")

# Create a raincloud plot!
PenguinPlot2 <- penguins %>%
  na.omit() %>% 
  mutate(BodyMass = body_mass_g/1000) %>% 
  ggplot(aes(x = species, y = BodyMass, colour = species, fill = species)) +
  geom_half_point(side = "l", size = 0.3, 
                  colour = "white") + 
  # Add a half boxplot
  geom_half_boxplot(side = "l", width = 0.5, 
                    alpha = 0.3, nudge = 0.1,
                    colour = "white") +
  # Add a half violin
  geom_half_violin(aes(colour = species), 
                   side = "r",
                   colour = "black") +
  guides(fill = FALSE, color = FALSE) +
  scale_fill_manual(values = pal) +
  coord_flip() +
  scale_y_continuous(breaks = c(3:6), labels = paste0(3:6, " kg")) +
  labs(x = NULL, y = NULL,
       title = "\n Distribution of Palmer Penguin body mass (kg), by species.",
       caption = "\n By @alicesweeting | #TidyTuesday | Source: Gorman, Williams and Fraser, 2014 | Artwork by @allison_horst") +
  dark_theme_gray()   + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = .5),
        legend.key = element_blank(),
        legend.position = "none") 
# Draw!
ggdraw(PenguinPlot2) +
  draw_image(img, x = 0.65, y = 0.2, height = 0.37, width = 0.35)  +
  ggsave(here(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "PenguinPlot", ".png")), type = 'cairo')
