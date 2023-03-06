library(tidyverse)
library(usmap)
library(ggtext)

# import data
gop_polls <- read.csv("primary-2024/data/gop-polls.csv")

# clean data
gop_polls <- gop_polls %>% 
  select(State, Margin) %>% 
  rename(state = State)

# make plot
plot_usmap(data = gop_polls, values = "Margin", regions = "states", color = "black") +
  scale_fill_gradient2(
    name = "Margin of\nTrump Lead",
    low = "darkgreen",
    mid = "white",
    high = "purple",
    midpoint = 0,
    guide = "colorbar",
    na.value = "grey50") +
  labs(
    title = "**<span style = 'color: #A020F0;'>Trump</span> vs. <span style = 'color: #0B7739;'>DeSantis</span>**",
    subtitle = "Current 2024 GOP Primary Poll Results\nData from FiveThirtyEight",
    caption = paste("Last Updated on ",format(Sys.Date(), "%B %d %Y"))
    ) +
  theme(
    text = element_text(family = "Trebuchet MS"),
    plot.background = element_rect(color = "lightgrey", fill = "lightgrey"),
    plot.title = element_markdown(size = 50, hjust = 0.5, margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    legend.title.align = 0.5,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 0.1, l = 15),
    legend.position = "bottom",
    legend.background = element_blank()
    )

# export plot
ggsave(filename = "gop-primary.png", path = "/Users/mgillis/Desktop/Projects/misc/primary-2024/final_product/", width = 200, height = 200, units = "mm")

