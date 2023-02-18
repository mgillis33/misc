library(tidyverse)
library(scales)
library(ggtext)

biden <- read_csv("/Users/mgillis/Desktop/Projects/misc/approval_topline.csv")

biden <- biden %>% 
  mutate(date = as.Date(modeldate, format = "%m/%d/%Y")) %>% 
  filter(subgroup == "All polls") %>% 
  mutate(range = approve_hi - approve_lo)

cur.date <- max(biden$date)
day <- as.numeric(max(biden$date)-min(biden$date))
cur.appr <- round(biden$approve_estimate[1], 1)
cur.disappr <- round(biden$disapprove_estimate[1], 1)

biden_approval <- biden %>% 
  ggplot() +
  geom_hline(aes(yintercept = 50), linewidth = 0.3) +
  geom_segment(aes(x = cur.date, xend = cur.date, y = 20, yend = 80), linetype = "dotted") +
  scale_x_date(date_breaks = "6 weeks" , date_labels = "%b. '%y") +
  geom_line(aes(x = date, y = approve_estimate), color = "#1E8449", linewidth = 1.5) +
  geom_ribbon(aes(date, ymin = approve_lo, ymax = approve_hi), fill = "#22D16C", alpha = 0.2) +
  geom_line(aes(x = date, y = disapprove_estimate), color = "#E67E22", linewidth = 1.25) +
  geom_ribbon(aes(date, ymin = disapprove_lo, ymax = disapprove_hi), fill = "#F68A2B", alpha = 0.2) +
  annotate("text", x = cur.date, y = 81.25, label = paste("DAY", day), family = "Roboto Condensed", size = 9, color = "#979797") +
  annotate("text", x = cur.date + 30, y = cur.appr, label = paste(cur.appr, "%", sep = ""), family = "Roboto Condensed", size = 12, fontface = "bold", color = "#1E8449") +
  annotate("text", x = cur.date + 30, y = cur.disappr, label = paste(cur.disappr, "%", sep = ""), family = "Roboto Condensed", size = 12, fontface = "bold", color = "#E67E22") +
  scale_y_continuous(limits = c(20,81.26), n.breaks = 7, labels = label_number(suffix = "%", scale = 1)) +
  labs(
    title = "**How <span style = 'color: #1E8449;'>Popular</span>/<span style = 'color: #E67E22;'>Unpopular</span> is Joe Biden?**",
    subtitle = "An updating calculation of the president's approval rating, accounting for each poll's quality,\n recency, sample size and partisan lean.",
    x = paste("Date Updated:",format(Sys.Date(), "%B %d %Y")),
    y = NULL
  ) +
  theme(
    plot.title = element_markdown(size = 40, hjust = 0.5, margin = margin(t = 20, 1, 1, 1, "pt")),
    text = element_text(family = "Roboto Condensed"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, margin = margin(t = 10, 1, b = 10, 1, "pt")),
    panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 17, margin = margin(1, l = 15, 1, 1, "pt"), color = "#979797"),
    axis.text.x = element_text(size = 17, margin = margin(1, 1, b = 15, 1, "pt"), color = "#979797"),
    axis.title.x = element_text(size = 15, color = "#979797"),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(t = 15, l = 30, b = 15, r = 40, "pt")
  )

biden_approval

ggsave(filename = "bidenapproval.png", path = "/Users/mgillis/Desktop/Projects/misc/", width = 527.04, height = 296.46, units = "mm")

