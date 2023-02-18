library(tidyverse)
library(scales)
library(ggtext)

# load in data
biden <- read.csv("/Users/mgillis/Desktop/Projects/misc/biden-approval/data/approval_topline.csv")
single_polls <- read.csv("/Users/mgillis/Desktop/Projects/misc/biden-approval/data/president_approval_polls.csv")

# mutate and filter data to obtain accurate dates and filter out unneeded data
biden <- biden %>% 
  mutate(date = as.Date(modeldate, format = "%m/%d/%Y")) %>% 
  filter(subgroup == "All polls") %>% 
  mutate(range = approve_hi - approve_lo)

single_polls <- single_polls %>% 
  mutate(date = as.Date(created_at, format = "%m/%d/%y"))

#obtain current date and properties about that day
cur.date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
day <- as.numeric(cur.date-min(biden$date - 3))
cur.appr <- round(biden$approve_estimate[1], 1)
cur.disappr <- round(biden$disapprove_estimate[1], 1)

# create the plot
biden_approval <- ggplot(data = biden) +
  geom_hline(aes(yintercept = 50), linewidth = 0.3) +
  geom_segment(aes(x = cur.date, xend = cur.date, y = 20, yend = 80), linetype = "dotted") +
  scale_x_date(date_breaks = "6 weeks" , date_labels = "%b. '%y") +
  geom_point(data = single_polls, aes(x = single_polls$date, y = yes), color = "#009f29", alpha = 0.05) +
  geom_point(data = single_polls, aes(x = single_polls$date, y = no), color = "#ff7400", alpha = 0.05) +
  geom_line(aes(x = date, y = approve_estimate), color = "#009f29", linewidth = 1.5) +
  geom_ribbon(aes(date, ymin = approve_lo, ymax = approve_hi), fill = "#009f29", alpha = 0.15) +
  geom_line(aes(x = date, y = disapprove_estimate), color = "#ff7400", linewidth = 1.25) +
  geom_ribbon(aes(date, ymin = disapprove_lo, ymax = disapprove_hi), fill = "#ff7400", alpha = 0.15) +
  annotate("text", x = cur.date, y = 81.25, label = paste("DAY", day), family = "Roboto Condensed", size = 9, color = "#979797") +
  annotate("text", x = cur.date + 30, y = cur.appr, label = paste(cur.appr, "%", sep = ""), family = "Roboto Condensed", size = 12, fontface = "bold", color = "#009f29") +
  annotate("text", x = cur.date + 30, y = cur.disappr, label = paste(cur.disappr, "%", sep = ""), family = "Roboto Condensed", size = 12, fontface = "bold", color = "#ff7400") +
  scale_y_continuous(limits = c(20,81.26), n.breaks = 7, labels = label_number(suffix = "%", scale = 1)) +
  labs(
    title = "**How <span style = 'color: #009f29;'>Popular</span>/<span style = 'color: #ff7400;'>Unpopular</span> is Joe Biden?**",
    subtitle = "An updating calculation of the president's approval rating, accounting for each poll's quality,\n recency, sample size and partisan lean.",
    x = paste("Date Updated:", format(Sys.Date(), "%B %d %Y")),
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

# display the plot
biden_approval

# save the plot
ggsave(filename = "bidenapproval.png", path = "/Users/mgillis/Desktop/Projects/misc/biden-approval/final_product/", width = 527.04, height = 296.46, units = "mm")

