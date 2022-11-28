library(data.table)
library(ggplot2)
library(patchwork)

DT_Ca <- fread("ignore/20180625_0708_Campbell.txt")
DT_Mo <- fread("ignore/20180628_1617_Mountain.txt")

p1 <- ggplot(DT_Ca) + 
    geom_point(aes(ODT, Gap, color = SSC), size = 0.8) + 
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_color_manual(values = c("GG" = "forestgreen", "RR" = "red", "RG" = "black")) + 
    labs(title = "(a) Speedway Blvd & Campbell Ave",
         x = "On-detector time (sec)",
         y = "Time gap (sec)",
         color = "Actuation signal status change") + 
    theme(panel.border = element_rect(fill = NA),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.direction = "horizontal",
          legend.key.width = unit(1, "cm"),
          legend.key.size = unit(2, "cm"),
          legend.key.height = unit(2, "cm"),
          legend.position = "top")

p2 <- ggplot(DT_Mo) + 
    geom_point(aes(ODT, Gap, color = SSC), size = 0.8) + 
    scale_x_continuous(breaks = seq(0, 70, 10)) +
    scale_color_manual(values = c("GG" = "forestgreen", "RR" = "red", "RG" = "black")) + 
    labs(title = "(b) Speedway Blvd & Mountain Ave",
         x = "On-detector time (sec)",
         y = "Time gap (sec)",
         color = "Actuation signal status change") + 
    theme(panel.border = element_rect(fill = NA),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.direction = "horizontal",
          legend.key.width = unit(1, "cm"),
          legend.key.size = unit(2, "cm"),
          legend.key.height = unit(2, "cm"),
          legend.position = "top")

plot1 <- p1 / p2 + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

plot1


# On-detector time vs wait time until green ------------------------------------

DT_Ca_RG <- fread("ignore/Campbell.txt")[SSC == "RG", ]
DT_Mo_RG <- fread("ignore/Mountain.txt")[SSC == "RG", ]

p3 <- ggplot() + 
    geom_point(data = DT_Ca_RG, aes(ODT, TUG)) + 
    scale_x_continuous(breaks = seq(0, 90, 10)) + 
    scale_y_continuous(breaks = seq(0, 80, 10)) + 
    labs(title = "(a) Speedway Blvd & Campbell Ave",
         x = "On-detector time (sec)",
         y = "Wait time until green (sec)",
         color = "Actuation signal status change") + 
    theme(panel.border = element_rect(fill = NA),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14))

p4 <- ggplot() + 
    geom_point(data = DT_Mo_RG, aes(ODT, TUG)) + 
    scale_size_continuous(breaks = c(0, 10, 2.5)) + 
    scale_x_continuous(breaks = seq(0, 50, 5), labels = seq(0, 50, 5)) + 
    scale_y_continuous(breaks = seq(0, 25, 5)) + 
    labs(title = "(b) Speedway Blvd & Mountain Ave",
         x = "On-detector time (sec)",
         y = "Wait time until green (sec)",
         color = "Actuation signal status change") + 
    theme(panel.border = element_rect(fill = NA),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14))

plot2 <- p3 / p4
plot2

ggsave("output/ODT_TUG.png",
       plot = plot2,
       units = 'cm',
       width = 25,
       height = 25,
       dpi = 600)
