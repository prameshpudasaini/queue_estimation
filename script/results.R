library(data.table)
library(ggplot2)
library(patchwork)

# Speedway & Campbell
qDT_Ca <- data.table(
    Cycle = as.factor(c(2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 18, 19, 20, 21, 22, 23, 25, 26, 27, 28)),
    Queue_Cal = c(6, 10, 10, 6, 10, 4, 4, 4, 7, 7, 13, 11, 9, 9, 9, 13, 4, 10, 9, 11, 6),
    Queue_Obs = c(6, 9, 9, 6, 7, 2, 4, 4, 6, 8, 9, 10, 11, 12, 10, 13, 4, 10, 9, 12, 6)
)

round(mean(abs(qDT_Ca$Queue_Obs - qDT_Ca$Queue_Cal) * 25), 2L)
round(mean(abs(qDT_Ca$Queue_Obs - qDT_Ca$Queue_Cal) / qDT_Ca$Queue_Obs) * 100, 2L)
round(sqrt(mean((qDT_Ca$Queue_Obs * 25 - qDT_Ca$Queue_Cal * 25) ^ 2)), 2L)

# Speedway & Mountain
qDT_Mo <- data.table(
    Cycle = as.factor(c(1, 3, 9, 20, 21, 22, 25, 26, 33, 34)),
    Queue_Obs = c(13, 12, 10, 15, 9, 10, 10, 11, 15, 12),
    Queue_Cal = c(13, 15, 7, 15, 10, 14, 10, 9, 11, 12)
)

round(mean(abs(qDT_Mo$Queue_Obs - qDT_Mo$Queue_Cal) * 25), 2L)
round(mean(abs(qDT_Mo$Queue_Obs - qDT_Mo$Queue_Cal) / qDT_Mo$Queue_Obs) * 100, 2L)
round(sqrt(mean((qDT_Mo$Queue_Obs * 25 - qDT_Mo$Queue_Cal * 25) ^ 2)), 2L)

qDT_Ca <- melt(qDT_Ca, id.vars = "Cycle", measure.vars = c("Queue_Obs", "Queue_Cal"))[, value := value * 25]
qDT_Mo <- melt(qDT_Mo, id.vars = "Cycle", measure.vars = c("Queue_Obs", "Queue_Cal"))[, value := value * 25]

p1 <- ggplot(qDT_Ca) + 
    stat_identity(aes(Cycle, value, fill = variable), color = "grey20", geom = "bar", position = "dodge") + 
    geom_text(aes(x = 7, y = 300, label = "MAPE = 14.77%"), hjust = 0) + 
    geom_text(aes(x = 7, y = 275, label = "MAE = 25.0 ft"), hjust = 0) + 
    geom_text(aes(x = 7, y = 250, label = "RMSE = 38.19 ft"), hjust = 0) + 
    scale_y_continuous(breaks = seq(0, 400, 50)) + 
    scale_fill_manual(values = c("Queue_Obs" = "darkorange", "Queue_Cal" = "forestgreen"),
                      labels = c("Ground truth", "Proposed method")) +
    labs(title = "(a) Speedway Blvd & Campbell Ave",
         x = "Cycles with long queue",
         y = "Maximum queue length (ft)",
         fill = "") + 
    theme(panel.border = element_rect(fill = NA),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.direction = "horizontal",
          legend.position = "bottom")

p2 <- ggplot(qDT_Mo) + 
    stat_identity(aes(Cycle, value, fill = variable), color = "grey20", geom = "bar", position = "dodge") + 
    geom_text(aes(x = 6.65, y = 350, label = "MAPE = 15.1%"), hjust = 0) + 
    geom_text(aes(x = 6.65, y = 325, label = "MAE = 42.5 ft"), hjust = 0) + 
    geom_text(aes(x = 6.65, y = 300, label = "RMSE = 58.63 ft"), hjust = 0) + 
    scale_y_continuous(breaks = seq(0, 400, 50)) + 
    scale_fill_manual(values = c("Queue_Obs" = "darkorange", "Queue_Cal" = "forestgreen"),
                      labels = c("Ground truth", "Proposed method")) +
    labs(title = "(b) Speedway Blvd & Mountain Ave",
         x = "Cycles with long queue",
         y = "Maximum queue length (ft)",
         fill = "") + 
    theme(panel.border = element_rect(fill = NA),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.direction = "horizontal",
          legend.position = "bottom")

plot <- p1 / p2 + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

ggsave("output/results.png",
       plot = plot,
       units = 'cm',
       width = 25,
       height = 25,
       dpi = 600)
