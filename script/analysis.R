library(data.table)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Speedway & Campbell ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat <- getQS(listDT[[24]])
dat$QS

# queue length estimation parameters

ffs <- 35 * 5280 / 3600
dist <- 100
sat_headway <- 2.1

ff_time_stop_bar <- dist / ffs

stop_acc <- -(ffs ^ 2 / (2 * dist))
stop_time <- 8.571

discharge_acc <- 3.5
discharge_time <- sqrt(2 * dist / discharge_acc) + 2 # 2 sec for start-up lost time
discharge_speed <- ffs / 2

data <- dat[QS != "FF", ]
i <- nrow(data)
Tc <- data$TimeStamp[i]
Tg <- data$GreenStart[i]
Tc_Tg <- as.numeric(Tc + data$ODT[i] - Tg)

temp <- Tc_Tg + ff_time_stop_bar

floor(temp / sat_headway)


# results
qDT_Ca <- data.table(
    Cycle = as.factor(c(2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 18, 19, 20, 21, 22, 23, 25, 26, 27, 28)),
    Queue_Cal = c(6, 10, 10, 6, 10, 4, 4, 4, 7, 7, 13, 11, 9, 9, 9, 13, 4, 10, 9, 11, 6),
    Queue_Obs = c(6, 9, 9, 6, 7, 2, 4, 4, 6, 8, 9, 10, 11, 12, 10, 13, 4, 10, 9, 12, 6)
)

round(mean(abs(qDT_Ca$Queue_Obs - qDT_Ca$Queue_Cal) * 25), 2L)
round(mean(abs(qDT_Ca$Queue_Obs - qDT_Ca$Queue_Cal) / qDT_Ca$Queue_Obs) * 100, 2L)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Speedway & Mountain ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (k in 1:19) {
    qDT[[k]] <- getQS(listDT[[k]])
}

qDT[[20]] <- getQS(listDT[[20]])

for (k in 21:25) {
    qDT[[k]] <- getQS(listDT[[k]])
}

qDT[[26]] <- getQS(listDT[[26]])

for (k in 27:34) {
    qDT[[k]] <- getQS(listDT[[k]])
}

# queue length estimation parameters

ffs <- 35 * 5280 / 3600
dist <- 220

ff_time_stop_bar <- dist / ffs

stop_acc <- -(ffs ^ 2 / (2 * dist))
stop_time <- 8.571

discharge_acc <- 3.5
discharge_time <- sqrt(2 * dist / discharge_acc) + 2 # 2 sec for start-up lost time
discharge_speed <- ffs / 2

data <- qDT[[9]][QS != "FF", ]
i <- nrow(data)
Tc <- data$TimeStamp[i]
Tg <- data$GreenStart[i]
Tc_Tg <- as.numeric(Tc + data$ODT[i] - Tg)

temp <- Tc_Tg + ff_time_stop_bar - 2

floor(temp / 2.13)

# results
qDT_Mo <- data.table(
    Cycle = as.factor(c(1, 3, 9, 20, 21, 22, 25, 26, 33, 34)),
    Queue_Obs = c(13, 12, 10, 15, 9, 10, 10, 11, 15, 12),
    Queue_Cal = c(13, 15, 7, 15, 10, 14, 10, 9, 11, 12)
)

round(mean(abs(qDT_Mo$Queue_Obs - qDT_Mo$Queue_Cal) * 25), 2L)
round(mean(abs(qDT_Mo$Queue_Obs - qDT_Mo$Queue_Cal) / qDT_Mo$Queue_Obs) * 100, 2L)

qDT_Mo <- melt(qDT_Mo, id.vars = "Cycle", measure.vars = c("Queue_Obs", "Queue_Cal"))

ggplot(qDT_Mo) + 
    geom_point(aes(Cycle, Queue_Obs), color = "blue", )

ggplot(queue_mountain) + 
    stat_identity(aes(Cycle, value, fill = variable), color = "black", geom = "bar", position = "dodge") 
labs(title = "(c) Speedway Blvd & Mountain Ave",
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
