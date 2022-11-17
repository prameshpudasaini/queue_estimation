library(RODBC)
library(data.table)
library(plotly)
library(patchwork)

source("ignore/keys.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import Data ------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_device <- "[Pramesh].[dbo].[Event_DeviceID]"
db_comm <- "[Maxview_Data_2017].[dbo].[ASC_Comm_Events_06-25-2018]"
db_phase <- "[Maxview_Data_2017].[dbo].[ASC_PhasePed_Events_06-25-2018]"
db_det <- "[Maxview_Data_2017].[dbo].[ASC_Det_Events_06-25-2018]"

st_major <- "Speedway"
st_minor <- "Campbell"

# Speedway & Campbell: Monday, 2018-06-25, 7 am to 8 am
# Speedway & Mountain: Thursday, 2018-06-28, 4 pm to 5 pm

query_phase <- paste0(
    "SELECT * FROM ", db_phase,
    " WHERE TimeStamp > '2018-06-25 06:58:00' AND TimeStamp < '2018-06-25 08:03:00' AND Parameter = 2 AND EventId IN (1, 7, 8, 9, 10, 11) AND",
    " DeviceId IN (SELECT ID FROM ", db_device,
    " WHERE Name LIKE '%", st_major, "%' AND Name LIKE '%", st_minor, "%')"
)
query_det <- paste0(
    "SELECT * FROM ", db_det,
    " WHERE TimeStamp > '2018-06-25 06:58:00' AND TimeStamp < '2018-06-25 08:03:00' AND Parameter = 2 AND EventId IN (81, 82) AND",
    " DeviceId IN (SELECT ID FROM ", db_device,
    " WHERE Name LIKE '%", st_major, "%' AND Name LIKE '%", st_minor, "%')"
)

options(digits.secs = 3L)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process Events --------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# signal phase change events
phase <- sqlQuery(getSQLConnection("STL4"), query_phase)
phase$DeviceId <- NULL
phase$Parameter <- NULL
seq_phase_event <- c(7, 8, 9, 10, 11, 1)
phase <- as.data.table(phase)[order(TimeStamp)]
phase <- phase[phase[, rep(which(do.call(paste0, shift(EventId, 1:6, type = "lead")) == 78910111), each = 6) + 1:6]]

minCycleTime <- phase$TimeStamp[min(which(phase$EventId == 7L))]
maxCycleTime <- phase$TimeStamp[max(which(phase$EventId == 7L))]

phase <- phase[between(TimeStamp, minCycleTime, maxCycleTime), ]
phase <- phase[EventId %in% c(8L, 10L, 1L), ]

yellowStartTime <- phase$TimeStamp[phase$EventId == 8L]
redStartTime <- phase$TimeStamp[phase$EventId == 10L]
greenStartTime <- phase$TimeStamp[phase$EventId == 1L]

Cycle <- seq(1L, length(yellowStartTime))
CL <- round(as.numeric(difftime(shift(yellowStartTime, type = "lead"), yellowStartTime, units = "secs")), 3L)
GreenTime <- round(as.numeric(difftime(greenStartTime, head(yellowStartTime, -1L))), 3L)
GreenTime <- append(GreenTime, NA)


# detector actuation events
det <- sqlQuery(getSQLConnection("STL4"), query_det)
det$DeviceId <- NULL
det$Parameter <- NULL
det <- as.data.table(det)[TimeStamp >= minCycleTime & TimeStamp <= maxCycleTime, ][order(TimeStamp)]
det <- det[min(which(EventId == 82L)):max(which(EventId == 81L)), ]

detOn <- det$TimeStamp[det$EventId == 82L]
detOff <- det$TimeStamp[det$EventId == 81L]

Headway <- round(as.numeric(difftime(shift(detOn, type = "lead"), detOn, units = "secs")), 3L)
ODT <- round(as.numeric(difftime(detOff, detOn, units = "secs")), 3L)
Gap <- round(as.numeric(difftime(shift(detOn, type = "lead"), detOff, units = "secs")), 3L)

check_OHG <- data.table(ODT = ODT, Headway = Headway, Gap = Gap)[, Check := round(Headway - ODT - Gap, 1L)]


# merge two events data sets
DT <- rbindlist(list(phase, det))[EventId != 7L, ][order(TimeStamp)]

DT[, Signal := fifelse(EventId %in% c(8L, 10L, 1L), EventId, as.integer(NA))]
DT <- DT[, setnafill(.SD, type = "locf", cols = "Signal")]
DT[, Signal := factor(as.factor(fcase(Signal == 1L, "G", 
                                      Signal == 8L, "Y",
                                      Signal == 10L, "R")), levels = c("G", "Y", "R"))]

DT$Cycle[DT$EventId == 8L] <- Cycle
DT$CL[DT$EventId == 8L] <- CL

DT$YellowStart[DT$EventId == 8L] <- yellowStartTime
DT$RedStart[DT$EventId == 8L] <- append(redStartTime, NA)
DT$GreenStart[DT$EventId == 8L] <- append(greenStartTime, NA)

DT$GreenTime[DT$EventId == 8L] <- GreenTime
DT <- DT[, setnafill(.SD, type = "locf", cols = c("Cycle", "CL", "GreenStart", "YellowStart", "RedStart", "GreenTime"))]

DT[, YellowStart := as.POSIXct(YellowStart, origin = "1970-01-01")]
DT[, RedStart := as.POSIXct(RedStart, origin = "1970-01-01")]
DT[, GreenStart := as.POSIXct(GreenStart, origin = "1970-01-01")]
DT[, AIC := as.numeric(TimeStamp - YellowStart)]
DT[, TUG := round(as.numeric(GreenStart - TimeStamp), 3L)]

SSC82 <- as.character(DT$Signal[DT$EventId == 82L])
SSC81 <- as.character(DT$Signal[DT$EventId == 81L])
length(SSC82) == length(SSC81)
SSC <- paste0(SSC82, SSC81)
DT$SSC[DT$EventId == 82L] <- SSC

DT$Headway[DT$EventId == 82L] <- Headway
DT$ODT[DT$EventId == 82L] <- ODT
DT$Gap[DT$EventId == 82L] <- Gap

DT <- DT[EventId == 82L, ][order(TimeStamp)]

unique(DT$SSC)
DT[, SSC := gsub("GY", "GG", SSC)]
DT[, SSC := gsub("YY", "RR", SSC)]
DT[, SSC := gsub("YR", "RR", SSC)]
DT[, SSC := factor(as.factor(SSC), levels = c("RR", "RG", "GG"))]
levels(DT$SSC)

signal_color <- c("red", "black", "forestgreen")

plot_ly(DT, type = "scatter", x = ~ODT, y = ~Gap, color = ~SSC,
        mode = "markers", colors = signal_color, marker = list(size = 5))
