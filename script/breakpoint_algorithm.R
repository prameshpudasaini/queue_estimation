library(data.table)

DT <- fread("ignore/20180625_0708_Campbell.txt")
# DT <- fread("ignore/20180628_1617_Mountain.txt")

# Breakpoint identification ----------------------------------------------------

DT[, CumODT := cumsum(ODT), by = Cycle]
DT[, CumODT := shift(CumODT, type = "lag"), by = Cycle]

listDT <- split(DT, by = "Cycle")

crit_gap <- 1.5

getQS <- function(data) {
    len_data <- vector("character", nrow(data))
    j <- 1L
    if (data$SSC[j] == "GG") {
        for (i in j:nrow(data)) {
            len_data[i] <- "FF"
        }
    } else {
        while (data$SSC[j] %in% c("YY", "YR", "RR")) {
            len_data[j] <- "QBS"
            j <- j + 1L
        }
        if (data$SSC[j] == "GG") {
            if (data$Gap[j-1L] <= crit_gap) {
                if (data$Gap[j] > crit_gap) {
                    len_data[j] <- "QBS"
                    j <- j + 1L
                    for (i in j:nrow(data)) {
                        len_data[i] <- "FF"
                    }
                } else {
                    while (data$Gap[j] <= crit_gap) {
                        len_data[j] <- "QBS"
                        j <- j + 1L
                    }
                    for (i in j:nrow(data)) {
                        len_data[i] <- "FF"
                    }
                }
            } else {
                for (i in j:nrow(data)) {
                    len_data[i] <- "FF"
                }
            }
        } else if (data$SSC[j] == "RG") {
            if (data$TUG[j] <= 2) {
                len_data[j] <- "QBS"
                j <- j + 1L
                for (i in j:nrow(data)) {
                    len_data[i] <- "FF"
                }
            } else {
                len_data[j] <- "QOD"
                if (data$Gap[j] > crit_gap) {
                    j <- j + 1L
                    for (i in j:nrow(data)) {
                        len_data[i] <- "FF"
                    }
                } else {
                    j <- j + 1L
                    while (data$Gap[j] <= crit_gap) {
                        len_data[j] <- "QBD"
                        j <- j + 1L
                        if (j == nrow(data)) {
                            len_data[j] <- "QBD"
                            data$QS <- len_data
                            return(data)
                            break
                        }
                    }
                    len_data[j] <- "QBD"
                    if (j == nrow(data)) {
                        next
                    } else {
                        j <- j + 1L
                        for (i in j:nrow(data)) {
                            len_data[i] <- "FF"
                        }
                    }
                }
            }
        }
    }
    data$QS <- len_data
    return(data)
}

qDT <- vector("list", length(listDT))
