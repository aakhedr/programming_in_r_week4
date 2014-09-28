match_outcome <- function(string, strings) {
        string <- tolower(gsub(" ", ".", string))
        
        for (i in 1:length(strings)) {
                if (grepl(string, tolower(strings[i]))) {
                        return(strings[i])
                }
        }
}

rankall <- function(outcome, num="best") {
        ## read outcome date
        data <- read.csv("outcome-of-care-measures.csv", 
                         na.string="Not Available")
        names <- names(data)
        
        ## check that state and outcome are valid
        matched <- match_outcome(outcome, names)
        if (is.null(matched)) {
                stop("invalid outcome")
        }
        ## return the hospital name in the state with the lowest 30 day-death 
        ## rate
        ordered <- data[order(data[["State"]], data[[matched]], 
                              data[["Hospital.Name"]]), ]
        states <- as.character(unique(ordered[, "State"]))
        ordered[["Rank"]] <- 0
        k <- 1          ## index into the 4706 observations
        for (i in 1:length(states)) {
                ordered_subset <- subset(ordered, ordered$State==states[i])
                for (j in 1:nrow(ordered_subset)) {
                        ordered[k, "Rank"] <- j
                        k <- k + 1
                }
        }
        if (num == "best") {
                hospitals <- as.character(ordered[ordered$Rank==1, 
                                                  "Hospital.Name"])
                return(data.frame(hospital=hospitals, state=states, 
                                  row.names=states))
        } else if (num == "worst") {
                hospitals <- character()
                for (i in 1:length(states)) {
                        ordered_subset <- subset(ordered, ordered$State==
                                                         states[i])
                        num <- nrow(ordered_subset)
                        while (is.na(ordered_subset[num, matched])) {
                                num <- num - 1
                        }
                        hospital <- as.character(ordered[ordered$Rank==num & 
                                        ordered$State==states[i], 
                                        "Hospital.Name"])
                        hospitals <- c(hospitals, hospital)
                }
                return(data.frame(hospital=hospitals, state=states, 
                                  row.names=states))
        } else {
                hospitals <- character()
                for (i in 1:length(states)) {
                        hospital <- as.character(ordered[ordered$Rank==num & 
                                        ordered$State==states[i], 
                                        "Hospital.Name"])
                        if (length(hospital)==0) {
                                hospital <- "<NA>"
                        }
                        hospitals <- c(hospitals, hospital)
                }
                return(data.frame(hospital=hospitals, state=states, 
                                  row.names=states))
        }
}