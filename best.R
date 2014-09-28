match_outcome <- function(string, strings) {
        string <- tolower(gsub(" ", ".", string))
        
        for (i in 1:length(strings)) {
                if (grepl(string, tolower(strings[i]))) {
                        return(strings[i])
                }
        }
}

best <- function(state, outcome) {
        ## read outcome date
        data <- read.csv("outcome-of-care-measures.csv", 
                         na.string="Not Available")
        names <- names(data)

        ## check that state and outcome are valid
        matched <- match_outcome(outcome, names)
        if (is.null(matched)) {
                stop("invalid outcome")
        }
        states <- unique(data[, "State"])
        if (!is.element(state, states)) {
                stop("invalid state")
        }
        ## return the hospital name in the state with the lowest 30 day-death 
        ## rate
        data_subset <- subset(data, data$State==state)
        ordered <- data_subset[order(data_subset[[matched]]), ]
        return(as.character(ordered[1:1, "Hospital.Name"]))
}