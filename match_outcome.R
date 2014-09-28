match_outcome <- function(string, strings) {
        string <- tolower(gsub(" ", ".", string))
        
        for (i in 1:length(strings)) {
                if (grepl(string, tolower(strings[i]))) {
                        return(strings[i])
                }
        }
}