packages <- c("MiMeMo.tools", "stars", "ggplot2", "sf", "viridis") # List packages
lapply(packages, library, character.only = TRUE) # Load packages

rgx_separate_year_month <- function(name) {
    if (!str_detect(name, "(\\d+)(\\_)(\\d+)") && (str_detect(name, "(\\d+)"))) {
        start <- str_split(name, "(\\d{5})")[[1]][1]
        end <- str_split(name, "(\\d{4})")[[1]][2]
        rgx <- paste0("(?<=[", start, "])(\\d{4})(?=[", end, "])")

        middle <- str_match(name, rgx)[1]
        end <- ifelse(nchar(end) == 1, paste0("0", end), end)

        name <- paste0(start, middle, "_", end)

        return(name)
    } else {
        return(name)
    }
}
