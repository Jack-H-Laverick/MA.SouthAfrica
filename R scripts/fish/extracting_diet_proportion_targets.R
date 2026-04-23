Sys.setenv(JAVA_HOME = "C:/Program Files/Microsoft/jdk-21.0.7.6-hotspot")
ackages <- c("ggplot2", "arrow", "tidyverse", "glue", "tabulapdf", "gganimate", "randomcoloR")
sapply(packages, library, character.only = TRUE)

extract_numb_spec_names <- function(species_numb_names, numbers) {
    species_names <- species_numb_names[match(numbers, species_numb_names[, 1]), 2]
    return(species_names)
}

supp_pdf_filename <- "../../Fishing Data/Shannon_2020_supplementary.pdf"

# Table page 12 to 13
table_page_1 <- extract_tables(supp_pdf_filename, output = "tibble", pages = c(12))[[1]]
table_page_1 <- as.data.frame(table_page_1)

table_page_2 <- extract_tables(supp_pdf_filename, output = "tibble", pages = c(13))[[1]]
table_page_2 <- rbind(names(table_page_2), table_page_2)
names(table_page_2) <- names(table_page_1)
table_page_2 <- as.data.frame(table_page_2)

table_1_predators <- extract_numb_spec_names(rbind(table_page_1[, 1:2], table_page_2[, 1:2]), names(table_page_1[, 3:ncol(table_page_1)]))
if (table_1_predators[1] != "Microzooplankton" | table_1_predators[length(table_1_predators)] != "Chokka Squid") {
    stop("Error, wrong predator names taken.")
}

names(table_page_1) <- c("Num", "Prey", table_1_predators)
names(table_page_2) <- c("Num", "Prey", table_1_predators)
prey_predator_table_1 <- rbind(table_page_1, table_page_2)


# Table page 14 to 15
table_page_3 <- extract_tables(supp_pdf_filename, output = "tibble", pages = c(14))[[1]]
table_page_3 <- as.data.frame(table_page_3)

table_page_4 <- extract_tables(supp_pdf_filename, output = "tibble", pages = c(15))[[1]]
table_page_4 <- rbind(names(table_page_4), table_page_4)
names(table_page_4) <- names(table_page_3)
table_page_4 <- as.data.frame(table_page_4)

table_2_predators <- extract_numb_spec_names(rbind(table_page_3[, 1:2], table_page_4[, 1:2]), names(table_page_3[, 3:ncol(table_page_3)]))
if (table_2_predators[1] != "Other cephalopods" | table_2_predators[length(table_2_predators)] != "SC rock lobster") {
    stop("Error, wrong predator names taken.")
}

names(table_page_3) <- c("Num", "Prey", table_2_predators)
names(table_page_4) <- c("Num", "Prey", table_2_predators)
prey_predator_table_2 <- rbind(table_page_3, table_page_4)


for (r in seq_len(nrow(prey_predator_table_1))) {
    if (all(is.na(prey_predator_table_1[r, 3:ncol(prey_predator_table_1)]))) {
        prey_predator_table_1[r - 1, "Prey"] <- paste(prey_predator_table_1[r - 1, "Prey"], prey_predator_table_1[r, "Prey"], sep = " ")
        prey_predator_table_1 <- prey_predator_table_1[-r, ]
    }
}
prey_predator_table_1$Prey <- prey_predator_table_2$Prey

prey_consump_table <- left_join(prey_predator_table_1, prey_predator_table_2, by = c("Num", "Prey"))

ewe_match <- read_xlsx("../../Fishing Data/Strath_EwE_match.xlsx")
prey_consump_table$strath_guild <- ewe_match$StrathE2E_group_name[match(prey_consump_table$Prey, ewe_match$EwE_group_name)]
prey_consump_table[c(6, 19, 35, 37), "strath_guild"] <- c("Carnivorous zooplankton", "Migratory fish", "Demersal fish", "Demersal fish")
prey_consump_table <- prey_consump_table[-nrow(prey_consump_table), ]
prey_consump_table[prey_consump_table$Prey == "Cape Cormorant", "Apex\rChondrichthyans"] <- 0.0001

for (c in names(prey_consump_table[, 3:(ncol(prey_consump_table) - 1)])) {
    for (r in seq_len(nrow(prey_consump_table))) {
        if (str_detect(prey_consump_table[r, c], "(\\.{3})")) {
            prey_consump_table[r, c] <- gsub(prey_consump_table[r, c], pattern = "\\.{3}.*", replacement = "")
        }
    }
    prey_consump_table[, c] <- as.numeric(prey_consump_table[, c])
}

predators <- names(prey_consump_table[, 3:(ncol(prey_consump_table) - 1)])

diet_comp <- expand.grid(unique(prey_consump_table$strath_guild), predators, stringsAsFactors = FALSE)
names(diet_comp) <- c("strath_guild", "ewe_predator")
diet_comp$diet_proportion <- 0

for (pred in unique(diet_comp$ewe_predator)) {
    predator_diet <- prey_consump_table[, c("Prey", "strath_guild", pred)] %>%
        group_by(strath_guild) %>%
        summarise(diet_proportion = sum(.data[[pred]]))
    predator_diet$ewe_predator <- pred
    diet_comp <- rows_update(diet_comp, predator_diet, by = c("strath_guild", "ewe_predator"))
}

diet_comp$sd <- 0.75 * diet_comp$diet_proportion

seals <- diet_comp[diet_comp$ewe_predator == "Seals", ]
cetaceans <- diet_comp[diet_comp$ewe_predator == "Cetaceans", ]
