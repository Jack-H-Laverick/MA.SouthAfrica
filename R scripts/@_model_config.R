start_year <- 2010
end_year <- 2019

domain_name <- "SOUTHERN_BENGUELA"

strathe2e_gear_types <- c(
    "MWT" = "midwater trawl",
    "NTS" = "nets including small scale",
    "TPL" = "pole and line",
    "SJ" = "squid jig",
    "LL" = "longline",
    "PS" = "purse seine",
    "DMT" = "demersal trawl",
    "WCRLT" = "WC Rock Lobster traps",
    "RFG" = "recreational fishing gear",
    "SSL" = "small scale lines",
    "SBF" = "subsistence fishing gear"
)
gear_codes <- names(strathe2e_gear_types)
strathe2e_guilds <- c(
    "PF" = "Planktivore",
    "DF" = "Demersal",
    "MF" = "Migratory",
    "FDB" = "Benthos filter/deposit feeder",
    "CSB" = "Benthos carnivore/scavenge feeder",
    "CZ" = "Zooplankton carnivore",
    "BD" = "Birds",
    "SL" = "Pinnipeds",
    "CT" = "Cetacean",
    "KP" = "Macrophyte"
)
guild_codes <- names(strathe2e_guilds)

# Table of nitrogen per unit wet weight - from Table 18 of SE2E North Sea implementation
mMNpergWW <- c(
    PF = 2.038, DF = 1.340, MF = 2.314, FDB = 0.503,
    CSB = 1.006, CZ = 1.258, BD = 2.518, SL = 2.518,
    CT = 2.518, KP = 2.070
)
