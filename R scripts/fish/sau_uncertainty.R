strath_e2e_gear_landings <- arrow::read_parquet("./Objects/sau_landings_strath_gears.parq")

# Seas Around Us uncertainty scores
## For each gear type
strath_e2e_gear_landings$uncertainty_score <- as.factor(strath_e2e_gear_landings$uncertainty_score)
strath_e2e_gear_landings$uncertainty_score <- ifelse(is.na(strath_e2e_gear_landings$uncertainty_score), "NA", strath_e2e_gear_landings$uncertainty_score)
year_gear_total_landings <- strath_e2e_gear_landings %>%
    group_by(year, gear_type_se2e) %>%
    summarise(year_sum_tonnes = sum(tonnes))
sau_landings_uncertain_gear <- strath_e2e_gear_landings %>%
    group_by(year, uncertainty_score, gear_type_se2e) %>%
    summarise(tonnes = sum(tonnes)) %>%
    left_join(., year_gear_total_landings, by = c("year", "gear_type_se2e")) %>%
    mutate(proportion_catch_uncertainty = tonnes / year_sum_tonnes)

ggplot() +
    geom_col(
        data = sau_landings_uncertain_gear,
        aes(x = year, y = proportion_catch_uncertainty, fill = as.factor(uncertainty_score))
    ) +
    facet_wrap(~gear_type_se2e)

## For each Guild and gear type
year_guild_total_landings <- strath_e2e_gear_landings %>%
    group_by(year, Guild) %>%
    summarise(year_sum_tonnes = sum(tonnes))
sau_landings_uncertain_guild <- strath_e2e_gear_landings %>%
    group_by(year, uncertainty_score, Guild) %>%
    summarise(tonnes = sum(tonnes)) %>%
    left_join(., year_guild_total_landings, by = c("year", "Guild")) %>%
    mutate(proportion_catch_uncertainty = tonnes / year_sum_tonnes)

ggplot() +
    geom_col(
        data = sau_landings_uncertain_guild,
        aes(x = year, y = proportion_catch_uncertainty, fill = uncertainty_score)
    ) +
    facet_wrap(~Guild)
