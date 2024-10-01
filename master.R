#~############################################################################~#
# Preparations ----
#~############################################################################~#

# Load dependencies
source("dependencies/dependencies.R")

# Download the data
dat_lead <- read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1nYo06qvcfvmXwaygPLCF0_KQodGhtmGvebs0Pe36lOo/",
    sheet = "BLL<>SWB",
    skip = 1
)

# Wrangle
dat_lead <- dat_lead %>%
    # Make sure we have just the analysis for childhood
    filter(analysis == "childhood-lag") %>%
    # calculate effect size
    mutate(
        study_label_outcome = paste0(study_label, " (", outcome_general, ")"),
        t = m_diff / m_diff_se,
        n_t = n/2,
        n_c = n/2,
        d = t * sqrt((1/n_t) + (1/n_c)),
        d = ifelse(str_detect(higher_better_swb, "^F"), d*-1, d),
        d = d/exposure_dose,
        g = get_g(d, n_t, n_c),
        g_se = get_d_se(d, n_t, n_c),
        g_var = g_se^2,
        ci_lb = g - 1.96*g_se,
        ci_ub = g + 1.96*g_se,
    )

#~############################################################################~#
# Analysis Lead Impact ----
#~############################################################################~#

#~=======================================================~=
## Effect ----
#~=======================================================~=

# Effect size
ma_lead_l3 <- rma.mv(
    data = dat_lead,
    yi = g, V = g_var,
    random = ~1 | study_label/study_label_outcome,
    slab = study_label_outcome,
    method = "REML", test = "t",
); summary(ma_lead_l3)

# Forest plot
forest(ma_lead_l3)

# Funnel plot
my_funnel_rma.mv(ma_lead_l3)

#~=======================================================~=
## Trajectory over time ----
#~=======================================================~=

avg_follow_up_age <- dat_lead %>% pull(age) %>% mean()
avg_follow_up_age; dat_lead %>% pull(age) %>% range()

effect_per_year <- ma_lead_l3$b[1]/avg_follow_up_age
effect_per_year

age_of_development <- 30 # assumption

age_variations <- data.frame(
    age_of_exposure = 0:20,
    age_of_development,
    effect_per_year
) %>% mutate(
    years = age_of_development - age_of_exposure,
    total_effect_at_development = effect_per_year * years
)

spillover_ratio <- 0.1624

#~############################################################################~#
# BLLs & Population ----
#~############################################################################~#

#~=======================================================~=
## Preparing the data ----
#~=======================================================~=

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Age-Years ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare the relationship between the two age classifications

# Create a mapping of age ranges to IHME age groups
map_age_to_group <- function(age) {
    case_when(
        age == 0 ~ "Age 0",  # Age 0 includes these groups
        age == 1 ~ "Age 1",  # Age 1 corresponds to this group
        age %in% 2:4 ~ "2 to 4",              # Ages 2 to 4
        age %in% 5:9 ~ "5 to 9",              # Ages 5 to 9
        age %in% 10:14 ~ "10 to 14",          # Ages 10 to 14
        age %in% 15:19 ~ "15 to 19",          # Ages 15 to 19
        age %in% 20:24 ~ "20 to 24",          # Ages 20 to 24
        age %in% 25:29 ~ "25 to 29",          # Ages 25 to 29
        age %in% 30:34 ~ "30 to 34",          # Ages 30 to 34
        age %in% 35:39 ~ "35 to 39",          # Ages 35 to 39
        age %in% 40:44 ~ "40 to 44",          # Ages 40 to 44
        age %in% 45:49 ~ "45 to 49",          # Ages 45 to 49
        age %in% 50:54 ~ "50 to 54",          # Ages 50 to 54
        age %in% 55:59 ~ "55 to 59",          # Ages 55 to 59
        age %in% 60:64 ~ "60 to 64",          # Ages 60 to 64
        age %in% 65:69 ~ "65 to 69",          # Ages 65 to 69
        age %in% 70:74 ~ "70 to 74",          # Ages 70 to 74
        age %in% 75:79 ~ "75 to 79",          # Ages 75 to 79
        age %in% 80:84 ~ "80 to 84",          # Ages 80 to 84
        age %in% 85:89 ~ "85 to 89",          # Ages 85 to 89
        age %in% 90:94 ~ "90 to 94",          # Ages 90 to 94
        age >= 95 ~ "95 plus",                # Ages 95 and above
        TRUE ~ NA_character_                  # Handle any unexpected ages
    )
}
age_years <- data.frame(age = 0:100)
age_years <- age_years %>%
    mutate(age_group_label = map_age_to_group(age))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### IHME ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global burden of disease lead level

IHME <- read_csv(
    "data/IHME_GBD_2021_LEAD_RISK_1990_2021_BLOOD/IHME_GBD_2021_LEAD_RISK_1990_2021_BLOOD_2011_2021_Y2024M06D05.CSV"
    , show_col_types = F
)
# summary(IHME)
# rename some columns for simplicity
IHME <- IHME %>% rename(
    location = location_name,
    year = year_id
)
# use information that doesn't split between sexes
IHME_both <- IHME %>% filter(sex == "Both")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### World population (UNDP) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WPP <- read_csv(
    "data/UNPD/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv"
    , show_col_types = F
)
# summary(WPP)

WPP <- WPP %>% rename(
    PopTotal_1000s = PopTotal,
    location = Location,
    year = Time
) %>%
    # Put the population in individuals rather than 1000s
    mutate(PopTotal = PopTotal_1000s * 1000)
# unique(WPP$Location)
# unique(WPP$Time)

#~=======================================================~=
## Weighting the BLLs ----
#~=======================================================~=

# Get the population for each age group
all_locations_time <- WPP %>% distinct(location, year)
age_years_all_locations_time <- crossing(
    age_years,  # Your age_years dataframe
    all_locations_time  # Distinct Location and Time values
)
age_years_all_locations_time <- age_years_all_locations_time %>% right_join(
    WPP %>% select(AgeGrpStart, PopTotal, location, year, LocTypeName) %>%
        rename(age = AgeGrpStart),
    by = c("age", "location", "year"))

# Get the BLLs for each time and location
IHME_both <- IHME_both %>%
    mutate(age_group_label = case_when(
        age_group_name %in% c(
            "Early Neonatal", "Late Neonatal", "1-5 months", "6-11 months",
            "<1", "<5", "<20", "0-27 days"
        ) ~ "Age 0",
        age_group_name == "12 to 23 months" ~ "Age 1",
        .default = age_group_name
    )
) %>% group_by(location, year, age_group_label) %>%
    mutate(BLL = mean(mean)) %>%
    ungroup() %>% ungroup() %>% ungroup() %>%
    mutate(row_label = paste(location, year, age_group_label)) %>%
    filter(!duplicated(row_label))

# Combine population and BLLs

# Only use locations that are countries
age_years_all_locations_time <- age_years_all_locations_time %>%
    filter(LocTypeName == "Country/Area") %>%
    # align names
    mutate(
        location = case_when(
            location == "Dem. People's Republic of Korea" ~
                "Democratic People's Republic of Korea",
            location == "China, Taiwan Province of China" ~
                "Taiwan (Province of China)",
            location == "Micronesia (Fed. States of)" ~
                "Micronesia (Federated States of)",
            location == "State of Palestine" ~
                "Palestine",
            .default = location
        )
    )

IHME_both_countries <- IHME_both %>%
    filter(!str_detect(ihme_loc_id, "_"))

# check correspondance in location
IHME_both_countries %>% filter(location %ni% age_years_all_locations_time$location) %>% pull(location) %>% unique()
age_years_all_locations_time %>% filter(location %ni% IHME_both_countries$location) %>% pull(location) %>% unique()

BLLs <- IHME_both_countries %>%
    select(location, year, age_group_label, BLL) %>%
    left_join(age_years_all_locations_time, by = c("location", "year", "age_group_label")) %>%
    arrange(location, year, age) %>% select(-LocTypeName)

BLLs_u10 <- BLLs %>% filter(age <= 10)

BLLs_u10_weighted <- BLLs_u10 %>%
    group_by(location, year) %>%
    summarise(
        BLL = weighted.mean(BLL, PopTotal, na.rm = TRUE)
    ) %>%
    ungroup()

#~############################################################################~#
# Analyses BLLs over time ----
#~############################################################################~#

BLLs_u10_weighted %>%
    ggplot(aes(x = year, y = BLL)) +
    geom_point(alpha = 0.7, aes(color = location)) +
    geom_smooth() +
    labs(
        x = "Year",
        y = "Blood Lead Level (µg/dL)",
        title = "Blood Lead Levels in Children Under 10",
        subtitle = "Weighted by Population"
    ) +
    coord_cartesian(
        xlim = c(2011, 2021),
        ylim = c(0, max(BLLs_u10_weighted$BLL, na.rm = TRUE)+1)
    ) +
    scale_x_continuous(breaks = seq(2011, 2021, 1)) +
    theme_hli_wbg() +
    theme(legend.position = "none")

# map of the world

# modelling
lmer(BLL ~ year + (1|location), data = BLLs_u10_weighted)

#~############################################################################~#
# Ban on leaded fuel ----
#~############################################################################~#

lead_ban <- read_csv(
    "data/OWID/lead-petrol-ban.csv"
    , show_col_types = F
)

lead_ban <- lead_ban %>% filter(lead_petrol_banned == "Yes") %>%
    group_by(Entity) %>% mutate(min_year = min(Year)) %>% ungroup() %>%
    filter(Year == min_year)

lead_ban %>% filter(Entity %in% c("Ghana", "Bangladesh", "India"))

#~############################################################################~#
# Ghana ----
#~############################################################################~#

#~=======================================================~=
## Average age for <= 10 y.o. ----
#~=======================================================~=

WPP_2023_Ghana <- WPP %>% filter(year == 2023) %>% filter(location == "Ghana")
# sum(WPP_2023_Ghana$PopTotal) # check total population
WPP_2023_Ghana %>% filter(AgeGrpStart <= 10) %>% summarise(
    age = weighted.mean(AgeGrpStart, PopTotal, na.rm = TRUE),
    total = sum(PopTotal)
)

#~=======================================================~=
## BLLs ----
#~=======================================================~=

BLLs_u10_weighted_Ghana <- BLLs_u10_weighted %>% filter(location == "Ghana") %>%
    mutate(year_maxed = year - min(year), year_maxed_1 = year - min(year) + 1)
BLLs_u10_weighted_Ghana %>% filter(year == 2021) %>% pull(BLL)
# BLLs_u10_Ghana <- BLLs_u10 %>% filter(location == "Ghana")
# BLLs_u10_Ghana_2021 <- BLLs_u10_Ghana %>% filter(year == 2021)

BLLs_u10_weighted_Ghana %>%
    ggplot(aes(x = year, y = BLL)) +
    geom_point(alpha = 0.7) +
    geom_smooth() +
    labs(
        x = "Year",
        y = "Blood Lead Level (µg/dL)",
        title = "Blood Lead Levels in Children Under 10 in Ghana",
        subtitle = "Weighted by Population"
    ) +
    coord_cartesian(
        xlim = c(2011, 2021),
        ylim = c(0, max(BLLs_u10_weighted_Ghana$BLL, na.rm = TRUE)+1)
        ) +
    scale_x_continuous(breaks = seq(2011, 2021, 1)) +
    theme_hli_wbg()

BLL_years_Ghana <- lm(BLL ~ year_maxed, data = BLLs_u10_weighted_Ghana)
summary(BLL_years_Ghana)

lm(log(BLL) ~ year_maxed_1, data = BLLs_u10_weighted_Ghana)
exp(-0.02921)

2024-6
BLLs_u10_weighted_Ghana %>% filter(year == 2024-6) %>% pull(BLL)
4.323246 - (4.323246 * exp(-0.02921*6))

#~############################################################################~#
# India ----
#~############################################################################~#

#~=======================================================~=
## Average age for <= 10 y.o. ----
#~=======================================================~=

WPP_2023_India <- WPP %>% filter(year == 2023) %>% filter(location == "India")
# sum(WPP_2023_India$PopTotal) # check total population
WPP_2023_India %>% filter(AgeGrpStart <= 10) %>% summarise(
    age = weighted.mean(AgeGrpStart, PopTotal, na.rm = TRUE),
    total = sum(PopTotal)
)

# Get proportion that is under 10years to apply to the states
india_per_un10 <-
    (WPP_2023_India %>% filter(AgeGrpStart <= 10) %>% summarise(n = sum(PopTotal))) %>% pull(n) /
    (WPP_2023_India %>% summarise(n = sum(PopTotal))) %>% pull(n)

# We also want the specific population of the indian states
# Chhattisgarh and Madhya Pradesh
indian_census_2011 <- readxl::read_xlsx(
    "data/indian census 2011/A-1_NO_OF_VILLAGES_TOWNS_HOUSEHOLDS_POPULATION_AND_AREA.xlsx",
    sheet = "machine_readable_copy"
)
indian_census_2011 %>% filter(area == "STATE") %>%
    filter(name %in% c("CHHATTISGARH", "MADHYA PRADESH")) %>%
    filter(rural_urban_total == "Total") %>%
    select(name, persons) %>%
    mutate(un10 = persons * india_per_un10)

#~=======================================================~=
## BLLs ----
#~=======================================================~=

BLLs_u10_weighted_India <- BLLs_u10_weighted %>% filter(location == "India") %>%
    mutate(year_maxed = year - min(year), year_maxed_1 = year - min(year) + 1)
BLLs_u10_weighted_India %>% filter(year == 2021) %>% pull(BLL)

# actually, what we want is the BLLs for the specific region:
# Chhattisgarh and Madhya Pradesh
# assume the same proportion in ages from India more generally

## Chhattisgarh
BLLs_u10_ind_chh <- IHME_both %>% filter(location == "Chhattisgarh") %>%
    select(location, year, age_group_label, BLL) %>%
    left_join(
        age_years_all_locations_time %>% filter(location == "India"),
        by = c("year", "age_group_label")
        ) %>% select(-LocTypeName) %>%
    filter(age <= 10)
BLLs_u10_weighted_ind_chh <- BLLs_u10_ind_chh %>% group_by(year) %>%
    summarise(
        BLL = weighted.mean(BLL, PopTotal, na.rm = TRUE)
    ) %>%
    ungroup()
BLLs_u10_weighted_ind_chh %>% filter(year == 2021) %>% pull(BLL)

## Madhya Pradesh
BLLs_u10_ind_mp <- IHME_both %>% filter(location == "Madhya Pradesh") %>%
    select(location, year, age_group_label, BLL) %>%
    left_join(
        age_years_all_locations_time %>% filter(location == "India"),
        by = c("year", "age_group_label")
    ) %>% select(-LocTypeName) %>%
    filter(age <= 10)
BLLs_u10_weighted_ind_mp <- BLLs_u10_ind_mp %>% group_by(year) %>%
    summarise(
        BLL = weighted.mean(BLL, PopTotal, na.rm = TRUE)
    ) %>%
    ungroup()
BLLs_u10_weighted_ind_mp %>% filter(year == 2021) %>% pull(BLL)
