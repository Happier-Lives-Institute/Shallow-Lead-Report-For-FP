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
