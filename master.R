source("_dependencies/dependencies.R")

d <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1OszN1ZltU3nPZYBskJuhuPzhh-Zn4gg7yvYRKOR_SSs/",
    sheet = "BLL<>SWB time lagged"
)

d <- d %>% filter(Citation != "Winter & Sampson, 2017")
d <- d %>% rowwise() %>%
    mutate(
        d = d/Dosage
    ) %>%
    mutate(
    n.c = n/2,
    n.e = n/2,
    d_se = getDSE(d, n.e, n.c)
) %>% ungroup()

# Model with heterogeneity between the studies
mod.1 <- rma.mv(yi = d, V = d_se^2,
       slab = cohort,
       random =  ~1 | cohort,
       # mods = ~ `FU from lead measure in years`,
       test = "t", method="REML",
       data = d)

effect <- coef(mod.1)[[1]]; effect
linear.growth <- abs(effect/27); linear.growth
effectBy30 <- pracma::integral(function(t){0+linear.growth*t}, 0, 30); effectBy30
