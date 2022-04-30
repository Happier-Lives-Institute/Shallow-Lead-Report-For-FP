source("_dependencies/dependencies.R")

d.all <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1OszN1ZltU3nPZYBskJuhuPzhh-Zn4gg7yvYRKOR_SSs/",
    sheet = "BLL<>SWB"
)

d.childlag <- d.all %>% filter(analysis == "childhood-lag")
d.childlag <- d.childlag %>%
    rowwise() %>%
    mutate(
        d = d/Dosage
    ) %>%
    mutate(
    n.c = `sample size`/2,
    n.e = `sample size`/2,
    d_se = getDSE(d, n.e, n.c)
) %>% ungroup()

# Model with heterogeneity between the studies
mod.1 <- rma.mv(yi = d, V = d_se^2,
       slab = cohort,
       random =  ~1 | cohort,
       # mods = ~ `FU from lead measure in years`,
       test = "t", method="REML",
       data = d.childlag)

effect <- coef(mod.1)[[1]]; effect
linear.growth <- abs(effect/(27-3)); linear.growth
effectAt30 <- effect + 3 * linear.growth; effectAt30
effectBy30 <- pracma::integral(function(t){0+linear.growth*t}, 3, 30); effectBy30
