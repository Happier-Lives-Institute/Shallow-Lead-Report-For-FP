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
linear.growth <- abs(effect/(27-2)); linear.growth
effectAt30 <- effect + 3 * linear.growth; effectAt30
effectBy30 <- pracma::integral(function(t){0+linear.growth*t}, 2, 30); effectBy30

figureData <- as.data.frame(c(
    NA, NA,
    sapply(0:28, FUN=function(t){0+linear.growth*t}),
    rep(0.0180, 43)))
colnames(figureData) <- c("SDs of MHa")
figureData$`SDs of MHa` <- -figureData$`SDs of MHa`
figureData$years <- 0:73

p <- ggplot(figureData, aes(y=`SDs of MHa`, x=years)) +
    geom_line() +
    theme_cowplot() +
    scale_y_continuous(limits = c(-0.02, 0)) +
    scale_x_continuous(breaks=seq(0, 75, 5), limits = c(0, 75))
p

ggsave(filename = paste0("causalgraphs/Figure 3_prep.png"),
       plot = p,
       width = 8,
       height = 8,
       dpi = 1200)
