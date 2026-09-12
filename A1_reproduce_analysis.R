######################################################################
#
#  Preparation
#
#######################################################################



# load packages ----------------------------------------------------------

library(duckdb)
library(dplyr)
library(arrow)
library(tidyverse)
library(stargazer)
library(sjPlot)
library(patchwork)
library(xtable)
options(xtable.floating = FALSE, xtable.include.rownames = FALSE)
library(lme4)
library(glue)
library(marginaleffects)
library(modelsummary)
options(modelsummary_output = "latex_tabular")
library(tinytable)
options(tinytable_latex_environment_table = FALSE)


# duckdb config ----------------------------------------------------------



dir.create("tmp", recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(duckdb("tmp/convert.duckdb"))

# set memory usage to 4GB max so it doesn't break
dbExecute(con, "SET memory_limit = '2GB'")
dbExecute(con, "SET max_temp_directory_size = '100GB'")

# load data --------------------------------------------------------------


if(run_on_sample == TRUE) {
  # import sample 
  contributors <- open_dataset("data/analysis/processed_contributors_parquet_sample", format = "parquet") |> 
    to_duckdb(con, "processed_contributors_parquet_sample")

  contributions <- open_dataset("data/analysis/processed_contributions_parquet_sample", format = "parquet") |> 
    to_duckdb(con, "processed_contributions_parquet_sample")

} else if(run_on_sample == FALSE) {
  # import full dataset
  contributors <- open_dataset("data/analysis/processed_contributors_parquet", format = "parquet") |> 
    to_duckdb(con, "processed_contributors_parquet")

  contributions <- open_dataset("data/analysis/processed_contributions_parquet", format = "parquet") |> 
    to_duckdb(con, "processed_contributions_parquet")
} else {
  stop("Please specifiy whether you want to run this script on a sample or on the full dataset by setting the flag run_on_sample =")
}

# messages for logs

messages <- c()

# create results directory

dir.create("results/", recursive = TRUE)


#########
# compute modelling dataset and pull into R
#########

# CONTRIBUTORS: construct a common estimation sample (if not already done so in A3) for contributors

contributors |> count()

contributors <- contributors |> 
  collect()

variables_ces_contributors <- c(
  "contributor.cfscore", "occupation_std", "mean_cfscore_zipcode_wo_i", "ff49_abbr", 
  "contributor.gender", "is_sec_insider"
)

contributors |>
  mutate(across(all_of(variables_ces_contributors),
                ~ is.na(.x),
                .names = "NA_{.col}")) |>
  count(across(starts_with("NA_")), sort = TRUE)

contributors_ces <- contributors |>
    filter(if_all(all_of(variables_ces_contributors), ~ !is.na(.x)))

omitted_rows <- (contributors |> count() |> pull(n)) - (contributors_ces |> count() |> pull(n))

omitted_rows

contributors_ces |> count()

message("The number of total omitted rows in the common estimation sample of contributors is: ", omitted_rows, " of ", contributors |> count() |> pull(n), " which represents ", 100 * omitted_rows / contributors |> count() |> pull(n), "% of the observations in the dataset")
messages[1] <- glue("The number of total omitted rows in the common estimation sample of contributors is: ", omitted_rows, " of ", contributors |> count() |> pull(n), " which represents ", 100 * omitted_rows / contributors |> count() |> pull(n), "% of the observations in the dataset")
  
# relevel occupation_std and standardize cfscores

contributors_ces <- contributors_ces |> 
  mutate(
    occupation_std = relevel(factor(occupation_std), ref = 'manager'),
    contributor.cfscore_std = (contributor.cfscore - mean(contributor.cfscore)) / sd(contributor.cfscore),
    contributor.gender = relevel(factor(contributor.gender), ref = "M"),
    ff49_is_tech = factor(case_when(
            ff49_abbr %in% c("Softw", "Hardw", "Chips") ~ TRUE, 
            .default = FALSE
        ), labels = c("Non-tech", "Tech")),
    ff49_abbr = relevel(factor(ff49_abbr), ref = "Chips")
  )

# CONTRIBUTIONS: construct a common estimation sample (if not already done so in A3) for contributors

contributions <- contributions |> 
  collect()

variables_ces_contributions <- c(
  "cfscore_dyn_cycle", "contributor.cfscore", "occupation_std", "mean_dyn_cfscore_zipcode_cycle_wo_i", 
  "ff49_abbr", "contributor.gender", "is_sec_insider"
)

contributions |>
  mutate(across(all_of(variables_ces_contributions),
                ~ is.na(.x),
                .names = "NA_{.col}")) |>
  count(across(starts_with("NA_")), sort = TRUE)

contributions_ces <- contributions |>
  filter(if_all(all_of(variables_ces_contributions), ~ !is.na(.x)),
         if_all(all_of(variables_ces_contributions) & where(is.character), ~ .x != ""))

omitted_rows <- (contributions |> count() |> pull(n)) - (contributions_ces |> count() |> pull(n))

omitted_rows

contributions_ces |> count()

message("The number of total omitted rows in the common estimation sample of contributions is: ", omitted_rows, " of ", contributions |> count() |> pull(n), " which represents ", 100 * omitted_rows / contributions |> count() |> pull(n), "% of the observations in the dataset")
messages[2] <- glue("The number of total omitted rows in the common estimation sample of contributions is: ", omitted_rows, " of ", contributions |> count() |> pull(n), " which represents ", 100 * omitted_rows / contributions |> count() |> pull(n), "% of the observations in the dataset")

messages |> writeLines("results/omitted_rows.txt")

# test for outlier

# how many outlier?
contributions_ces  |> 
  filter(-5 > cfscore_dyn_cycle | cfscore_dyn_cycle  > 5)  |> 
  count()

contributions_ces  |> 
  filter(-5 > cfscore_dyn_cycle | cfscore_dyn_cycle  > 5) |>
  select(cfscore_dyn_cycle) |> 
  print(n = 100) # why does this come out?

contributions_ces <- contributions_ces  |> 
  filter(-5 < cfscore_dyn_cycle, cfscore_dyn_cycle  < 5) 


contributions_ces  |> ggplot(aes(cfscore_dyn_cycle)) + geom_density()

# standardize the cfscore_dyn_cycle in contributions ces

contributions_ces <- contributions_ces |> 
  mutate(
    cfscore_dyn_cycle_std = (cfscore_dyn_cycle - mean(cfscore_dyn_cycle)) / sd(cfscore_dyn_cycle)
  )

# load contributions_ces into memory

contributions_ces <- contributions_ces |>
  collect()

# relevel cycle, occupation_std and contributor.employer.matched

contributions_ces <- contributions_ces |> 
  mutate(
    cycle = relevel(factor(cycle), ref = '2016'),
    occupation_std = relevel(factor(occupation_std), ref = "manager"),
    contributor.gender = relevel(factor(contributor.gender), ref = "M"),
    ff49_is_tech = factor(case_when(
        ff49_abbr %in% c("Softw", "Hardw", "Chips") ~ TRUE, 
        .default = FALSE
      ), labels = c("Non-tech", "Tech")),
    ff49_abbr = factor(ff49_abbr)
  )

###########################################################################
#
# Validation
#
###########################################################################

# Dynamic cfscores -------------------------------------------------------

#' Show whether the total cfscores differ much from the dynamic ones, make
#' a descriptive timeline graphic of where they do and when - do you see a trend in 
#' polarization?

tbl_mean_cfscore_total <- contributions_ces |> 
  summarise(mean_cfscore = mean(contributor.cfscore, na.rm = TRUE)) |> 
  mutate(cycle = "All") |> 
  select(mean_cfscore, cycle)


tbl_mean_cfscores <- contributions_ces |> 
  group_by(cycle) |> 
  summarise(
    mean_cfscore = mean(cfscore_dyn_cycle, na.rm = TRUE),
  ) |>
  mutate(
    cycle = as.character(cycle)
  ) |> 
  select(mean_cfscore, cycle) |> 
  union(tbl_mean_cfscore_total)

print.xtable(xtable(
        tbl_mean_cfscores,
        caption = "Mean cfscores per cycle and total in the estimation sample",
    ),
    type = "latex",
    file = "results/tbl_mean_cfscores.tex"
)

p_density_cfscores_cycle_std <- contributions_ces |> 
  ggplot(aes(cfscore_dyn_cycle_std)) +
  geom_density() +
  facet_wrap(vars(cycle))

p_density_cfscores_total_std <- contributors_ces |> 
  ggplot(aes(contributor.cfscore_std)) +
  geom_density()

p_density_cfscores_cycle <- contributions_ces |> 
  ggplot(aes(cfscore_dyn_cycle)) +
  geom_density() +
  facet_wrap(vars(cycle))

p_density_cfscores_total <- contributors_ces |> 
  ggplot(aes(contributor.cfscore)) +
  geom_density()

p_cfscores_density_comparisons <- p_density_cfscores_cycle / p_density_cfscores_total + 
  plot_annotation(title = "Comparison of dynamic and static cfscores")

ggsave("results/cfscores_density_comparisons.png", p_cfscores_density_comparisons)

cfscores_standardisation_density_comparisons <- (p_density_cfscores_cycle + p_density_cfscores_total) / 
  (p_density_cfscores_cycle_std + p_density_cfscores_total_std) +
  plot_annotation(title = "Comparison of standardised and non-standardised cfscores")

ggsave("results/cfscores_standardisation_density_comparisons.png", cfscores_standardisation_density_comparisons)

# Gender -----------------------------------------------------------------

#' Report the general gender distribution of the different 
#' industries and occupation_stds

p1 <- contributors_ces |> 
    ggplot(
        aes(ff49_abbr, fill = contributor.gender)
    ) +
    geom_bar(position = "fill") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "per industry")

p2 <- contributors_ces |> 
    ggplot(
        aes(occupation_std, fill = contributor.gender)
    ) +
    geom_bar(position = "fill") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "per occupation_std")

p3 <- contributors_ces |> 
    ggplot(
        aes(is_sec_insider, fill = contributor.gender)
    ) +
    geom_bar(position = "fill") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "per is_sec_insider")

p_gender_distribution <- p1 / p2 / p3 + plot_annotation("Gender distribution")

ggsave("results/gender_distribution.png", p_gender_distribution)

###########################################################################
#
# Analysis
#
###########################################################################



# DAG --------------------------------------------------------------------


library(dagitty)
library(ggdag)

dag <- dagitty('dag {
bb="-3.98,-2.866,2.565,2.968"
X [latent,pos="-0.807,1.763"]
location [exposure,pos="1.734,-1.856"]
gender [exposure,pos="-3.197,0.110"]
ideology [outcome,pos="1.768,0.110"]
industry [exposure,pos="-0.903,-1.263"]
occupation [exposure,pos="-0.889,0.110"]
X -> ideology
location -> ideology
location <-> industry
gender -> X
gender -> industry
gender -> occupation
industry -> ideology
industry <-> occupation
occupation -> ideology
}')

p_dag <- dag |>
  tidy_dagitty() |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_label(aes(label = name), size = 5, fill = "white", color = "black") +
  theme_dag()

ggsave("results/dag_gender.png", p_dag)

# H1: Tech employees are on average more liberal than employees in other firms --------------

# Descriptive Dummy is_tech

contributors_ces <- contributors_ces |> 
  mutate(ff12_is_tech = ifelse(ff12_abbr == "BusEq", TRUE, FALSE))

p_ff49_is_tech <- contributors_ces |> 
    ggplot(aes(ff49_is_tech, contributor.cfscore)) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "Using FF49's classification (Softw, Hardw, Chips)")

p_ff12_is_tech <- contributors_ces |> 
    ggplot(aes(ff12_is_tech, contributor.cfscore)) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "Using FF12's classification (BusEq)")

p_industry_is_tech <- p_ff12_is_tech + p_ff49_is_tech + plot_annotation(
  title = "Comparing operationalisations of industry",
  subtitle = "from democrat (-) to republican (+)"
)

# Boxplots of ff49

p_ff12_boxplot <- contributors_ces |> 
    ggplot(aes(ff12_abbr, contributor.cfscore, color = ff49_is_tech)) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "ff12")

p_ff49_boxplot <- contributors_ces |> 
    ggplot(aes(ff49_abbr, contributor.cfscore, color = ff49_is_tech)) +
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "ff49")

p_industry_boxplot <- p_ff12_boxplot / p_ff49_boxplot + plot_annotation(
  title = "Employee ideology distribution by industrial sector", 
  subtitle = "from democrat (-) to republican (+)"
)

# Mean and CI of industries

p_ff12_means <- contributors_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(ff12_abbr, ff12_is_tech)) |> 
  mutate(se = sd / sqrt(N)) |> 
  ggplot(aes(ff12_abbr, mean, group = ff12_abbr)) +
  geom_point(aes(color = ff12_is_tech)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=ff12_is_tech), width=.1)

p_ff49_means <- contributors_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(ff49_abbr, ff49_is_tech)) |> 
  mutate(
    se = sd / sqrt(N)
  ) |> 
  ggplot(aes(ff49_abbr, mean, group = ff49_abbr)) +
  geom_point(aes(color = ff49_is_tech)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=ff49_is_tech), width=.1) +
  theme(axis.text.x = element_text(angle = 90))

p_industry_means <- p_ff12_means / p_ff49_means + plot_annotation(
  title = "Mean employee ideology by industry", 
  subtitle = "from democrat (-) to republican (+)"
)

p_ff49_means <- p_ff49_means +
  labs(
    title = "Mean employee ideology by industry", 
    subtitle = "from democrat (-) to republican (+)"
  )

# Densities by industry

p_ff12_density <- contributors_ces |>
  ggplot(aes(contributor.cfscore)) +
  geom_density() +
  facet_wrap(vars(ff12_abbr))

p_ff49_density <- contributors_ces |>
  ggplot(aes(contributor.cfscore)) +
  geom_density() +
  facet_wrap(vars(ff49_abbr)) +
  scale_x_continuous(breaks = c(-3,0,3)) +
  scale_y_continuous(breaks = c(0,5))

p_industry_density <- p_ff12_density + p_ff49_density + plot_annotation(
  title = "Density of cfscores per industry"
)

p_ff49_density <- p_ff49_density +
  labs(title = "Density of cfscores by industry")

# create the final plot reported

design <- "
  1
  1
  1
  1
  2
"

p_final_report <- p_ff49_density + p_ff49_means + plot_layout(design = design)

# saving

ggsave("results/ideology_by_is_tech.png", p_industry_is_tech)
ggsave("results/ideology_by_industry_boxplot.png", p_industry_boxplot)
ggsave("results/ideology_by_industry_means.png", p_industry_means)
ggsave("results/ideology_by_industry_density.png", p_industry_density) # probably not that strong, thus I also export the following
ggsave("results/ideology_by_ff49_density.png", p_ff49_density)
ggsave("results/ideology_by_ff49_means.png", p_ff49_means)
ggsave("results/ideology_by_ff49_final.png", p_final_report)


# model based on is_tech

h1_I <- list(
  "Baseline" = lm(contributor.cfscore ~ ff49_is_tech, contributors_ces),
  "+ occupation" = lm(contributor.cfscore ~ ff49_is_tech + occupation_std, contributors_ces),
  "+ gender" = lm(contributor.cfscore ~ ff49_is_tech + occupation_std + contributor.gender, contributors_ces),
  "+ zip-mean" = lm(contributor.cfscore ~ ff49_is_tech + occupation_std + contributor.gender + mean_cfscore_zipcode_wo_i, contributors_ces)
)

modelsummary(h1_I, output = "results/h1_I.tex")
modelsummary(h1_I, output = "results/h1_I.html")

# model based on ff49

h1_II <- list(
  "Baseline" = lm(contributor.cfscore ~ ff49_abbr, contributors_ces),
  "+ occupation" = lm(contributor.cfscore ~ ff49_abbr + occupation_std, contributors_ces),
  "+ gender" = lm(contributor.cfscore ~ ff49_abbr + occupation_std + contributor.gender, contributors_ces),
  "+ zip-mean" = lm(contributor.cfscore ~ ff49_abbr + occupation_std + contributor.gender + mean_cfscore_zipcode_wo_i, contributors_ces)
)

modelsummary(h1_II, output = "results/h1_II.tex")
modelsummary(h1_II, output = "results/h1_II.html")


# remove objects:

rm(p_ff49_is_tech, p_ff12_is_tech, p_industry_is_tech,
   p_ff12_boxplot, p_ff49_boxplot, p_industry_boxplot,
   p_ff12_means, p_ff49_means, p_industry_means,
   p_ff12_density, p_ff49_density, p_industry_density,
   h1_I, h1_II)
gc()








# H2: TMTs are more conservative than other occupation groups --------

# descriptive: comparisons en/man/other

p_occupation_boxplot <- contributors_ces |> 
  ggplot(aes(occupation_std, contributor.cfscore)) +
  geom_boxplot()

p_occupation_means <- contributors_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(occupation_std)) |> 
  mutate(
    se = sd / sqrt(N)
  ) |> 
  ggplot(aes(occupation_std, mean, group = occupation_std)) +
  geom_point(aes(color = occupation_std)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=occupation_std), width=.1)
      
p_occupation_descr <- p_occupation_means + p_occupation_boxplot +
  plot_annotation(subtitle = "by occupation")


# descriptive: comparisons is_sec_insider

p_is_sec_insider_boxplot <- contributors_ces |> 
  ggplot(aes(is_sec_insider, contributor.cfscore)) +
  geom_boxplot()

p_is_sec_insider_means <- contributors_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(is_sec_insider)) |> 
  mutate(
    se = sd / sqrt(N)
  ) |> 
  ggplot(aes(is_sec_insider, mean, group = is_sec_insider)) +
  geom_point(aes(color = is_sec_insider)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=is_sec_insider), width=.1)

p_is_sec_insider_descr <- p_is_sec_insider_means + p_is_sec_insider_boxplot +
  plot_annotation(subtitle = "by insider status")

p_h2 <- p_occupation_descr / p_is_sec_insider_descr +
  plot_annotation(title = "Contributor ideology")

ggsave("results/p_h2.png", p_h2)


# model: cfscore ~ occupation

## try fixed slopes first to evaluate how the fixed effects vary when nested:

h2_I_fs <- list(
  "Null-Model" = lmer(contributor.cfscore ~ 1 + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "+ occupation" = lmer(contributor.cfscore ~ occupation_std + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "+ gender" = lmer(contributor.cfscore ~ occupation_std + contributor.gender + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "+ zip-means" = lmer(contributor.cfscore ~ occupation_std + contributor.gender + mean_cfscore_zipcode_wo_i + (1 | ff49_abbr), contributors_ces, REML = FALSE)
)

coef(h2_I_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h2_I_fixed_effects_coefs.txt")

anova(
  h2_I_fs[[1]],
  h2_I_fs[[2]],
  h2_I_fs[[3]],
  h2_I_fs[[4]]
) |> 
  capture.output() |> writeLines("results/h2_I_anova.txt")

modelsummary(
  h2_I_fs, 
  output = "results/h2_I_fixed_effects.tex")
modelsummary(
  h2_I_fs, 
  output = "results/h2_I_fixed_effects.html")

## compare fixed sloped with random slopes to see whether it makes sense to include them

h2_I_rs <- list(
  "Fixed slope" = lmer(contributor.cfscore ~ occupation_std + contributor.gender + mean_cfscore_zipcode_wo_i + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "Random slope" = lmer(contributor.cfscore ~ occupation_std + contributor.gender + mean_cfscore_zipcode_wo_i + (1 + occupation_std | ff49_abbr), contributors_ces, REML = FALSE)
)

coef(h2_I_rs$`Fixed slope`) |> capture.output() |> writeLines("results/h2_I_fixed_effects_coefs.txt")
coef(h2_I_rs$`Random slope`) |> capture.output() |> writeLines("results/h2_I_random_effects_coefs.txt")

anova(
  h2_I_rs[[1]],
  h2_I_rs[[2]]
) |> capture.output() |> writeLines("results/h2_I_random_slope_anova.txt")

modelsummary(
  h2_I_rs, 
  output = "results/h2_I_random_slopes.tex")
modelsummary(
  h2_I_rs, 
  output = "results/h2_I_random_slopes.html")


## estimate the final model with REML = true to report the parameters

h2_I_REML <- lmer(contributor.cfscore ~ occupation_std + contributor.gender + mean_cfscore_zipcode_wo_i + (1 + occupation_std | ff49_abbr), contributors_ces, REML = TRUE)

modelsummary(h2_I_REML, output = "results/h2_I_REML.tex")
modelsummary(h2_I_REML, output = "results/h2_I_REML.html")

coef(h2_I_REML) |> capture.output() |> writeLines("results/h2_I_REML_coefs.txt")



# model: cfscore ~ is_sec_insider

## try fixed slopes first to evaluate how the fixed effects vary when nested:

h2_I_insiders_fs <- list(
  "Null-Model" = lmer(contributor.cfscore ~ 1 + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "+ insider" = lmer(contributor.cfscore ~ is_sec_insider + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "+ gender" = lmer(contributor.cfscore ~ is_sec_insider + contributor.gender + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "+ zip-means" = lmer(contributor.cfscore ~ is_sec_insider + contributor.gender + mean_cfscore_zipcode_wo_i + (1 | ff49_abbr), contributors_ces, REML = FALSE)
)

coef(h2_I_insiders_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h2_I_insiders_fixed_effects_coefs.txt")

anova(
  h2_I_insiders_fs[[1]],
  h2_I_insiders_fs[[2]],
  h2_I_insiders_fs[[3]],
  h2_I_insiders_fs[[4]]
) |> 
  capture.output() |> writeLines("results/h2_I_insiders_anova.txt")

modelsummary(
  h2_I_insiders_fs, 
  output = "results/h2_I_insiders_fixed_effects.tex")
modelsummary(
  h2_I_insiders_fs, 
  output = "results/h2_I_insiders_fixed_effects.html")

## compare fixed sloped with random slopes to see whether it makes sense

h2_I_insiders_rs <- list(
  "Fixed slope" = lmer(contributor.cfscore ~ is_sec_insider + contributor.gender + mean_cfscore_zipcode_wo_i + (1 | ff49_abbr), contributors_ces, REML = FALSE),
  "Random slope" = lmer(contributor.cfscore ~ is_sec_insider + contributor.gender + mean_cfscore_zipcode_wo_i + (1 + is_sec_insider | ff49_abbr), contributors_ces, REML = FALSE)
)

coef(h2_I_insiders_rs$`Fixed slope`) |> capture.output() |> writeLines("results/h2_I_insiders_fixed_effects_coefs.txt")
coef(h2_I_insiders_rs$`Random slope`) |> capture.output() |> writeLines("results/h2_I_insiders_random_effects_coefs.txt")

anova(
  h2_I_insiders_rs[[1]],
  h2_I_insiders_rs[[2]]
) |> capture.output() |> writeLines("results/h2_I_insiders_random_slope_anova.txt")

modelsummary(
  h2_I_insiders_rs, 
  output = "results/h2_I_insiders_random_slopes.tex")
modelsummary(
  h2_I_insiders_rs, 
  output = "results/h2_I_insiders_random_slopes.html")


## estimate the final model with REML = true to report the parameters

h2_I_insiders_REML <- lmer(contributor.cfscore ~ is_sec_insider + contributor.gender + mean_cfscore_zipcode_wo_i + (1 + is_sec_insider | ff49_abbr), contributors_ces, REML = TRUE)

modelsummary(h2_I_insiders_REML, output = "results/h2_I_insiders_REML.tex")
modelsummary(h2_I_insiders_REML, output = "results/h2_I_insiders_REML.html")

coef(h2_I_insiders_REML) |> capture.output() |> writeLines("results/h2_I_insiders_REML_coefs.txt")


## make a more beautiful table to report

modelsummary(c("manager" = h2_I_REML, "insiders" = h2_I_insiders_REML),
  gof_omit = 'AIC|BIC|RMSE',
  output = "results/h2_I_final.tex")

modelsummary(c("manager" = h2_I_REML, "insiders" = h2_I_insiders_REML),
  gof_omit = 'AIC|BIC|RMSE',
  output = "results/h2_I_final.html")


# remove objects

rm(
  p_occupation_boxplot, p_occupation_means, p_is_sec_insider_means, p_occupation_descr, p_is_sec_insider_descr, p_h2,
  h2_I_fs, h2_I_rs, h2_I_REML, h2_I_insiders_fs, h2_I_insiders_fs, h2_I_insiders_REML
)
gc()






# H3: Tech TMTs are less conservative than in other industries ------------

# filter data

contributors_m_ces <- contributors_ces |> filter(occupation_std == "manager")
contributors_i_ces <- contributors_ces |> filter(is_sec_insider == TRUE)

# descriptives: managers

p_managers_is_tech_boxplot <- contributors_m_ces |> 
  ggplot(aes(ff49_is_tech, contributor.cfscore)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution the ideologies of managers in and outside of tech")

p_managers_is_tech_means <- contributors_m_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(ff49_is_tech)) |> 
  mutate(
    se = sd / sqrt(N)
  ) |> 
  ggplot(aes(ff49_is_tech, mean, group = ff49_is_tech)) +
  geom_point(aes(color = ff49_is_tech)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=ff49_is_tech), width=.1) +
  labs(title = "Filtered on managers")

p_managers_industries_boxplot <- contributors_m_ces |> 
  ggplot(aes(ff49_abbr, contributor.cfscore, color = ff49_is_tech)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution of the ideologies of managers in different sectors")

p_managers_industries_means <- contributors_m_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(ff49_abbr, ff49_is_tech)) |> 
  mutate(
    se = sd / sqrt(N)
  ) |> 
  ggplot(aes(ff49_abbr, mean, group = ff49_abbr)) +
  geom_point(aes(color = ff49_is_tech)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=ff49_is_tech), width=.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "filtered on managers")

ggsave("results/managers_is_tech_boxplot.png", p_managers_is_tech_boxplot)
ggsave("results/managers_is_tech_means.png", p_managers_is_tech_means)
ggsave("results/managers_industries_boxplot.png", p_managers_industries_boxplot)
ggsave("results/managers_industries_means.png", p_managers_industries_means)



# descriptives: is_sec_insider

p_insiders_is_tech_boxplot <- contributors_i_ces |> 
  ggplot(aes(ff49_is_tech, contributor.cfscore)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution of the ideologies of insiders in and outside of tech")

p_insiders_is_tech_means <- contributors_i_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(ff49_is_tech)) |> 
  mutate(
    se = sd / sqrt(N)
  ) |> 
  ggplot(aes(ff49_is_tech, mean, group = ff49_is_tech)) +
  geom_point(aes(color = ff49_is_tech)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=ff49_is_tech), width=.1) +
  labs(title = "Filtered on insiders")

p_insiders_industries_boxplot <- contributors_i_ces |> 
  ggplot(aes(ff49_abbr, contributor.cfscore, color = ff49_is_tech)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution of the ideologies of insiders in different sectors")

p_insiders_industries_means <- contributors_i_ces |> 
  summarise(
    N = n(),
    mean = mean(contributor.cfscore),
    sd = sd(contributor.cfscore),
    .by = c(ff49_abbr, ff49_is_tech)) |> 
  mutate(
    se = sd / sqrt(N)
  ) |> 
  ggplot(aes(ff49_abbr, mean, group = ff49_abbr)) +
  geom_point(aes(color = ff49_is_tech)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=ff49_is_tech), width=.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "filtered on insiders")

ggsave("results/insiders_is_tech_boxplot.png", p_insiders_is_tech_boxplot)
ggsave("results/insiders_is_tech_means.png", p_insiders_is_tech_means)
ggsave("results/insiders_industries_boxplot.png", p_insiders_industries_boxplot)
ggsave("results/insiders_industries_means.png", p_insiders_industries_means)

design <- "
    13
    22
    44
"

p_h3 <- p_managers_is_tech_means + p_managers_industries_means + p_insiders_is_tech_means + p_insiders_industries_means +
  plot_annotation(title = "Means and CIs of the ideologies in different sectors:") +
  plot_layout(design = design)

ggsave("results/p_h3.png", p_h3, scale = 1.8)

# model: cf ~ ff49_is_tech in managers

h3_I <- list(
  "Baseline" = lm(contributor.cfscore ~ ff49_is_tech, contributors_m_ces),
  "+ gender" = lm(contributor.cfscore ~ ff49_is_tech + contributor.gender, contributors_m_ces),
  "+ zip-means" = lm(contributor.cfscore ~ ff49_is_tech + contributor.gender + mean_cfscore_zipcode_wo_i, contributors_m_ces)
)

modelsummary(h3_I, output = "results/h3_I.tex")
modelsummary(h3_I, output = "results/h3_I.html")

h3_II <- list(
  "Baseline" = lm(contributor.cfscore ~ ff49_abbr, contributors_m_ces),
  "+ gender" = lm(contributor.cfscore ~ ff49_abbr + contributor.gender, contributors_m_ces),
  "+ zip-means" = lm(contributor.cfscore ~ ff49_abbr + contributor.gender + mean_cfscore_zipcode_wo_i, contributors_m_ces)
)

modelsummary(h3_II, output = "results/h3_II.tex")
modelsummary(h3_II, output = "results/h3_II.html")


# model: cf ~ ff49_is_tech in insiders

h3_I_insiders <- list(
  "Baseline" = lm(contributor.cfscore ~ ff49_is_tech, contributors_i_ces),
  "+ gender" = lm(contributor.cfscore ~ ff49_is_tech + contributor.gender, contributors_i_ces),
  "+ zip-means" = lm(contributor.cfscore ~ ff49_is_tech + contributor.gender + mean_cfscore_zipcode_wo_i, contributors_i_ces)
)

modelsummary(h3_I_insiders, output = "results/h3_I_insiders.tex")
modelsummary(h3_I_insiders, output = "results/h3_I_insiders.html")


h3_II_insiders <- list(
  "Baseline" = lm(contributor.cfscore ~ ff49_abbr, contributors_i_ces),
  "+ gender" = lm(contributor.cfscore ~ ff49_abbr + contributor.gender, contributors_i_ces),
  "+ zip-means" = lm(contributor.cfscore ~ ff49_abbr + contributor.gender + mean_cfscore_zipcode_wo_i, contributors_i_ces)
)

modelsummary(h3_II_insiders, output = "results/h3_II_insiders.tex")
modelsummary(h3_II_insiders, output = "results/h3_II_insiders.html")

# save the final table split to report on one page in the appendix 

full_df <- modelsummary(
  h3_II_insiders,
  output   = "data.frame",
  gof_omit = "AIC|BIC|RMSE"
)

# Clean up industry labels (same fix as before)
full_df$term <- full_df$term |>
  str_replace("^ff49_abbr", "Industry: ")

full_df |>
  tt(caption = "Firm effects on ideology in data filtered on insiders") |>
  theme_latex(multipage = TRUE) |>
  save_tt("results/h3_II_insiders_long.tex", overwrite = TRUE)


# remove objects

rm(contributors_m_ces, contributors_i_ces,
   p_managers_is_tech_boxplot, p_managers_is_tech_means,
   p_managers_industries_boxplot, p_managers_industries_means,
   p_insiders_is_tech_boxplot, p_insiders_is_tech_means,
   p_insiders_industries_boxplot, p_insiders_industries_means,
   h3_I, h3_II)
gc()


# H4: TMTs in general shifted to the left in recent years -----------------

# filter managers

contributions_m_ces <- contributions_ces |> 
  filter(occupation_std == "manager")

contributions_i_ces <- contributions_ces |> 
  filter(is_sec_insider == TRUE)

# descriptives: managers

p_manager_cycle_means <- contributions_m_ces |> 
  summarise(
    N = n(),
    mean = mean(cfscore_dyn_cycle),
    sd = sd(cfscore_dyn_cycle),
    .by = cycle) |> 
  mutate(
    se = sd / sqrt(N),
  ) |> 
  ggplot(aes(cycle, mean, group = cycle)) +
  geom_point(aes(color = cycle)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=cycle), width=.1)

p_manager_cycle_boxplot <- contributions_m_ces |> 
  ggplot(aes(cycle, cfscore_dyn_cycle, group = cycle)) +
  geom_boxplot()

p_manager_cycle_density <- contributions_m_ces |> 
  ggplot(aes(y = cfscore_dyn_cycle)) +
  geom_density() + 
  facet_wrap(vars(cycle)) +
  coord_flip()

ggsave("results/manager_cycle_means.png", p_manager_cycle_means)
ggsave("results/manager_cycle_boxplot.png", p_manager_cycle_boxplot)
ggsave("results/manager_cycle_density.png", p_manager_cycle_density)


# descriptives: insiders

p_insiders_cycle_means <- contributions_i_ces |> 
  summarise(
    N = n(),
    mean = mean(cfscore_dyn_cycle),
    sd = sd(cfscore_dyn_cycle),
    .by = cycle) |> 
  mutate(
    se = sd / sqrt(N),
  ) |> 
  ggplot(aes(cycle, mean, group = cycle)) +
  geom_point(aes(color = cycle)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
      color=cycle), width=.1) +
  labs(title = "Means of the dynamic cfscore of insiders by cycle")

p_insiders_cycle_boxplot <- contributions_i_ces |> 
  ggplot(aes(cycle, cfscore_dyn_cycle, group = cycle)) +
  geom_boxplot()

p_insiders_cycle_density <- contributions_i_ces |> 
  ggplot(aes(y = cfscore_dyn_cycle)) +
  geom_density() + 
  facet_wrap(vars(cycle)) +
  coord_flip() +
  labs(title = "Density of the dynamic cfscore of insiders by cycle")

ggsave("results/insiders_cycle_means.png", p_insiders_cycle_means)
ggsave("results/insiders_cycle_boxplot.png", p_insiders_cycle_boxplot)
ggsave("results/insiders_cycle_density.png", p_insiders_cycle_density)

p_h4 <- p_insiders_cycle_density / p_insiders_cycle_means

ggsave("results/p_h4.png", p_h4, scale = 1.5)




# model: cf ~ cycle in managers

h4_I_fs <- list(
  "Null-Modell" = lmer(cfscore_dyn_cycle ~ 1 + (1 | ff49_abbr), contributions_m_ces, REML = FALSE),
  "+ cycle" = lmer(cfscore_dyn_cycle ~ cycle + (1 | ff49_abbr), contributions_m_ces, REML = FALSE),
  "+ gender" = lmer(cfscore_dyn_cycle ~ cycle + contributor.gender + (1 | ff49_abbr), contributions_m_ces, REML = FALSE),
  "+ zip-means" = lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 | ff49_abbr), contributions_m_ces, REML = FALSE)
)

coef(h4_I_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h4_I_fixed_effects_coefs.txt")

anova(
  h4_I_fs[[1]],
  h4_I_fs[[2]],
  h4_I_fs[[3]],
  h4_I_fs[[4]]
) |> 
  capture.output() |> writeLines("results/h4_I_anova.txt")

modelsummary(
  h4_I_fs, 
  output = "results/h4_I_fixed_effects.tex")
modelsummary(
  h4_I_fs, 
  output = "results/h4_I_fixed_effects.html")

## compare fixed sloped with random slopes to see whether it makes sense

h4_I_rs <- list(
  "Fixed slope" = lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 | ff49_abbr), contributions_m_ces, REML = FALSE),
  "Random slope" = lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 + cycle | ff49_abbr), contributions_m_ces, REML = FALSE)
)

coef(h4_I_rs$`Fixed slope`) |> capture.output() |> writeLines("results/h4_I_fixed_effects_coefs.txt")
coef(h4_I_rs$`Random slope`) |> capture.output() |> writeLines("results/h4_I_random_effects_coefs.txt")

anova(
  h4_I_rs[[1]],
  h4_I_rs[[2]]
) |> capture.output() |> writeLines("results/h4_I_random_slope_anova.txt")

modelsummary(
  h4_I_rs, 
  output = "results/h4_I_random_slopes.tex")

modelsummary(
  h4_I_rs, 
  output = "results/h4_I_random_slopes.html")


## estimate the final model with REML = true to report the parameters

h4_I_REML <- lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 + cycle | ff49_abbr), contributions_m_ces, REML = TRUE)

modelsummary(h4_I_REML, output = "results/h4_I_final.tex")
modelsummary(h4_I_REML, output = "results/h4_I_final.html")

coef(h4_I_REML) |> capture.output() |> writeLines("results/h4_I_final_coefs.txt")



# model: cf ~ cycle in insiders

h4_I_insiders_fs <- list(
  "Null-Modell" = lmer(cfscore_dyn_cycle ~ 1 + (1 | ff49_abbr), contributions_i_ces, REML = FALSE),
  "+ cycle" = lmer(cfscore_dyn_cycle ~ cycle + (1 | ff49_abbr), contributions_i_ces, REML = FALSE),
  "+ gender" = lmer(cfscore_dyn_cycle ~ cycle + contributor.gender + (1 | ff49_abbr), contributions_i_ces, REML = FALSE),
  "+ zip-means" = lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 | ff49_abbr), contributions_i_ces, REML = FALSE)
)

coef(h4_I_insiders_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h4_I_insiders_fixed_effects_coefs.txt")

anova(
  h4_I_insiders_fs[[1]],
  h4_I_insiders_fs[[2]],
  h4_I_insiders_fs[[3]],
  h4_I_insiders_fs[[4]]
) |> 
  capture.output() |> writeLines("results/h4_I_insiders_anova.txt")

modelsummary(
  h4_I_insiders_fs, 
  output = "results/h4_I_insiders_fixed_effects.tex")
modelsummary(
  h4_I_insiders_fs, 
  output = "results/h4_I_insiders_fixed_effects.html")

## compare fixed sloped with random slopes to see whether it makes sense

h4_I_insiders_rs <- list(
  "Fixed slope" = lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 | ff49_abbr), contributions_i_ces, REML = FALSE),
  "Random slope" = lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 + cycle | ff49_abbr), contributions_i_ces, REML = FALSE)
)

coef(h4_I_insiders_rs$`Fixed slope`) |> capture.output() |> writeLines("results/h4_I_insiders_fixed_effects_coefs.txt")
coef(h4_I_insiders_rs$`Random slope`) |> capture.output() |> writeLines("results/h4_I_insiders_random_effects_coefs.txt")

anova(
  h4_I_insiders_rs[[1]],
  h4_I_insiders_rs[[2]]
) |> capture.output() |> writeLines("results/h4_I_insiders_random_slope_anova.txt")

modelsummary(
  h4_I_insiders_rs, 
  output = "results/h4_I_insiders_random_slopes.tex")
modelsummary(
  h4_I_insiders_rs, 
  output = "results/h4_I_insiders_random_slopes.html")

## estimate the final model with REML = true to report the parameters

h4_I_insiders_REML <- lmer(cfscore_dyn_cycle ~ cycle + mean_dyn_cfscore_zipcode_cycle_wo_i + contributor.gender + (1 + cycle | ff49_abbr), contributions_i_ces, REML = TRUE)

modelsummary(h4_I_insiders_REML, gof_omit = "AIC|BIC|RMSE", output = "results/h4_I_insiders_final.tex")
modelsummary(h4_I_insiders_REML, gof_omit = "AIC|BIC|RMSE", output = "results/h4_I_insiders_final.html")

coef(h4_I_insiders_REML) |> capture.output() |> writeLines("results/h4_I_insiders_final_coefs.txt")




# remove objects

rm(contributions_m_ces, contributions_i_ces,
   p_manager_cycle_means, p_manager_cycle_boxplot, p_manager_cycle_density,
   p_insiders_cycle_means, p_insiders_cycle_boxplot, p_insiders_cycle_density,
   h4_I_fs, h4_I_insiders_fs, h4_I_rs, h4_I_insiders_rs, h4_I_REML, h4_I_insiders_REML)
gc()



# Final Models -----------------------------------------------------------


# on contributions

# filter:
contributions_tech_ces <- contributions_ces |> 
  filter(ff49_is_tech == "Tech")

# model: cf ~ cycle*manager in contributions_tech_ces => Robustness / Einfacher interpretierbarer Test

h56_I_fs <- list(
  "Baseline" = lmer(cfscore_dyn_cycle ~ cycle + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "+ occupation_std" = lmer(cfscore_dyn_cycle ~ cycle + occupation_std + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "* occupation_std" = lmer(cfscore_dyn_cycle ~ cycle*occupation_std + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "+ gender" = lmer(cfscore_dyn_cycle ~ cycle*occupation_std + contributor.gender + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "+ zip-means" = lmer(cfscore_dyn_cycle ~ cycle*occupation_std + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE)
)

coef(h56_I_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h56_I_fixed_effects_coefs.txt")

anova(
  h56_I_fs[[1]],
  h56_I_fs[[2]],
  h56_I_fs[[3]],
  h56_I_fs[[4]]
) |> 
  capture.output() |> writeLines("results/h56_I_anova.txt")

modelsummary(
  h56_I_fs, 
  output = "results/h56_I_fixed_effects.tex")
modelsummary(
  h56_I_fs, 
  output = "results/h56_I_fixed_effects.html")



# estimated with REML

h56_I_REML = lmer(cfscore_dyn_cycle ~ cycle*occupation_std + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_tech_ces, REML = TRUE)

modelsummary(h56_I_REML, output = "results/h56_I_final.tex")
modelsummary(h56_I_REML, output = "results/h56_I_final.html")


coef(h56_I_REML) |> capture.output() |> writeLines("results/h56_I_final_coefs.txt")





# model: cf ~ cycle*is_tech_insider in contributions_tech_ces => Robustness / Einfacher interpretierbarer Test

h56_I_insiders_fs <- list(
  "Baseline" = lmer(cfscore_dyn_cycle ~ cycle + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "+ is_sec_insider" = lmer(cfscore_dyn_cycle ~ cycle + is_sec_insider + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "* is_sec_insider" = lmer(cfscore_dyn_cycle ~ cycle*is_sec_insider + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "+ gender" = lmer(cfscore_dyn_cycle ~ cycle*is_sec_insider + contributor.gender + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE),
  "+ zip-means" = lmer(cfscore_dyn_cycle ~ cycle*is_sec_insider + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_tech_ces, REML = FALSE)
)

coef(h56_I_insiders_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h56_I_insiders_fixed_effects_coefs.txt")

anova(
  h56_I_insiders_fs[[1]],
  h56_I_insiders_fs[[2]],
  h56_I_insiders_fs[[3]],
  h56_I_insiders_fs[[4]]
) |> 
  capture.output() |> writeLines("results/h56_I_insiders_anova.txt")

modelsummary(
  h56_I_insiders_fs, 
  output = "results/h56_I_insiders_fixed_effects.tex")
modelsummary(
  h56_I_insiders_fs, 
  output = "results/h56_I_insiders_fixed_effects.html")


# estimated with REML (don't report isn't a lot different)

h56_I_insiders_REML = lmer(cfscore_dyn_cycle ~ cycle*is_sec_insider + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_tech_ces, REML = TRUE)

modelsummary(h56_I_insiders_REML, output = "results/h56_I_insiders_final.tex")
modelsummary(h56_I_insiders_REML, output = "results/h56_I_insiders_final.html")

coef(h56_I_insiders_REML) |> capture.output() |> writeLines("results/h56_I_insiders_final_coefs.txt")





# model: cf ~ cycle*occupation_std*ff49_is_tech => Eigentlicher Test

h56_II_fs <- list(
  "Baseline" = lmer(cfscore_dyn_cycle ~ cycle + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ is_tech" = lmer(cfscore_dyn_cycle ~ cycle + ff49_is_tech + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ occupation_std" = lmer(cfscore_dyn_cycle ~ cycle + ff49_is_tech + occupation_std + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "^2" = lmer(cfscore_dyn_cycle ~ (cycle + ff49_is_tech + occupation_std) ^ 2 + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "* is_tech * occupation_std" = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * occupation_std + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ gender" = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * occupation_std + contributor.gender + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ zip-means" = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * occupation_std + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_ces, REML = FALSE)
)

coef(h56_II_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h56_II_fixed_effects_coefs.txt")

modelsummary(h56_II_fs, output = "results/h56_II_fixed_effects.tex")
modelsummary(h56_II_fs, output = "results/h56_II_fixed_effects.html")

anova(
  h56_II_fs[[1]],
  h56_II_fs[[2]],
  h56_II_fs[[3]],
  h56_II_fs[[4]],
  h56_II_fs[[5]],
  h56_II_fs[[6]],
  h56_II_fs[[7]]  
) |> 
  capture.output() |> writeLines("results/h56_II_fixed_effects_anova.txt")



# estimated with REML (don't report isn't a lot different)

h56_II_REML = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * occupation_std + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_ces, REML = TRUE)

modelsummary(h56_II_REML, output = "results/h56_II_final.tex")
modelsummary(h56_II_REML, output = "results/h56_II_final.html")

coef(h56_II_REML) |> capture.output() |> writeLines("results/h56_II_final_coefs.txt")




# model: cf ~ cycle*is_sec_insider*ff49_is_tech => Eigentlicher Test

h56_II_insiders_fs <- list(
  "Baseline" = lmer(cfscore_dyn_cycle ~ cycle + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ is_tech" = lmer(cfscore_dyn_cycle ~ cycle + ff49_is_tech + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ is_sec_insider" = lmer(cfscore_dyn_cycle ~ cycle + ff49_is_tech + is_sec_insider + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "^2" = lmer(cfscore_dyn_cycle ~ (cycle + ff49_is_tech + is_sec_insider) ^ 2 + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "* is_tech * is_sec_insider" = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * is_sec_insider + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ gender" = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * is_sec_insider + contributor.gender + (1 | contributor.employer.matched), contributions_ces, REML = FALSE),
  "+ zip-means" = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * is_sec_insider + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_ces, REML = FALSE)
)

coef(h56_II_insiders_fs$`+ zip-means`) |> capture.output() |> writeLines("results/h56_II_insiders_fixed_effects_coefs.txt")

modelsummary(h56_II_insiders_fs, output = "results/h56_II_insiders_fixed_effects.tex")
modelsummary(h56_II_insiders_fs, output = "results/h56_II_insiders_fixed_effects.html")

anova(
  h56_II_insiders_fs[[1]],
  h56_II_insiders_fs[[2]],
  h56_II_insiders_fs[[3]],
  h56_II_insiders_fs[[4]],
  h56_II_insiders_fs[[5]],
  h56_II_insiders_fs[[6]],
  h56_II_insiders_fs[[7]]  
) |> 
  capture.output() |> writeLines("results/h56_II_insiders_fixed_effects_anova.txt")



# estimated with REML (don't report isn't a lot different)

h56_II_insiders_REML = lmer(cfscore_dyn_cycle ~ cycle * ff49_is_tech * is_sec_insider + contributor.gender + mean_dyn_cfscore_zipcode_cycle_wo_i + (1 | contributor.employer.matched), contributions_ces, REML = TRUE)

modelsummary(h56_II_insiders_REML, output = "results/h56_II_insiders_final.tex")
modelsummary(h56_II_insiders_REML, output = "results/h56_II_insiders_final.html")

coef(h56_II_insiders_REML) |> capture.output() |> writeLines("results/h56_II_insiders_final_coefs.txt")




# H5: In tech firms from 2016 to 2024, TMTs kept being more conservative than tech workers and other occupational groups inside of their own firms ---------------------------



# COMPARISONS



# h56_I_REML: filtered on is_tech

# Occupation gap by cycle
avg_comparisons(h56_I_REML,
                variables = "occupation_std",
                by = c("cycle")) |> 
  capture.output() |> writeLines("results/h56_I_comparisons_table.txt")

h56_I_p1 <- plot_comparisons(h56_I_REML,
                      variables = "occupation_std",
                      by = c("cycle")) +
  labs(
    title = "Comparison of ideology between engineers and managers / between others and managers in tech",
    subtitle = "model controls on gender, zip-means and firm"
)

ggsave("results/h56_I_comparisons_occupation_std.png", h56_I_p1, width = 8, height = 5, dpi = 300)


# h56_I_insiders: filtered on is_tech

# Is_tech_insider gap by cycle
avg_comparisons(h56_I_insiders_REML,
                variables = "is_sec_insider",
                by = c("cycle")) |> 
  capture.output() |> writeLines("results/h56_I_insiders_comparisons_table.txt")

h56_I_insiders_p1 <- plot_comparisons(h56_I_insiders_REML,
                      variables = "is_sec_insider",
                      by = c("cycle")) +
  labs(
    title = "Comparison of ideology between insiders and non insiders in tech",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_I_insiders_comparisons_is_sec_insider.png", h56_I_insiders_p1, width = 8, height = 5, dpi = 300)


# h56_II: not filtered but including the triple interaction term

# occupation gap by cycle and is_tech
avg_comparisons(h56_II_REML,
                variables = "occupation_std",
                by = c("cycle", "ff49_is_tech")) |> 
  capture.output() |> writeLines("results/h56_II_comparisons_table_tmt.txt")

h56_II_p1 <- plot_comparisons(h56_II_REML,
                      variables = "occupation_std",
                      by = c("cycle", "ff49_is_tech")
                    ) +
  labs(
    title = "Comparisons of ideology between engineers and managers / between others and managers",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_II_comparisons_tmt.png", h56_II_p1, width = 8, height = 5, dpi = 300)

# h56_II_insiders: not filtered but including the triple interaction term

# occupation gap by cycle and is_tech
avg_comparisons(h56_II_insiders_REML,
                variables = "is_sec_insider",
                by = c("cycle", "ff49_is_tech")) |> 
  capture.output() |> writeLines("results/h56_II_insiders_comparisons_table_tmt.txt")

h56_II_insiders_p1 <- plot_comparisons(h56_II_insiders_REML,
                      variables = "is_sec_insider",
                      by = c("cycle", "ff49_is_tech")) +
  labs(
    title = "Comparison of ideology between insiders and non insiders",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_II_insiders_comparisons_tmt.png", h56_II_insiders_p1, width = 8, height = 5, dpi = 300)

# remove objects

rm(h56_I_p1, h56_I_insiders_p1, h56_II_p1, h56_II_insiders_p1)
gc()

# H6: While tech employees shifted further left from 2016 to 2024, there is a trend in tech TMTs that shifted towards the Republicans from 2020 to 2024 -----------------

# COMPARISONS

# h56_I and h59_insiders: filtered on is_tech

# Difference in cycle by occupation
avg_comparisons(h56_I_REML,
                variables = "cycle",
                by = c("occupation_std")) |> 
  capture.output() |> writeLines("results/h56_I_comparisons_table.txt")

h56_I_p2 <- plot_comparisons(h56_I_REML,
                      variables = "cycle",
                      by = c("occupation_std")) +
  labs(
    title = "Comparison of ideology between cycles in different occupations in tech",
    subtitle = "model controls on gender, zip-means and firm"
)

ggsave("results/h56_I_comparisons_cycle.png", h56_I_p2, width = 8, height = 5, dpi = 300)

# Difference in cycle by is_sec_insider
avg_comparisons(h56_I_insiders_REML,
                variables = "cycle",
                by = c("is_sec_insider")) |> 
  capture.output() |> writeLines("results/h56_I_insiders_comparisons_table.txt")

h56_I_insiders_p2 <- plot_comparisons(h56_I_insiders_REML,
                      variables = "cycle",
                      by = c("is_sec_insider")) +
  labs(
    title = "Comparison of ideology between cycles and insider status in tech",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_I_insiders_comparisons_cycle.png", h56_I_insiders_p2, width = 8, height = 5, dpi = 300)


# h56_II and h59_II_insiders: not filtered but including the triple interaction term

# Difference in cycle by occupation and is_tech
avg_comparisons(h56_II_REML,
                variables = "cycle",
                by = c("occupation_std", "ff49_is_tech")) |> 
  capture.output() |> writeLines("results/h56_II_comparisons_table_cycle.txt")

h56_II_p2 <- plot_comparisons(h56_II_REML,
                      variables = "cycle",
                      by = c("occupation_std", "ff49_is_tech")) +
  labs(
    title = "Comparison of ideology between cycles in different occupations",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_II_comparisons_cycle.png", h56_II_p2, width = 8, height = 5, dpi = 300)

# Difference in cycle by is_tech_insider and is_tech
avg_comparisons(h56_II_insiders_REML,
                variables = "cycle",
                by = c("is_sec_insider", "ff49_is_tech")) |> 
  capture.output() |> writeLines("results/h56_II_insiders_comparisons_table_cycle.txt")

h56_II_insiders_p2 <- plot_comparisons(h56_II_insiders_REML,
                      variables = "cycle",
                      by = c("is_sec_insider", "ff49_is_tech")) +
  labs(
    title = "Comparison of ideology between cycles and insider status",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_II_insiders_comparisons_cycle.png", h56_II_insiders_p2, width = 8, height = 5, dpi = 300)

p_h6_comps <- h56_II_p2 / h56_II_insiders_p2

ggsave("results/p_h6_comps.png", p_h6_comps)


# PREDICTIONS

# occupation

h56_I_p3 <- plot_predictions(h56_I_REML,
                by = c("cycle", "occupation_std")) +
  labs(
    title = "Predictions of ideology in tech",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_I_predictions.png", h56_I_p3, width = 8, height = 5, dpi = 300)


h56_II_p3 <- plot_predictions(h56_II_REML,
                    by = c("cycle", "occupation_std", "ff49_is_tech")) +
  labs(
    title = "Predictions of ideology by tech and non-tech",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_II_predictions.png", h56_II_p3, width = 8, height = 5, dpi = 300)

# insiders

h56_I_insiders_p3 <- plot_predictions(h56_I_insiders_REML,
                by = c("cycle", "is_sec_insider")) +
  labs(
    title = "Predictions of ideology in tech",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_I_insiders_predictions.png", h56_I_insiders_p3, width = 8, height = 5, dpi = 300)


h56_II_insiders_p3 <- plot_predictions(h56_II_insiders_REML,
                    by = c("cycle", "is_sec_insider", "ff49_is_tech"))  +
  labs(
    title = "Predictions of ideology by tech and non-tech",
    subtitle = "model controls on gender, zip-means and firm"
)
ggsave("results/h56_II_insiders_predictions.png", h56_II_insiders_p3, width = 8, height = 5, dpi = 300)

p_h6_preds <- h56_II_insiders_p3 / h56_II_p3

ggsave("results/p_h6_preds.png", p_h6_preds)

# remove objects 
 
rm(h56_I_p2, h56_I_insiders_p2, h56_II_p2, h56_II_insiders_p2,
   h56_I_p3, h56_II_p3, h56_I_insiders_p3, h56_II_insiders_p3,
  #  h56_I, h56_I_insiders, h56_II, h56_II_insiders,
   contributions_tech_ces, contributors_ces, contributions_ces)
gc()

# shutdown ---------------------------------------------------------------------

dbDisconnect(con)
unlink("tmp", recursive = TRUE)
