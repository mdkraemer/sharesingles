# Model test runs 

library(nlme)

lme_cust = function(outcome, group, df, random_slope=FALSE) {
  form = as.formula(paste0(outcome, " ~ 1 + ", group))
  if (random_slope==TRUE) {
    form_r <-  as.formula(paste0(" ~ 1 + ", group, " | country"))
  }
  else {
    form_r <-  as.formula(" ~ 1 | country")
  }
  nlme::lme(fixed = form, random = form_r, method = "ML", 
            na.action='na.omit', data = get(df)) # posaff - sde_ep would not converge otherwise
}

summary(lme_cust("extra", "evermarried", "df_singles", random_slope = T))
summary(lme_cust("agree", "evermarried", "df_singles", random_slope = T))
summary(lme_cust("consc", "evermarried", "df_singles", random_slope = T))
summary(lme_cust("neuro", "evermarried", "df_singles", random_slope = T))
summary(lme_cust("open", "evermarried", "df_singles", random_slope = T))

summary(lme_cust("lifesat", "evermarried", "df_singles", random_slope = T))
summary(lme_cust("lifequal", "evermarried", "df_singles", random_slope = T))


# rename for easier to interpret var names
colnames(df_singles)
df_singles <- df_singles %>% 
  rename(extra = bfi10_extra, agree = bfi10_agree, consc = bfi10_consc, neuro = bfi10_neuro, openn = bfi10_open,
         everpartner = everlat, lifesat = ac012_, lifequal = casp, income = thinc2, religiosity = Religiosity,
         gender_ratio = `Gender_ratio(maleper100female)`, singles_proportion = Total_Singlehood_Ratio)
# code education
df_singles <- df_singles %>% 
  mutate(education = case_when(
    isced1997_r == 1 ~ "ISCED_1",
    isced1997_r == 2 ~ "ISCED_2",
    isced1997_r == 3 ~ "ISCED_3",
    isced1997_r == 4 ~ "ISCED_4",
    isced1997_r == 5 ~ "ISCED_5",
    isced1997_r == 6 ~ "ISCED_6",
    isced1997_r %in% c(0, 95, 96) ~ "other/none",
    .default = NA_character_
  )) %>% # also dummy coding (1 = reference category)
  mutate(isced_2 = ifelse(!is.na(education), ifelse(education=="ISCED_2", 1, 0), NA),
         isced_3 = ifelse(!is.na(education), ifelse(education=="ISCED_3", 1, 0), NA),
         isced_4 = ifelse(!is.na(education), ifelse(education=="ISCED_4", 1, 0), NA),
         isced_5 = ifelse(!is.na(education), ifelse(education=="ISCED_5", 1, 0), NA),
         isced_6 = ifelse(!is.na(education), ifelse(education=="ISCED_6", 1, 0), NA),
         isced_other = ifelse(!is.na(education), ifelse(education=="other/none", 1, 0), NA)) %>% 
  mutate(income_z = scale(income), # z-standardize income
         religiosity_z = scale(religiosity),
         gender_ratio_z = scale(gender_ratio),
         singles_proportion_z = scale(singles_proportion))


# SCA

# custom function
# Including random slopes (only country as grouping variable)
lmer_rs_1 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  slopevars <- unlist(strsplit(formula, " ~ "))[2]
  formula <- paste0(formula, "+ (1 + ", slopevars, "|country_str)" )
  lme4::lmer(formula, data, control=lmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e5)))
}

library(specr)
library(lme4)
library(furrr) # for parallelization

specifications_b5 <- crossing(
  vec_outcome = c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness"), 
  vec_predictor = c("ever_partnered", "ever_cohabitating", "ever_married"),
  vec_controls = c("none", "age", "gender", "income", "education", "religiosity", "gender_ratio", "singles_proportion", "all")) %>% 
  mutate(specnr = row_number()) %>% 
  mutate(spec_name = paste0("outcome=", vec_outcome, ", predictor=", vec_predictor, 
                            ", controls=", vec_controls))


df_singles <- df_singles %>% mutate_at(c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness",
                                         "life_satisfaction", "quality_of_life"), as.numeric)
  
# personality
# Setup Specifications
specs_b5 <- specr::setup(data = df_singles, 
                  y = c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness"), 
                  x = c("ever_partnered", "ever_cohabitating", "ever_married"), 
                  controls = c("age_c", "female", "income_z", "education", 
                               "religiosity_z", "gender_ratio_z", "singles_proportion_z"),
                  simplify = T, # not all combinations of covars
                  model = c("lmer_rs_1"))

# Run Specification Curve Analysis
opts <- furrr_options( # for parallelization (using multiple cores)
  globals = list(lmer_rs_1 = lmer_rs_1)
)
plan()
plan(strategy = multisession, workers = 6)
sca_b5 <- specr(specs_b5, .options = opts,   # Pass opts to specr
                .progress = TRUE)
plan(sequential)

# Plot Specification Curve
plot(sca_b5, 
     choices = c("x", "y", "controls"))

# well-being
# Setup Specifications
specs_wb <- specr::setup(data = df_singles, 
                         y = c("life_satisfaction", "quality_of_life"), 
                         x = c("ever_partnered", "ever_cohabitating", "ever_married"), 
                         controls = c("age_c", "female", "income_z", "education", 
                                      "religiosity_z", "gender_ratio_z", "singles_proportion_z"),
                         simplify = T, # not all combinations of covars
                         model = c("lmer_rs_1"))

# Run Specification Curve Analysis
sca_wb <- specr(specs_wb)

# Plot Specification Curve
plot(sca_wb, 
     choices = c("x", "y", "controls"))


# winsorizing of outliers (code from Yannick Roos):
# calculate means and standard deviations
means <- sapply(df_singles[, c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness",
                               "life_satisfaction", "quality_of_life")], mean, na.rm = TRUE)
sds <- sapply(df_singles[, c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness",
                             "life_satisfaction", "quality_of_life")], sd, na.rm = TRUE)

# combine results into a data frame
result_df <- data.frame(variable = names(means), mean = round(means,digits = 2), sd = round(sds,digits = 2))

# detect outliers and winsorize as necessary
for (col_name in c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness",
                    "life_satisfaction", "quality_of_life")) {
  # detect outliers
  outlier_indices <- which(df_singles[, col_name] < means[col_name] - 3 * sds[col_name] |
                             df_singles[, col_name] > means[col_name] + 3 * sds[col_name])
  
  # winsorize if necessary
  if (length(outlier_indices) > 0) {
    print(paste(col_name, "has", length(outlier_indices),"outlier(s)"))
    # create a new column with the winsorized values
    winsorized_col_name <- paste0(col_name, "_w")
    df_singles[winsorized_col_name] <- df_singles[, col_name]
    df_singles[winsorized_col_name][df_singles[winsorized_col_name] < means[col_name] - 3 * sds[col_name]] <- means[col_name] - 3 * sds[col_name]
    df_singles[winsorized_col_name][df_singles[winsorized_col_name] > means[col_name] + 3 * sds[col_name]] <- means[col_name] + 3 * sds[col_name]
  }
}

# POMP scores for well-being
library(quest)
df_singles <- df_singles %>% 
  mutate(life_satisfaction_p = quest::pomp(life_satisfaction, mini=0, maxi=10, relative = FALSE, unit = 1),
         quality_of_life_p = quest::pomp(quality_of_life, mini=12, maxi=48, relative = FALSE, unit = 1))

