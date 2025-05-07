# Load necessary libraries
library(tidyverse)
library(modelsummary)


# Import data
df <- read.csv("corrected_distance_treatment_new.csv", stringsAsFactors = FALSE)
checc <- read.csv("all_checc_anon_final.csv", stringsAsFactors = FALSE)

# Preview structure
glimpse(df)
head(df)
tail(df)

# Question 2
# -------------------------------
# 2.1: Check whether the distance is commutative
# -------------------------------

# Create a reverse origin/destination pair
df_reversed <- df %>%
  select(origin_gecc_id, destination_gecc_id, total_meters) %>%
  rename(temp_origin = origin_gecc_id, temp_dest = destination_gecc_id) %>%
  mutate(origin_gecc_id = temp_dest,
         destination_gecc_id = temp_origin) %>%
  select(-temp_origin, -temp_dest)

# merge raw data and reverse data 
df_check <- df %>%
  select(origin_gecc_id, destination_gecc_id, total_meters) %>%
  inner_join(df_reversed, by = c("origin_gecc_id", "destination_gecc_id"),
             suffix = c("_orig", "_rev")) %>%
  mutate(same = abs(total_meters_orig - total_meters_rev) < 1e-6)

# check if symmetry
all(df_check$same)

# View asymmetric pairings
df_counterexample <-df_check %>%
  filter(!same) %>%
  select(origin_gecc_id, destination_gecc_id, total_meters_orig, total_meters_rev) %>%
  arrange(origin_gecc_id, destination_gecc_id) %>%
  head(10)

#Conclusion: distance is not strictly symmetric in this data (non-commutative)

datasummary_df(df_counterexample, output = "latex")



# -------------------------------
# 2.2: infer the meaning of 'pkb2011'
# -------------------------------

unique(df$pkb2011)
table(df$pkb2011, useNA = "ifany")

pkb_check <- df %>%
  group_by(destination_gecc_id) %>%
  summarise(n_unique = n_distinct(pkb2011)) %>%
  filter(n_unique > 1)

print("Rows where destination child has more than one unique pkb2011 value:")
print(pkb_check)


# conclusion：The variable pkb2011 refers to the treatment status of the destination child in 2011.


# Question 3：
# -------------------------------
# 3.1: Summarize number of children by cohort and treatment
# -------------------------------

# Preview structure
glimpse(checc)
head(checc)
tail(checc)

# Create cohort-treatment data in long format
library(tidyverse)
#install.packages("kableExtra")
library(kableExtra)

# Convert to long format for treatment across years
treatment_long <- checc %>%
  select(child, starts_with("treatment20")) %>%
  pivot_longer(cols = starts_with("treatment20"),
               names_to = "year",
               values_to = "treatment") %>%
  filter(treatment != "") %>%
  mutate(cohort = as.integer(str_extract(year, "\\d{4}")) - 2009)

# Create frequency table
cohort_table <- treatment_long %>%
  group_by(cohort, treatment) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = treatment, values_from = n, values_fill = 0)

# Output LaTeX table
kbl(cohort_table,
    caption = "Table 1: Number of Children by Cohort and Treatment Status",
    format = "latex", booktabs = TRUE, longtable = TRUE,
    label = "tab:cohort_treatment") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

print(cohort_table)

# -------------------------------
# 3.2: Conduct a balance test for the year 2010.
# -------------------------------


# balance of randomization of treatment


test1 <- chisq.test( x = c(75,78,87,85), p = c(0.25, 0.25, 0.25, 0.25))
# reject the hypothesis that treatments and control are balanced

chisq_df1 <- data.frame(
  Statistic = round(test1$statistic, 3),
  DF        = test1$parameter,
  `P-value` = signif(test1$p.value, 3)
)
knitr::kable(chisq_df1, format = "latex", caption = "Chi-Squared Test Results")

test2 <- chisq.test( x = c(75,78,242,87,85), p = c(0.125, 0.125, 0.5, 0.125, 0.125))
# fail to reject the hypothesis that treatements are assigned with different probabilities

chisq_df2 <- data.frame(
  Statistic = round(test2$statistic, 3),
  DF        = test2$parameter,
  `P-value` = signif(test2$p.value, 3)
)
knitr::kable(chisq_df2, format = "latex", caption = "Chi-Squared Test")


# balance on covariates

# Load necessary libraries
#install.packages("tableone")
library(tidyverse)
library(tableone)
library(xtable)

# Filter for children with treatment info in 2010
checc_2010 <- checc %>%
  filter(treatment2010 != "")

# Define covariates to test for balance
vars <- c("black", "white", "hispanic", "gender", "home_language", "education")

# Convert categorical variables to factors
checc_2010$gender <- as.factor(checc_2010$gender)
checc_2010$home_language <- as.factor(checc_2010$home_language)
checc_2010$education <- as.factor(checc_2010$education)

# Generate the balance table by treatment group
tab <- CreateTableOne(vars = vars, strata = "treatment2010", data = checc_2010, factorVars = vars)

# Print results with statistical tests

tab_df <- print(tab, method = "latex", showAllLevels = TRUE, test = TRUE)
xtable(tab_df)

# I conducted a balance test on baseline demographic characteristics using the 
#CreateTableOne() function from the tableone package. included gender, race
#(Black, White, Hispanic), home language, and parental education as covariates. 
#Each variable was tested across treatment groups for the 2010 cohort. 
#All p-values were well above 0.05, suggesting no statistically significant differences 
#across treatment arms. This implies that the randomization procedure successfully
#created balanced groups in 2010.






# -------------------------------
# Question4
# -------------------------------
library(tidyverse)
library(fixest)


# Filter to only 2010 and 2011 cohorts and define treatment group
checc_filtered <- checc %>%
  mutate(cohort = case_when(
    treatment2010 != "" ~ 2010,
    treatment2011 != "" ~ 2011,
    TRUE ~ NA_integer_
  )) %>%
  filter(cohort %in% c(2010, 2011)) %>%
  mutate(treatment = ifelse(cohort == 2010, treatment2010, treatment2011),
         treatment_group = ifelse(treatment == "kinderprep", "control", treatment))

# Extract relevant test scores based on cohort timing
checc_scores <- checc_filtered %>%
  mutate(
    cog_mid = case_when(
      cohort == 2010 ~ cog2011_01,
      cohort == 2011 ~ cog2012_01
    ),
    cog_post = case_when(
      cohort == 2010 ~ cog2011_05,
      cohort == 2011 ~ cog2012_05
    ),
    ncog_mid = case_when(
      cohort == 2010 ~ ncog2011_01,
      cohort == 2011 ~ ncog2012_01
    ),
    ncog_post = case_when(
      cohort == 2010 ~ ncog2011_05,
      cohort == 2011 ~ ncog2012_05
    )
  ) %>%
  select(child, cohort, treatment_group, cog_pre, cog_mid, cog_post,
         ncog_pre, ncog_mid, ncog_post) %>%
  drop_na(cog_pre, cog_mid, cog_post, ncog_pre, ncog_mid, ncog_post)

# Cognitive gains: post - pre
checc_scores <- checc_scores %>%
  mutate(cog_gain = cog_post - cog_pre,
         ncog_gain = ncog_post - ncog_pre)


# Run linear regressions to estimate treatment effects on gains
model_cog <- lm(cog_gain ~ treatment_group, data = checc_scores)
model_ncog <- lm(ncog_gain ~ treatment_group, data = checc_scores)

summary(model_cog)
summary(model_ncog)

library(ggplot2)

long_scores <- checc_scores %>%
  select(treatment_group, cog_gain, ncog_gain) %>%
  pivot_longer(cols = c(cog_gain, ncog_gain),
               names_to = "score_type",
               values_to = "gain") %>%
  mutate(score_type = recode(score_type,
                             "cog_gain" = "Cognitive Gain",
                             "ncog_gain" = "Non-Cognitive Gain"))

#Boxplot of cognitive and non-cognitive gain distributions

ggplot(long_scores, aes(x = treatment_group, y = gain, fill = score_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Score Gains by Treatment Group",
       x = "Treatment Group",
       y = "Gain",
       fill = "Score Type") +
  theme_minimal(base_size = 14)



checc_scores <- checc_filtered %>%
  mutate(
    cog_mid = case_when(
      cohort == 2010 ~ cog2011_01,
      cohort == 2011 ~ cog2012_01
    ),
    cog_post = case_when(
      cohort == 2010 ~ cog2011_05,
      cohort == 2011 ~ cog2012_05
    ),
    ncog_mid = case_when(
      cohort == 2010 ~ ncog2011_01,
      cohort == 2011 ~ ncog2012_01
    ),
    ncog_post = case_when(
      cohort == 2010 ~ ncog2011_05,
      cohort == 2011 ~ ncog2012_05
    )
  ) %>%
  select(child, cohort, treatment_group, cog_pre, cog_mid, cog_post,
         ncog_pre, ncog_mid, ncog_post, gender, black, white, hispanic, education) %>%
  drop_na(cog_pre, cog_mid, cog_post, ncog_pre, ncog_mid, ncog_post,
          gender, black, white, hispanic, education)

checc_scores <- checc_scores %>%
  mutate(
    gender = as.factor(gender),
    education = as.factor(education),
    black = as.numeric(black),
    white = as.numeric(white),
    hispanic = as.numeric(hispanic),
    treatment_group = relevel(as.factor(treatment_group), ref = "control")
    )


# Updated regression (with control)
model_cog_cov <- lm(cog_gain - cog_pre ~ treatment_group + gender + black + white + hispanic + education, data = checc_scores)
model_ncog_cov <- lm(ncog_post - ncog_pre ~ treatment_group + gender + black + white + hispanic + education, data = checc_scores)

# Run linear regressions to estimate treatment effects on post
model_cog_cl <- feols(cog_post ~ treatment_group  + cog_pre + ncog_pre + black + white + gender + education, data = checc_scores, cluster = ~child)
model_ncog_cl <- feols(ncog_post ~ treatment_group + cog_pre + ncog_pre + black + white + gender + education, data = checc_scores, cluster = ~child)


summary(model_cog_cov)
summary(model_ncog_cov)

summary(model_cog_cl)
summary(model_ncog_cl)


modelsummary(
  list("Cog Gain" = model_cog_cov,"NCog Gain" = model_ncog_cov,"Cog Post" = model_cog_cl,"NCog Post" = model_ncog_cl ),
  output = "latex",
  stars = TRUE,
  title = "Effect of treatments on scores"
)





library(broom)
library(ggplot2)
library(dplyr)

tidy_cog <- tidy(model_cog_cov, conf.int = TRUE) %>%
  mutate(model = "Cognitive Gain")

tidy_ncog <- tidy(model_ncog_cov, conf.int = TRUE) %>%
  mutate(model = "Non-Cognitive Gain")

tidy_all <- bind_rows(tidy_cog, tidy_ncog) %>%
  filter(term != "(Intercept)") %>% 
  mutate(term = gsub("treatment_group", "", term)) 

# # Get cognitive gain and non-cognitive gainn
checc_scores <- checc_scores %>%
  mutate(
    cog_gain = cog_post - cog_pre,
    ncog_gain = ncog_post - ncog_pre
  )

library(broom)

# Control covariates and extract residuals
cog_resid <- augment(lm(cog_gain ~ gender + black + white + hispanic + education, data = checc_scores))
ncog_resid <- augment(lm(ncog_gain ~ gender + black + white + hispanic + education, data = checc_scores))

# Add residual back to checc scores
checc_scores$cog_resid <- cog_resid$.resid
checc_scores$ncog_resid <- ncog_resid$.resid

long_resid <- checc_scores %>%
  select(treatment_group, cog_resid, ncog_resid) %>%
  pivot_longer(cols = c(cog_resid, ncog_resid),
               names_to = "score_type",
               values_to = "gain") %>%
  mutate(score_type = recode(score_type,
                             "cog_resid" = "Cognitive Gain (Adjusted)",
                             "ncog_resid" = "Non-Cognitive Gain (Adjusted)"))

ggplot(long_resid, aes(x = treatment_group, y = gain, fill = score_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Adjusted Score Gains by Treatment Group",
       x = "Treatment Group",
       y = "Adjusted Gain (Residuals)",
       fill = "Score Type") +
  theme_minimal(base_size = 14)


# In this analysis, I ran linear regressions to estimate the effects of 
# different treatments on children’s cognitive and non-cognitive skill gains.
# I first measured raw gains by subtracting pre-test scores from post-test scores and
# regressed these gains on the treatment group indicators. 
# Then, I added controls for gender, race/ethnicity (Black, White, Hispanic), 
# and parental education to account for potential confounding factors. 
# I also visualized the adjusted score gains by extracting residuals from covariate-controlled
# models to better isolate treatment effects.
# I also ran regression of post-treatment scores on the treatment and pre-treatment scores.

# Interpretation.
# The results overall are inconclussive as treatement effects estimates cannot be
# statistically distinguished from zero.
# Only pre-treatment test scores are significant predictors of post-treatment test scores.
# The results indicate that children in the PKB group experienced greater
# cognitive gains compared to the baseline group and other treatment groups,
# and this effect remained robust even after controlling for demographic covariates.
# In contrast, non-cognitive gains were not significantly different across most groups, 
# although the control group showed marginally higher non-cognitive gains before 
# covariate adjustment. These findings suggest that the PKB intervention may have had
# a stronger and more consistent impact on cognitive development, while non-cognitive
# outcomes appear more variable and less clearly associated with treatment.


#Question 5,6,7 TBD


















#Due to time constraints, I used the provided dataset question_8_data.dta for analysis in Question 8.


#Question 8

# Read in the final dataset
Q8 <- read.csv("/Users/riza/desktop/question_8_data_final.csv", stringsAsFactors = FALSE)
head(Q8)


# Load necessary libraries
library(tidyverse)
library(broom)
library(ggplot2)




# Keep only years 2010 and 2011
q8_data <- Q8 %>%
  filter(year %in% c(2010, 2011))

# Step 3: Filter to *post-test* observations only 
q8_post <- q8_data %>%
  filter(test == "post")

# Prepare modeling dataset
model_data <- q8_post %>%
  select(child, year, std_cog, std_ncog, treatment2, 
         parent_1000_new, pk_1000_new, control_1000_new,
         gender, race, baseline_stdcog, baseline_stdncog)

# Run regression
# Outcome: std_cog (post cognitive test)
# Predictors: treatment group, neighbor treatment counts, baseline score, demographics
model1 <- lm(std_cog ~ treatment2 + parent_1000_new + pk_1000_new + control_1000_new +
               baseline_stdcog + gender + race, data = model_data)

summary(model1)


library(broom)
library(ggplot2)
library(dplyr)
library(stringr)

# Visualize coefficients for spillover terms 
tidy(model1) %>%
  filter(str_detect(term, "_1000_new")) %>%
  mutate(term_clean = case_when(
    str_detect(term, "pk_1000_new") ~ "Pre-K Neighbor (PK)",
    str_detect(term, "parent_1000_new") ~ "Parenting Program Neighbor",
    str_detect(term, "control_1000_new") ~ "Control Group Neighbor"
  )) %>%
  ggplot(aes(x = fct_reorder(term_clean, estimate), y = estimate)) +
  geom_point(size = 4, color = "#0072B2") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), 
                width = 0.2, color = "#0072B2") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Spillover Effects from Neighbor Treatments (within 1000m)",
       x = "Neighbor Treatment Type",
       y = "Effect on Cognitive Score (std_cog)") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


tidy(model1) %>%
  filter(str_detect(term, "_1000_new")) %>%
  mutate(term_clean = case_when(
    str_detect(term, "pk_1000_new") ~ "Pre-K Neighbor (PK)",
    str_detect(term, "parent_1000_new") ~ "Parenting Program Neighbor",
    str_detect(term, "control_1000_new") ~ "Control Group Neighbor"
  )) %>%
  ggplot(aes(x = estimate, y = fct_reorder(term_clean, estimate))) +
  geom_col(fill = "#56B4E9") +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                     xmax = estimate + 1.96 * std.error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Spillover Effects of Neighbor Treatment (1000m)",
       x = "Effect on std_cog",
       y = "") +
  theme_minimal(base_size = 14)

tidy(model1) %>%
  filter(str_detect(term, "_new")) %>%
  separate(term, into = c("treatment", "distance", "new"), sep = "_") %>%
  mutate(treatment = case_when(
    treatment == "pk" ~ "Pre-K",
    treatment == "parent" ~ "Parent",
    treatment == "control" ~ "Control"
  ),
  distance = paste0(distance, "m")) %>%
  ggplot(aes(x = distance, y = estimate, color = treatment, group = treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Spillover Effects Across Distances",
       x = "Distance to Treated Neighbor",
       y = "Effect on std_cog",
       color = "Neighbor Treatment Type") +
  theme_minimal(base_size = 14)


# Load required libraries
library(tidyverse)
library(broom)
library(forcats)

# Filter for post-test data in 2010 and 2011
q8_post <- Q8 %>% 
  filter(year %in% c(2010, 2011), test == "post")

# Define the distances of interest
distances <- c(200, 500, 1000, 2000)

# Function to run regression for a given distance
run_spillover_reg <- function(dist) {
  
  # Construct variable names for neighbor treatments at given distance
  parent_var   <- paste0("parent_", dist, "_new")
  pk_var       <- paste0("pk_", dist, "_new")
  control_var  <- paste0("control_", dist, "_new")
  
  # Create formula string
  formula_str <- paste(
    "std_cog ~ treatment2 +", 
    parent_var, "+", pk_var, "+", control_var, 
    "+ baseline_stdcog + gender + race"
  )
  
  # Convert to formula object
  fml <- as.formula(formula_str)
  
  # Run regression and tidy results
  model <- lm(fml, data = q8_post)
  
  # Return tidy output with distance info
  broom::tidy(model) %>%
    mutate(distance = dist)
}

# Run regressions for all distances and combine results
spillover_all_distances <- map_dfr(distances, run_spillover_reg)

# Filter to keep only neighbor treatment terms
spillover_terms <- spillover_all_distances %>%
  filter(str_detect(term, "_new"))

# Prepare data for plotting
spillover_plot_data <- spillover_terms %>%
  mutate(
    treat_type = str_extract(term, "^[^_]+")  # Extract "parent", "pk", or "control"
  )

# Plot the spillover effects across distances
p_spillover <- ggplot(spillover_plot_data, 
       aes(x = factor(distance), y = estimate, color = treat_type, group = treat_type)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Spillover Effects by Distance",
       x = "Distance (m)",
       y = "Effect on Cognitive Score (std_cog)",
       color = "Neighbor Treatment") +
  theme_minimal(base_size = 14)
ggsave("spilloverEffects.pdf", plot = p_spillover, width = 6, height = 4)


#In this question, I examined whether a child’s cognitive outcomes were indirectly 
#influenced by living near neighbors who received different treatments—either the
#Parenting program, Pre-K, or were in the Control group. I used post-test data from 
#2010 and 2011 and ran separate regressions at multiple distance bands (200m, 500m, 1000m, 
#2000m), controlling for the child’s own treatment status, baseline cognitive scores, 
#gender, and race. The regressions included variables counting how many nearby neighbors
#received each type of treatment within the specified distance. I then visualized the 
#estimated spillover effects by distance and treatment type.

#The resulting line plot shows that the spillover effects from treated neighbors are
#consistently close to zero across all distances and treatment types, with confidence 
#intervals crossing zero throughout. This suggests that there is no statistically 
#significant evidence of spillover on untreated children's cognitive outcomes, even
#when neighbors received beneficial interventions. Any potential effect appears minimal
#and localized, fading beyond short distances. Overall, the findings imply that the 
#cognitive benefits of the program are likely concentrated among direct participants, 
#with little to no indirect impact on nearby peers.




