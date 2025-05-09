

## "Neighborhood Treatment Effects Analysis"

### Overview

This R script performs several statistical analyses on neighborhood treatment data and child assessments across cohorts.
1. **Distance Symmetry Check** (Q2.1)

   * Verifies whether the `total_meters` distance measure between origin and destination IDs is symmetric (commutative).
   * Identifies any asymmetrical pairs.

2. **Treatment Variable Interpretation** (Q2.2)

   * Inspects the `pkb2011` variable to infer its meaning.
   * Concludes that `pkb2011` indicates the treatment status of the destination child in 2011.

3. **Cohort and Treatment Summary & Balance** (Q3)

   * Summarizes the number of children by cohort and treatment status.
   * Conducts chi-squared tests for treatment assignment balance in 2010.
   * Uses `tableone` to test balance on baseline covariates (race, gender, language, education).

4. **Treatment Effects on Score Gains** (Q4)

   * Filters 2010/2011 cohorts and defines treatment groups.
   * Computes cognitive (`cog_gain`) and non-cognitive (`ncog_gain`) gains.
   * Runs regressions on raw gains and with demographic controls.
   * Visualizes gain distributions and adjusted residual gains.

5. **Spillover Effects Analysis** (Q8)

   * Loads post-test dataset for 2010/2011.
   * Estimates neighbor spillover effects at multiple distance bands (200m, 500m, 1000m, 2000m).
   * Controls for own treatment, baseline scores, gender, race.
   * Generates line plots of spillover coefficients with confidence intervals and saves as PDF.


