# We use data from the US Census Bureau with the tidycensus and tigris packages,
# but they require personal API keys so we can't make this part super
# reproducible. Instead, we save these ACS and block group datasets as CSV files
# in "data/census/" and load them in the individual question scripts. We include
# the code here for the sake of reproducibility

# Load packages ----

install.packages("pacman")

pacman::p_load(
  "tidyverse",
  "tidycensus",
  "tigris",
  "here",
  install = TRUE
)


# Get ACS data ----

# See a list of variable names
# Also available at https://api.census.gov/data/2020/acs/acs5/variables.html
# acs_possible_vars <- load_variables(2020, "acs5", cache = TRUE)

acs_vars <- tribble(
  ~name,        ~var_name,       ~description,
  "B01003_001", "bg_population",  "Total population",
  "B02001_001", "bg_race_denom",  "Race denominator",
  "B02001_002", "bg_race_white",  "Race: White alone",
  "B19013_001", "bg_income",      "Median household income",
  "B01001_001", "bg_age_denom",   "Age denominator",
  "B01001_002", "bg_male",        "Male",
  "B15003_001", "bg_educ_denom",  "Education denominator",
  "B15003_017", "bg_hs",          "High school",
  "B15003_022", "bg_ba",          "Bachelor's degree",
  "B08134_061", "bg_pub_trans",   "Minutes on public transportation",
  "B08303_001", "bg_travel_time", "Total travel time",
  "B25064_001", "bg_median_rent", "Median rent",
  "B28002_001", "bg_internet_denom", "Internet denominator",
  "B28002_013", "bg_no_internet", "No internet"
)

# Create a named vector to pass to get_acs()
vars_to_get <- acs_vars$name %>% 
  set_names(acs_vars$var_name)

# Get 2019 ACS data
# 2020 would be neat, but ≈20% of it is missing, ugh
# 2020 decennial would be neat too, but it's a huge mess
acs_raw <- get_acs(geography = "block group", 
                   variables = vars_to_get,
                   state = 53, year = 2019, survey = "acs5")

acs_wa <- acs_raw %>% 
  select(-NAME, -moe) %>% 
  # Make the data wide
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  # Calculate a bunch of stuff
  mutate(bg_pct_white = bg_race_white / bg_race_denom,
         bg_pct_male = bg_male / bg_population,
         bg_travel_time_per_capita = bg_travel_time / bg_population,
         bg_pub_trans_per_capita = bg_pub_trans / bg_population,
         bg_pct_hs = bg_hs / bg_educ_denom,
         bg_pct_ba = bg_ba / bg_educ_denom,
         bg_internet = 1 - (bg_no_internet / bg_internet_denom)) %>% 
  select(GEOID, bg_pct_white, bg_pct_male, bg_pct_hs, bg_pct_ba, 
         bg_internet, bg_travel_time_per_capita, bg_pub_trans_per_capita, 
         bg_income, bg_median_rent)


# Block group boundaries ----

wa_bgs <- tigris::block_groups(state = 53, cb = FALSE, year = 2019)


# Save files ----

saveRDS(acs_wa, here("data", "census_data", "acs_wa.rds"))
saveRDS(wa_bgs, here("data", "census_data", "wa_bgs.rds"))
