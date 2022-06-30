# Options ----

# By default, R uses polynomial contrasts for ordered factors in linear models:
# > options("contrasts") 
# So we make ordered factors use treatment contrasts instead
options(contrasts = rep("contr.treatment", 2),
        dplyr.summarise.inform = FALSE)


# Load packages ----

if (!require("pacman")) {
  install.packages("pacman", dependencies = TRUE)
}

pacman::p_load(
  "tidyverse",
  "lubridate",
  "WeightIt",
  "broom",
  "broom.mixed",
  "lme4",
  "marginaleffects",
  "ggokabeito",
  "ggmosaic",
  "scales",
  "here",
  install = TRUE
)

source(here("R", "graphics.R"))


# Clean data ----

## Load data ----

lift_boardings1_raw <- read_csv(here("data", "raw_data_from_king_county", 
                                     "Question 2_ Fare Subsidies", "LIFT_boardings.csv"))

lift_boardings2_raw <- read_csv(here("data", "raw_data_from_king_county", 
                                     "Question 2_ Fare Subsidies", 
                                     "LIFT_boardings_2021-11-01_to_2022-03-06.csv"))

lift_sales_raw <- read_csv(here("data", "raw_data_from_king_county", 
                                "Question 2_ Fare Subsidies", "LIFT_sales_2022-04-01.csv"))

lift_registry_raw <- read_csv(here("data", "raw_data_from_king_county", 
                                   "Question 2_ Fare Subsidies", "LIFT_registry_2022-04-01.csv"))

# Census data; this was originally created using R/get_census_data.R, but isn't
# run as part of the overall project because it requires an API key
acs_wa <- readRDS(here("data", "census_data", "acs_wa.rds"))
wa_bgs <- readRDS(here("data", "census_data", "wa_bgs.rds"))

incentives <- c("0", "10", "15", "20", "30", "50", "70", "Misc. Pass", 
                "Monthly Pass", "Passport", "Subsidized Annual Pass")
incentives_labs <- c("$0", "$10", "$15", "$20", "$30", "$50", "$70", "Misc. Pass", 
                     "Monthly Pass", "Passport", "Subsidized Annual Pass")

## Clean rider data ----

riders_clean <- lift_registry_raw %>% 
  # Split id column to get enrollment count
  mutate(card_id_orig = card_id) %>% 
  separate(card_id, into = c("id", "times"), sep = "-") %>% 
  # Clean up column types
  mutate(across(c(id, times), as.integer)) %>% 
  mutate(FIPS = as.character(FIPS)) %>% 
  # This date is a typo
  mutate(Expiration = ifelse(Expiration == "00534984", NA, Expiration)) %>% 
  # Make the expiration date an actual date
  mutate(Expiration = mdy(Expiration)) %>%
  # Exclude rows where the expiration date is before the issued date
  filter(Expiration > DateIssued) %>% 
  # Sometimes the same person gets enrolled twice in one day (like 2716-3)
  # Only keep the last row of duplicate issue dates for individuals
  group_by(id, DateIssued) %>% 
  slice_tail() %>% 
  # Renumber the times column
  # group_by(id) %>% 
  # mutate(times = 1:n()) %>% 
  # Get rid of extra columns
  ungroup() %>% 
  select(-duplicate) %>% 
  # Sort
  arrange(id, times)


## Clean sales data ----

sales <- lift_sales_raw %>% 
  group_by(week, card_id) %>% 
  summarize(total_amount = sum(Amount),
            total_loadings = sum(loadings)) %>% 
  ungroup()


## Clean boardings data ----

boardings <- lift_boardings1_raw %>%
  bind_rows(lift_boardings2_raw) %>% 
  select(week, card_id, 
         boardings_king_county = `King County Metro`,
         boardings_sound_transit = `Sound Transit`) %>% 
  mutate(card_id_orig = card_id) %>% 
  separate(card_id, into = c("id", "times"), sep = "-") %>% 
  mutate(across(c(id, times), as.integer)) %>% 
  mutate(across(starts_with("boardings"), ~replace_na(., 0)))


## Merge data ----

# Make a smaller dataset of just IDs and enrollment dates and then make a column
# of enrollment dates + 180 days - we'll use this to process and collapse the
# sales and boarding data
rider_enrollment_dates <- riders_clean %>% 
  select(id, times, DateIssued) %>% 
  mutate(six_months_later = DateIssued + days(180))

# Calculate how much money riders put on cards + frequency of refills in the 0-6
# and 6+ months after getting issued a card
sales_before_after_six_months <- sales %>%
  # Split id column to get enrollment count
  mutate(card_id_orig = card_id) %>% 
  separate(card_id, into = c("id", "times"), sep = "-") %>% 
  # Clean up column types
  mutate(across(c(id, times), as.integer)) %>% 
  # Bring in rider dates
  left_join(rider_enrollment_dates, by = c("id", "times")) %>% 
  # Get rid of rider/time combinations that don't exist
  filter(!is.na(DateIssued)) %>% 
  # Create indicator for whether each sale is 6+ months after the initial care issue
  mutate(after_six = (week - days(6)) > six_months_later,
         after_six = ifelse(after_six, "after_six", "before_six")) %>% 
  # Get total of money loaded and frequency of loading before/after 6+ months
  group_by(id, times, after_six) %>% 
  summarize(across(c(total_amount, total_loadings), ~sum(.))) %>% 
  ungroup() %>% 
  # Make wide
  pivot_wider(names_from = after_six, values_from = c(total_amount, total_loadings)) %>% 
  mutate(load_after_six = total_amount_after_six > 0)

boardings_before_after_six_months <- boardings %>%
  # Bring in rider dates
  left_join(rider_enrollment_dates, by = c("id", "times")) %>% 
  # Get rid of rider/time combinations that don't exist
  filter(!is.na(DateIssued)) %>% 
  # Create indicator for whether each sale is 6+ months after the initial care issue
  mutate(after_six = (week - days(6)) > six_months_later,
         after_six = ifelse(after_six, "after_six", "before_six")) %>% 
  # Get total boardings before/after 6+ months
  group_by(id, times, after_six) %>% 
  summarize(across(starts_with("boardings_"), ~sum(.))) %>% 
  ungroup() %>% 
  # Make wide
  pivot_wider(names_from = after_six, values_from = starts_with("boardings_")) 

riders_final <- riders_clean %>% 
  # Create treatment variables
  # Consider NA initial loads to be 0
  replace_na(list(`Initial Load` = "0")) %>% 
  mutate(incentive_cat = factor(`Initial Load`, levels = incentives, 
                                labels = incentives_labs, ordered = TRUE),
         treatment_passport_binary = !is.na(`Study Card`),
         treatment_sap_binary = incentive_cat == "Subsidized Annual Pass") %>%
  group_by(id) %>% 
  mutate(incentive_cat_collapsed = fct_collapse(
    incentive_cat,
    ">$10" = c("$15", "$20", "$30", "$50", "$70"),
    "Shorter Pass" = c("Misc. Pass", "Monthly Pass", "Passport"))) %>% 
  mutate(across(c(incentive_cat, incentive_cat_collapsed, treatment_passport_binary, treatment_sap_binary), 
                ~lag(.), .names = "{.col}_prev")) %>% 
  #
  # Create outcome variables
  # Bring in collapsed sales data for reloading outcome
  left_join(sales_before_after_six_months, by = c("id", "times")) %>% 
  # Create reenrollment outcomes
  group_by(id) %>% 
  mutate(ever_reenroll = any(times > 1),
         reenrolled = times > 1,
         enrolled_previously = n() != 1,
         reenroll_after_study = treatment_passport_binary_prev & ever_reenroll) %>% 
  ungroup() %>% 
  #
  # Create controls and confounders
  # Bring in collapsed boarding data for ride use history
  left_join(boardings_before_after_six_months, by = c("id", "times")) %>% 
  # Bring in census data
  left_join(acs_wa, by = c("FIPS" = "GEOID")) %>% 
  #
  # Final data cleaning
  # Replace NAs with actual data
  mutate(across(c(starts_with("total_amount"), starts_with("total_loadings"),
                  starts_with("boardings_")), 
                ~replace_na(., 0))) %>% 
  # Add "Nothing" as a category
  mutate(across(starts_with("incentive_cat"), ~fct_expand(.x, "Nothing"))) %>%
  replace_na(list(load_after_six = FALSE, treatment_passport_binary_prev = FALSE,
                  treatment_sap_binary_prev = FALSE,
                  incentive_cat_prev = "Nothing", incentive_cat_collapsed_prev = "Nothing")) %>% 
  # Put "Nothing" first so it's the reference category
  mutate(across(starts_with("incentive_cat"), ~fct_relevel(.x, "Nothing"))) %>% 
  # Get rid of "Nothing" if there aren't any
  mutate(across(starts_with("incentive_cat"), ~fct_drop(.x, only = "Nothing")))

# Only look at rows starting on March 13, 2019, since that's when the study
# formally began
riders_final_2019 <- riders_final %>% 
  filter(DateIssued >= ymd("2019-03-13"))

# Data to use in models, with NAs omitted
riders_model <- riders_final_2019 %>% 
  select(load_after_six, total_amount_after_six, total_loadings_after_six, 
         reenrolled, treatment_passport_binary, treatment_passport_binary,
         incentive_cat_collapsed, incentive_cat_collapsed_prev, treatment_sap_binary,
         total_amount_before_six, total_loadings_before_six, enrolled_previously,
         boardings_king_county_before_six, boardings_sound_transit_before_six,
         Age, RaceDesc, LanguageSpoken, id, 
         bg_pct_white, bg_pct_male, bg_pct_hs, bg_pct_ba, 
         bg_internet, bg_travel_time_per_capita, bg_pub_trans_per_capita, 
         bg_income, bg_median_rent) %>% 
  na.omit()


# Q2~A~: Effect of different levels of incentives on longer-term loading of value and passes ----

# Outcomes representing long term loading: 
#   
# - `load_after_six`: Binary indicator for whether they reloaded the card 6+ months after card is issued
# - `total_amount_after_six`: Amount of money refilled 6+ months after card is issued
# - `total_loadings_after_six`: Count of refills 6+ months after card is issued
# 
# Treatment: 
#   
#   - `incentive_cat_collapsed`: Categorical variable showing the kind of incentive each person was given with the card, if any. Possible values are 0, 10, 10+ (15, 20, 30, 50, 70), Shorter Pass (Misc. Pass, Monthly Pass, Passport), and Subsidized Annual Pass; the values are ordered based on their intensity

## Inverse probability weights ----

incentive_weights <- weightit(
  incentive_cat_collapsed_prev ~ Age + RaceDesc + LanguageSpoken + 
    total_amount_before_six + total_loadings_before_six + enrolled_previously +
    boardings_king_county_before_six + boardings_sound_transit_before_six +
    bg_pct_white + bg_pct_male + bg_pct_hs + bg_pct_ba + bg_internet + 
    bg_travel_time_per_capita + bg_pub_trans_per_capita + bg_income + bg_median_rent,
  data = riders_model, estimand = "ATE", method = "ps")

# Add weights as column in data and truncate big ones
riders_model_with_weights <- riders_model %>%
  mutate(ipw = incentive_weights$weights) %>% 
  mutate(ipw = ifelse(ipw >= 30, 30, ipw))

passport_weights <- weightit(
  treatment_passport_binary ~ Age + RaceDesc + LanguageSpoken + 
    total_amount_before_six + total_loadings_before_six + enrolled_previously +
    boardings_king_county_before_six + boardings_sound_transit_before_six +
    bg_pct_white + bg_pct_male + bg_pct_hs + bg_pct_ba + bg_internet + 
    bg_travel_time_per_capita + bg_pub_trans_per_capita + bg_income + bg_median_rent,
  data = riders_model, estimand = "ATE", method = "ps")

# Add weights as column in data and truncate big ones
passport_model_with_weights <- riders_model %>% 
  mutate(ipw = passport_weights$weights) %>% 
  mutate(ipw = ifelse(ipw >= 30, 30, ipw))


## Reloading after six months ----

### Look at distribution ----

plot_load_after_six <- ggplot(riders_model) +
  geom_mosaic(aes(x = product(incentive_cat_collapsed_prev), fill = load_after_six), alpha = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_okabe_ito() +
  labs(x = "Subsidy provided", y = "Proportion", fill = "Reloaded 6+ months later") +
  theme_kc() +
  theme(legend.position = "top") +
  coord_flip()

ggsave(here("output", "eda_q2_mosaic_load_after_six.png"), plot_load_after_six,
       width = 6, height = 6)
ggsave(here("output", "eda_q2_mosaic_load_after_six.pdf"), plot_load_after_six,
       width = 6, height = 6)


### Model ----

model_q2a_1 <- glmer(load_after_six ~ incentive_cat_collapsed_prev + (1 | id),
                     family = binomial(link = "logit"),
                     nAGQ = 0,  # Speed up estimation by skipping some integration
                     control = glmerControl(optimizer = "nloptwrap"),  # Faster optimizer
                     data = riders_model_with_weights, weights = ipw)

model_q2a_1_cmp <- comparisons(model_q2a_1, 
                               variables = "incentive_cat_collapsed_prev", 
                               contrast_factor = "reference")
tidy(model_q2a_1_cmp)

plot_model_q2a_1_cmp <- model_q2a_1_cmp %>% 
  tidy() %>% 
  mutate(contrast = fct_rev(fct_inorder(contrast))) %>% 
  ggplot(aes(x = estimate * 100, y = contrast, color = contrast)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low * 100, xmax = conf.high * 100)) +
  scale_color_okabe_ito(guide = "none") +
  scale_x_continuous(labels = ~paste0(.x, " pp")) +
  labs(x = "Percentage point change", y = NULL) +
  theme_kc()

ggsave(here("output", "model_q2a_1_comparisons.png"), plot_model_q2a_1_cmp,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q2a_1_comparisons.pdf"), plot_model_q2a_1_cmp,
       width = 6, height = (6 * 0.618))


## Total amount refilled after 6 months ----

### Look at averages ----

plot_refill_amount <- ggplot(riders_model, 
                             aes(x = incentive_cat_collapsed_prev, 
                                 y = total_amount_after_six, 
                                 color = incentive_cat_collapsed_prev)) +
  geom_point(size = 0.2, alpha = 0.25, 
             position = position_jitter(width = 0.25, seed = 1234)) +
  scale_color_okabe_ito(guide = "none") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = "Incentive categories", y = "Total amount loaded\nafter 6 months") +
  theme_kc()

ggsave(here("output", "eda_q2_refill_amount.png"), plot_refill_amount,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "eda_q2_refill_amount.pdf"), plot_refill_amount,
       width = 6, height = (6 * 0.618))

plot_refill_amount_avg <- ggplot(riders_model, 
                                 aes(x = incentive_cat_collapsed_prev, 
                                     y = total_amount_after_six, 
                                     color = incentive_cat_collapsed_prev)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  scale_color_okabe_ito(guide = "none") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = "Incentive categories", y = "Average total amount loaded\nafter 6 months") +
  theme_kc()

ggsave(here("output", "eda_q2_refill_amount_avg.png"), plot_refill_amount_avg,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "eda_q2_refill_amount_avg.pdf"), plot_refill_amount_avg,
       width = 6, height = (6 * 0.618))


### Model ----

model_q2a_2 <- lmer(total_amount_after_six ~ incentive_cat_collapsed_prev + (1 | id),
                    data = riders_model_with_weights, weights = ipw)

model_q2a_2_cmp <- comparisons(model_q2a_2, 
                               variables = "incentive_cat_collapsed_prev", 
                               contrast_factor = "reference")

plot_model_q2a_2_cmp <- model_q2a_2_cmp %>% 
  tidy() %>% 
  mutate(contrast = fct_rev(fct_inorder(contrast))) %>% 
  ggplot(aes(x = estimate, y = contrast, color = contrast)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_color_okabe_ito(guide = "none") +
  scale_x_continuous(labels = label_dollar()) +
  labs(x = "Difference in total amount loaded six months later", y = NULL) +
  theme_kc()

ggsave(here("output", "model_q2a_2_comparisons.png"), plot_model_q2a_2_cmp,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q2a_2_comparisons.pdf"), plot_model_q2a_2_cmp,
       width = 6, height = (6 * 0.618))


## Total loadings after 6 months ----

### Model ----

model_q2a_3 <- lmer(total_loadings_after_six ~ incentive_cat_collapsed_prev + (1 | id),
                    data = riders_model_with_weights, weights = ipw)

model_q2a_3_cmp <- comparisons(model_q2a_3, 
                               variables = "incentive_cat_collapsed_prev", 
                               contrast_factor = "reference")

plot_model_q2a_3_cmp <- model_q2a_3_cmp %>% 
  tidy() %>% 
  mutate(contrast = fct_rev(fct_inorder(contrast))) %>% 
  ggplot(aes(x = estimate, y = contrast, color = contrast)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_color_okabe_ito(guide = "none") +
  labs(x = "Difference in average count of card loadings", y = NULL) +
  theme_kc()

ggsave(here("output", "model_q2a_3_comparisons.png"), plot_model_q2a_3_cmp,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q2a_3_comparisons.pdf"), plot_model_q2a_3_cmp,
       width = 6, height = (6 * 0.618))


# Q2~B~: Effect of different levels of incentives on longer-term loading of value and passes ----

# Outcome: 
#   
# - `reenrolled`: Binary indicator for whether current card issuing is a reenrollment (TRUE when the suffix for the card ID is greater than 1)


## Look at distribution ----

ggplot(riders_model) +
  geom_mosaic(aes(x = product(incentive_cat_collapsed_prev), fill = reenrolled), alpha = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_okabe_ito() +
  labs(x = "Subsidy provided", y = "Proportion", fill = "Reenrolled") +
  theme_kc() +
  theme(legend.position = "top") +
  coord_flip()


## Model ----

model_q2b <- glmer(reenrolled ~ incentive_cat_collapsed_prev + (1 | id),
                   family = binomial(link = "logit"),
                   nAGQ = 0,  # Speed up estimation by skipping some integration
                   control = glmerControl(optimizer = "nloptwrap"),  # Faster optimizer
                   data = riders_model_with_weights, weights = ipw)

model_q2b_cmp <- comparisons(model_q2b, 
                             variables = "incentive_cat_collapsed_prev", 
                             contrast_factor = "reference")

plot_model_q2b_cmp <- model_q2b_cmp %>% 
  tidy() %>% 
  mutate(contrast = fct_rev(fct_inorder(contrast))) %>% 
  ggplot(aes(x = estimate * 100, y = contrast, color = contrast)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low * 100, xmax = conf.high * 100)) +
  scale_color_okabe_ito(guide = "none") +
  scale_x_continuous(labels = ~paste0(.x, " pp")) +
  labs(x = "Percentage point change", y = NULL) +
  theme_kc()

ggsave(here("output", "model_q2b_comparisons.png"), plot_model_q2b_cmp,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q2b_comparisons.pdf"), plot_model_q2b_cmp,
       width = 6, height = (6 * 0.618))

# lolwut I have no idea what's going on here


# Q2~C~: Effect of different levels of incentives on longer-term loading of value and passes ----

# Outcome: 
#   
# - `reenrolled`: Binary indicator for whether current card issuing is a reenrollment (TRUE when the suffix for the card ID is greater than 1)

## Look at distribution ----

ggplot(passport_model_with_weights) +
  geom_mosaic(aes(x = product(treatment_passport_binary), fill = reenrolled), alpha = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_okabe_ito() +
  labs(x = "Subsidy provided", y = "Proportion", fill = "Reenrolled") +
  theme_kc() +
  theme(legend.position = "top") +
  coord_flip()

## Model ----

model_q2c_4 <- glmer(reenrolled ~ treatment_passport_binary + (1 | id),
                     family = binomial(link = "logit"),
                     nAGQ = 0,  # Speed up estimation by skipping some integration
                     control = glmerControl(optimizer = "nloptwrap"),  # Faster optimizer
                     data = passport_model_with_weights, weights = ipw)

model_q2c_4_cmp <- comparisons(model_q2c_4, 
                               variables = "treatment_passport_binary", 
                               contrast_factor = "reference")

plot_model_q2c_4_cmp <- model_q2c_4_cmp %>% 
  tidy() %>% 
  mutate(contrast = fct_rev(fct_inorder(contrast))) %>% 
  ggplot(aes(x = estimate * 100, y = contrast, color = contrast)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low * 100, xmax = conf.high * 100)) +
  scale_color_okabe_ito(guide = "none") +
  scale_x_continuous(labels = ~paste0(.x, " pp")) +
  labs(x = "Percentage point change", y = NULL) +
  theme_kc()

ggsave(here("output", "model_q2c_4_comparisons.png"), plot_model_q2c_4_cmp,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q2c_4_comparisons.pdf"), plot_model_q2c_4_cmp,
       width = 6, height = (6 * 0.618))
