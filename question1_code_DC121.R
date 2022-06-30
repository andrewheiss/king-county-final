# Options ----

options(dplyr.summarise.inform = FALSE)


# Load packages ----

if (!require("pacman")) {
  install.packages("pacman", dependencies = TRUE)
}

pacman::p_load(
  "tidyverse",
  "data.table",
  "sf",
  "tigris",
  "lubridate",
  "broom",
  "marginaleffects",
  "ggokabeito",
  "scales",
  "here",
  install = TRUE
)

source(here("R", "graphics.R"))


# Clean data ----

## Load data ----

trip_changes <- read_csv(here("data", "raw_data_from_king_county", 
                              "Question 1_ Fare Reinstatement", 
                              "alltrips_2020-09_to_2020-10.csv"))

apc_stops <- fread(here("data", "raw_data_from_king_county", 
                        "Question 1_ Fare Reinstatement", 
                        "stop_activity_granular_2020-09-01_2020-10-31.csv"))

shapename <- read_sf(here("data", "raw_data_from_king_county", 
                          "KCM_Stops_Data", "kcm_stops.shp"))

# Census data; this was originally created using R/get_census_data.R, but isn't
# run as part of the overall project because it requires an API key
acs_wa <- readRDS(here("data", "census_data", "acs_wa.rds")) %>% 
  mutate(GEOID = str_sub(GEOID, 1, 11))

trip_change_dates <- tribble(
  ~SERVICE_CHANGE_NUM, ~MINOR_CHANGE_NUM,  ~Start_Date,  ~End_Date,
  202,                 5,                  "8/22/2020",  "9/18/2020",
  203,                 0,                  "9/19/2020",  "10/2/2020",
  203,                 1,                  "10/3/2020",  "10/16/2020",
  203,                 2,                  "10/17/2020", "10/30/2020",
  203,                 3,                  "10/31/2020", "11/13/2020"
) %>% 
  mutate(change_start_date = mdy(Start_Date),
         change_end_date = mdy(End_Date)) %>%
  select(-Start_Date, -End_Date)

# Only keep the service change code, trip_id and change data to merge with the
# stop apc data
trip_changes_clean <- trip_changes %>% 
  select(SERVICE_CHANGE_NUM, MINOR_CHANGE_NUM, TRIP_ID) %>% 
  left_join(trip_change_dates, by = c("SERVICE_CHANGE_NUM","MINOR_CHANGE_NUM")) %>%
  rename(major_change_code = SERVICE_CHANGE_NUM,
         minor_change_code = MINOR_CHANGE_NUM,
         trip_id = TRIP_ID) %>%
  group_by(trip_id) %>% slice(1)  

apc_stops_clean <- apc_stops %>% 
  # drop BOOKING_ID since we have a stand-alone service change data and drop
  # other variables not to be used
  select(date = OPERATION_DATE,
         day_type = SCHED_DAY_TYPE_CODED_NUM,
         route_id = SERVICE_RTE_LIST,
         stop_id = STOP_ID,
         boarding_num = PSNGR_BOARDINGS,
         alighting_num = PSNGR_ALIGHTINGS,
         trip_id = TRIP_ID) %>% 
  mutate(day_type = case_when(day_type == 0 ~ "weekday",
                              day_type == 1 ~ "Saturday",
                              day_type == 2 ~ "Sunday")) %>% 
  
  # add route change dates corresponding to major/minor change codes to the stop level apc
  left_join(trip_changes_clean, by = "trip_id") %>% 
  # generate dummy indicating whether under route change
  mutate(under_temp_change = ifelse(!is.na(change_start_date) &
                                      date >= change_start_date & date <= change_end_date, TRUE, FALSE),
         # KMC suggests using data after 2020/09/19 due to major change on that
         # date but also welcome analyzing the full time range
         after_major_change = ifelse(date >= ymd("2019-09-19"),TRUE, FALSE),
         after_reinstate = ifelse(date >= ymd("2020-10-01"), TRUE, FALSE))

apc_stop_day <- apc_stops_clean %>% 
  group_by(stop_id, date) %>% 
  summarise(day_type = first(day_type),
            after_major_change = first(after_major_change),
            after_reinstate = first(after_reinstate),
            boarding_day_count = sum(boarding_num),
            alighting_day_count = sum(alighting_num),
            number_of_rows = n(),
            number_under_temp = sum(under_temp_change == TRUE)) %>% 
  mutate(under_change_ratio = number_under_temp / number_of_rows,
         stop_encounter_day_count = boarding_day_count + alighting_day_count) %>% 
  ungroup()

# sf data with GEOID
stops <- cbind(shapename, st_transform(x = shapename, crs = 4326) %>% st_coordinates)

# load the census tract shape files for King County using the tigris package
kctracts <- tracts(state = 53, county = 033, cb = FALSE, year = 2020)

stops_sf <- st_as_sf(data.frame(x = stops$X, y = stops$Y), 
                     coords = c('x', 'y'), 
                     crs = st_crs(kctracts))

# adding the stop_id back 
stops_sf <- cbind(shapename$STOP_ID, stops_sf)

# create a new data frame that matches the bus stop data to the census tract
stops_census <- stops_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, kctracts)),
  GEOID = if_else(is.na(intersection), '', kctracts$GEOID[intersection])
) %>% 
  rename(stop_id = shapename.STOP_ID)

# merging acs and stop_census 
stops_census_data <- stops_census %>% 
  left_join(acs_wa, by = "GEOID")

# Final data!
apc_stop_final <- apc_stop_day %>% 
  left_join(stops_census_data, by = "stop_id")

apc_stops_after <- apc_stop_final %>% 
  filter(after_major_change == TRUE) %>% 
  mutate(day_number = yday(date))


# Initial models ----

## Percent white * after_reinstate ----
model_q1a <- lm(stop_encounter_day_count ~ after_reinstate +
                  under_change_ratio+
                  bg_pct_white * after_reinstate +
                  bg_pct_white + bg_pct_male + bg_pct_hs + bg_pct_ba + bg_internet + 
                  bg_travel_time_per_capita + bg_pub_trans_per_capita + bg_income + bg_median_rent +
                  day_number,
                data = apc_stops_after)

mfx_model_q1a <- marginaleffects(model_q1a, variables = "after_reinstate", 
                                 newdata = datagrid(bg_pct_white = seq(0, 1, 0.01)))

summary(mfx_model_q1a)

plot_mfx_model_q1a <- ggplot(mfx_model_q1a, aes(x = bg_pct_white, y = dydx)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Percent white (block group level)",
       y = "Effect of after_reinstate (marginal effect)") +
  theme_kc()

ggsave(here("output", "model_q1a_mfx.png"), plot_mfx_model_q1a,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q1a_mfx.pdf"), plot_mfx_model_q1a,
       width = 6, height = (6 * 0.618))


## Education * after_reinstate ----
model_q1b <- lm(stop_encounter_day_count ~ after_reinstate +
                  under_change_ratio+
                  bg_pct_ba * after_reinstate +
                  bg_pct_white + bg_pct_male + bg_pct_hs + bg_pct_ba + bg_internet + 
                  bg_travel_time_per_capita + bg_pub_trans_per_capita + bg_income + bg_median_rent +
                  day_number,
                data = apc_stops_after)

mfx_model_q1b <- marginaleffects(model_q1b, variables = "after_reinstate", 
                                 newdata = datagrid(bg_pct_ba = seq(0, 1, 0.01)))

summary(mfx_model_q1b)

plot_mfx_model_q1b <- ggplot(mfx_model_q1b, aes(x = bg_pct_ba, y = dydx)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Percent with BA (block group level)",
       y = "Effect of after_reinstate (marginal effect)") +
  theme_kc()

ggsave(here("output", "model_q1b_mfx.png"), plot_mfx_model_q1b,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q1b_mfx.pdf"), plot_mfx_model_q1b,
       width = 6, height = (6 * 0.618))


# ## Income * after_reinstate ----
model_q1c <- lm(stop_encounter_day_count ~ after_reinstate +
                  under_change_ratio+
                  bg_income * after_reinstate +
                  bg_pct_white + bg_pct_male + bg_pct_hs + bg_pct_ba + bg_internet + 
                  bg_travel_time_per_capita + bg_pub_trans_per_capita + bg_income + bg_median_rent +
                  day_number,
                data = apc_stops_after)

mfx_model_q1c <- marginaleffects(model_q1c, variables = "after_reinstate", 
                                 newdata = datagrid(bg_income = seq(0, 250000, 1000)))

summary(mfx_model_q1c)

plot_mfx_model_q1c <- ggplot(mfx_model_q1c, aes(x = bg_income, y = dydx)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Average block group income",
       y = "Effect of after_reinstate (marginal effect)") +
  theme_kc()

ggsave(here("output", "model_q1c_mfx.png"), plot_mfx_model_q1c,
       width = 6, height = (6 * 0.618))
ggsave(here("output", "model_q1c_mfx.pdf"), plot_mfx_model_q1c,
       width = 6, height = (6 * 0.618))
