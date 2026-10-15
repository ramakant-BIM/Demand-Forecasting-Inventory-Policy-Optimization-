# forecasting

library(tidyverse)
library(modeltime) # for applying forecasting models
library(timetk) # for time series transformation and manipulation
library(tidymodels) # for applying workflow with clean steps to generate forecast
library(openxlsx) # to load data from .xlsx files
library(bonsai) # for light gbm mode
library(vip) # to visualise variable importance plot

data = read.xlsx("Data for class.xlsx")%>%
  mutate(date = as_date(date, origin = "1899-12-30"))%>%
  filter(Rollno==41038)

# Step 1
# Split the data in to train and test
# test period is set as 13 weeks
split = data %>%
  time_series_split(date_var = date,
                    assess = "13 weeks",
                    cumulative = TRUE)

# Step 2 - Define ML Recipe
# ML Receipe

# First attempt
# Enhanced recipe with promotional information
ml_recipe_1 = recipe(formula = sellout ~ .,
                     data = training(x = split))

ml_recipe_2 = recipe(formula = sellout ~ .,
                     data = training(x = split)) %>%
  step_timeseries_signature(date)

baked_output_1 = ml_recipe_1 %>%
  prep()%>%
  bake(new_data = NULL)

baked_output_2 = ml_recipe_2 %>%
  prep()%>%
  bake(new_data = NULL)

ml_recipe_3 = recipe(formula = sellout ~ .,
                     data = training(x = split)) %>%
  step_timeseries_signature(date) %>%
  step_select(-Product,-Rollno,-contains("iso"), -contains("minute"), -contains("hour"), -contains("am.pm"),-date)

baked_output_3 = ml_recipe_3 %>%
  prep()%>%
  bake(new_data = NULL)

ml_recipe_4 = recipe(formula = sellout ~ .,
                     data = training(x = split)) %>%
  step_timeseries_signature(date) %>%
  step_select(-Product,-Rollno,-contains("iso"), -contains("minute"), -contains("hour"), -contains("am.pm"),-date)%>%
  step_dummy(all_nominal_predictors())

baked_output_4 = ml_recipe_4 %>%
  prep()%>%
  bake(new_data = NULL)


# Enhanced recipe with promotional information
ml_recipe = recipe(formula = sellout ~ .,
                   data = training(x = split)) %>%
  step_timeseries_signature(date) %>%
  step_select(-Product,-Rollno,-contains("iso"), -contains("minute"), -contains("hour"), -contains("am.pm"),-date) %>%
  step_dummy(all_nominal_predictors())

# How to check the results of a recipe
# by defining NULL for new_data we apply recipe for training data. 
baked_output = ml_recipe %>% prep()%>% bake(new_data = NULL)


## Step 3 Defining ML Models

# Random Forest model
workflow_rf = workflow() %>%
  add_model(rand_forest(mode = "regression") %>%
              set_engine(engine = "ranger",
                         importance = "impurity"))%>%
  add_recipe(ml_recipe) %>%
  fit(data = training(split))

# XGBoost model
workflow_xgboost = workflow() %>%
  add_model(boost_tree(mode = "regression") %>%
              set_engine(engine = "xgboost"))%>%
  add_recipe(ml_recipe) %>%
  fit(data = training(split))

# XGBoost with explicit hyperparameters
workflow_xgboost_hyperparameters_set = workflow() %>%
  add_model(
    boost_tree(
      mode = "regression",
      trees = 500,
      min_n = 5,
      tree_depth = 6,
      learn_rate = 0.1,
      loss_reduction = 0.01
    ) %>% set_engine(engine = "xgboost")
  ) %>%
  add_recipe(ml_recipe) %>%
  fit(data = training(split))

# Light GBM Model
workflow_lgbm = workflow() %>%
  add_model(boost_tree(mode = "regression") %>%
              set_engine(engine = "lightgbm"))%>%
  add_recipe(ml_recipe) %>%
  fit(data = training(split))

# Step 4

# Use modeltime_table() function
model_table = modeltime_table(
  workflow_xgboost_hyperparameters_set,
  workflow_xgboost,
  workflow_rf,
  workflow_lgbm
)

# Step 5
resample_results = model_table %>%
  modeltime_calibrate(
    new_data = testing(split)
  )
accuracy = resample_results %>%
  modeltime_accuracy()

# pick best model

best_model = accuracy %>%
  filter(mae == min(mae)) %>%
  pull(.model_id)

# Step 6

model_refit = resample_results %>%
  #  filter(.model_id == best_model)%>%
  modeltime_refit(data = data)

# Step 7

# Future Forecast generation for 14 weeks

future_data = read.xlsx("future_data.xlsx") %>%
  mutate(date = as_date(date, origin = "1899-12-30"))%>%
  filter(Rollno == "41038")

# step 8

fcast = model_refit %>%
  modeltime_forecast(new_data = future_data,
                     actual_data = data)
# Step 9
fcast %>%
  plot_modeltime_forecast()

# VIP Plot
# Extract the XGBoost model and examine feature importance

xgboost_fit = workflow_xgboost %>%
  extract_fit_parsnip()

lgbm_fit = workflow_lgbm %>%
  extract_fit_parsnip()

# Create variable importance plot from the model fit
vip(lgbm_fit)

####### --------------------------------
vip(xgboost_fit)

fcast_comp = fcast%>%
  filter(.model_id == 1)%>%
  select(.index,.value)

actuals = read.xlsx("sellout_actual.xlsx") %>%
  mutate(date = as_date(date, origin = "1899-12-30")) %>% 
  filter(Rollno == "41038")

error_calculation = actuals %>%
  left_join(fcast_comp, by = join_by(date == .index)) %>%
  mutate(abs_error = abs(sellout - .value))

error_mae = mean(error_calculation$abs_error)

error_mape = error_mae / sum(error_calculation$sellout)

print(error_mae)


write_csv(fcast,"forecast.csv")

library(gt)

# Step 2: Load and Create Input Data

# !!! ACTION REQUIRED: CHECK YOUR CSV AND CHOOSE THE CORRECT FORMAT BELOW !!!
# Delete or comment out the lines that do NOT match your file's date format.

sellout_fcast <- read_csv("forecast.csv") %>%
  mutate(.index = as.Date(.index, format = "%d-%m-%Y"))%>%
  filter(.model_id==2 , .model_desc=="XGBOOST")

# Manually create the retailer demand data frame
retailer_demand_df <- tibble(
  .index = as.Date(c("2023-07-03", "2023-07-10")),
  retailer_demand = c(105, 32)
)

# Step 3: Define the Simulation Function (No changes needed here)
projected_sellin_order <- function(sellout_fcast,
                                   current_inv,
                                   target_method = "weeks_cover",
                                   weeks_cover = 2,
                                   fixed_target = 600) {
  # Get the current week from the sellout forecast
  current_week <- sellout_fcast %>%
    filter(.key == "prediction") %>%
    slice(1) %>%
    select(.index) %>%
    pull()
  
  # Prepare the base tibble
  tibble_base <- sellout_fcast %>%
    filter(.key == "prediction") %>% 
    select(forecast = .value, week = .index) %>%
    left_join(retailer_demand_df, by = c("week" = ".index")) %>%
    replace(is.na(.), 0) %>%
    mutate(
      target_inventory = case_when(
        target_method == "weeks_cover" ~ slidify_vec(
          .x = forecast, .f = sum, .period = weeks_cover,
          .align = "left", .partial = TRUE),
        target_method == "fixed" ~ fixed_target,
        TRUE ~ slidify_vec(
          .x = forecast, .period = 2, .f = sum,
          .align = "left", .partial = TRUE)
      ),
      opening_inventory = 0,
      ending_inventory = 0,
      intransit_t1 = 0,
      order_quantity = 0,
      late_sales = 0,
      horizon = if_else(week <= current_week + lubridate::weeks(2), "execution", "planning")
    ) %>%
    arrange(week)
  
  # Initialize the first row (Current Week)
  tibble_base$opening_inventory[1] = current_inv
  
  # Calculate the projected sell in order row by row (Week by Week)
  for (i in 1:nrow(tibble_base)) {
    
    # Standardized demand logic to be consistent across all weeks
    demand_to_use <- pmax(tibble_base$retailer_demand[i], tibble_base$forecast[i])
    
    if (i == 1) {
      # First week calculations
      tibble_base$intransit_t1[i]      <- 0
      tibble_base$ending_inventory[i]  <- tibble_base$opening_inventory[i] - demand_to_use
      tibble_base$late_sales[i]        <- abs(pmin(0, tibble_base$ending_inventory[i]))
      
    } else {
      # Subsequent weeks
      tibble_base$intransit_t1[i]      <- tibble_base$order_quantity[i - 1]
      tibble_base$opening_inventory[i] <- tibble_base$ending_inventory[i - 1] + tibble_base$intransit_t1[i]
      tibble_base$ending_inventory[i]  <- tibble_base$opening_inventory[i] - demand_to_use
      tibble_base$late_sales[i]        <- abs(pmin(0, tibble_base$ending_inventory[i]))
    }
    
    # Calculate order quantity for all weeks except the last one
    if (i < nrow(tibble_base)) {
      tibble_base$order_quantity[i] = tibble_base$target_inventory[i] - tibble_base$ending_inventory[i]
    } else {
      tibble_base$order_quantity[i] = NA
    }
    
    # Ensure order quantity is not negative
    if (!is.na(tibble_base$order_quantity[i])) {
      tibble_base$order_quantity[i] <- pmax(0, tibble_base$order_quantity[i])
    }
  }
  return(tibble_base)
}

# Step 4: Define the Table Formatting Function (No changes needed)
table_formating_function <- function(input_table, policy_type, policy_value = NULL){
  subtitle_text <- case_when(
    policy_type == "fixed" ~ paste("Distributor Ordering Behavior Based on Fixed Target Inventory Policy (",
                                   policy_value, " cases)"),
    policy_type == "weeks_cover" ~ paste("Distributor Ordering Behavior Based on",
                                         policy_value,
                                         "Weeks Coverage Target Inventory Policy"),
    TRUE ~ "Distributor Ordering Behavior"
  )
  
  input_table %>%
    select(week, forecast, retailer_demand, opening_inventory, intransit_t1, ending_inventory,
           order_quantity, target_inventory, late_sales, horizon) %>%
    gt() %>%
    tab_header(
      title = "Projected Sell-In Order Simulation",
      subtitle = subtitle_text
    ) %>%
    cols_label(
      week = "Week",
      forecast = "Sell-Out Forecast",
      retailer_demand = "Retailer Orders",
      opening_inventory = "Opening Inventory",
      intransit_t1 = "In-Transit Inventory",
      ending_inventory = "Ending Inventory",
      order_quantity = "Sellin Order Quantity",
      target_inventory = "Target Inventory",
      late_sales = "Late Sales",
      horizon = "Planning Horizon"
    ) %>%
    fmt_number(columns = where(is.numeric), decimals = 0) %>%
    tab_style(style = list(cell_text(whitespace = "nowrap")), 
              locations = cells_body(columns = week)) %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_date(columns = week, date_style = "yMd") %>%
    data_color(columns = horizon, 
               colors = scales::col_factor(palette = c("lightblue", "lightgray"), 
                                           domain = c("execution", "planning"))) %>%
    data_color(columns = late_sales, 
               colors = scales::col_numeric(palette = c("white", "red"), domain = c(0, 1))) %>%
    data_color(columns = target_inventory, 
               colors = scales::col_numeric(palette = c("lightgreen", "lightcoral"), domain = c(0, 600))) %>%
    tab_style(style = list(cell_text(weight = "bold")), 
              locations = cells_column_labels()) %>%
    tab_style(style = list(cell_fill(color = "#F0F0F0"), 
                           cell_text(weight = "bold")), 
              locations = cells_title()) %>%
    tab_footnote(footnote = "Execution: Current + Next weeks with retailer order visibility",
                 locations = cells_column_labels(columns = horizon)) %>%
    tab_footnote(footnote = "Late sales occur when demand exceeds available inventory",
                 locations = cells_column_labels(columns = late_sales)) %>%
    tab_footnote(footnote = "Green: <= 600 cases capacity, Red: > 600 cases",
                 locations = cells_column_labels(columns = target_inventory))
}


# Step 5: Run All Simulations (No changes needed)

sellinorder_fixedinvpolicy_100 <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 70,
    target_method = "fixed",
    fixed_target = 100
  )

sellinorder_fixedinvpolicy_200 <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 70,
    target_method = "fixed",
    fixed_target = 200
  )

sellinorder_fixedinvpolicy_400 <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 70,
    target_method = "fixed",
    fixed_target = 400
  )

# Step 6: Display the Formatted Results (No changes needed)
table_formating_function(
  sellinorder_fixedinvpolicy_200,
  policy_type = "fixed",
  policy_value = 200
)


# --- NEW SIMULATION: 1-Week Coverage Policy ---
sellinorder_weeksofcoverpolicy_1w <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 70,
    target_method = "weeks_cover",
    weeks_cover = 1
  )
# --- NEW SIMULATION: 2-Week Coverage Policy ---
sellinorder_weeksofcoverpolicy_2w <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 70,
    target_method = "weeks_cover",
    weeks_cover = 2
  )

# --- NEW SIMULATION: 3-Week Coverage Policy ---
sellinorder_weeksofcoverpolicy_3w <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 70,
    target_method = "weeks_cover",
    weeks_cover = 3
  )


# --- NEW TABLE: 1-Week Coverage Policy ---
table_formating_function(
  sellinorder_weeksofcoverpolicy_2w,
  policy_type = "weeks_cover",
  policy_value = 2
)


# Step 5: Run ALL Simulations
# Fixed Target Policies
sim_fixed_100 <- projected_sellin_order(sellout_fcast, 70, "fixed", fixed_target = 100)
sim_fixed_200 <- projected_sellin_order(sellout_fcast, 70, "fixed", fixed_target = 200)
sim_fixed_400 <- projected_sellin_order(sellout_fcast, 70, "fixed", fixed_target = 400)
# Weeks of Cover Policies
sim_woc_1 <- projected_sellin_order(sellout_fcast, 70, "weeks_cover", weeks_cover = 1)
sim_woc_2 <- projected_sellin_order(sellout_fcast, 70, "weeks_cover", weeks_cover = 2)
sim_woc_3 <- projected_sellin_order(sellout_fcast, 70, "weeks_cover", weeks_cover = 3)

# ---------------------------------------------------------------------------
# Step 7: Calculate KPIs and Create the Final Comparison Table
# ---------------------------------------------------------------------------

# Create a function to calculate KPIs for any simulation result
calculate_kpis <- function(sim_data, policy_name) {
  
  sim_data %>%
    # Calculate total demand to be used in metrics
    mutate(total_demand = pmax(retailer_demand, forecast)) %>%
    summarise(
      Policy = policy_name,
      Total_late_sales = sum(late_sales),
      Weeks_with_Stockouts = sum(late_sales > 0),
      
      # Calculate Fill Rate
      Fillrate = (sum(total_demand) - sum(late_sales)) / sum(total_demand),
      
      # Calculate Inventory Efficiency metrics
      Avg_Ending_Inventory = mean(ending_inventory[ending_inventory > 0], na.rm = TRUE),
      
      # COGS = Demand that was successfully met
      COGS = sum(total_demand) - sum(late_sales),
      
      # Inventory Turns = COGS / Average Inventory
      Inventory_Turns = COGS / Avg_Ending_Inventory,
      
      # Calculate Capacity metrics
      Weeks_Over_Capacity = sum(opening_inventory > 800) # Capacity set to 800
    )
}

# Apply the function to all simulation results
kpi_list <- list(
  calculate_kpis(sim_fixed_100, "Fixed 100"),
  calculate_kpis(sim_fixed_200, "Fixed 200"),
  calculate_kpis(sim_fixed_400, "Fixed 400"),
  calculate_kpis(sim_woc_1, "1-Week Cover"),
  calculate_kpis(sim_woc_2, "2-Week Cover"),
  calculate_kpis(sim_woc_3, "3-Week Cover")
)

# Combine all KPIs into one table
kpi_summary_table <- bind_rows(kpi_list)

# Create the formatted GT table
kpi_summary_table %>%
  gt(rowname_col = "Policy") %>%
  tab_header(
    title = "Inventory Policy Performance Comparison",
    subtitle = "Key Performance Metrics Across Different Ordering Policies"
  ) %>%
  # Group columns
  tab_spanner(label = "Service Performance", columns = c(Total_late_sales, Weeks_with_Stockouts, Fillrate)) %>%
  tab_spanner(label = "Inventory Efficiency", columns = c(Avg_Ending_Inventory, Inventory_Turns)) %>%
  tab_spanner(label = "Capacity Constraints", columns = Weeks_Over_Capacity) %>%
  
  # Format numbers and percentages
  fmt_number(columns = where(is.numeric), decimals = 0) %>%
  fmt_percent(columns = Fillrate, decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "NA") %>%
  
  # Add color scales (lower is better)
  data_color(
    columns = c(Total_late_sales, Weeks_with_Stockouts),
    colors = scales::col_numeric(palette = c("green", "yellow", "red"), domain = NULL)
  ) %>%
  data_color(
    columns = c(Weeks_Over_Capacity),
    colors = scales::col_numeric(palette = c("green", "yellow", "red"), domain = c(0, 5))
  ) %>%
  # Add color scales (higher is better)
  data_color(
    columns = c(Fillrate),
    colors = scales::col_numeric(palette = c("red", "yellow", "green"), domain = c(0.80, 1.0))
  ) %>%
  data_color(
    columns = c(Inventory_Turns),
    colors = scales::col_numeric(palette = c("gray", "lightgreen", "green"), domain = NULL, na.color = "#C8C8C8")
  ) %>%
  
  # Add footnotes
  tab_footnote(footnote = "Lower values indicate better service performance", locations = cells_column_labels(columns = Total_late_sales)) %>%
  tab_footnote(footnote = "Higher fill rates are better (>95% is excellent)", locations = cells_column_labels(columns = Fillrate)) %>%
  tab_footnote(footnote = "Higher inventory turns indicate more efficient inventory usage", locations = cells_column_labels(columns = Inventory_Turns)) %>%
  tab_footnote(footnote = "Zero weeks over capacity is ideal (capacity limit = 800 units)", locations = cells_column_labels(columns = Weeks_Over_Capacity))
