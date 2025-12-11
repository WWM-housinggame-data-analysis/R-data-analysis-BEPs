income_dist_table <- function(csv_list_2510 = "dataset list", GP2_tables = "tables list", var_income_dist = "Variables list", output_prefix = "Dataset_") {
  # Load required libraries
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("sqldf")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("stringr")
  
  library(readr)
  library(openxlsx)
  library(rstudioapi)
  # Load for database manipulation
  library(sqldf)
  # Load for data manipulation
  library(dplyr)
  library(stringr)
  
  # Get the current script directory
 # script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  
    # Build a new list with only the elements you want
  GP2_data <- csv_list_2510[GP2_tables]
  names(GP2_data)

  # Assign a table to a variable in the global environment
  gamesession <- GP2_data[["gamesession"]]
  group <- GP2_data[["group"]]
  groupround <- GP2_data[["groupround"]]
  playerround <- GP2_data[["playerround"]]
  player <- GP2_data[["player"]]
  measuretype <- GP2_data[["measuretype"]]
  personalmeasure <- GP2_data[["personalmeasure"]]
  housemeasure <- GP2_data[["housemeasure"]]
  housegroup <- GP2_data[["housegroup"]]
  house <- GP2_data[["house"]]
  initialhousemeasure <- GP2_data[["initialhousemeasure"]]
  
  # Rename the session name variable in the dataframe to avoid name overlap with the group name variable
  gamesession <- sqldf("SELECT * FROM gamesession")
  names(gamesession)[names(gamesession) == "name"] <- "gamesession_name"
  
  # Extract the dataset date to name the data and figure outputs accordingly 
  dataset_date <- str_extract(gamesession$gamesession_name, "\\d+")
  dataset_output<- file.path(data_output_path,paste0("GP2_",dataset_date))

  # Add to the group dataframe the gamesession_name by the group = gamesession_id
  # Leftjoin Keeps only the rows that have matching values in both data frames
  group <- sqldf("
  SELECT g.*, gs.gamesession_name
  FROM [group] AS g
  LEFT JOIN [gamesession] AS gs
  ON g.gamesession_id = gs.id
")
  # Add to groupround the group variables selection
  groupround <- sqldf("
  SELECT gr.*, g.name, g.gamesession_id, g.gamesession_name, g.scenario_id
  FROM [groupround] AS gr
  LEFT JOIN [group] AS g
  ON gr.group_id = g.id
")
  
  # Rename the added columns in the dataframe to know from which table first come from
  names(groupround)[names(groupround) == "scenario_id"] <- "group_scenario_id"
  # Rename name variable in the groupround dataframe for variable naming consistency
  groupround <- sqldf("SELECT * FROM groupround")
  
  # Rename the added columns in the dataframe to know from which table first come from
  names(groupround)[names(groupround) == "name"] <- "group_name"
  
  # Add to playerround the groupround selection to filter per round, group and session id and names by playerround = groupround id
  playerround <- sqldf("
  SELECT pr.*, gr.round_number, gr.group_id, gr.group_name, gr.gamesession_id, gr.gamesession_name, gr.group_scenario_id
  FROM [playerround] AS pr
  LEFT JOIN [groupround] AS gr
  ON pr.groupround_id = gr.id
")
  
  # Rename the added columns in the dataframe to know from which table first come from
  names(playerround)[names(playerround) == "round_number"] <- "groupround_round_number"
  names(playerround)[names(playerround) == "scenario_id"] <- "group_scenario_id"
  
  # Rename id with the table prefix to avoid id ambiguity
  names(playerround)[names(playerround) == "id"] <- "playerround_id"
  
  # Add to the playerround the p.code
  playerround <- sqldf("
  SELECT pr.*, p.code AS player_code
  FROM [playerround] AS pr
  LEFT JOIN [player] AS p
  ON pr.player_id = p.id
  ORDER BY player_code ASC
")
  playerround <- sqldf("
  SELECT pr.*, hg.code AS house_code
  FROM [playerround] AS pr
  LEFT JOIN [housegroup] AS hg
  ON pr.final_housegroup_id = hg.id
  ORDER BY pr.player_code ASC
")
  # Add to the personalmeasure the playerround selection to filter per player, table, round and cost of measures
  personalmeasure <- sqldf("
  SELECT pm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought, pr.final_housegroup_id, pr.mortgage_payment
  FROM [personalmeasure] AS pm
  LEFT JOIN [playerround] AS pr
  ON pm.playerround_id = pr.playerround_id
  ORDER BY pr.player_code ASC
")
  
  # Add to the personalmeasure the housegroup selection to calculate the cost of measures
  personalmeasure <- sqldf("
  SELECT pm.*, hg.code AS house_code, hg.last_sold_price, hg.owner_id
  FROM [personalmeasure] AS pm
  LEFT JOIN [housegroup] AS hg
  ON pm.final_housegroup_id = hg.id
  ORDER BY pm.player_code ASC
")
  
  # Add to the measuretype selection to compare it with the costs of measures per round
  personalmeasure <- sqldf("
  SELECT pm.*, m.short_alias, m.cost_absolute, m.cost_percentage_income, m.cost_percentage_house, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [personalmeasure] AS pm
  LEFT JOIN [measuretype] AS m
  ON pm.measuretype_id = m.id
  ORDER BY pm.player_code ASC
")
  str(personalmeasure)
  #calculate the costs of the personal measures bough
  personalmeasure$calculated_costs <- 
    personalmeasure$cost_absolute + 
    (personalmeasure$cost_percentage_income/100)*personalmeasure$round_income + 
    (personalmeasure$cost_percentage_house/100)*personalmeasure$last_sold_price
  
  head(personalmeasure)
  #calculate the cumulative of the personal measures to compare it against the cost of house measures bought
  personalmeasure_cumulative <- personalmeasure %>%
    arrange(player_code, groupround_round_number) %>%   # ensure proper order
    group_by(player_code, groupround_round_number) %>%  # group by player and round
    #add up costs within each round for each player (since you may have multiple rows per round)
    summarise(calculated_costs_personal_measures = sum(calculated_costs),# sum across rows in the round
              total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
              .groups = "drop"
    ) %>% 
    #ensure cumulative totals are calculated separately for each player
    mutate(
      difference = calculated_costs_personal_measures - total_bought_measures
    ) %>%
    group_by(player_code) %>%
    arrange(groupround_round_number) %>%
    # compute the running total across rounds
    mutate(
      cum_costs       = cumsum(calculated_costs_personal_measures),
      cum_difference  = cumsum(difference)
    )
  
  # Add to the housemeasure the housegroup selection to calculate the cost of measures
  housemeasure <- sqldf("
  SELECT hm.*, hg.code AS house_code, hg.owner_id
  FROM [housemeasure] AS hm
  LEFT JOIN [housegroup] AS hg
  ON hm.housegroup_id = hg.id
  ORDER BY hg.owner_id
")
  
  housemeasure <- sqldf("
  SELECT hm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought
  FROM [housemeasure] AS hm
  LEFT JOIN [playerround] AS pr
  ON hm.owner_id = pr.player_id AND hm.bought_in_round = pr.groupround_round_number
  ORDER BY pr.player_code ASC
")
  
  # Add the measuretype variables to calculate the costs of house measures per round 
  housemeasure <- sqldf("
  SELECT hm.*, m.short_alias, m.cost_absolute, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [housemeasure] AS hm
  LEFT JOIN [measuretype] AS m
  ON hm.measuretype_id = m.id
  ORDER BY hm.player_code ASC
")
  
  # Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
  initialhousemeasure <- sqldf("
  SELECT ihm.*, h.code AS house_code, h.rating, h.initial_pluvial_protection, h.initial_fluvial_protection, h.community_id
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [house] AS h
  ON ihm.house_id = h.id
  ORDER BY ihm.house_id ASC
")
  
  # Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
  initialhousemeasure <- sqldf("
  SELECT ihm.*, m.short_alias 
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [measuretype] AS m
  ON ihm.measuretype_id = m.id
  ORDER BY ihm.house_id ASC
")
  
  # Add the measuretype variables to calculate the costs of house measures per round 
  housemeasure <- sqldf("
  SELECT 
    hm.*,
    CASE 
      WHEN EXISTS (
        SELECT TRUE FROM [initialhousemeasure] AS ihm
        WHERE ihm.measuretype_id = hm.measuretype_id
          AND ihm.house_code = hm.house_code
      )
      THEN TRUE ELSE FALSE
    END AS initialhousemeasure
  FROM [housemeasure] AS hm
")
  
  #calculate the cumulative of the personal measures to compare it against the cost of house measures bought
  housemeasure_cumulative <- housemeasure %>%
    arrange(player_code, groupround_round_number) %>%   # ensure proper order
    group_by(player_code, groupround_round_number) %>%  # group by player and round
    #add up costs within each round for each player (since you may have multiple rows per round)
    summarise(
      # sum only cost_absolute where initialhousemeasure == FALSE
      calculated_costs_house_measures = sum(
        ifelse(initialhousemeasure, 0, cost_absolute)
      ),
      total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
      .groups = "drop"
    ) %>%
    #ensure cumulative totals are calculated separately for each player
    mutate(
      difference = calculated_costs_house_measures - total_bought_measures
    ) %>%
    group_by(player_code) %>%
    arrange(groupround_round_number) %>%
    # compute the running total across rounds
    mutate(
      cum_costs       = cumsum(calculated_costs_house_measures),
      cum_difference  = cumsum(difference)
    )
  
  #Add to playerround the calculated costs of measures
  playerround <- sqldf("
  SELECT pr.*, calculated_costs_house_measures
  FROM [playerround] AS pr
  LEFT JOIN [housemeasure_cumulative] AS hmc
  ON pr.player_code = hmc.player_code AND pr.groupround_round_number = hmc.groupround_round_number
  ORDER BY pr.player_code ASC
")
  
  playerround <- sqldf("
  SELECT pr.*, calculated_costs_personal_measures
  FROM [playerround] AS pr
  LEFT JOIN [personalmeasure_cumulative] AS pmc
  ON pr.player_code = pmc.player_code AND pr.groupround_round_number = pmc.groupround_round_number
  ORDER BY pr.player_code ASC
")
  if (dataset_date == "2409") {
  playerround$calculated_costs_measures_difference <- playerround$cost_house_measures_bought - 
    (playerround$calculated_costs_personal_measures + playerround$calculated_costs_house_measures)
  } else {
    playerround$calculated_costs_measures_difference <- (playerround$cost_house_measures_bought +  playerround$cost_personal_measures_bought) - 
      (playerround$calculated_costs_personal_measures + playerround$calculated_costs_house_measures)
  }
  # Filter the playerround dataset for the income distribution
  ## Add the new calculated columns for the measures costs
  new_vars <- c("calculated_costs_personal_measures", "calculated_costs_house_measures", "calculated_costs_measures_difference")
  var_income_dist <- c(var_income_dist, new_vars)

#  Collapse the column vector into a comma-separated string
  col_income_dist <- paste(var_income_dist, collapse = ", ")
  
  # Run the query to filter the playerround dataframe with the var_income_dist 
  df_income_dist <- sqldf(paste0("
  SELECT ", col_income_dist, "
  FROM playerround
"))
  
  # Calculate the round costs to check the spendable income
  # "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
  # If either column has NA, the sum will also be NA unless the sum is done this way
  df_income_dist$calculated_costs <- rowSums(df_income_dist[, c("living_costs", 
                                                                "cost_taxes",
                                                                "spent_savings_for_buying_house",
                                                                "mortgage_payment",
                                                                "cost_house_measures_bought",
                                                                "cost_personal_measures_bought",
                                                                "cost_fluvial_damage",
                                                                "cost_pluvial_damage"
  )], na.rm = TRUE) 
  
  # Calculate the spendable income
  df_income_dist$calculated_spendable <- df_income_dist$spendable_income
  for (i in 1:nrow(df_income_dist)) {
    if (df_income_dist$groupround_round_number[i] != "0") {
      df_income_dist$calculated_spendable[i] <- sum(df_income_dist$calculated_spendable[i-1],
                                                    df_income_dist$round_income[i],
                                                    df_income_dist$profit_sold_house[i],
                                                    -df_income_dist$calculated_costs[i],
                                                    na.rm = TRUE)   }
  } 
  
  df_income_dist$calculated_difference_spendable <- df_income_dist$spendable_income - df_income_dist$calculated_spendable
  
  # Step 3: Income distribution specification ---------------------------------------------------
  # Create a list with the tables used in the calculation
  list_income_dist <- list(
    df_income_dist = df_income_dist,
    playerround = playerround,
    measuretype = measuretype,
    personalmeasure = personalmeasure,
    housemeasure = housemeasure,
    initialhousemeasure = initialhousemeasure,
    house = house,
    housegroup = housegroup,
    group = group,
    groupround = groupround,
    player = player,
    gamesession = gamesession
  )
  
  # Write to Excel with sheet names matching table names

  tryCatch({
    write_xlsx(list_income_dist, file.path(data_output_path, paste0(github, "_G2_Income_dist_", dataset_date, ".xlsx")))
    message("File written successfully.")
  }, error = function(e) {
    message("Error: ", e$message)
  })
  
  return (df_income_dist)
}