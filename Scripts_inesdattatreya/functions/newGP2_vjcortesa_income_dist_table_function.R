#CHANGES annehuitema2003
# Dec 11th github sync
# CHANGES annehuitema2003-1: Added welafaretype_id to df_income_dist from player_round
# # CHANGES annehuitema2003-2: Added the welfare_level so not coded into the functions unless a different grouping is needed
# CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to playerround and df_income_dist
# CHANGES annehuitema2003-not done: Added new column housing_area as comunity_area via the vjcortesa-2 CHANGES
                                    #In personalmeasure table hg.code AS personal_house_code and housemeasure table hg.code as housemasure_house_code
#CHANGES vjcortesa:                 #the house_code is the same in both tables so the name of the column remained the same
# Dec 11th github sync
# CHANGES vjcortesa-0: removed the output_prefix = "Dataset_" in the function because it was not used and added the dataset data to the file name  
# CHANGES vjcortesa-1: Added tables"community","house","initialhousemeasure","question","questionitem","questionscore"
#                     Tables need to be listed in GP2_tables input for the function to work 
# CHANGES vjcortesa-2: Added house query to get community_area into the playerround table
# CHANGES vjcortesa-3: Corrected the calculation of the personal measure with the last_sold price instead of the mortgage_payment*10
# CHANGES vjcortesa-4: Added to the initialhouse measure the house_code to identify in the housemeasure calculation which measures came already implemented when player bought the house
# CHANGES vjcortesa-5: Updated the var_income_dist list with the variables added by vcortesa and annehuitema2003, except for the welfare level to be added in the plot function
# CHANGES vjcortesa-6: Added to question score the question, question item and player_round tables relevant variables
# CHANGES vjcortesa-7: Added to the list_income_dist file the tables added in the code
# CHANGES vjcortesa-9ab: Added the game_session_name to the measures tables

income_dist_table <- function(csv_list_2510 = "dataset list", GP2_tables = "tables list", var_income_dist = "Variables list") {
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
  #CHANGES vjcortesa-1:
  community <- GP2_data[["community"]]
  house <- GP2_data[["house"]]
  initialhousemeasure <- GP2_data[["initialhousemeasure"]]
  question <- GP2_data[["question"]]
  questionitem <- GP2_data[["questionitem"]]
  questionscore <- GP2_data[["questionscore"]]
  #end CHANGES
  
  # Rename the session name variable in the dataframe to avoid name overlap with the group name variable
  gamesession <- sqldf("SELECT * FROM gamesession")
  names(gamesession)[names(gamesession) == "name"] <- "gamesession_name"
  # CHANGES vjcortesa-0: 
  # Extract the dataset date to name the data and figure outputs accordingly 
  dataset_date <- str_extract(gamesession$gamesession_name, "\\d+")
  dataset_output<- file.path(data_output_path,paste0("GP2_",dataset_date))
  # END changes
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
  #CHANGES annehuitema2003-1: added welfaretype_id from player to player round
  # Add to the playerround the p.code and welfaretype_id
  playerround <- sqldf("
  SELECT pr.*, p.code AS player_code, p.welfaretype_id AS welfaretype_id
  FROM [playerround] AS pr
  LEFT JOIN [player] AS p
  ON pr.player_id = p.id
  ORDER BY player_code ASC
  ")
  
  #end CHANGES
  # CHANGES vjcortesa-2: Added house query to get community area into the playerround table
  house <- sqldf("
  SELECT h.*, c.name AS community_name
  FROM [house] AS h
  LEFT JOIN [community] as c
  ON c.id = h.community_id
  ")

  playerround <- sqldf("
  SELECT pr.*, hg.code AS house_code, h.community_name
  FROM [playerround] AS pr
  LEFT JOIN [housegroup] AS hg
  ON pr.final_housegroup_id = hg.id
  LEFT JOIN [house] AS h
  ON hg.code = h.code
  ORDER BY pr.player_code ASC
  ")
  #END CHANGES
  
  # CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to playerround and df_income_dist
  # Map numeric welfaretype_id to welfare text levels
  #converts numeric welfare IDs into human‑readable ordered categories
  # Only if there are exactly six distinct IDs. Otherwise, it warns you that the mapping isn’t valid.
  welfare_labels <- c("Very Low",
                      "Low",
                      "Low-average",
                      "High-average",
                      "High",
                      "Very High")
  
  wt_codes <- sort(unique(playerround$welfaretype_id))
  
  if (length(wt_codes) == 6) {
    playerround$welfare_level <- factor(
      welfare_labels[match(playerround$welfaretype_id, wt_codes)],
      levels = welfare_labels,
      ordered = TRUE
    )
  } else {
    warning("Expected 6 distinct welfaretype_id values, but found ",   #make sure that it returns warning if not applicable
            length(wt_codes),
            ". welfare_level not created.")
  }
  #END CHANGES
  # CHANGES vjcortesa-9a: Added the game_session_name to the measures tables
  # Add to the personalmeasure the playerround selection to filter per player, table, round and cost of measures
  personalmeasure <- sqldf("
  SELECT pr.gamesession_name, pm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought, pr.final_housegroup_id, pr.mortgage_payment
  FROM [personalmeasure] AS pm
  LEFT JOIN [playerround] AS pr
  ON pm.playerround_id = pr.playerround_id
  ORDER BY pr.player_code ASC
")
  #END CHANGE
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
  
  # CHANGES vjcortesa-3: Corrected the calculation of the personal measure with the last_sold price instead of the mortgage_payment*10
  #calculate the costs of the personal measures bough
  personalmeasure$calculated_costs <- 
    personalmeasure$cost_absolute + 
    (personalmeasure$cost_percentage_income/100)*personalmeasure$round_income + 
    (personalmeasure$cost_percentage_house/100)*personalmeasure$last_sold_price
  #END CHANGES
  
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
  # CHANGES vjcortesa-9a: Added the game_session_name to the measures tables
  housemeasure <- sqldf("
  SELECT pr.gamesession_name, hm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought
  FROM [housemeasure] AS hm
  LEFT JOIN [playerround] AS pr
  ON hm.owner_id = pr.player_id AND hm.bought_in_round = pr.groupround_round_number
  ORDER BY pr.player_code ASC
")
  #END CHANGES
  
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
  
# CHANGES vjcortesa-4: 
# Added to the initialhouse measure the house_code to identify in the housemeasure calculation which measures came already implemented when player bought the house
initialhousemeasure <- sqldf("
  SELECT ihm.*, m.short_alias 
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [measuretype] AS m
  ON ihm.measuretype_id = m.id
  ORDER BY ihm.house_id ASC
")

  #The subquery checks if there is at least one measure from the initialhousemeasure table is in the housemeasure table according to the house_code 
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
  
  #calculate the cumulative of the house measures to compare it against the cost of house measures bought
  #exclude the costs of the housemeasures that came implemented in the house when bought
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
  #END CHANGES  
  
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
  
  # CHANGES annehuitema2003-3: Added pluvial&fluvial costs as total_damage to playerround and df_income_dist
  if (all(c("cost_fluvial_damage", "cost_pluvial_damage") %in% names(playerround))) {
    playerround$total_damage_costs <- rowSums(
      playerround[, c("cost_fluvial_damage", "cost_pluvial_damage")],
      na.rm = TRUE
    )
  } else {
    warning("cost_fluvial_damage and/or cost_pluvial_damage missing in playerround.")
  }
  #END CHANGES
  # CHANGES vjcortesa-6: 
  # Add to question score the question, question item and player_round tables relevant variables
  questionscore <- sqldf("
  SELECT 
    qs.id AS answer_id, qs.answer, qs.late_answer,qi.name AS answer_option, CAST(qs.answer AS INTEGER) || ' - ' || qi.name AS answer_plus_option, 
    qs.question_id, q.name AS question_name, q.description AS question_description,
    qs.playerround_id, pr.groupround_round_number, pr.player_code, pr.group_name, pr.gamesession_name
  FROM questionscore AS qs
  LEFT JOIN question AS q
    ON qs.question_id = q.id
  LEFT JOIN questionitem AS qi
    ON qs.answer = qi.code
   AND qs.question_id = qi.question_id
  LEFT JOIN  playerround AS pr
   ON qs.playerround_id = pr.playerround_id
")
  
  questionitem <- sqldf("
  SELECT 
    qi.id AS questionitem_id, qi.code AS answer_code, qi.name AS answer_name, 
    CAST(qi.code AS INTEGER) || ' - ' || qi.name AS answercode_plus_name,
    q.name AS question_name, q.description AS question_description
  FROM questionitem AS qi
  LEFT JOIN question AS q
    ON qi.question_id = q.id
  ")
  #END CHANGES
  
  # Filter the playerround dataset for the income distribution
  # CHANGES vjcortesa-5: # Updated the var_income_dist list with the variables added by vcortesa and annehuitema2003, except for the welfare level to be added in the plot function
  ## Add the new calculated columns for the measures costs
  new_vars <- c("calculated_costs_personal_measures", 
                "calculated_costs_house_measures", 
                "calculated_costs_measures_difference",
                "satisfaction_total",
                "welfaretype_id",
                "total_damage_costs",
                "community_name", #instead of housing_area to keep variable naming consistent
                "fluvial_house_delta",
                "pluvial_house_delta",
                "welfare_level"
  )
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
  # CHANGES vjcortesa-7: Added to the list_income_dist file the tables added in the code
  # Create a list with the tables used in the calculation
  list_income_dist <- list(
    df_income_dist = df_income_dist,
    playerround = playerround,
    measuretype = measuretype,
    personalmeasure = personalmeasure,
    housemeasure = housemeasure,
    questionscore = questionscore,
    questionitem = questionitem,
    initialhousemeasure = initialhousemeasure,
    house = house,
    housegroup = housegroup,
    group = group,
    groupround = groupround,
    player = player,
    gamesession = gamesession
  )
  #END CHANGES
  
  # Write to Excel with sheet names matching table names

  tryCatch({
    write_xlsx(list_income_dist, file.path(data_output_path, paste0(github, "_G2_Income_dist_", dataset_date, ".xlsx")))
    message("File written successfully.")
  }, error = function(e) {
    message("Error: ", e$message)
  })
  
  return (df_income_dist)
}