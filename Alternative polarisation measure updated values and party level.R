# Alternative polarisation measure (reijlan) Over multiple waves
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
remotes::install_github("joon-e/allespaletti")
library(allespaletti)
library(patchwork)

## Party Skalometer (reference party_)


variables <- c('skalometer_parties_spd', 'skalometer_parties_cdu', 
               'skalometer_parties_csu', 'skalometer_parties_b90', 
               'skalometer_parties_fdp', 'skalometer_parties_afd', 
               'skalometer_parties_linke',
               'skalometer_parties_andere'
)

con_survey <- dbConnect(RMySQL::MySQL(), 
                        dbname = 'poltrack_survey', 
                        host = '18.156.8.93', 
                        port = 3306, 
                        user = 'poltrack',
                        password = '###')
dbListTables(con_survey)

party_ids <- SQL_query_function(c('partyid'), c(1, 2, 3, 4), "YES")
survey_w1 <- SQL_query_function(variables, 1, "YES")
survey_w2 <- SQL_query_function(variables, 2, "YES")
survey_w3 <- SQL_query_function(variables, 3, "YES")
survey_w4 <- SQL_query_function(variables, 4, "YES")

survey_w1 <- merge(survey_w1, party_ids, by="p_0002", all.x=TRUE)
survey_w2 <- merge(survey_w2, party_ids, by="p_0002", all.x=TRUE)
survey_w3 <- merge(survey_w3, party_ids, by="p_0002", all.x=TRUE)
survey_w4 <- merge(survey_w4, party_ids, by="p_0002", all.x=TRUE)

##Waves 3 and 4 dropped andere from the skalometer question, so removing them from the data
survey_w3 <- subset( survey_w3, select = -skalometer_parties_andere)
survey_w4 <- subset( survey_w4, select = -skalometer_parties_andere)

##replace na in parties andere column with 0 to support the function
survey_w1$skalometer_parties_andere[is.na(survey_w1$skalometer_parties_andere)] <- 0
survey_w2$skalometer_parties_andere[is.na(survey_w2$skalometer_parties_andere)] <- 0
## Have to add the Andere partyid back into W2 for unknown strange reasons

# Define the mapping of codes to party names
party_mapping <- c(
  "1.0" = "SPD",
  "2.0" = "UNION",
  "3.0" = "UNION",
  "4.0" = "B90",
  "5.0" = "FDP",
  "6.0" = "AfD",
  "7.0" = "LINKE",
  "8.0" = "Andere",#### ausgescholssen für den beispiel
  "9.0" = "Keine Partei"
)


dbDisconnect(con_survey)
# Define the function
process_survey_data <- function(data, party_mapping) {
  data %>%
    mutate(partyid = party_mapping[as.character(partyid)]) %>%
    mutate(across(
      -c(p_0002, partyid), # Exclude these columns
      as.numeric
    )) %>%
    mutate(across(-c(p_0002, partyid), ~na_if(.x, -77))) %>% ## seems to always correspond with all values being-77
    mutate(across(-c(p_0002, partyid), ~na_if(.x, 12))) %>%
    mutate(across(-c(p_0002, partyid), ~na_if(.x, 0)))%>%
    filter(rowSums(!is.na(select(., -c(p_0002, partyid)))) > 0)
    
}
#The result are dataframes where 0 (seen but not answered), 12 (I dont know), and -77 (unseen question) are
#coded as NA

# application to survey data frames

survey_w1_processed <- process_survey_data(survey_w1, party_mapping)
survey_w2_processed <- process_survey_data(survey_w2, party_mapping)
survey_w3_processed <- process_survey_data(survey_w3, party_mapping)
survey_w4_processed <- process_survey_data(survey_w4, party_mapping)

## Adding the rating of partyid
add_partyid_rating <- function(survey_data) {
  survey_data %>%
    mutate(
      party_rating = case_when(
        partyid == "SPD" ~ skalometer_parties_spd,
        partyid == "UNION" & "skalometer_parties_cdu" %in% names(.) ~ skalometer_parties_cdu,
        partyid == "UNION" & "skalometer_parties_csu" %in% names(.) ~ skalometer_parties_csu,
        partyid == "B90" ~ skalometer_parties_b90,
        partyid == "FDP" ~ skalometer_parties_fdp,
        partyid == "AfD" ~ skalometer_parties_afd,
        partyid == "LINKE" ~ skalometer_parties_linke,
        partyid == "Andere" ~ skalometer_parties_andere,
        TRUE ~ NA_real_  # If no match, assign NA
      )
    )
}
survey_w1_processed <- add_partyid_rating(survey_w1_processed)
survey_w2_processed <- add_partyid_rating(survey_w2_processed)
## Adding the rating of partyid
add_partyid_rating <- function(survey_data) {
  survey_data %>%
    mutate(
      party_rating = case_when(
        partyid == "SPD" ~ skalometer_parties_spd,
        partyid == "UNION" & "skalometer_parties_cdu" %in% names(.) ~ skalometer_parties_cdu,
        partyid == "UNION" & "skalometer_parties_csu" %in% names(.) ~ skalometer_parties_csu,
        partyid == "B90" ~ skalometer_parties_b90,
        partyid == "FDP" ~ skalometer_parties_fdp,
        partyid == "AfD" ~ skalometer_parties_afd,
        partyid == "LINKE" ~ skalometer_parties_linke,
        TRUE ~ NA_real_  # If no match, assign NA
      )
    )
}
survey_w3_processed <- add_partyid_rating(survey_w3_processed)
survey_w4_processed <- add_partyid_rating(survey_w4_processed)


## FROM https://www.forschungsgruppe.de/Umfragen/Politbarometer/Langzeitentwicklung_-_Themen_im_Ueberblick/Politik_I/#PolStimm
party_v_dict_w1 <- c(
  SPD = 19, UNION = 28, B90 = 22,
  FDP = 5, AfD = 15, LINKE = 5, Andere = 6
)
party_v_dict_w2 <- c(
  SPD = 18, UNION = 31, B90 = 18,
  FDP = 6, AfD = 15, LINKE = 4, Andere = 8
)
party_v_dict_w3 <- c(
  SPD = 17, UNION = 26, B90 = 16,
  FDP = 6, AfD = 21, LINKE = 5, Andere = 9
)
party_v_dict_w4 <- c(
  SPD = 15, UNION = 31, B90 = 15,
  FDP = 5, AfD = 22, LINKE = 4, Andere = 8
)

# Convert vote shares to proportions
# party_v_dict_w1 <- party_v_dict_w1 / 100
# party_v_dict_w2 <- party_v_dict_w2 / 100
# party_v_dict_w3 <- party_v_dict_w3 / 100
# party_v_dict_w4 <- party_v_dict_w4 / 100



##### looped code library(dplyr


calculate_apn <- function(survey_data, party_v_dict) {
    # Ensure party_v_dict is normalized
    party_v_dict <- party_v_dict / 100

    survey_data %>%
      rowwise() %>%
      mutate(
        ratings = list(c(
          SPD = if ("skalometer_parties_spd" %in% names(.)) skalometer_parties_spd else NA,
          UNION = if ("skalometer_parties_cdu" %in% names(.)) skalometer_parties_cdu else NA,
          UNION = if ("skalometer_parties_csu" %in% names(.)) skalometer_parties_csu else NA,
          B90 = if ("skalometer_parties_b90" %in% names(.)) skalometer_parties_b90 else NA,
          FDP = if ("skalometer_parties_fdp" %in% names(.)) skalometer_parties_fdp else NA,
          AfD = if ("skalometer_parties_afd" %in% names(.)) skalometer_parties_afd else NA,
          LINKE = if ("skalometer_parties_linke" %in% names(.)) skalometer_parties_linke else NA,
          Andere = if ("skalometer_parties_andere" %in% names(.)) skalometer_parties_andere else NA
        )),
        vote_n = ifelse(partyid %in% names(party_v_dict), party_v_dict[partyid], NA),
        APn = if (!is.na(vote_n) && vote_n < 1) {
          sum((party_rating - unlist(ratings)) * (party_v_dict / (1 - vote_n)), na.rm = TRUE)
        } else {
          NA
        }
      ) %>%
      ungroup() #%>%
      #select(p_0002, partyid, APn)
  }

# Process each survey dataset in a loop
surveys <- list(survey_w1_processed, survey_w2_processed, survey_w3_processed, survey_w4_processed)
party_v_dicts <- list(party_v_dict_w1, party_v_dict_w2, party_v_dict_w3, party_v_dict_w4)

# Store results in a list
apn_results <- list()

for (i in seq_along(surveys)) {
  apn_results[[i]] <- calculate_apn(surveys[[i]], party_v_dicts[[i]])
}

# Optional: Assign names to the results list for easier reference
names(apn_results) <- c("survey_w1_apn", "survey_w2_apn", "survey_w3_apn", "survey_w4_apn")

# Access individual results
survey_w1_apn <- apn_results[[1]]
survey_w2_apn <- apn_results[[2]]
survey_w3_apn <- apn_results[[3]]
survey_w4_apn <- apn_results[[4]]


### boxplots of individual polarisations scores by party

# List of survey datasets
survey_apn_list <- list(
  survey_w1_apn,
  survey_w2_apn,
  survey_w3_apn,
  survey_w4_apn
)
# Assign names to the list elements
names(survey_apn_list) <- c("survey_w1_apn", "survey_w2_apn", "survey_w3_apn", "survey_w4_apn")
####


###stacked boxplots



### lineplots over time # Define the order of party levels
party_levels <- c("UNION", "SPD", "AfD", "FDP", "LINKE", "B90", "Andere")

# Create a dataframe to store the averages grouped by partyid
avg_apn_grouped <- bind_rows(
  lapply(names(survey_apn_list), function(name) {
    survey_data <- survey_apn_list[[name]]
    wave_label <- sub("survey_w(\\d+)_apn", "W\\1", name)
    survey_data %>%
      group_by(partyid) %>%
      summarise(Average_APn = mean(APn, na.rm = TRUE), .groups = "drop") %>%
      mutate(Wave = wave_label)
  })
) %>%
  mutate(partyid = factor(partyid, levels = party_levels))  # Set factor levels for partyid

# Create the line plot grouped by partyid
line_plot_grouped <- ggplot(avg_apn_grouped, aes(x = Wave, y = Average_APn, color = partyid, group = partyid)) +
  geom_line(size = 1) +  # Line for each partyid
  geom_point(size = 2) +  # Points for each wave
  labs(
    title = "Average APn by Party Supporters Over Time",
    x = "Wave",
    y = "Average APn",
    color = "Party ID"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, 12)) +  # Ensure y-axis is between 0 and 1
  scale_color_politics(palette = "fckafd")  # Use the political party palette

# Print the plot
print(line_plot_grouped)



################################Party Level Analysis


calculate_partylevel <- function(data) {
  output_name <- paste0(deparse(substitute(data)), "_partylevel")
  result <- data %>%
    group_by(partyid) %>%
    summarise(across(starts_with("skalometer"), \(x) mean(x, na.rm = TRUE)))
  
  assign(output_name, result, envir = .GlobalEnv)  # Save the result in the global environment
}
calculate_partylevel(survey_w1_processed)
calculate_partylevel(survey_w2_processed)
calculate_partylevel(survey_w3_processed)
calculate_partylevel(survey_w4_processed)

## Add vote share 
survey_w1_processed_partylevel <- survey_w1_processed_partylevel %>%
  mutate(vote_share = party_v_dict_w1[partyid])
survey_w2_processed_partylevel <- survey_w2_processed_partylevel %>%
  mutate(vote_share = party_v_dict_w2[partyid])
survey_w3_processed_partylevel <- survey_w3_processed_partylevel %>%
  mutate(vote_share = party_v_dict_w3[partyid])
survey_w4_processed_partylevel <- survey_w4_processed_partylevel %>%
  mutate(vote_share = party_v_dict_w4[partyid])

survey_w1_processed_partylevel <- survey_w1_processed_partylevel %>%
  mutate(
    skalometer_parties_union = rowMeans(
      select(., skalometer_parties_csu, skalometer_parties_cdu), 
      na.rm = TRUE
    )
  ) %>%
  select(-skalometer_parties_csu, -skalometer_parties_cdu)
survey_w2_processed_partylevel <- survey_w2_processed_partylevel %>%
  mutate(
    skalometer_parties_union = rowMeans(
      select(., skalometer_parties_csu, skalometer_parties_cdu), 
      na.rm = TRUE
    )
  ) %>%
  select(-skalometer_parties_csu, -skalometer_parties_cdu)
survey_w3_processed_partylevel <- survey_w3_processed_partylevel %>%
  mutate(
    skalometer_parties_union = rowMeans(
      select(., skalometer_parties_csu, skalometer_parties_cdu), 
      na.rm = TRUE
    )
  ) %>%
  select(-skalometer_parties_csu, -skalometer_parties_cdu)
survey_w4_processed_partylevel <- survey_w4_processed_partylevel %>%
  mutate(
    skalometer_parties_union = rowMeans(
      select(., skalometer_parties_csu, skalometer_parties_cdu), 
      na.rm = TRUE
    )
  ) %>%
  select(-skalometer_parties_csu, -skalometer_parties_cdu)
survey_w1_processed_partylevel <- survey_w1_processed_partylevel %>%
  filter(!partyid %in% c("Keine Partei", NA))
survey_w2_processed_partylevel <- survey_w2_processed_partylevel %>%
  filter(!partyid %in% c("Keine Partei", NA))
survey_w3_processed_partylevel <- survey_w3_processed_partylevel %>%
  filter(!partyid %in% c("Keine Partei", NA, "Andere"))
survey_w4_processed_partylevel <- survey_w4_processed_partylevel %>%
  filter(!partyid %in% c("Keine Partei", NA, "Andere"))


# Assuming `survey_w1_processed_partylevel` is your dataframe
survey_w1_processed_partylevel <- survey_w1_processed_partylevel %>%
  rowwise() %>%  # Process row by row
  mutate(
    APn = sum(
      sapply(names(party_v_dict_w1), function(party) {
        # Skip calculation if the party is the same as 'partyid'
        if (party == partyid) return(0)
        
        # Dynamically construct column names, ensuring lowercase consistency
        Liken_col <- paste0("skalometer_parties_", tolower(partyid))  # Current party
        Likem_col <- paste0("skalometer_parties_", tolower(party))    # Other party
        
        # Access column values
        Liken <- get(Liken_col)
        Likem <- get(Likem_col)
        
        # Vote shares
        Vote_share_m <- party_v_dict_w1[party] / 100  # Other party vote share
        Vote_share_n <- vote_share / 100  # Current party vote share
        
        # Formula component
        (Liken - Likem) * (Vote_share_m / (1 - Vote_share_n))
      }),
      na.rm = TRUE # Ensure NA values are handled
    )
  ) %>%
  ungroup()  # Remove rowwise grouping

survey_w2_processed_partylevel <- survey_w2_processed_partylevel %>%
  rowwise() %>%  # Process row by row
  mutate(
    APn = sum(
      sapply(names(party_v_dict_w2), function(party) {
        # Skip calculation if the party is the same as 'partyid'
        if (party == partyid) return(0)
        
        # Dynamically construct column names, ensuring lowercase consistency
        Liken_col <- paste0("skalometer_parties_", tolower(partyid))  # Current party
        Likem_col <- paste0("skalometer_parties_", tolower(party))    # Other party
        
        # Access column values
        Liken <- get(Liken_col)
        Likem <- get(Likem_col)
        
        # Vote shares
        Vote_share_m <- party_v_dict_w2[party] / 100  # Other party vote share
        Vote_share_n <- vote_share / 100  # Current party vote share
        
        # Formula component
        (Liken - Likem) * (Vote_share_m / (1 - Vote_share_n))
      }),
      na.rm = TRUE # Ensure NA values are handled
    )
  ) %>%
  ungroup()  # Remove rowwise grouping

party_v_dict_w3 <- c(
  SPD = 17, UNION = 26, B90 = 16,
  FDP = 6, AfD = 21, LINKE = 5
)
party_v_dict_w4 <- c(
  SPD = 15, UNION = 31, B90 = 15,
  FDP = 5, AfD = 22, LINKE = 4
)
survey_w3_processed_partylevel <- survey_w3_processed_partylevel %>%
  rowwise() %>%  # Process row by row
  mutate(
    APn = sum(
      sapply(names(party_v_dict_w3), function(party) {
        # Skip calculation if the party is the same as 'partyid'
        if (party == partyid) return(0)
        
        # Dynamically construct column names, ensuring lowercase consistency
        Liken_col <- paste0("skalometer_parties_", tolower(partyid))  # Current party
        Likem_col <- paste0("skalometer_parties_", tolower(party))    # Other party
        
        # Access column values
        Liken <- get(Liken_col)
        Likem <- get(Likem_col)
        
        # Vote shares
        Vote_share_m <- party_v_dict_w3[party] / 100  # Other party vote share
        Vote_share_n <- vote_share / 100  # Current party vote share
        
        # Formula component
        (Liken - Likem) * (Vote_share_m / (1 - Vote_share_n))
      }),
      na.rm = TRUE # Ensure NA values are handled
    )
  ) %>%
  ungroup()  # Remove rowwise grouping
survey_w4_processed_partylevel <- survey_w4_processed_partylevel %>%
  rowwise() %>%  # Process row by row
  mutate(
    APn = sum(
      sapply(names(party_v_dict_w4), function(party) {
        # Skip calculation if the party is the same as 'partyid'
        if (party == partyid) return(0)
        
        # Dynamically construct column names, ensuring lowercase consistency
        Liken_col <- paste0("skalometer_parties_", tolower(partyid))  # Current party
        Likem_col <- paste0("skalometer_parties_", tolower(party))    # Other party
        
        # Access column values
        Liken <- get(Liken_col)
        Likem <- get(Likem_col)
        
        # Vote shares
        Vote_share_m <- party_v_dict_w4[party] / 100  # Other party vote share
        Vote_share_n <- vote_share / 100  # Current party vote share
        
        # Formula component
        (Liken - Likem) * (Vote_share_m / (1 - Vote_share_n))
      }),
      na.rm = TRUE # Ensure NA values are handled
    )
  ) %>%
  ungroup()  # Remove rowwise grouping


##overall API
# List of dataframes and their corresponding wave labels
dataframes <- list(
  W1 = survey_w1_processed_partylevel,
  W2 = survey_w2_processed_partylevel,
  W3 = survey_w3_processed_partylevel,
  W4 = survey_w4_processed_partylevel
)

# Calculate API for each dataframe
API <- lapply(names(dataframes), function(wave) {
  df <- dataframes[[wave]]
  api_value <- df %>%
    summarise(API = sum(APn * (vote_share / 100), na.rm = TRUE)) %>%
    pull(API)
  data.frame(Wave = wave, API = api_value)
}) %>%
  bind_rows()  # Combine results into a single dataframe

# View the result
print(API)

###line plots
party_order <- c( "UNION","SPD", "AfD","FDP","LINKE","B90",    "Andere")

combined_data <- bind_rows(
  survey_w1_processed_partylevel %>% mutate(Wave = "W1"),
  survey_w2_processed_partylevel %>% mutate(Wave = "W2"),
  survey_w3_processed_partylevel %>% mutate(Wave = "W3"),
  survey_w4_processed_partylevel %>% mutate(Wave = "W4")
)
# Reorder the 'partyid' column in your data frame based on the desired party order
combined_data$partyid <- factor(combined_data$partyid, levels = party_order)

# Now create the plot with the ordered 'partyid'
line_plot_all_parties <- ggplot(combined_data, aes(x = Wave, y = APn, color = partyid, group = partyid)) +
  geom_line(size = 1) +  # Line for each partyid
  geom_point(size = 2) +  # Points for each wave
  labs(
    title = "APn Values of All Parties Over Time",
    x = "Wave",
    y = "APn",
    color = "Party ID"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, 7)) +  # Adjust y-axis to appropriate scale for APn
  scale_color_politics(palette = "fckafd") 
print(line_plot_all_parties)
