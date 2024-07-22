#install packages
library(tidyverse)
library(dplyr)
#load data (every election by year)
P_1920 <- read.csv("data/State_Presidential_Election_Data_1920_0_0_1.csv")
P_1924 <- read.csv("data/State_Presidential_Election_Data_1924_0_0_1.csv")
P_1928 <- read.csv("data/State_Presidential_Election_Data_1928_0_0_1.csv")
P_1932 <- read.csv("data/State_Presidential_Election_Data_1932_0_0_1.csv")
P_1936 <- read.csv("data/State_Presidential_Election_Data_1936_0_0_1.csv")
P_1940 <- read.csv("data/State_Presidential_Election_Data_1940_0_0_1.csv")
P_1944 <- read.csv("data/State_Presidential_Election_Data_1944_0_0_1.csv")
P_1948 <- read.csv("data/State_Presidential_Election_Data_1948_0_0_1.csv")
P_1952 <- read.csv("data/State_Presidential_Election_Data_1952_0_0_1.csv")
P_1956 <- read.csv("data/State_Presidential_Election_Data_1956_0_0_1.csv")
P_1960 <- read.csv("data/State_Presidential_Election_Data_1960_0_0_1.csv")
P_1964 <- read.csv("data/State_Presidential_Election_Data_1964_0_0_1.csv")
P_1968 <- read.csv("data/State_Presidential_Election_Data_1968_0_0_1.csv")
P_1972 <- read.csv("data/State_Presidential_Election_Data_1972_0_0_1.csv")
P_1976 <- read.csv("data/State_Presidential_Election_Data_1976_0_0_1.csv")
P_1980 <- read.csv("data/State_Presidential_Election_Data_1980_0_0_1.csv")
P_1984 <- read.csv("data/State_Presidential_Election_Data_1984_0_0_1.csv")
P_1988 <- read.csv("data/State_Presidential_Election_Data_1988_0_0_1.csv")
P_1992 <- read.csv("data/State_Presidential_Election_Data_1992_0_0_1.csv")
P_1996 <- read.csv("data/State_Presidential_Election_Data_1996_0_0_1.csv")
P_2000 <- read.csv("data/State_Presidential_Election_Data_2000_0_0_1.csv")
P_2004 <- read.csv("data/State_Presidential_Election_Data_2004_0_0_1.csv")
P_2008 <- read.csv("data/State_Presidential_Election_Data_2008_0_0_1.csv")
P_2012 <- read.csv("data/State_Presidential_Election_Data_2012_0_0_1.csv")
P_2016 <- read.csv("data/State_Presidential_Election_Data_2016_0_0_1_b.csv")
P_2020 <- read.csv("data/State_Presidential_Election_Data_2020_0_0_1.csv")

#list
dfs <- mget(ls(pattern = "^P_"))

# Add a new column for each data frame indicating the year
dfs <- lapply(names(dfs), function(x) {
  df <- dfs[[x]]
  df$Year <- as.numeric(sub("^P_", "", x))
  return(df)
})

# Merge all data frames into one
elections <- bind_rows(dfs)
#clean out extra rows
elections <- elections %>%
  mutate_all(as.numeric)
elections <- elections |>
  filter((!is.na(Total.Vote)))
#move Year first
elections <- elections |> 
  select(Year, everything())
View(elections)

#FIPS to state DF
fips <- read.csv("data/fips.csv")
fips <- fips |>
  select(FIPS, State)

#merge to label elections df
elections <- left_join(elections, fips, by = "FIPS")
elections <- elections |>
  select(FIPS, State, everything()) |>
  select(!Geographic.Name) |>
  select(!Geographic.Subtype)

# create 10 percent benchmark
elections <- elections |>
  mutate(tenp_benchmark = Total.Vote * 0.1) |>
  select(FIPS, State, Year, Total.Vote, tenp_benchmark, everything())
elections$tenp_benchmark <- elections$tenp_benchmark |>
  round()

# Eliminate columns where no candidate received 10% of the vote
candidate_columns <- names(elections)[!names(elections) %in% c("FIPS", "State", "Year", "Total.Vote", "tenp_benchmark")]
keep_columns <- sapply(candidate_columns, function(col) {
  any(elections[[col]] >= elections$tenp_benchmark, na.rm = TRUE)
})

# Add the non-candidate columns to keep_columns
keep_columns <- c(TRUE, TRUE, TRUE, TRUE, TRUE, keep_columns)

# Filter the elections data frame and get rid of "No.Candidate" and "Unpledged.Elector"
elections <- elections[, keep_columns]
elections <- elections |>
  select(!No.Candidate) |>
  select(!Unpledged.Elector) |>
  select(!Unpledged.Electors)
  
#make state-year variable for subsequent electoral vote merge
elections <- elections %>%
  mutate(State.Year = str_c(State, Year, sep = "_")) |>
  select(State.Year, everything())
  
#load in and merge Electoral College votes by state-year, which I built from hand,
# using information at https://www.270towin.com/state-electoral-vote-history/

ecv <- read.csv("data/EC_Votes.csv")
ecv_select <- ecv |>
  select(State.Year, Votes)
elections <- elections |>
  left_join(ecv_select, by = "State.Year")
elections <- elections %>%
  rename(EC_votes = Votes) %>%
  select(State.Year, FIPS, State, Year, EC_votes, everything())


# I would try to see if you can determine alignment from this data. 

#First, I would compute the fraction of the electoral votes a state has in a year.  

elections <- elections |>
  group_by(Year) |>
  mutate(total_EC_votes = sum(EC_votes, na.rm = TRUE)) 
 
elections <- elections |>
  mutate(EC_percentage = (EC_votes / total_EC_votes)*100)

#Second, I would determine the percentage of the state’s voters who voted for the winner. 

         # Identify the columns with candidate vote counts
candidate_columns <- setdiff(names(elections), c("FIPS", "State", "Year", "Total.Vote", 
                                                 "tenp_benchmark", "EC_votes","EC_percentage", 
                                                 "State.Year", "total_EC_votes"))

        # Calculate the percentage of the total vote for the winning candidate
elections <- elections %>%
  rowwise() %>%
  mutate(Winner_Vote_Count = max(c_across(all_of(candidate_columns)), na.rm = TRUE),
         Winner_Vote_Percentage = (Winner_Vote_Count / Total.Vote) * 100)


#Third, I would conduct a regression where the percent voting for the winner 
#was a function of the % of electoral votes.

#plot

ggplot(data = elections, aes(x = EC_percentage, y = Winner_Vote_Percentage)) +
  geom_point() +
  labs(title = "Percentage of Vote for Winning Candidate vs. Percentage of Electoral Votes",
       x = "Percentage of Electoral Votes",
       y = "Percentage of Vote for Winning Candidate") +
  geom_text(aes(label = ifelse((Winner_Vote_Percentage > 77 | EC_percentage > 5) | Winner_Vote_Percentage < 40 | (EC_percentage > 3.75 & Winner_Vote_Percentage > 70), State.Year, "")),
            hjust = 0.5, vjust = -1, size = 1.5, color = "red") +
  theme_minimal() +
  geom_smooth()

#regression
# Fit a linear regression model
regression_model_1 <- lm(Winner_Vote_Percentage ~ EC_percentage, data = elections)

# Summarize the regression results
summary(regression_model_1)

## Next, two more computations

#Function of your fraction, plus your gap (absolute value of the percent difference between
#the 1st and 2nd national candidates). Thus, if you had 60% for one and 40% for the other,
#your gap would be 20%, regardless of who won.

#compute second place vote count & gap

elections <- elections |>
  rowwise() |>
  mutate(Second_Vote_Count = sort(c_across(all_of(candidate_columns)), decreasing = TRUE)[2],
         Gap = abs(Winner_Vote_Count - Second_Vote_Count) / Total.Vote * 100)

# model

regression_model_2 <- lm(Winner_Vote_Percentage ~ EC_percentage + Gap, data = elections)
summary(regression_model_2)

#model 3: Function of your fraction, your gap, and an interaction term of 
                                              #your fraction times your gap.

regression_model_3 <- lm(Winner_Vote_Percentage ~ EC_percentage + Gap
                         + EC_percentage * Gap, data = elections)
summary(regression_model_3)

# more regressions, now with Alignment as the dependent variable and EC and Gap fracs 
# normalized to 100

elections <- elections |>
  mutate(alignment = Winner_Vote_Percentage / 100)
elections <- elections |>
  mutate(EC_frac = EC_percentage / 100)
elections <- elections |>
  mutate(Gap_frac = Gap / 100)

# next set of three regressions
#just ec fraction and gap

reg1 <- lm(alignment ~ EC_frac + Gap_frac, data = elections)
summary(reg1)

# just ec fraction and interaction term
elections$EC_Gap_interaction <- elections$EC_frac * elections$Gap_frac
reg2 <- lm(alignment ~ EC_frac + EC_Gap_interaction, data=elections)
summary(reg2)

#just gap

reg3 <- lm(alignment ~ Gap_frac, data=elections)
summary(reg3)

# og model

reg4 <- lm(alignment ~ EC_frac + Gap_frac + EC_frac*Gap_frac, data=elections)
summary(reg4)

# just gap and EC*g interaction term
elections$Gap_EC_interaction <- elections$Gap_frac * elections$EC_frac
reg5 <- lm(alignment ~ Gap_frac + Gap_EC_interaction, data=elections)
summary(reg5)


## functions table. the below fulfills RJZ's request for a table of values
# that displays outcomes for each model.

# Define the specific values for e and g
e_values <- c(0.005, 0.01, 0.015, 0.035, 0.075, 0.1, 0.2)
g_values <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 
              0.1, 0.15, 0.2, 0.3, 0.4, 0.6)

# Function to compute expected alignment for each model
compute_alignment <- function(e, g, model) {
  switch(model,
         "Model 1" = 0.475 + 0.066 * e + 0.515 * g,
         "Model 2" = 0.573 - 3.109 * e + 18.441 * e * g,
         "Model 3" = 0.477 + 0.514 * g,
         "Model 4" = 0.4728 + 0.183 * e + 0.528 * g - 0.732 * e * g,
         "Model 5" = 0.477 + 0.515 * g - 0.0509 * e * g)
}

# Create a data frame to store the results
results <- expand.grid(
  Gap = g_values,
  EC = e_values
)

# Calculate expected alignment for each model and interval
models <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")

for (model in models) {
  results[[model]] <- mapply(compute_alignment, results$EC, 
                             results$Gap, MoreArgs = list(model = model))
}

# Order the rows and columns correctly
results$Gap <- factor(results$Gap, levels = g_values)
results$EC <- factor(results$EC, levels = e_values)

# Print the results in the correct order
library(reshape2)

for (model in models) {
  cat(paste("\n", model, "\n"))
  model_data <- melt(results, id.vars = c("Gap", "EC"), measure.vars = model)
  model_table <- dcast(model_data, Gap ~ EC, value.var = "value")
  
  # Order the columns explicitly
  column_order <- as.character(e_values)
  model_table <- model_table[, c("Gap", column_order)]
  
  print(model_table[order(model_table$Gap), ])
}

# Include National Gap as a consideration
#load in organic national level data
national_elections <- read.csv("National_Results.csv")
elections <- left_join(elections, national_elections, by = "Year")

#add G to regression

model_g <- lm(alignment ~ EC_frac + Gap_frac + NatGap, data = elections)
summary(model_g)


#next steps: year variables

elections$Year <- as.factor(elections$Year)
model_fixed_effects <- lm(alignment ~ EC_frac + Gap_frac + 
                            EC_frac * Gap_frac + Year, data = elections)

summary(model_fixed_effects)

#and negative G


national_winner <- data.frame(
  Year = c(1920, 1924, 1928, 1932, 1936, 1940, 1944, 
           1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 
           1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 
           2012, 2016, 2020),
  National_Winner = c("Warren.Harding", "Calvin.Coolidge", "Herbert.Hoover", 
                      "Franklin.Roosevelt", "Franklin.Roosevelt", 
                      "Franklin.Roosevelt", "Franklin.Roosevelt", 
                      "Harry.Truman", "Dwight.Eisenhower", 
                      "Dwight.Eisenhower", "John.Kennedy", 
                      "Lyndon.Johnson", "Richard.Nixon", 
                      "Richard.Nixon", "James.Carter", 
                      "Ronald.Reagan", "Ronald.Reagan", 
                      "George.Bush", "William.Clinton", 
                      "William.Clinton", "George.W..Bush", 
                      "George.W..Bush", "Barack.H..Obama", 
                      "Barack.H..Obama", 
                      "Donald.J..Trump", "Joseph.R..Biden.Jr.")
)

elections <- elections %>%
  mutate(Year = as.numeric(as.character(Year)))

# Identify the columns with candidate names and their corresponding votes
candidate_columns <- names(elections)[8:54]

# Find the candidate with the highest vote in each state-year
elections <- elections %>%
  rowwise() %>%
  mutate(State_Winner = candidate_columns[which.max(c_across(all_of(candidate_columns)))])

# Merge national winner information with the elections dataset
elections <- elections %>%
  left_join(national_winner, by = "Year")

# Create the Gap_frac_adj variable
elections <- elections %>%
  mutate(Gap_frac_adj = ifelse(State_Winner == National_Winner, Gap_frac, -Gap_frac))

# View the updated dataset
head(elections)


## next, rerun model 4 with adjusted gap

reg4_adjusted <- lm(alignment ~ EC_frac + Gap_frac_adj + EC_frac*Gap_frac_adj, 
                    data=elections)
summary(reg4_adjusted)


## next, rerun model 4 with fixed effects and adjusted gap

elections$Year <- as.factor(elections$Year)
reg4_adjusted_fixedeffects <- lm(alignment ~ 
                            EC_frac + Gap_frac_adj + 
                              EC_frac * Gap_frac_adj + Year, 
                          data = elections)
summary(reg4_adjusted_fixedeffects)


#how many times top 6 states won and lost, by year, then total

top_6_states_per_year <- elections %>%
  group_by(Year) %>%
  top_n(6, EC_votes)

elections <- elections %>%
  mutate(Winner_Match = ifelse(State_Winner == National_Winner, TRUE, FALSE))

top_6_voting_results <- top_6_states_per_year %>%
  left_join(select(elections, State.Year, Year, Winner_Match), by = c("State.Year", "Year")) %>%
  group_by(Year) %>%
  summarise(
    Top_6_Voted_For_Winner = sum(Winner_Match, na.rm = TRUE),
    Top_6_Not_Voted_For_Winner = 6 - sum(Winner_Match, na.rm = TRUE)
  )

print(top_6_voting_results, n = 26)

sum(top_6_voting_results$Top_6_Voted_For_Winner)
sum(top_6_voting_results$Top_6_Not_Voted_For_Winner)

top_6_states_per_year |>
  ungroup() |>
  count(State)

# Identify the states not in the top 6 electoral votes for each year
states_not_top_6_per_year <- elections %>%
  group_by(Year) %>%
  mutate(rank = rank(-EC_votes, ties.method = "first")) %>%
  filter(rank > 6)

# Count the number of times the state winner matched the national winner and the number of times it did not for states not in top 6
states_not_top_6_results <- states_not_top_6_per_year %>%
  mutate(Match_National_Winner = ifelse(State_Winner == National_Winner, 1, 0)) %>%
  group_by(Year) %>%
  summarise(
    Matches = sum(Match_National_Winner),
    Non_Matches = n() - Matches
  )

# Count occurrences for states not in top 6 across all years
states_not_top_6_count <- states_not_top_6_per_year %>%
  ungroup() %>%
  count(State) %>%
  arrange(desc(n))

# Print the results
print(states_not_top_6_results, n = 26)
print(states_not_top_6_count, n = 47)

# Identify the states in the bottom 10 electoral votes for each year
bottom_10_states_per_year <- elections %>%
  group_by(Year) %>%
  mutate(rank = rank(EC_votes, ties.method = "first")) %>%
  filter(rank <= 10)

# Count the number of times the state winner matched the national winner and the number of times it did not for bottom 10 states
bottom_10_states_results <- bottom_10_states_per_year %>%
  mutate(Match_National_Winner = ifelse(State_Winner == National_Winner, 1, 0)) %>%
  group_by(Year) %>%
  summarise(
    Matches = sum(Match_National_Winner),
    Non_Matches = n() - Matches
  )

# Count occurrences for bottom 10 states across all years
bottom_10_states_count <- bottom_10_states_per_year %>%
  ungroup() %>%
  count(State) %>%
  arrange(desc(n))

# Print the results
print(bottom_10_states_results, n = 26)
print(bottom_10_states_count, n = 16)

# find alignment frequency of top 48 bundles

# Function to select top N states with hard cutoff for ties
select_top_n_states <- function(df, n) {
  df %>%
    arrange(desc(EC_votes)) %>%
    group_by(Year) %>%
    slice_head(n = n) %>%
    ungroup()
}

# Create a function to calculate alignment frequency for each bundle size
calculate_alignment_frequencies <- function(max_bundle_size) {
  frequencies <- c()
  
  for (i in 1:max_bundle_size) {
    # Select the top i states for each year with a hard cutoff
    top_states <- elections %>%
      group_by(Year) %>%
      arrange(desc(EC_votes), .by_group = TRUE) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= i) %>%
      ungroup()
    
    # If there's a tie on the cutoff, randomly sample the required number of states
    top_states <- top_states %>%
      group_by(Year, rank) %>%
      mutate(tie_group = ifelse(rank == i, sample(1:n(), 1), 1)) %>%
      filter(tie_group == 1) %>%
      ungroup()
    
    # Calculate alignment
    top_states <- top_states %>%
      mutate(Aligned = ifelse(State_Winner == National_Winner, TRUE, FALSE))
    
    alignment_freq <- top_states %>%
      group_by(Year) %>%
      summarise(Alignment_Percentage = mean(Aligned, na.rm = TRUE)) %>%
      summarise(Frequency = mean(Alignment_Percentage, na.rm = TRUE))
    
    frequencies <- c(frequencies, alignment_freq$Frequency)
  }
  
  data.frame(Bundle_Size = 1:max_bundle_size, Frequency = frequencies)
}

# Calculate alignment frequencies for bundle sizes from 1 to 48
alignment_frequencies_df <- calculate_alignment_frequencies(48)

# Print the alignment frequencies data frame
print(alignment_frequencies_df)

ggplot(alignment_frequencies_df, aes(x = Bundle_Size, y = Frequency)) +
  geom_point(color = "red") +                  # Add points
  geom_line(color = "red") +                   # Connect points with lines
  labs(title = "Alignment Frequency of Top Electoral Vote States",
       x = "Number of Top Electoral Vote States (Bundled)",
       y = "Alignment Frequency") +
  theme_minimal()                 # Use a minimal theme for a clean look


##now the bottom 48 bundles 
# Function to select bottom N states with hard cutoff for ties
select_bottom_n_states <- function(df, n) {
  df %>%
    arrange(EC_votes) %>%
    group_by(Year) %>%
    slice_head(n = n) %>%
    ungroup()
}

# Create a function to calculate alignment frequency for each bundle size
calculate_alignment_frequencies_bottom <- function(max_bundle_size) {
  frequencies <- c()
  
  for (i in 1:max_bundle_size) {
    # Select the bottom i states for each year with a hard cutoff
    bottom_states <- elections %>%
      group_by(Year) %>%
      arrange(EC_votes, .by_group = TRUE) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= i) %>%
      ungroup()
    
    # If there's a tie on the cutoff, randomly sample the required number of states
    bottom_states <- bottom_states %>%
      group_by(Year, rank) %>%
      mutate(tie_group = ifelse(rank == i, sample(1:n(), 1), 1)) %>%
      filter(tie_group == 1) %>%
      ungroup()
    
    # Calculate alignment
    bottom_states <- bottom_states %>%
      mutate(Aligned = ifelse(State_Winner == National_Winner, TRUE, FALSE))
    
    alignment_freq <- bottom_states %>%
      group_by(Year) %>%
      summarise(Alignment_Percentage = mean(Aligned, na.rm = TRUE)) %>%
      summarise(Frequency = mean(Alignment_Percentage, na.rm = TRUE))
    
    frequencies <- c(frequencies, alignment_freq$Frequency)
  }
  
  data.frame(Bundle_Size = 1:max_bundle_size, Frequency = frequencies)
}

# Calculate alignment frequencies for bundle sizes from 1 to 48 for bottom states
alignment_frequencies_df_bottom <- calculate_alignment_frequencies_bottom(48)

# Ensure Bundle_Size is treated as numeric for plotting
alignment_frequencies_df_bottom$Bundle_Size <- as.numeric(alignment_frequencies_df_bottom$Bundle_Size)

# Print the first few rows of alignment_frequencies_df_bottom
head(alignment_frequencies_df_bottom)

  # Ensure Bundle_Size is numeric for both data frames
  alignment_frequencies_df$Bundle_Size <- as.numeric(as.character(alignment_frequencies_df$Bundle_Size))
  alignment_frequencies_df_bottom$Bundle_Size <- as.numeric(as.character(alignment_frequencies_df_bottom$Bundle_Size))
  
  # Plot using ggplot
  
  ggplot() +
    geom_line(data = alignment_frequencies_df, aes(x = Bundle_Size, y = Frequency, color = "Top by EV"), linewidth = 1) +
    geom_point(data = alignment_frequencies_df, aes(x = Bundle_Size, y = Frequency, color = "Top by EV")) +
    geom_line(data = alignment_frequencies_df_bottom, aes(x = Bundle_Size, y = Frequency, color = "Bottom by EV"), linewidth = 1) +
    geom_point(data = alignment_frequencies_df_bottom, aes(x = Bundle_Size, y = Frequency, color = "Bottom by EV")) +
    labs(title = "National-State Winner Match Frequency",
         x = "Bundle Size (Number of States)",
         y = "Match (Alignment) Frequency") +
    scale_color_manual(values = c("Top by EV" = "blue", 
                                  "Bottom by EV" = "red")) +  # Custom color palette
    theme_minimal()
  

#### now swing state thing
  
  # how many swing states by either metric?
  
  sum(elections$Gap <= 1)
  
  sum(elections$Gap <= 2)

  # Ensure Year is numeric
  elections$Year <- as.numeric(elections$Year)
  
  # Sort by State and Year to ensure proper comparison
  elections <- elections %>%
    arrange(State, Year)
  
  # Create swing_state_1 (previous election)
  elections$swing_state_1 <- c(FALSE, elections$State[-1] == 
                                 elections$State[-nrow(elections)] & 
                                 elections$Gap[-nrow(elections)] < 1)
  
  # Create swing_state_2
  elections$swing_state_2 <- elections$Gap < 1
  View(elections)

  # table with average alignment
  alignment_summary_1 <- elections %>%
    group_by(swing_state_1) %>%
    summarise(Average_Alignment = mean(alignment, na.rm = TRUE))
  colnames(alignment_summary_1) <- c("Swing State Defined by Previous Election Gap <1%", 
                                     "Average Alignment")

  
  # Calculate mean alignment for swing_state_2
  alignment_summary_2 <- elections %>%
    group_by(swing_state_2) %>%
    summarise(Average_Alignment = mean(alignment, na.rm = TRUE))
  colnames(alignment_summary_2) <- c("Swing State Defined by Current Election Gap <1%", 
                                     "Average Alignment")
  
  
### LOAD IN POPULATION DATA
  
  # Load the dataset
  population_data <- read.csv("population.csv")
  population_data <- population_data[-1, ]
  # Correct the column names
  colnames(population_data) <- c("State", as.character(1900:2022))
  
  # Remove the row with NA values in 'State' column (if it exists)
  population_data <- population_data[!is.na(population_data$State), ]
  
  # Convert all year columns to numeric
  population_data <- population_data %>%
    mutate(across(-State, as.numeric))
  
  # Reshape the data from wide to long format
  long_population_data <- pivot_longer(
    population_data,
    cols = -State,
    names_to = "Year",
    values_to = "Population"
  )
  
  # Remove the " (People)" part from state names
  long_population_data <- long_population_data %>%
    mutate(State = str_replace(State, " \\(People\\)", "")) |>
    mutate(State = str_replace(State, "District of Columbia", "DC"))

  
  # Create the State_Year column
  long_population_data <- long_population_data %>%
    mutate(State.Year = paste0(State, "_", Year))
  
  # Reorder the columns
  long_population_data <- long_population_data %>%
    select(State.Year, Population)
  
  #put in early population data for HI and AK ??
  
  #match in pop
  long_population_data <- long_population_data %>%
    mutate(State.Year = toupper(str_trim(State.Year)))
  elections <- left_join(elections, long_population_data, by = "State.Year")
  
  # Calculate the national population for each year
  national_population <- elections %>%
    group_by(Year) %>%
    summarise(National_Population = sum(Population, na.rm = TRUE))
  # Merge the national population back into the original dataset
  elections <- elections %>%
    left_join(national_population, by = "Year")
  
  #Calculate total votes for significant candidates 
  national_votes <- elections |>
    mutate(State_Votes = Winner_Vote_Count + Second_Vote_Count)
  national_votes <- national_votes |>
    group_by(Year) |>
    summarise(National_Votes = sum(State_Votes))
  elections <- elections |>
    left_join(national_votes, by = "Year")
  
  #State Percentage Population Variables
  elections <- elections |>
    mutate(Pop_Frac = Population / National_Population)
  elections <- elections |>
    mutate(Voter_Frac = (Winner_Vote_Count + Second_Vote_Count) / National_Votes)

  
## USE THESE TO CREATE NEW BUNDLES BASED ON POPN
  
  # Function to select top N states based on Pop_Frac with a hard cutoff for ties
  select_top_n_states_by_pop_frac <- function(df, n) {
    df %>%
      arrange(Year, desc(Pop_Frac)) %>%
      group_by(Year) %>%
      slice(1:n) %>%
      ungroup()
  }
  
  # Create top bundles based on Pop_Frac
  top_pop_bundles <- list()
  
  for (i in 1:10) {
    top_pop_bundles[[i]] <- select_top_n_states_by_pop_frac(elections, i)
  }
  
  # Combine the top bundles into one data frame and add a Bundle_Size column
  top_pop_bundles_df <- bind_rows(top_pop_bundles, .id = "Bundle_Size")
  top_pop_bundles_df$Bundle_Size <- as.integer(top_pop_bundles_df$Bundle_Size)
  
  # Function to create bottom bundles based on Pop_Frac
  create_bottom_bundles <- function(df, top_bundles_df) {
    bottom_pop_bundles <- list()
    
    for (i in unique(top_bundles_df$Bundle_Size)) {
      top_pop_sum <- top_bundles_df %>%
        filter(Bundle_Size == i) %>%
        group_by(Year) %>%
        summarise(Pop_Sum = sum(Pop_Frac)) %>%
        ungroup()
      
      bottom_bundle <- df %>%
        arrange(Year, Pop_Frac) %>%
        group_by(Year) %>%
        mutate(cum_pop_frac = cumsum(Pop_Frac)) %>%
        filter(cum_pop_frac <= top_pop_sum$Pop_Sum[match(Year, top_pop_sum$Year)]) %>%
        ungroup()
      
      # Adjust for cases where we might have exceeded the top population sum
      bottom_bundle <- bottom_bundle %>%
        group_by(Year) %>%
        filter(cumsum(Pop_Frac) - Pop_Frac < top_pop_sum$Pop_Sum[match(Year, top_pop_sum$Year)]) %>%
        ungroup()
      
      bottom_pop_bundles[[i]] <- bottom_bundle
    }
    
    bottom_pop_bundles_df <- bind_rows(bottom_pop_bundles, .id = "Bundle_Size")
    bottom_pop_bundles_df$Bundle_Size <- as.integer(bottom_pop_bundles_df$Bundle_Size)
    
    return(bottom_pop_bundles_df)
  }
  
  # Create bottom bundles
  bottom_pop_bundles_df <- create_bottom_bundles(elections, top_pop_bundles_df)
  
  ## VERIFY POPULATION FRACTIONS USED
  
  # Calculate the total Pop_Frac for each top bundle size
  top_pop_frac <- top_pop_bundles_df %>%
    group_by(Bundle_Size, Year) %>%
    summarise(Total_Pop_Frac = sum(Pop_Frac)) %>%
    ungroup() %>%
    group_by(Bundle_Size) %>%
    summarise(Average_Pop_Frac = mean(Total_Pop_Frac, na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate the total Pop_Frac for each bottom bundle size
  bottom_pop_frac <- bottom_pop_bundles_df %>%
    group_by(Bundle_Size, Year) %>%
    summarise(Total_Pop_Frac = sum(Pop_Frac)) %>%
    ungroup() %>%
    group_by(Bundle_Size) %>%
    summarise(Average_Pop_Frac = mean(Total_Pop_Frac, na.rm = TRUE)) %>%
    ungroup()
  
  
  # Add Bundle_Pop_Frac to top bundles
  top_pop_bundles_df <- top_pop_bundles_df %>%
    group_by(Bundle_Size, Year) %>%
    mutate(Bundle_Pop_Frac = sum(Pop_Frac)) %>%
    ungroup()
  
  # Add Bundle_Pop_Frac to bottom bundles
  bottom_pop_bundles_df <- bottom_pop_bundles_df %>%
    group_by(Bundle_Size, Year) %>%
    mutate(Bundle_Pop_Frac = sum(Pop_Frac)) %>%
    ungroup()
  
  # Calculate alignment frequencies for top bundles
  top_alignment_frequencies <- top_pop_bundles_df %>%
    group_by(Bundle_Size) %>%
    summarise(Alignment_Frequency = mean(Winner_Match, na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate alignment frequencies for bottom bundles
  bottom_alignment_frequencies <- bottom_pop_bundles_df %>%
    group_by(Bundle_Size) %>%
    summarise(Alignment_Frequency = mean(Winner_Match, na.rm = TRUE)) %>%
    ungroup()
  
  # Combine alignment frequencies with population fractions
  alignment_frequencies_combined <- bind_rows(
    top_alignment_frequencies %>% mutate(Type = "Top Bundles"),
    bottom_alignment_frequencies %>% mutate(Type = "Bottom Bundles")
  ) %>%
    left_join(top_pop_fractions, by = "Bundle_Size") %>%
    left_join(bottom_pop_fractions, by = "Bundle_Size", suffix = c("_Top", "_Bottom"))
  
  # Plot the alignment frequencies with Bundle_Size and add text labels for Bundle_Pop_Frac
  ggplot(alignment_frequencies_combined, aes(x = Bundle_Size, y = Alignment_Frequency, color = Type)) +
    geom_line() +
    geom_point() +
    geom_text(aes(label = round(Bundle_Pop_Frac_Top, 2)), data = alignment_frequencies_combined %>% filter(Type == "Top Bundles"), vjust = -1.5, color = "blue") +
    geom_text(aes(label = round(Bundle_Pop_Frac_Bottom, 2)), data = alignment_frequencies_combined %>% filter(Type == "Bottom Bundles"), vjust = 1.5, color = "red") +
    scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +  # Adjust x-axis to show only integer breaks
    labs(title = "E.C. Alignment Frequency for Top and Bottom States by Population Fraction",
         x = "Bundle Size (Popn. Percent Labeled)",
         y = "Alignment Frequency",
         color = "Bundle Type") +
    theme_minimal()
  
## NOW WITH VOTER FRAC
  # Function to select top N states by Voter_Frac with hard cutoff for ties
  select_top_n_states_voter <- function(df, n) {
    df %>%
      arrange(desc(Voter_Frac)) %>%
      group_by(Year) %>%
      slice_head(n = n) %>%
      ungroup()
  }
  
  # Function to select bottom N states by Voter_Frac with hard cutoff for ties
  select_bottom_n_states_voter <- function(df, n) {
    df %>%
      arrange(Voter_Frac) %>%
      group_by(Year) %>%
      slice_head(n = n) %>%
      ungroup()
  }
  
  # Function to calculate alignment frequency for each bundle size using Voter_Frac
  calculate_alignment_frequencies_voter <- function(max_bundle_size) {
    top_frequencies <- c()
    bottom_frequencies <- c()
    top_bundle_pop_fracs <- c()
    bottom_bundle_pop_fracs <- c()
    
    for (i in 1:max_bundle_size) {
      # Top i states by Voter_Frac
      top_states_voter <- select_top_n_states_voter(elections, i) %>%
        mutate(Aligned = Winner_Match)
      
      top_alignment_freq <- top_states_voter %>%
        group_by(Year) %>%
        summarise(Alignment_Percentage = mean(Aligned, na.rm = TRUE)) %>%
        summarise(Frequency = mean(Alignment_Percentage, na.rm = TRUE))
      
      top_frequencies <- c(top_frequencies, top_alignment_freq$Frequency)
      
      # Calculate the total Voter_Frac for top bundle size i
      total_voter_frac_top <- top_states_voter %>%
        group_by(Year) %>%
        summarise(total = sum(Voter_Frac))
      
      # Calculate the bottom bundle states
      bottom_states_voter <- elections %>%
        arrange(Voter_Frac) %>%
        group_by(Year) %>%
        mutate(cumulative_frac = cumsum(Voter_Frac)) %>%
        filter(cumulative_frac <= total_voter_frac_top$total[i])
      
      bottom_states_voter <- bottom_states_voter %>%
        mutate(Aligned = Winner_Match)
      
      bottom_alignment_freq <- bottom_states_voter %>%
        group_by(Year) %>%
        summarise(Alignment_Percentage = mean(Aligned, na.rm = TRUE)) %>%
        summarise(Frequency = mean(Alignment_Percentage, na.rm = TRUE))
      
      bottom_frequencies <- c(bottom_frequencies, bottom_alignment_freq$Frequency)
      
      # Calculate the Voter_Frac for the current bundle size
      top_bundle_pop_frac <- top_states_voter %>%
        group_by(Year) %>%
        summarise(total = sum(Voter_Frac)) %>%
        summarise(Bundle_Pop_Frac = mean(total))
      
      bottom_bundle_pop_frac <- bottom_states_voter %>%
        group_by(Year) %>%
        summarise(total = sum(Voter_Frac)) %>%
        summarise(Bundle_Pop_Frac = mean(total))
      
      top_bundle_pop_fracs <- c(top_bundle_pop_fracs, top_bundle_pop_frac$Bundle_Pop_Frac)
      bottom_bundle_pop_fracs <- c(bottom_bundle_pop_fracs, bottom_bundle_pop_frac$Bundle_Pop_Frac)
    }
    
    # Create data frames for top and bottom bundles
    top_bundles_df <- data.frame(Bundle_Size = 1:max_bundle_size, 
                                 Alignment_Frequency = top_frequencies, 
                                 Type = "Top Bundles", 
                                 Bundle_Pop_Frac = top_bundle_pop_fracs)
    
    bottom_bundles_df <- data.frame(Bundle_Size = 1:max_bundle_size, 
                                    Alignment_Frequency = bottom_frequencies, 
                                    Type = "Bottom Bundles", 
                                    Bundle_Pop_Frac = bottom_bundle_pop_fracs)
    
    # Combine the data frames
    rbind(top_bundles_df, bottom_bundles_df)
  }
  
  # Calculate alignment frequencies for bundle sizes from 1 to 10 using Voter_Frac
  alignment_frequencies_df_voter <- calculate_alignment_frequencies_voter(10)
  
  # Plot the alignment frequencies with Bundle_Size and add text labels for Bundle_Pop_Frac
  ggplot(alignment_frequencies_df_voter, aes(x = Bundle_Size, y = Alignment_Frequency, color = Type)) +
    geom_line() +
    geom_point() +
    geom_text(aes(label = round(Bundle_Pop_Frac, 2)), 
              data = alignment_frequencies_df_voter %>% filter(Type == "Top Bundles"), 
              vjust = -1.5, color = "blue") +
    geom_text(aes(label = round(Bundle_Pop_Frac, 2)), 
              data = alignment_frequencies_df_voter %>% filter(Type == "Bottom Bundles"), 
              vjust = 1.5, color = "red") +
    scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +  # Adjust x-axis to show only integer breaks
    labs(title = "E.C. Alignment Frequency for Top and Bottom States by Voter Fraction",
         x = "Bundle Size (Voter Fraction Labeled)",
         y = "Alignment Frequency",
         color = "Bundle Type") +
    theme_minimal()+
    theme(text=element_text(size=12,  family="Times New Roman"))
  
  
  