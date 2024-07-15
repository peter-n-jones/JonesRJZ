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
    geom_line(data = alignment_frequencies_df, aes(x = Bundle_Size, y = Frequency, color = "Top"), linewidth = 1) +
    geom_point(data = alignment_frequencies_df, aes(x = Bundle_Size, y = Frequency, color = "Top")) +
    geom_line(data = alignment_frequencies_df_bottom, aes(x = Bundle_Size, y = Frequency, color = "Bottom"), linewidth = 1) +
    geom_point(data = alignment_frequencies_df_bottom, aes(x = Bundle_Size, y = Frequency, color = "Bottom")) +
    labs(title = "Alignment Frequency of State Winners with National Winners",
         x = "Bundle Size (Number of States)",
         y = "Alignment Frequency") +
    scale_color_manual(values = c("Top" = "blue", "Bottom" = "red")) +  # Custom color palette
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
  
  View(alignment_summary_2)