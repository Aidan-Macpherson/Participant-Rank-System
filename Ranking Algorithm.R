library(readr)
library(dplyr)

# CUSTOMISE: import the data
data = read_csv("PATH/Example Reviewer Form.csv")

# Change the column names
colnames(data) <- c('Timestamp', 'Team Name', 'Reviewer Name', 'Impact', 'Feasibility', 'Innovation', 'Insights', 'Comprehensiveness', 'Oral Presentation', 'Slide Deck Presentation', 'Feedback')

# Specify the score columns (exclude non-score columns like 'Timestamp', 'Team Name', 'Reviewer Name')
score_columns <- c('Impact', 'Feasibility', 'Innovation', 'Insights', 'Comprehensiveness', 'Oral Presentation', 'Slide Deck Presentation')

# Convert the string scores to number scores only in the score columns
for (col in score_columns) {
  data[[col]] <- ifelse(grepl('Excellent', data[[col]]), 5,
                        ifelse(grepl('Very good', data[[col]]), 4,
                               ifelse(grepl('Good', data[[col]]), 3, 
                                      ifelse(grepl('Fair', data[[col]]), 2,
                                             ifelse(grepl('Poor', data[[col]]), 1, data[[col]])))))
}

# Select the columns of interest
data = data %>% select(-c('Timestamp', 'Feedback'))

# Convert the score columns to numeric
data <- data %>%
  mutate_at(vars(all_of(score_columns)), ~ as.numeric(.))

# Create a column for the row sums
data = data %>%
  mutate(Sum = rowSums(select(., all_of(score_columns)), na.rm = TRUE))

# CUSTOMISE: for each judge, rank the teams
Reviewer_Names = c('Reviewer1', 'Reviewer2', 'Reviewer3')

# CUSTOMISE: Create empty list with all the reviewers' names
Reviewer1 = list()
Reviewer2 = list()
Reviewer3 = list()

# CUSTOMISE: create a list where each element will hold the reviewers' rankings of the teams. Should be isomorphic to 'Reviewer_Names'
Reviewer_Names_List = list(Reviewer1, Reviewer2, Reviewer3)

# Assigns the teams and proper ranks to each of the judges' datasets
for (i in Reviewer_Names){
  Reviewer_Names_List[[i]] = data %>%
    filter(`Reviewer Name` == i) %>%
    select(`Team Name`, Sum) %>%
    arrange(desc(Sum)) %>%           # Arrange teams in descending order of Sum (highest score first)
    mutate(Rank = row_number())     # Add a Rank column based on the order
    
    for (k in 2:nrow(Reviewer_Names_List[[i]])) {
        if (Reviewer_Names_List[[i]]$Sum[k] == Reviewer_Names_List[[i]]$Sum[k-1])
          Reviewer_Names_List[[i]]$Rank[k] = Reviewer_Names_List[[i]]$Rank[k-1]
    }
    
    # Select the relevant columns
    Reviewer_Names_List[[i]] <- Reviewer_Names_List[[i]] %>%
      select(`Team Name`, Rank)
}

# CUSTOMISE: extract the reviewers' datasets for easy use 
Reviewer1 = Reviewer_Names_List[['Reviewer1']]
Reviewer2 = Reviewer_Names_List[['Reviewer2']]
Reviewer3 = Reviewer_Names_List[['Reviewer3']]

Reviewer_Names_List = list(Reviewer1, Reviewer2, Reviewer3)

# Export to CSV
# write.csv(average_ranks, file = "PATH.csv", row.names = FALSE)

###################################

# Method: SVD

# Initialize an empty data frame
all_ranks = data.frame()

# Loop through each reviewer's data and perform full join by 'Team Name'. Results in a complete 'all_ranks' data frame
for (i in seq_along(Reviewer_Names_List)) {
  colnames(Reviewer_Names_List[[i]]) = c('Team Name', Reviewer_Names[i])
  all_ranks = if (nrow(all_ranks) == 0) {
    # For the first iteration, set `all_ranks` to the first reviewer's data
    Reviewer_Names_List[[i]]
  } else {
    # For subsequent iterations, join with the existing `all_ranks`
    full_join(all_ranks, Reviewer_Names_List[[i]], by = 'Team Name')
  }
}

# CUSTOMISE: Make necessary imputations, if any. Ex:

#all_ranks$`Reviewer1`[12] = 5.5

#########

# A) Use subtraction as the comparison function

# CUSTOMISE: make the subtraction comparison matrices for each judge
comp_Reviewer1 = outer(all_ranks[[2]], all_ranks[[2]], FUN = '-')
comp_Reviewer2 = outer(all_ranks[[3]], all_ranks[[3]], FUN = '-')
comp_Reviewer3 = outer(all_ranks[[4]], all_ranks[[4]], FUN = '-')

# CUSTOMISE: store all comparison matrices in a list
comp_matrices = list(comp_Reviewer1, comp_Reviewer2, comp_Reviewer3)

# Convert all NA elements to 0
for(i in seq_along(comp_matrices)){
  comp_matrices[[i]][is.na(comp_matrices[[i]])] <- 0
}

# add all comparison matrices to make one final comparison matrix
final_comp_matrix = Reduce(`+`, comp_matrices)

# Perform Singular Value Decomposition (SVD)
svd_result = svd(final_comp_matrix)

# Extract the first singular vector from U (left singular vectors)
first_singular_vector = svd_result$u[, 1]   # First column of U

# Combine with team names, assuming 'all_ranks' has a 'Team Name' column
first_singular_vector_df = data.frame(`Team Name` = all_ranks$`Team Name`, `Relative Rank` = first_singular_vector)

# Arrange by Relative Rank
SVD_ranks_subtraction = first_singular_vector_df %>%
  arrange(desc(`Relative.Rank`)) %>%           # Arrange teams in descending order of Sum (highest score first)
  mutate(`SVD Subtraction Rank` = row_number())

############

# B) Use division as the comparison function

# CUSTOMISE: make the division comparison matrices for each judge
comp_Reviewer1 = outer(all_ranks[[2]], all_ranks[[2]], FUN = '/')
comp_Reviewer2 = outer(all_ranks[[3]], all_ranks[[3]], FUN = '/')
comp_Reviewer3 = outer(all_ranks[[4]], all_ranks[[4]], FUN = '/')

# CUSTOMISE: store all comparison matrices in a list
comp_matrices = list(comp_Reviewer1, comp_Reviewer2, comp_Reviewer3)

# Convert all NA elements to 0
for(i in seq_along(comp_matrices)){
  comp_matrices[[i]][is.na(comp_matrices[[i]])] <- 0
}

# add all comparison matrices to make one final comparison matrix
final_comp_matrix = Reduce(`+`, comp_matrices)

# Perform Singular Value Decomposition (SVD)
svd_result = svd(final_comp_matrix)

# Extract the first singular vector from U (left singular vectors)
first_singular_vector = svd_result$u[, 1]   # First column of U

# Combine with team names, assuming 'all_ranks' has a 'Team Name' column
first_singular_vector_df = data.frame(`Team Name` = all_ranks$`Team Name`, `Relative Rank` = first_singular_vector)

# Arrange by Relative Rank
SVD_ranks_division = first_singular_vector_df %>%
  arrange(desc(`Relative.Rank`)) %>%           # Arrange teams in descending order of Sum (highest score first)
  mutate(`SVD Division Rank` = row_number())

############

# C) Summate the individual ranks

all_ranks = all_ranks %>%
  mutate(Sum = rowSums(select(., all_of(Reviewer_Names)), na.rm = TRUE)) %>%
  arrange(Sum) %>%           # Arrange teams in descending order of Sum (highest score first)
  mutate('Summation Rank' = row_number())

##################

# Make the matrix with all the SVD Division, SVD Subtraction, and Summation ranks

model_ranks_df = full_join(SVD_ranks_division, SVD_ranks_subtraction, by = 'Team.Name')

model_ranks_df = model_ranks_df %>%
  rename(`Team Name` = `Team.Name`) %>%
  full_join(., all_ranks, by = 'Team Name') %>%
  select(c('Team Name', 'SVD Division Rank', 'SVD Subtraction Rank', 'Summation Rank')) %>%
  mutate(Sum = rowSums(select(., all_of(c('SVD Division Rank', 'SVD Subtraction Rank', 'Summation Rank'))), na.rm = TRUE)) %>%
  arrange(Sum) %>%           # Arrange teams in descending order of Sum (highest score first)
  mutate('Summation Rank' = row_number())

final_ranks = model_ranks_df %>%
  select(c(`Team Name`, Sum))

# Export to CSV
write.csv(final_ranks, file = "PATH.csv", row.names = FALSE)
