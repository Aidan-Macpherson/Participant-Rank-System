library(readr)
library(dplyr)

# CUSTOMISE: import the data
data = read_csv("Quarter Finals Video Submission Evaluation (Responses) - Form Responses 1.csv")

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
Reviewer_Names = c('Aditya', 'Lisa', 'Bailey', 'Sophia', 'Jennifer', 'Nadeem', 'Hannah', 'David', 'Aidan', 'Xueer', 'Haihui')

# CUSTOMISE: Create empty list with all the judges' names
Aditya = list()
Lisa = list()
Bailey = list()
Sophia = list()
Jennifer = list()
Nadeem = list()
Hannah = list()
David = list()
Aidan = list()
Xueer = list()
Haihui = list()

# CUSTOMISE: create a list where each element will hold the judges' rankings of the teams. Should be isomorphic to 'Reviewer_Names'
Reviewer_Names_List = list(Aditya, Lisa, Bailey, Sophia, Jennifer, Nadeem, Hannah, David, Aidan, Xueer, Haihui)

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

# CUSTOMISE: extract the judges' datasets for easy use 
Aditya = Reviewer_Names_List[['Aditya']]
Lisa = Reviewer_Names_List[['Lisa']]
Bailey = Reviewer_Names_List[['Bailey']]
Sophia = Reviewer_Names_List[['Sophia']]
Jennifer = Reviewer_Names_List[['Jennifer']]
Nadeem = Reviewer_Names_List[['Nadeem']]
Hannah = Reviewer_Names_List[['Hannah']]
David = Reviewer_Names_List[['David']]
Aidan = Reviewer_Names_List[['Aidan']]
Xueer = Reviewer_Names_List[['Xueer']]
Haihui = Reviewer_Names_List[['Haihui']]

Reviewer_Names_List = list(Aditya, Lisa, Bailey, Sophia, Jennifer, Nadeem, Hannah, David, Aidan, Xueer, Haihui)

# Export to CSV
# write.csv(average_ranks, file = "MHCC 2024 Quarter Finals Average Ranks.csv", row.names = FALSE)

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

# CUSTOMISE: Make necessary imputations, if any

#all_ranks$`Aditya`[12] = 5.5
#all_ranks$`Aidan`[24] = 1
#all_ranks$`Aditya`[21] = 1.5
#all_ranks$`Bailey`[2] = 1
#all_ranks$`Nadeem`[8] = 2
#all_ranks$`David`[22] = 3.5

#########

# A) Use subtraction as the comparison function

# CUSTOMISE: make the subtraction comparison matrices for each judge
comp_Adi = outer(all_ranks[[2]], all_ranks[[2]], FUN = '-')
comp_Lisa = outer(all_ranks[[3]], all_ranks[[3]], FUN = '-')
comp_Bailey = outer(all_ranks[[4]], all_ranks[[4]], FUN = '-')
comp_Sophia = outer(all_ranks[[5]], all_ranks[[5]], FUN = '-')
comp_Jennifer = outer(all_ranks[[6]], all_ranks[[6]], FUN = '-')
comp_Nadeem = outer(all_ranks[[7]], all_ranks[[7]], FUN = '-')
comp_Hannah = outer(all_ranks[[8]], all_ranks[[8]], FUN = '-')
comp_David = outer(all_ranks[[9]], all_ranks[[9]], FUN = '-')
comp_Aidan = outer(all_ranks[[10]], all_ranks[[10]], FUN = '-')
comp_Xueer = outer(all_ranks[[11]], all_ranks[[11]], FUN = '-')

# CUSTOMISE: store all comparison matrices in a list
comp_matrices = list(comp_Adi, comp_Lisa, comp_Bailey, comp_Sophia, comp_Jennifer, comp_Nadeem, comp_Hannah, comp_David, comp_Aidan, comp_Xueer)

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

# CUSTOMISE: make the subtraction comparison matrices for each judge
comp_Adi = outer(all_ranks[[2]], all_ranks[[2]], FUN = '/')
comp_Lisa = outer(all_ranks[[3]], all_ranks[[3]], FUN = '/')
comp_Bailey = outer(all_ranks[[4]], all_ranks[[4]], FUN = '/')
comp_Sophia = outer(all_ranks[[5]], all_ranks[[5]], FUN = '/')
comp_Jennifer = outer(all_ranks[[6]], all_ranks[[6]], FUN = '/')
comp_Nadeem = outer(all_ranks[[7]], all_ranks[[7]], FUN = '/')
comp_Hannah = outer(all_ranks[[8]], all_ranks[[8]], FUN = '/')
comp_David = outer(all_ranks[[9]], all_ranks[[9]], FUN = '/')
comp_Aidan = outer(all_ranks[[10]], all_ranks[[10]], FUN = '/')
comp_Xueer = outer(all_ranks[[11]], all_ranks[[11]], FUN = '/')

# CUSTOMISE: store all comparison matrices in a list
comp_matrices = list(comp_Adi, comp_Lisa, comp_Bailey, comp_Sophia, comp_Jennifer, comp_Nadeem, comp_Hannah, comp_David, comp_Aidan, comp_Xueer)

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
write.csv(final_ranks, file = "MHCC 2024 Quarter Finals Final Rankings.csv", row.names = FALSE)