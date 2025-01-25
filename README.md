# Participant-Rank-System
This repository provides R code for ranking participants based on the scores of reviewers/judges. It was used in the scoring of Quarter Finalists of the 2024 Midwest Healthcare Case Competition, and so the names of teams and reviewers has been omitted for anonymity. The usefulness of this repository comes from its ability to utilize Singular Value Decomposition such that variance in how reviewers score participants and differences in which participants were scored per reviewer is negated.

The input CSV file, "Example Reviewer Form," contains review entries submitted via a Google Form corresponding to a reviewer's scoring of a team, i.e., each entry is a singular review. Different categories are qualitatively scored, and there is an optional comments section. For the sake of anonymity, team and reviewer names have been omitted. 

The first section of this algorithm cleans the data, quantifies the total score for each entry, and ranks the teams for each judge. These team name-ranking pairs are stored as datasets under the corresponding reviewer's name.

Next, we need to build a rank matrix with all the teams as the rows and the reviewers as the columns. This information is stored in a matrix called "all_ranks"

In order to run SVD, we need to utilize comparison matrices where, for each reviewer, every team's rank is compared to each other. In this algorithm, we use both subtraction (A) and division (B) as methods to compare the rankings of teams.

Next, the comparison matrices are added together to build the final comparison matrix. This final comparison matrix labeled, "final_comp_matrix" is what we input for singular value decomposition.

We then take the first singular value, which holds the true rankings of the team.

As an alternative and simpler method, we also have an algorithm that adds the rankings to get a final result (C). This method is not as comprehensive, but it is fairly accurate and significantly simpler.
