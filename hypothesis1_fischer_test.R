# Create a contingency table of the counts of the two variables
cont_table <- table(crime_counts_wide$year, crime_counts_wide$geometry)

# Perform the Fisher's exact test on the contingency table
fisher_result <- fisher.test(cont_table)

# Print the result
fisher_result
