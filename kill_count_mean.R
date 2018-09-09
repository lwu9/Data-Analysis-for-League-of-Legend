# Read in data
kill_count_data = read.csv('kill_count_list.csv')

# Take means for increasing sample size
sample_sizes_for_means = 1:1000:100000

# Collect means up to each sample size
list_of_means = c()
for(i in 1:len(sample_sizes_for_means)){
  n = sample_sizes_for_means[i]
  mean_up_to_n = mean(kill_count_data[1:n])
  list_of_means[i] = mean_up_to_n
}

# Plot
plot(list_of_means ~ sample_sizes_for_means)
