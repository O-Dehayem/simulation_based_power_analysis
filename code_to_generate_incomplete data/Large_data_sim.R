# Set a seed for reproducibility
set.seed(1)

# Define the number of replicate simulations to generate
replicates_to_run <- 2000

# Set the island age (in millions of years)
island_age <- 21

# Define the DAISIE model parameters to generate the Large dataset
# Format: c(cladogenesis_rate, extinction_rate, carrying_capacity (K), colonization_rate, anagenesis_rate)
ML_parameters_data_DI <- c(0.843, 1.12, Inf, 0.007085235, 1.65)

# Simulate island communities under the DAISIE model using the specified parameters

Large_data_sims <- DAISIE_sim(
  time = island_age,               
  M = 50000,                       # Size of the mainland species pool
  pars = ML_parameters_data_DI,   
  replicates = replicates_to_run, 
  plot_sims = FALSE           
)
