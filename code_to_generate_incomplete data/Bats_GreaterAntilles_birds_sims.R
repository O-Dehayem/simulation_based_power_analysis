# Set a seed for reproducibility
# library(DAISIE)  # Uncomment if the package is not already loaded
set.seed(1)

# Define model parameters used to simulate the dataset.
# These parameters were estimated from the empirical dataset of the Greater Antilles bats 
# available in the DAISIE package.
# Format: c(cladogenesis_rate, extinction_rate, carrying_capacity (K), colonization_rate, anagenesis_rate)

ML_parameters_data_DI <- c(0.2843482, 0.3331817, Inf, 0.03094487, 0.1536679)

# Load empirical data of the Greater Antilles bats available in the DAISIE package
data("Bats_GreaterAntilles")

# Extract island age from empirical data
island_age <- Bats_GreaterAntilles[[1]]$island_age

# Set the number of replicates for the simulation
replicates_to_run <- 10000

# Simulate island communities under the DAISIE model using the specified parameters

Bats_GreaterAntilles_sims <- DAISIE_sim(
  time = island_age,              
  M = 100,                         # Mainland species pool size
  pars = ML_parameters_data_DI,  
  replicates = replicates_to_run,
  plot_sims = FALSE               
)
