# Set a seed for reproducibility
# library(DAISIE)  # Uncomment if the package is not already loaded

# Set a seed for reproducibility
set.seed(5)

# Define the number of replicate simulations
replicates_to_run <- 2000

# Set the island age (in millions of years)
island_age <- 30

# Define model parameters used to simulate the dataset.
# These parameters were estimated from the empirical dataset of the Hispaniola frogs 
# available in the DAISIE package.

ML_parameters_data_DI <- c(
  0.4368881,        # Cladogenesis rate
  0.1125735,        # Extinction rate
  36.43655,         # Carrying capacity
  0.0007358279,     # Colonization rate
  7.829828e-06      # Anagenesis rate
)

# Simulate island communities under the DAISIE model using the specified parameters

Hispaniola_frogs_sims_G <- DAISIE_sim(
  time = island_age,               
  M = 301,                         # Size of the mainland species pool
  pars = ML_parameters_data_DI,   
  replicates = replicates_to_run, 
  plot_sims = FALSE               
)



