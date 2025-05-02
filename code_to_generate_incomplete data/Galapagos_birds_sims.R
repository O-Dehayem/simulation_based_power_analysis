# Set a seed for reproducibility
set.seed(1)

# Define DAISIE model parameters estimated from the Galápagos birds dataset

# These parameters were estimated from the empirical dataset of the Galapagos birds dataset 
# available in the DAISIE package.
ML_parameters_data_DI <- c(
  2.546591,        # Cladogenesis rate
  2.678781,        # Extinction rate
  Inf,             # Carrying capacity (infinite, i.e., no diversity limit)
  0.009326754,     # Colonization rate
  1.008583         # Anagenesis rate
)

# Check subset of parameters used in analyses (optional)
ML_parameters_data_DI[c(1, 2, 4, 5)]

# Load empirical data of the Galápagos island biota (available in the DAISIE package)
data("Galapagos_datalist")

# Extract island age from the first element of the data list
island_age <- Galapagos_datalist[[1]]$island_age



# Define the number of replicate simulations
replicates_to_run <- 20000

# Simulate island communities under the DAISIE model using the specified parameters

Galapagos_birds_sims <- DAISIE_sim(
  time = island_age,               
  M = 1000,                        # Mainland species pool size
  pars = ML_parameters_data_DI,   
  replicates = replicates_to_run, 
  plot_sims = FALSE               
)


