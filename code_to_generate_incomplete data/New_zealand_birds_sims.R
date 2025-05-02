# Set a seed for reproducibility
set.seed(228)

# Define DAISIE model parameters estimated from the New Zealand birds dataset

# These parameters were estimated from the empirical dataset of the New Zealand birds dataset
# available in the DAISIE package.
ML_parameters_data_DI <- c(
  0.1255591,       # Cladogenesis rate
  0.1910084,       # Extinction rate
  Inf,             # Carrying capacity (no diversity limit)
  0.004697986,     # Colonization rate
  0.326698         # Anagenesis rate
)



# Load empirical data of New Zealand birds from the DAISIE package
data("NewZealand_birds_datalist")

# Extract island age from the empirical dataset
island_age <- NewZealand_birds_datalist[[1]]$island_age



# Define the number of simulation replicates
replicates_to_run <- 3500

# Simulate island community dynamics under the DAISIE model using specified parameters

NewZealand_birds_sims <- DAISIE_sim(
  time = island_age,               
  M = 1000,                        # Mainland species pool size
  pars = ML_parameters_data_DI,   
  replicates = replicates_to_run, 
  plot_sims = FALSE            
)



