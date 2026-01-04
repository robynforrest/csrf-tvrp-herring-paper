# Make the figures for the paper
# January 2, 2026

# Load the historical MSEs.
# The OM scenarios will be loaded in the loop
histMSEs <- readRDS(here(SpDirMSE, "hist_hMSEs.rda"))
ScenarioNamesHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))
stocks <- names(hist_MSEs)
nstocks <- length(stocks)


# FIGURE 7. NO FISHING SCENARIO WITH ALTERNATIVE LRPS - BIOMASS AND PROBABILITIES


