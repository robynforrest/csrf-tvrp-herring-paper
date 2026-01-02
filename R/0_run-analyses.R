use
# Build OMs
# Looping through species is handled in 1_make-oms.R
# If make_oms == F, this won't do anything
# Makes plots as well
source("R/1a_make-pac-herring-oms.R")

# Run MSEs
# If run_mses == F, this won't do anything
# Makes plots as well
source("R/2a_run-pac-herring-mses.R")

# ====================================================================================================================
# ======= END OF SCRIPT ==============================================================================================
# ====================================================================================================================

# FINDING THINGS
# hOMs[[i]] is the parameter inputs for the OM
# Use slotNames to query the slots of the OM (OM is S4 class)
# Use the @ operator rather than the $
# e.g.:
# slotNames(hOMs[[i]])  to see the slots
# hOMs[[i]]@R0  to see the value
# cpars has a lot of extra stuff in it including time series of anomalies (Perr_y)
# hOMs[[i]]@cpars

# To see the data object being produced by the OM:
# hist_hMSEs[[1]]@Data

# Some this might be out of date given the new SAMtool
# For the MSE runs. Each MSE rds file has the four M scenarios in it.
# Query the same as for the OM file:
# slotNames(MSEfile[[i]]) to see the slots
# If an extended analysis was run. Use mse@Misc$extended to see the estimated
#   values across sims for:
# N, B, SSB, Catch, Removals, FM, FMret

# Find assessment outputs - only if the MP diagnostic is set to "full"
# mse@PPD contains all the "rep files" from each simulation including Mort,
# but this is a point estimate
# M is buried deep!! k = mp, sim=simulation,  y = assessment year,
# Choose the first column as it is the same for all ages
# MSE@PPD[[k]]@Misc[[sim]]$Assessment_report[[y]]@TMB_report$M[,1]
# SO, for each MP in each assessment year we want the spread of M across simulations

# Annual estimated Ref points are reported in Hist@Ref$ByYear and MSE@RefPoint$ByYear
# The averaged MSY reference points are returned in MSE@RefPoint

# Note from openMSE wiki (https://openmse.com/tutorial-reference-points/msy_ref_points/):
# The MSY metrics in the MSE object are always calculated using annual values.
# The slot MSE@SB_SBMSY returns the spawning biomass in the projections divided by SBMSY
# in each year of the projections.
# For alternative methods to calculate SB/SBMSY, such as relative to the constant SBMSY described above,
# use MSE@SSB or MSE@B and the data stored in MSE@RefPoint.


