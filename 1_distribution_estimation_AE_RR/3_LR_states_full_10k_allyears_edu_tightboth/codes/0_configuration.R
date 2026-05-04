set.seed(1509)

# Spec tag: appears in graph filenames so different specifications don't
# overwrite each other. Change this when you copy the codes folder to a
# new specification (e.g. "10k_relaxboth", "5k_tightboth").
SPEC_TAG           <- "10k_tightboth"

# configuration files
SUBSAMPLE_N        <- 50
MAX_ELECTIONS      <- 35
RESULTS_MIN        <- 1e-5

# Tolerances
AFD_GROUP_TOL      <- 0.025  
SC_TOL             <- 0.025 
EDU_DIFF_TOL       <- 0.2   

ABS_EPS            <- 1e-5   
REL_EPS            <- 1e-5
MAX_BISECT_IT      <- 15
BISECT_TOL         <- 5e-3

SMALL_MUNI_MAX_ELIGIBLE <- 10000

MIN_ROWS_PER_ELECTION  <- 5    

SEED_LIST <- c(1509)  

VOR_PAD            <- 5 # Voronoi padding for geometry
 
TINY_CELL_FRAC_EPS <- 1e-5 # tiny-cell merging:

SC_PARTIES         <- c(5L)  # AfD second-choice LEFT only

