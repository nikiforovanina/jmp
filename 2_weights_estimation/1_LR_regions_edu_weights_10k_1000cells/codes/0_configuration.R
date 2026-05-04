
# Spec tag: appears in graph filenames so different specifications don't
# overwrite each other.  Change when copying this codes folder to a new
# spec (e.g. "50k", "SC", "all_parties").
SPEC_TAG                  <- "500_upd"

# -------------------- knobs --------------------
# w1 is fixed at 1 (only w2 is searched over W2_GRID below).
W1_FIXED                  <- 1

SUBSAMPLE_N               <- 2
MAX_ELECTIONS_PER_STATE   <- 2     # electoral districts sampled per state
RESULTS_MIN             <- 1e-5

# Tolerances
AFD_GROUP_TOL           <- 0.4    
SC_TOL                  <- 0.4

EDU_DIFF_TOL            <- 0.20
ABS_EPS                 <- 1e-5
REL_EPS                 <- 1e-5

SMALL_MUNI_MAX_ELIGIBLE <- 10000
MIN_ROWS_PER_ELECTION   <- 5

SEED_LIST               <- c(1004)

# Voronoi padding
VOR_PAD                 <- 5

# Keep this many largest AfD cells; merge all smaller ones into their neighbours
KEEP_TOP_N_CELLS        <- 500

# AfD second-choice: track LINKE (5) only
SC_PARTIES              <- c(5L)

# w2 grid  (w1 fixed at 1)
W2oneside <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
W2_GRID  <- c(W2oneside, 1/W2oneside[order(1/W2oneside)][-1])
#W2_GRID <- c(0.1)

# Optional cell-count complexity penalty (0 = off)
LAMBDA_CELLS <- 0.0





