
# Spec tag: appears in graph filenames so different specifications don't
# overwrite each other.  Change when copying this codes folder to a new
# spec.
SPEC_TAG                  <- "cdu_upd"

# -------------------- knobs --------------------
# w1 is fixed at 1 (only w2 is searched over W2_GRID below).
W1_FIXED                  <- 1

SUBSAMPLE_N               <- 50
MAX_ELECTIONS_PER_STATE   <- 10    # electoral districts sampled per state
RESULTS_MIN               <- 1e-5

# Tolerances
EDU_DIFF_TOL              <- 0.20
ABS_EPS                   <- 1e-5
REL_EPS                   <- 1e-5

SMALL_MUNI_MAX_ELIGIBLE   <- 10000
MIN_ROWS_PER_ELECTION     <- 5

SEED_LIST                 <- c(1509)

# Voronoi padding
VOR_PAD                   <- 5

# Keep this many largest CDU cells; merge all smaller ones into their neighbours
KEEP_TOP_N_CELLS          <- 500

# w2 grid  (w1 fixed at 1)
W2_GRID  <- c(0.2, 0.25, 0.4, 0.5, 0.75, 1, 1.25, 2, 2.5, 4, 5)

# Optional cell-count complexity penalty (0 = off)
LAMBDA_CELLS              <- 0.0
