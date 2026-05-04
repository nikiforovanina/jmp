# Spec tag: appears in graph filenames so different specifications don't
# overwrite each other. Change this when you copy the codes folder to a
# new specification (e.g. "5k_relaxboth_LINKE", "10k_tightboth_LINKE").
#
# Note: per-run knobs (SUBSAMPLE_N, MAX_ELECTIONS, tolerances, SEED_LIST,
# SMALL_MUNI_MAX_ELIGIBLE, etc.) live inline at the top of 0_2parts_linke.R
# in this folder — only SPEC_TAG is centralised here so the graph script can
# read it.
SPEC_TAG <- "10k_relaxboth_LINKE"
