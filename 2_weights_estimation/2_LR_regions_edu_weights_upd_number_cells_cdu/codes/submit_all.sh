#!/bin/bash
# ===============================================================
# Submit the full w2 grid search for this specification (CDU):
#   - one job per (region, year) = 6 jobs (east/west x 2013/2017/2021)
#     each runs 0_weights_by_region_state_pi.R <YEAR> --region <region>
#   - 1_graph.sh runs once all 6 weight jobs finish successfully
#     (SLURM dependency: afterok), producing the plots from
#     diagnostics/ into ../graphs/.
# ===============================================================

set -euo pipefail

mkdir -p out err

JIDS=()
for region in east west; do
  for year in 2013 2017 2021; do
    f="0_region_state_pi_cdu_${region}_${year}.sh"
    JID=$(sbatch --parsable "$f")
    echo "submitted ${region} ${year}: $JID"
    JIDS+=("$JID")
  done
done

DEP=$(IFS=:; echo "${JIDS[*]}")
JIDG=$(sbatch --parsable --dependency="afterok:${DEP}" 1_graph.sh)
echo "submitted graph (waits for all 6 weight jobs): $JIDG"
