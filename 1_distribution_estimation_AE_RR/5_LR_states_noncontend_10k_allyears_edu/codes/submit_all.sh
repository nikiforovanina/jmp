#!/bin/bash
# ===============================================================
# Submit the full pipeline for this specification (noncontend):
#   - one estimation job per year (2013, 2017, 2021)
#   - 1_graph.sh runs once all three years finish successfully
#     (SLURM dependency: afterok)
# ===============================================================

set -euo pipefail

mkdir -p out err

JID13=$(sbatch --parsable 0_2parts_2013.sh)
echo "submitted 2013: $JID13"

JID17=$(sbatch --parsable 0_2parts_2017.sh)
echo "submitted 2017: $JID17"

JID21=$(sbatch --parsable 0_2parts_2021.sh)
echo "submitted 2021: $JID21"

JIDG=$(sbatch --parsable \
  --dependency="afterok:${JID13}:${JID17}:${JID21}" \
  1_graph.sh)
echo "submitted graph (waits for 2013/2017/2021): $JIDG"
