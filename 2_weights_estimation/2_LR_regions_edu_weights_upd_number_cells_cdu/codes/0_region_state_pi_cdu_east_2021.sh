#!/bin/bash
#SBATCH --job-name="cdu-e-21"
#SBATCH --account=3177784
#SBATCH --output=out/%x_%j.out
#SBATCH --error=err/%x_%j.err
#SBATCH --mail-type=END
#SBATCH --partition=defq
#SBATCH --mem=200G

set -euxo pipefail

MM=/home/3177784/bin/micromamba
export MAMBA_ROOT_PREFIX=/home/3177784/micromamba

eval "$($MM shell hook -s bash)"
micromamba activate Rgeo_new
hash -r

export LD_LIBRARY_PATH="$MAMBA_ROOT_PREFIX/envs/Rgeo_new/lib:$MAMBA_ROOT_PREFIX/envs/Rgeo_new/lib64:${LD_LIBRARY_PATH:-}"

RSCRIPT="$MAMBA_ROOT_PREFIX/envs/Rgeo_new/bin/Rscript"
echo "Using: $RSCRIPT"
echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
ls -l "$MAMBA_ROOT_PREFIX/envs/Rgeo_new/lib/libbz2.so"* || true

$RSCRIPT --version

rm -fr OUTR

Rscript 0_weights_by_region_state_pi.R 2021 --region east
