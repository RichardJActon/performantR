# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
export TEMP=/scratch/$USER/tmp
# module load R/3.5.1_intel_mkl_parallel >/dev/null 2>&1
module load R/3.5.1_intel_mkl >/dev/null 2>&1
# don't save R session data by default
alias R='R --no-save'

alias rstudio='module use /opt/rrzk/modules/experimental/; module load rstudio/1.2.5033_server; run-rstudio-server.sh'

