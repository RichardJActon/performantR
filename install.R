# Install the renv R library from source (to get the same version as in the lockfile).
# The renv library will be located on the docker image, in the /home/rstudio directory,
# yet it will use the renv.lock file located in the project's main directory to determine which dependencies to install.
setwd('/home/rstudio')
options(renv.consent = TRUE)
write("options(renv.consent = TRUE)", file="/home/rstudio/.Rprofile", append = TRUE)
install.packages(
	##!! Update to match version in lock file if you upgrade renv
	# current release:
	# 'https://cran.r-project.org/src/contrib/renv_0.15.6.tar.gz', 
	# older versions:
	'https://cran.r-project.org/src/contrib/Archive/renv/renv_0.15.5.tar.gz', 
	repos = NULL, type = 'source', dependencies = TRUE
)
renv::init(force = TRUE)
# renv::restore(confirm = FALSE)
renv::restore(repos = "https://packagemanager.posit.co/cran/2023-06-09", confirm = FALSE)
# Uncomment and adapt to install additional R dependencies.
# The installed packages will not be included in the project's renv.lock file
# unless you call renv::snapshot() and commit the changes.
# See README.md for a prefered way of installing R packages.


