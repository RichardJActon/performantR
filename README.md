
# Performant R

The [slides](https://richardjacton.github.io/performantR/Performant_R.html) can be found here.

Find the [project page for this workshop on renkulab.io](https://renkulab.io/projects/racton/performantR).
From here you can fork your own copy and spin up an interactive RStudio session to follow along with in the cloud.
You'll need [an account with renkulab.io](https://renkulab.io/auth/realms/Renku/login-actions/authenticate?client_id=renku-ui)

Alternatively if you are familiar with [docker](https://docs.docker.com/engine/install/) you can pull down the image for the session and run it locally:

	# Install the CLI client for Renku using pipx
	# pipx installs python apps in isolated environments https://pypi.org/project/pipx/
	pip install pipx 
	# (you may need pip3 depending on your system)
	pipx install renku 
	# login to Renku
	renku login renkulab.io
	# Clone the project repo
	# If you just want to play with the local version go ahead and clone my project if you like.
	# Alternatively fork your own copy first so you can save your changes to Renku
	# renku clone https://renkulab.io/gitlab/racton/performantR.git
	renku clone https://renkulab.io/gitlab/<YOUR USERNAME>/performantR.git
	# Start the session container locally
	renku session start
	# Load the resulting link in your web browser to get to RStudio


