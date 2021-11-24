# CI version of linting script.
docker_spyglass:
	cd ci/ && bash lint_CI.sh
	exit 0
# Runs a list of self checking questasim TB
questa:
	cd ci/ && bash questa.sh
	exit 0
