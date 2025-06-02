
install.packages(c("devtools", "roxygen2", "usethis"))

# Create a new package structure
# First create an empty folder called "stantrace" in Downloads (or any place of convenience)
usethis::create_package("C:/Users/upenjor/Downloads/stantrace")
# This opens a new RStudio

# Add function (RStudio | File | New File | R script)
# Create a new R script inside the R/ directory
# - File: R/plot_traceplot.R
# - Paste your function there:

# Document the function
devtools::document()
# This creates the help files under man/ and updates NAMESPACE.

# Install the package locally
devtools::install()

# 1. Initialise Git in your Package directory
# Initialise Git repository by first opening your plot_traceplot.R in RStudio.
# Then go tp "Terminal" between "Console" and "Background Jobs".
# Type the following: (Copy and Paste do not work)
git init
git add .
git commit -m "Initial commit"

# 2. Create a Github Repository
# Go to your GitHub and create a new repository.
# Do not initialise with a README, .gitignore, or license (since this was already done locally)

# 3. Push your local repository to GitHub
# In Terminal, type the following:
git remote add origin https://github.com/ugyenpenjor1/stantrace.git
git branch -M main
git push -u origin main

# It will ask for authentication or sign in, do it diligently.


# Install the package from GitHub
install.packages("remotes")
remotes::install_github("ugyenpenjor1/stantrace")
remotes::install_github("ugyenpenjor1/stantrace", force = TRUE)


# Install from the local directory
devtools::install("C:/Users/upenjor/Downloads/stantrace")

stantrace::plot_traceplot()
