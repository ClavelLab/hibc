# hibc

`hibc` is the code repository for the website of ["Human Intestinal Bacteria Collection"](https://hibc.rwth-aachen.de). 
It contains the [R](https://www.r-project.org/) code to generate the website in the form of a [Shiny](https://shiny.posit.co/) interactive application. The application is then build as a [Docker](https://www.docker.com/) container and served online to the public at <https://hibc.rwth-aachen.de>.


## Development

Whilst all the code is accessible and pull-requests are welcome, some of the features for the deployment of the shiny application are restricted to RWTH personnel.

### Create the R project via Rstudio

1. Clone the repository using `git clone https://git.rwth-aachen.de/clavellab/hibc`
2. Open the Rstudio project in `hibc/app/app.Rproj` to create the `app` project
3. Rstudio warns you that `One or more packages recorded in the lockfile are not installed` because a couple of R packages and dependencies are needed.
	1. Install the dependencies by typing `renv::restore()` in the Console and agree to the installation of the packages.
	2. Check that all dependencies are set by typing `renv::status()` in the Console where you should have `No issues found`
4. Preview the Shiny app via the "Run app" button or by typing `shiny::runApp()` in the R console.


### Build the docker container locally

You can test if the Shiny application is ready for deployment by building the docker container locally.

```bash
# build the Dockerfile
docker build -t hibc-shiny .
# run the shiny app
docker run -it -d -p 3838:3838 hibc-shiny
# access locally on your browser with
firefox http://localhost:3838
```

### Create a release to trigger the deployment

Once the edits and tests are satisfying, the deployment of the website is triggered by the creation of a tagged release.

1. Tag the git commit using `git tag -a vX.Y.Z` where X, Y and Z follows the <https://semver.org>
2. Push the tag to the remote with `git push --tags`
3. Create a Gitlab release using the Releases section or <https://git.rwth-aachen.de/clavellab/hibc/-/releases/new>

## Citation


> Thomas C. A. Hitch, Johannes M. Masson, Charlie Pauvert, Johanna Bosch, Selina Nüchtern, Nicole Treichel, Marko Baloh, Soheila Razavi, Afrizal Afrizal, Ntana Kousetzi, Andrea M. Aguirre, David Wylensek, Amy Coates, Susan A. V. Jennings, Atscharah Panyot, Alina Viehof, Matthias A. Schmitz, Maximilian Stuhrmann, Evelyn C. Deis, Kevin Bisdorf, Thorsten Cramer, Artur Lissin, Isabel Schober, Julius Witte, Thomas Riedel, Marie Wende, Katrin A. Winter, Alessandra Riva, Stefanie Trinh, Laura Mitchell, Jonathan Hartman, David Berry, Jochen Seitz, Lukas C. Bossert, Thorsten Allers, Till Strowig, Birte Abt, Lorenz C. Reimer, Jörg Overmann, Thomas Clavel (2024). "Broad diversity of human gut bacteria accessible via a traceable strain deposition system". *bioRxiv*. 2024.06.20.599854. doi: [10.1101/2024.06.20.599854](https://doi.org/10.1101/2024.06.20.599854)

## Acknowledgements

This work is supported by the NFDI4Microbiota (<https://nfdi4microbiota.de>), a German consortium of the National Research Data Infrastructure that supports and train the microbiology community for better research data production and management. 

