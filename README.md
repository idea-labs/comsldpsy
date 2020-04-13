
<!-- README.md is generated from README.Rmd. Please edit that file! -->

# comsldpsy

This repository contains a reproducible research compendium for our
paper:

> Visser, L., Kalmar, J., Linkersdörfer, J., Görgen, R., Rothe, J.,
> Hasselhorn, M., & Schulte-Körne, G. (2020). Comorbidities between
> specific learning disorders and psychopathology in elementary school
> children in Germany. *Frontiers in Psychiatry*.
> <http://doi.org/10.3389/fpsyt.2020.00292>

### How to cite

Please cite this compendium as:

> Visser, L., Kalmar, J., Linkersdörfer, J., Görgen, R., Rothe, J.,
> Hasselhorn, M., & Schulte-Körne, G., (2020). *Compendium of R code and
> data for ‘Comorbidities between specific learning disorders and
> psychopathology in elementary school children in Germany’*. Accessed
> 13 Apr 2020. Online at <http://doi.org/10.17605/OSF.IO/9MXP2>

### Contents

The compendium contains all data, code, and text associated with the
paper. It is organized as follows:

  - The `R/` directory contains all the R code.
  - The `inst/analysis_dir/analysis/` directory contains 4 subfolders
    with the following contents
      - `data/`: the raw data
      - `figures/`: the figures used in the manuscript
      - `manuscript/`: the manuscript as accepted by the journal
      - `templates/`: the Microsoft word template used to prepare the
        manuscript

### How to download or install

There are several ways to use the compendium’s contents and reproduce
the analysis:

  - **Download the compendium as a zip archive** from this [GitHub
    repository](https://github.com/idea-labs/comsldpsy/archive/master.zip)
    or a registered/frozen version from the associated [OSF
    project](https://osf.io/9mxp2/files) (click on “GitHub: …”, then
    select “Download as zip”).
    
      - After unpacking the downloaded zip archive, you can explore the
        files on your computer.

  - **Install the compendium as an R package** from GitHub using the
    [`devtools`
    package](https://cran.r-project.org/web/packages/devtools/index.html):
    
        install.packages("devtools")
        devtools::install_github("idea-labs/comsldpsy")
    
      - This will install the R package locally including all its
        dependencies.
      - The package includes functions to
          - check the analysis for consistency
          - inspect the analysis pipeline and intermediate results
          - reproduce the analysis results on the user’s computer
      - For more details on how to use the package and its functions,
        see
        [here](https://idea-labs.github.io/comsldpsy/articles/comsldpsy.html).

  - **Reproduce the analysis locally inside a
    [Docker](https://www.docker.com/) container**. This allows to
    replicate the exact computational environment used by the authors.
    The compendium includes a Dockerfile that specifies how to build a
    Docker image that contains all the software dependencies needed to
    run the code and also includes the compendium’s R package, ready to
    use. After [installing
    Docker](https://docs.docker.com/install/#supported-platforms) (needs
    administrator privileges), one can simply download the image and run
    the container using:
    
        docker run -p 8888:8888 idealabsffm/comsldpsy
    
      - This command will print a URL to the terminal.
      - Copy this URL and paste it into a web browser.
      - This will open a Jupyter Notebook.
      - Start an interactive RStudio session in the browser by
        selecting:

<p align="center">

<img src="img/rstudio-session.jpg"/>

</p>

  - **Reproduce the analysis in the cloud** without having to install
    any software. The same Docker container replicating the
    computational environment used by the authors can be run using
    BinderHub on [mybinder.org](https://mybinder.org/):
    
      - Click
        [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/idea-labs/comsldpsy/master?urlpath=rstudio)
        to launch an interactive RStudio session in your web browser.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
