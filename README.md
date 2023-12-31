[![Open in Visual Studio Code](https://classroom.github.com/assets/open-in-vscode-718a45dd9cf7e7f842a935f5ebbe5719a5e09af4491e668f4dbf3b35d5cca122.svg)](https://classroom.github.com/online_ide?assignment_repo_id=11181010&assignment_repo_type=AssignmentRepo)

## Predicting When and Where to Go Birding

=========================

### Project Overview

This is the repository for the Destination Birding R Shiny App and markdown report. The project is
about birding in Knox County, Tennessee. The app has a species diversity map, a
species-specific time series analysis, and a map & graph that show when and
where the best times and spots are for looking for a specific bird species.
Please view the app [here](https://mlong1397.shinyapps.io/DestinationBirding/).

I also wrote a report detailing the thought behind the app and give code
explanations in the index.Rmd and .html files. The rendered html file can be
viewed by clicking the [here](https://mlong1397.github.io/DestinationBirding/)
or on the github pages tab on the right-hand side of the page.

### Walkthrough Demo

[video link](https://www.youtube.com/embed/2qpYJbUBwGE)

### Project Organization

- `DestinationBirdingApp`

  - contains a `data` folder with the pre-processed eBird csv files

  - the `ui.R` (front-end) and `server.R` (back-end) scripts that work together
    to run the R Shiny app.

  - a `www` folder with media (images)

- `docs`
  - contains the R markdown and html files (index.Rmd and index.html) detailing my process (code heavy), plus non-technical PDFs of my written report and presentation.

- `data`

  - contains the csv files and the link to google drive files. The original data was sourced from eBird. In order to follow the first block of the index.Rmd, it is necessary to download the eBird txt file linked in the `data-links.md`. Or you may skip that step and download `knox_birds.csv` from `data-links.md` only.

- `notebooks`

  - contains the data exploration phases of my project. These are like scratch sheets and are not well organized. Please see the `index.Rmd` or the rendered index.html file linked at the top of the page under Project Overview instead.

- `.gitignore`

  - Part of Git, includes files and folders to be ignored by Git version control

- `README.md`

  - Project landing page (this page)

- `LICENSE`
  - Project license

---
