# csm (development version)
* Added basic functions for data quality check
* `select_center`: function to select a center from the output given by
  `nest_tables`.
* `render_report`: function that automatically renders reports for 
  more than one center involved in the study.
* Added `rmarkdown` as imports.
* Fix `.travis.yml`

# csm 0.0.0.9000

* Added basic development support:
  - git + GitHub;
  - appVeyor + Travis + codecov;
  - gpl3 license;
  - testthat + roxygen2 + spellcheck;
  - `` magrittr::`%>%` `` + `tibble::tibble`;
  - `README.Rmd` + `README.md`.

* Added a `NEWS.md` file to track changes to the package.
