# csm (development version)
* Added `rmarkdown` as imports.
* `render_report`: function that automatically renders reports for 
  more than one center involved in the study.
* `select_center`: function to select a center from the output given by
  `nest_tables`.
* Added basic functions for data quality check
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
