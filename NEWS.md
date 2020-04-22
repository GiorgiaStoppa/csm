# csm (development version)
* `get_missing_vars_by`: function that identifies the missing fields
  associated to each value of one or more grouping variables.
* `assign_labels`: function that switches the elements of a 
  character vector with the labels associated to each element of the
  vector.
* `get_sheet_labels`: function that returns the labels associated
  to the fields of a sheet.
* `rmarkdown` as imports.
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
