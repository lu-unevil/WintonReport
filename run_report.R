render_report = function(filename, lng) {
  rmarkdown::render(
    filename, params = list(
      lng = lng
    ),
    output_file = paste0("docs/WintonReport-", lng, ".html")
  )
}

render_report("survey_report.Rmd", "se")
render_report("survey_report.Rmd", "en")
