customdef_block <- function(x) {
  if (knitr::is_html_output()) {
    sprintf('<span class="customdef">%s</span>', x)
  } else if (knitr::is_latex_output()) {
    sprintf('\\begin{customdef}%s\\end{customdef}', x)
  } else {
    x
  }
}