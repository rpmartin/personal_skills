source("01_source_rich.R")
source("02_source_ermias.R")
rmarkdown::render(
  "03_render.Rmd",
  output_format = "bookdown::word_document2",
  output_file = "Skills_in_the_BC_labour_market"
)

