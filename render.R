rmarkdown::render_site()
purrr::walk(
  .x = c("crowd_de.Rmd", "crowd_en.Rmd"),
  .f = function(x) {
    rmarkdown::render(
      input = x,
      output_format = bookdown::word_document2(),
      output_dir = "_site"
    )
  }
)
rmarkdown::render(
  input = "crowd_de.Rmd", 
  output_format = bookdown::pdf_book(base_format = rticles::springer_article),
  output_dir = "_site"
)
rmarkdown::render(
  input = "crowd_en.Rmd", 
  output_format = bookdown::pdf_document2(),
  output_dir = "_site"
)