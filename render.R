rmarkdown::render_site()
rmarkdown::render(
  input = "crowd_de.Rmd", 
  output_format = rticles::springer_article(), 
  output_dir = "_site"
)