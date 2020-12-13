
# Reminder!!!!!!
# Restart R first or it won't render right. Restart after EACH

# Restart
bookdown::render_book("index.Rmd", "bookdown::gitbook")

# Restart
#bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book")

# Restart
#bookdown::render_book("index.Rmd", output_format = "bookdown::epub_book")

#bookdown::serve_book(dir = ".", output_dir = "_book", preview = TRUE, in_session = TRUE, quiet = FALSE)
