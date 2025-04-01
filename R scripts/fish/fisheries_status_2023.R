library(pdftools)
library(stringr)
library(tesseract)
library(magick)
library(magrittr)

list <- c("../../statusofsouthafrican_marinefisheryresources2023.pdf")
sapply(list, function(x) {
    pdf_convert(x, format = "png", pages = NULL, filenames = NULL, dpi = 300, opw = "", upw = "", verbose = TRUE)
})


text <- image_read("statusofsouthafrican_marinefisheryresources2023_16.png") %>%
    image_resize("2000") %>%
    image_convert(colorspace = "gray") %>%
    image_trim() %>%
    image_ocr()


a <- print(text)


massili <- regmatches(a, gregexpr("\\d+(\\.\\d+){0,1} %", a))[[1]]
