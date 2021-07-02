source("Data Prep.R")

library(knitr)
library(rmarkdown)

for (i in 1:nrow(results_both)){
  rmarkdown::render(input = "report_handout.Rmd",
                    output_format = "pdf_document",
                    output_file = paste(results_both$Dist[i], " Report 2020.pdf", sep=''),
                    output_dir = "handouts/")
}