## !/usr/bin/Rscript
# lang <- Sys.getenv("QUARTO_PROFILE")

args <- commandArgs(trailingOnly = TRUE)

# lang <- "fr"

library(tidyverse)

book <- basename(getwd())

books <- data.frame(lg = c("en", "fr"), bk = c("Rway", "enfR"))

lang <- books$lg[books$bk == book]

files <- list.files("_book",
  pattern = ".html", recursive = TRUE
)

dat0 <- tibble(f_files = files, v1 = files) %>%
  mutate(v1 = ifelse(v1 == "index.html", "aa-index.html", v1)) %>%
  separate(v1, into = c("nb", "name"), sep = "-") %>%
  #  separate(name, into = c("ch", "ext"), sep = "-") %>%
  mutate(
    #    nb = as.numeric(nb),
    name = gsub(".html", "", name)
  ) # %>%
# drop_na()

ch_refs <- read.csv("utils/lang-chapters.csv")

dat_f <- inner_join(dat0, ch_refs, by = "nb") %>%
  mutate(nb = ifelse(nb == "aa", "", paste0(nb, "-")))


langs <- c("en", "fr")
o_lang <- setdiff(langs, lang)
print(o_lang)
for (j in o_lang) {
  o_book <- books$bk[books$lg == j]
  dat_o <- filter(dat_f, get(j) != "")
  for (k in seq_len(nrow(dat_o))) {
    readLines(paste0("_book/", dat_o$f_files[k])) |>
      stringr::str_replace(
        pattern = paste0("/", o_book, '">'),
        replace = paste0("/", o_book, "/", dat_o$nb[k], dat_o[[j]][k], '.html">')
      ) |>
      writeLines(con = paste0("_book/", dat_o$f_files[k]))
  }
}
