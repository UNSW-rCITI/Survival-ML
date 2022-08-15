path <- function(...) {
  fs::path(config::get("data_path"), ...)
}

read_table_builder_csv_string <- function(file) {
  lines <- readLines(file)

  # this usually the line where the headers are
  has_count <- grepl(",\"Count\"|,Count,|,Count$", x = lines)

  headers <- lines[has_count] %>%
    gsub("\"", "", .) %>%
    strsplit(., split = ",") %>%
    unlist()

  data.table::fread(file, skip = which(has_count), header = FALSE) %>%
    data.table::setnames(headers) %>%
    janitor::clean_names() %>%
    {
      .[rowSums(. == "Total", na.rm = T) == 0, ]
    }
}
