
library(tidyverse)


extract_def_link_pair <- function(list_str){
  # Extract all [definition_text]{.customdef #link_text} combinations
  matches <- str_extract_all(list_str, "\\[.*?\\]\\{.customdef #.*?( |\\})")
  # Process each match
  map_dfr(matches, function(match) {
    # Extract definition_text and link_text
    definition_text <- str_extract(match, "(?<=\\[).*?(?=\\])")
    link_text <- str_extract(match, "(?<=#).*?(?=( |\\}))")
    
    map2_dfr(definition_text, link_text, function(.x, .y){
      # Check if definition_text contains bold text
      if (length(.x) > 0 && str_detect(.x, "(\\*\\*|__).*?(\\*\\*|__)")) {
        .x <- str_replace_all(.x, "__", "**")
        # Extract bold text
        print(.x)
        bold_text <- str_extract(.x, "(?<=\\*\\*).*?(?=\\*\\*)")
        
        # Return results
        tibble(link_text = .y, bold_text = bold_text)
      } else {
        NULL
      }
    })
  }) %>% 
    return()
}

# Sample text
# c("[a sentence with a **This** is a sentence with a]{.customdef #deftag}",
#           "[Another __definition2__.]{.customdef #link2}",
#           "[Another __definition2__.]{.customdef #link2 bla}",
#           "No match",
#           "[Another **definition3**.]{.customdef #link3}") %>%
#   extract_def_link_pair()


create_glossary <- function(book_dir = "src", output_file = "98-glossary.Rmd") {
  library(stringr)
  library(dplyr)
  glossary_file_name <- file.path(book_dir, output_file)
  if(file.exists(glossary_file_name))
    file.remove(glossary_file_name)

  # Process each file
  mapping_glossary <- list.files(book_dir,
                                 pattern = "\\.Rmd$", 
                                 full.names = TRUE) %>% 
    map_dfr(function(file){
    readLines(file) %>% 
      extract_def_link_pair() %>% 
      return()
  })
  print(mapping_glossary)
  
  mapping_glossary <- mapping_glossary %>% 
    distinct(link_text,.keep_all = TRUE) %>% 
    mutate(arranged_bold_text = bold_text %>% 
             str_remove_all("\\$") %>% 
             tolower()) %>% 
    arrange(arranged_bold_text) %>% 
    select(-arranged_bold_text)

  # Create the glossary content
  c("# Begriffsverzeichnis {-}", # Header
    "", # Empty line
    mapping_glossary %>% 
    pmap_chr(function(link_text, bold_text){
      sprintf("- [%s](#%s)", bold_text, link_text)
      }),
    "",
    "# Literaturverzeichnis {-}") %>% 
  writeLines(file.path(book_dir, output_file))

  cat("Glossary created successfully in", output_file, "\n")
}

create_glossary()

