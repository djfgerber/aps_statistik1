create_glossary <- function(book_dir = "src", output_file = "98-glossary.Rmd") {
  library(stringr)
  library(dplyr)
  glossary_file_name <- file.path(book_dir, output_file)
  if(file.exists(glossary_file_name))
    file.remove(glossary_file_name)
  
  # Get all Rmd files in the book directory
  rmd_files <- list.files(book_dir, pattern = "\\.Rmd$", full.names = TRUE)

  # Initialize a list to store terms and their locations
  terms <- list()
  
  # Process each file
  for (file in rmd_files) {
    content <- readLines(file)
    # Find section IDs
    section_ids <- content %>% 
      str_extract("^#+ .* \\{-?#([^}]+)\\}") %>% 
      str_extract("(?<=\\{-?#)([^}]+)(?=\\})")
    section_ids_no_nas <- section_ids[!is.na(section_ids)]

    current_section <- NA_character_
    
    for (i in seq_along(content)) {
      line <- content[i]
      
      # Update current section if a new section ID is found
      if (!is.na(section_ids[i])) {
        current_section <- section_ids[i]
        # cat(paste("\n\n section: ", current_section))
      }
      cat(paste("\n\n line: ", line))
      
      
      # Extract bold terms
      bold_terms <- (line %>% 
        str_extract_all("((\\*){2}(.*?)(\\*){2}|(_){2}(.*?)(_){2})"))[[1]]
      cat(paste("\n    section: ", current_section))
      cat("\n   bold_terms", bold_terms)

      if (length(bold_terms) > 0 ) {
        for(bold_term in bold_terms){
          if(!bold_term %in% names(terms)){
            cat(paste("\n term: ", bold_term, "  section:", current_section))
            terms[[bold_term]] <- current_section
            print(terms)
          }
        }
      }
      # Add terms to the list with their section ID
      # or (term in bold_terms) {
      #   if (!term %in% names(terms)) {
      #   cat(paste("\n term: ", term, "  section:", current_section))
      #     terms[[term]] <- current_section
      #   }
      # }f
    }
  }
  
  print(terms)
  
  # Create the glossary content
  glossary_content <- c("# Begriffsverzeichnis {-}", "")
  sorted_terms <- sort(names(terms))
  for (term in sorted_terms) {
    section <- terms[[term]]
    if (is.na(section)) {
      glossary_content <- c(glossary_content, sprintf("- [%s](#%s)", term, section))
    } else {
      glossary_content <- c(glossary_content, sprintf("- %s", term))
    }
  }
  
  # Write the glossary to a file
  writeLines(glossary_content, file.path(book_dir, output_file))
  
  cat("Glossary created successfully in", output_file, "\n")
}

create_glossary()
