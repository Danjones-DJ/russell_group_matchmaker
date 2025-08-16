
library(shiny)
library(shinyjs)
library(tidyverse)
library(PostcodesioR)
library(geosphere)
library(scales)
library(conflicted)
library(ggplot2)
library(ggiraph)
library(text2vec)
library(FNN)
library(Matrix)
library(htmltools)
conflict_prefer("span", "shiny")
conflict_prefer("filter", "dplyr")

# Load the new dataset with QS rankings
degree_data1 <- read.csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/organised_russell_group_full_with_qs.csv", 
                         stringsAsFactors = FALSE)

# Clean and prepare the data - UPDATE COLUMN NAMES for new dataset with QS
degree_data1$salary_median <- as.numeric(degree_data1$salary_median)
degree_data1$salary_lq <- as.numeric(degree_data1$salary_lq)
degree_data1$salary_uq <- as.numeric(degree_data1$salary_uq)
degree_data1$salary_respondents <- as.numeric(degree_data1$salary_respondents)

# Handle QS rankings if they exist in the dataset
if("rank" %in% colnames(degree_data1)) {
  degree_data1$qs_rank <- as.numeric(degree_data1$rank)
  degree_data1$qs_previous_rank <- as.numeric(degree_data1$previous_rank)
  degree_data1$qs_overall_score <- as.numeric(degree_data1$overall_score)
  degree_data1$qs_academic_rep <- as.numeric(degree_data1$academic_rep)
  degree_data1$qs_employer_rep <- as.numeric(degree_data1$employer_rep)
  degree_data1$qs_staff_ratio <- as.numeric(degree_data1$staff_to_student_ratio)
  degree_data1$qs_employment_outcomes <- as.numeric(degree_data1$employment_outcomes)
  degree_data1$qs_size <- degree_data1$size
  cat("QS rankings loaded successfully!\n")
  cat("QS rank range:", min(degree_data1$qs_rank, na.rm = TRUE), "to", max(degree_data1$qs_rank, na.rm = TRUE), "\n")
} else {
  # Set QS fields to NA if not available
  degree_data1$qs_rank <- NA
  degree_data1$qs_previous_rank <- NA
  degree_data1$qs_overall_score <- NA
  degree_data1$qs_academic_rep <- NA
  degree_data1$qs_employer_rep <- NA
  degree_data1$qs_staff_ratio <- NA
  degree_data1$qs_employment_outcomes <- NA
  degree_data1$qs_size <- NA
  cat("QS columns not found in dataset - using NA values\n")
}

# Map old column names to new for compatibility
degree_data1$title <- degree_data1$programme
degree_data1$university_name <- degree_data1$university
degree_data1$median_salary <- degree_data1$salary_median
degree_data1$url <- degree_data1$crseurl
degree_data1$provaddress <- degree_data1$uni_address
degree_data1$placement_year <- ifelse(degree_data1$option_sandwich_year == "Yes", "Yes", "No")
degree_data1$year_abroad <- ifelse(degree_data1$option_year_abroad == "Yes", "Yes", "No")
degree_data1$foundation_year_available <- ifelse(degree_data1$option_foundation_year == "Yes", "Yes", "No")

# Set A-Level and IB requirements to NA/None for new dataset as specified
degree_data1$a_level_grade_req <- NA
degree_data1$a_level_subject_reqs <- "None"
degree_data1$ib_grade_req <- NA
degree_data1$ib_subject_req <- "None"
degree_data1$offer_rate <- NA

# Get dynamic degree types and universities from the actual data
degree_types <- sort(unique(degree_data1$degree_type))
universities <- sort(unique(degree_data1$university_name[!is.na(degree_data1$university_name) & degree_data1$university_name != ""]))

# OPTIMIZATION INFO: Check how many unique addresses vs total courses
unique_addresses_count <- length(unique(degree_data1$provaddress[!is.na(degree_data1$provaddress) & degree_data1$provaddress != ""]))
total_courses_count <- nrow(degree_data1)
cat("OPTIMIZATION: Only calculating", unique_addresses_count, "unique addresses instead of", total_courses_count, "courses\n")
cat("Speed improvement: ~", round(total_courses_count/unique_addresses_count, 1), "x faster\n")

# Load A-Level subjects from GitHub CSV
subjects_data <- read.csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/alevel_subjects.csv", 
                          stringsAsFactors = FALSE)
all_subjects <- sort(subjects_data$a_level_subjects)

btec_subjects <- c(
  "Applied Human Biology", "Applied Law", "Applied Psychology", "Applied Science", "Art and Design",
  "Business", "Children's Play, Learning and Development", "Computing", "Engineering",
  "Enterprise and Entrepreneurship", "Health and Social Care", "Information Technology",
  "Sport", "Sport and Exercise Science"
)
btec_grades <- c("D*", "D (Distinction)", "M (Merit)", "P (Pass)", "Fail")
# ...existing code...
all_subjects_combined <- c(all_subjects, btec_subjects)
all_grades_combined <- c("A*", "A", "B", "C", "D", "E", btec_grades)
# ...existing code...

# Parse taxonomy structure from the taxonomy file
parse_taxonomy <- function() {
  # Read taxonomy file from GitHub
  taxonomy_url <- "https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/taxonomy%20v4.txt"
  
  tryCatch({
    cat("Loading taxonomy from GitHub...\n")
    lines <- readLines(taxonomy_url, warn = FALSE)
    cat("Successfully loaded taxonomy from GitHub\n")
  }, error = function(e) {
    cat("Warning: Could not load taxonomy from GitHub:", e$message, "\n")
    cat("Trying local fallback...\n")
    
    # Fallback to local file if GitHub fails
    local_path <- "/Users/danieljones/Desktop/codin/coding in R/taxonomy v4.txt"
    if(file.exists(local_path)) {
      lines <- readLines(local_path, warn = FALSE)
      cat("Using local taxonomy file\n")
    } else {
      cat("Error: No taxonomy file available (GitHub or local)\n")
      return(list())
    }
  })
  
  taxonomy <- list()
  current_main <- NULL
  current_child <- NULL
  
  # Define the main categories as provided by user
  main_categories <- c(
    "Engineering", "Classics", "Languages", "Humanities", "Direct Pathways",
    "Creatives and Arts", "Technology and Computing", "Applied Medicine", 
    "Natural Sciences", "Life Sciences", "Earth, Geography and Environmental Sciences",
    "Management", "Mathematics & Computation", "Social Good", "Social Sciences"
  )
  
  for(line in lines) {
    line <- trimws(line)
    if(line == "") next
    
    # Skip the header line
    if(line == "Academic Subject Classification System") next
    
    # Check if this line is a main category (exact match)
    if(line %in% main_categories) {
      current_main <- line
      taxonomy[[current_main]] <- list(children = list())
      current_child <- NULL
    }
    # Check for numbered subcategories (like "1. Technology", "2. Electric and Electronic")
    else if(grepl("^[0-9]+\\.", line)) {
      cleaned_line <- gsub("^[0-9]+\\.\\s*", "", line)
      if(!is.null(current_main)) {
        current_child <- cleaned_line
        taxonomy[[current_main]]$children[[current_child]] <- list(stems = c())
      }
    }
    # Check for letter-coded stems (like "a. Computer Engineering", "b. Computer Science")
    else if(grepl("^[a-z]+\\.", line)) {
      cleaned_line <- gsub("^[a-z]+\\.\\s*", "", line)
      if(!is.null(current_main) && !is.null(current_child)) {
        taxonomy[[current_main]]$children[[current_child]]$stems <- 
          c(taxonomy[[current_main]]$children[[current_child]]$stems, cleaned_line)
      }
    }
  }
  
  cat("Parsed taxonomy with", length(taxonomy), "main categories:\n")
  for(main_cat in names(taxonomy)) {
    child_count <- length(taxonomy[[main_cat]]$children)
    total_stems <- sum(sapply(taxonomy[[main_cat]]$children, function(x) length(x$stems)))
    cat("-", main_cat, ":", child_count, "subcategories,", total_stems, "total stems\n")
    
    # DEBUG: Show all subcategories for Humanities
    if(main_cat == "Humanities") {
      for(sub_cat in names(taxonomy[[main_cat]]$children)) {
        stems <- taxonomy[[main_cat]]$children[[sub_cat]]$stems
        cat("  - Humanities ::", sub_cat, ":", length(stems), "stems:", paste(head(stems, 3), collapse = ", "), "\n")
      }
    }
  }
  
  return(taxonomy)
}

# Create course tagging cache with EXACT STRING MATCHING
create_course_tags_cache <- function(courses_data, taxonomy) {
  cat("Creating course tagging cache with SUBSTRING matching...\n")
  
  cache <- list()
  total_courses <- nrow(courses_data)
  
  for(i in 1:total_courses) {
    if(i %% 100 == 0) cat("Processing course", i, "of", total_courses, "\n")
    
    course_title <- tolower(courses_data$title[i])
    course_tags <- c()
    
    # Check each stem in the taxonomy for EXACT string presence
    for(main_cat in names(taxonomy)) {
      for(child_cat in names(taxonomy[[main_cat]]$children)) {
        for(stem in taxonomy[[main_cat]]$children[[child_cat]]$stems) {
          stem_lower <- tolower(stem)
          
          # Check if the ENTIRE tag string appears ANYWHERE in the course title
          if(grepl(stem_lower, course_title, fixed = TRUE)) {
            course_tags <- c(course_tags, paste0(main_cat, "::", child_cat, "::", stem))
          }
        }
      }
    }
    
    cache[[i]] <- unique(course_tags)
    
    # Debug: Show matches for courses with specific patterns
    if(grepl("economics|data science|social science|linguistic", course_title)) {
      cat("SUBSTRING MATCH DEBUG:", course_title, "->", length(cache[[i]]), "tags:", 
          paste(head(cache[[i]], 5), collapse = ", "), "\n")
    }
  }
  
  cat("Course tagging cache created for", total_courses, "courses\n")
  return(cache)
}

# Function to get courses by taxonomy category
get_courses_by_taxonomy <- function(main_category, child_category = NULL, course_tags_cache, courses_data) {
  matching_courses <- c()
  
  for(i in 1:length(course_tags_cache)) {
    tags <- course_tags_cache[[i]]
    
    if(is.null(child_category)) {
      # Match any tag starting with main category
      if(any(grepl(paste0("^", main_category), tags))) {
        matching_courses <- c(matching_courses, i)
      }
    } else {
      # Match specific child category
      target_pattern <- paste0("^", main_category, "::", child_category)
      if(any(grepl(target_pattern, tags))) {
        matching_courses <- c(matching_courses, i)
      }
    }
  }
  
  return(courses_data[matching_courses, ])
}

# Initialize taxonomy and cache
taxonomy_structure <- parse_taxonomy()
course_tags_cache <- NULL

# Initialize cache after data is loaded
if(nrow(degree_data1) > 0) {
  course_tags_cache <- create_course_tags_cache(degree_data1, taxonomy_structure)
}

# Subject synonym mapping for smart matching
create_subject_synonyms <- function() {
  list(
    "Mathematics" = c("Mathematics", "Maths", "Math", "Mathematical", "Further Mathematics", "Statistics"),
    "Physics" = c("Physics", "Physical", "Physical Sciences"),
    "Chemistry" = c("Chemistry", "Chemical", "Chemical Sciences"),
    "Biology" = c("Biology", "Biological", "Biological Sciences", "Life Sciences", "Life and Health Sciences"),
    "English Literature" = c("English Literature", "English", "Literature", "English Language and Literature"),
    "English Language" = c("English Language", "English", "Language", "English Language and Literature"),
    "History" = c("History", "Historical", "Ancient History"),
    "Geography" = c("Geography", "Geographical", "Environmental Geography"),
    "Computer Science" = c("Computer Science", "Computing", "ICT", "Information Technology", "Software Systems Development"),
    "Economics" = c("Economics", "Economic", "Business Economics"),
    "Psychology" = c("Psychology", "Psychological"),
    "Art and Design" = c("Art and Design", "Art", "Design", "Fine Art", "Visual Arts"),
    "Business" = c("Business", "Business Studies", "Commerce"),
    "French" = c("French", "French Language", "French Studies"),
    "German" = c("German", "German Language", "German Studies"),
    "Spanish" = c("Spanish", "Spanish Language", "Spanish Studies"),
    "Politics" = c("Politics", "Political Science", "Government", "Government and Politics"),
    "Philosophy" = c("Philosophy", "Philosophical"),
    "Sociology" = c("Sociology", "Social Sciences", "Sociological"),
    "Drama" = c("Drama", "Theatre", "Drama and Theatre", "Performing Arts"),
    "Music" = c("Music", "Musical", "Music Technology"),
    "Physical Education" = c("Physical Education", "PE", "Sports", "Sports Science"),
    "Religious Studies" = c("Religious Studies", "Religion", "Theology", "Islamic Studies", "Biblical Studies"),
    "Media Studies" = c("Media Studies", "Media", "Film Studies", "Digital Media and Design"),
    "Law" = c("Law", "Legal Studies", "Jurisprudence")
  )
}

# User's custom functions for distance calculation
extract_postcode <- function(provaddress) {
  address <- provaddress
  pattern <- "(?i)[A-Z]{1,2}[0-9]{1,2}[A-Z]?\\s[0-9][A-Z]{2}"
  postcode <- tolower(str_extract(address, pattern))
  return(postcode)
}

degree_dist <- function(post_code, provaddress) {
  tryCatch({
    home <- post_code
    dest <- extract_postcode(provaddress)
    
    if(is.na(home) || is.na(dest) || home == "" || dest == "") {
      return(data.frame(home = home, dest = dest, miles = NA, distance_range = "Unknown"))
    }
    
    df <- tibble(
      home = toupper(home),
      dest = toupper(dest)
    ) 
    
    df <- df %>%
      rowwise() %>%
      mutate(
        miles = distHaversine(
          postcode_lookup(home)[,7:8],
          postcode_lookup(dest)[,7:8]
        ) / 1609.34
      ) %>%
      ungroup()
    
    df <- df %>%
      mutate(          
        distance_range = cut(                                     
          miles,
          breaks = c(0, 10, 25, 50, 100, 150, Inf),
          labels = c("0‑10 miles", "10‑25 miles", "25‑50 miles", "50‑100 miles", "100‑150 miles", "150+ miles"),
          right  = FALSE                                
        )
      )
    
    return(df)
  }, error = function(e) {
    return(data.frame(home = post_code, dest = extract_postcode(provaddress), miles = NA, distance_range = "Error"))
  })
}

# Function to match subjects with requirements using synonyms
match_subjects_with_requirements <- function(selected_subjects, course_requirements) {
  if(is.null(selected_subjects) || length(selected_subjects) == 0 || 
     is.null(course_requirements) || is.na(course_requirements) || course_requirements == "") {
    return(FALSE)
  }
  
  synonyms <- create_subject_synonyms()
  
  # Create expanded list of all possible subject variations
  all_variations <- c()
  for(subject in selected_subjects) {
    if(subject %in% names(synonyms)) {
      all_variations <- c(all_variations, synonyms[[subject]])
    } else {
      all_variations <- c(all_variations, subject)
    }
  }
  
  # Check if any variation appears in the requirements
  any(sapply(all_variations, function(var) {
    grepl(var, course_requirements, ignore.case = TRUE)
  }))
}

# User's working matching function - UPDATED COLUMN NAME
find_matched_subjects <- function(a_levels, subject_requirements_example) {
  # Handle null/empty inputs
  if(is.null(a_levels) || length(a_levels) == 0 || 
     is.null(subject_requirements_example) || is.na(subject_requirements_example) || 
     subject_requirements_example == "") {
    return(character(0))
  }
  
  matched_subjects <- c()
  
  for (subject in a_levels) {
    if (grepl(subject, subject_requirements_example, ignore.case = TRUE)) {
      matched_subjects <- c(matched_subjects, subject)
      cat("match", subject, "\n")
    }
  }
  
  return(matched_subjects)
}

# Function to get student's selected subjects
get_selected_subjects <- function(input) {
  layout <- input$qual_layout
  if (is.null(layout) || layout == "a_levels") {
    subjects <- c(input$subject1, input$subject2, input$subject3, input$subject4)
    subjects <- subjects[!is.null(subjects) & subjects != ""]
    return(subjects)
  } else if (layout == "mix") {
    subjects <- c()
    for (i in 1:4) {
      subj <- input[[paste0("subject", i)]]
      if (!is.null(subj) && subj != "") {
        subjects <- c(subjects, subj)
      }
    }
    return(subjects)
  }
  return(character(0))
}

# Function to convert grade requirements to scores - FIXED A* PARSING
convert_grade_requirement <- function(grade_req) {
  if(is.na(grade_req) || grade_req == "") return(0)
  
  # Handle A* grades more carefully
  # Split by A* first, then handle remaining
  parts <- strsplit(grade_req, "A\\*")[[1]]
  
  # Count A* grades
  num_a_star <- length(parts) - 1
  
  # Get remaining grades from the last part
  remaining <- parts[length(parts)]
  remaining_grades <- if(remaining == "") character(0) else unlist(strsplit(remaining, ""))
  remaining_grades <- remaining_grades[remaining_grades %in% c("A", "B", "C", "D", "E", "U")]
  
  # Combine all grades
  all_grades <- c(rep("A*", num_a_star), remaining_grades)
  
  if(length(all_grades) == 0) return(0)
  
  # Calculate total score
  scores <- sapply(all_grades, grade_to_score, USE.NAMES = FALSE)
  return(sum(scores))
}

# Simple similar courses finder - UPDATED COLUMN NAMES
find_similar_courses <- function(current_course, all_courses, limit = 3) {
  all_courses <- all_courses[all_courses$title != current_course$title, ]
  if (nrow(all_courses) == 0) return(all_courses[0, ])
  
  # Helper: extract all 1-3 word phrases (and single words > 4 chars)
  extract_phrases <- function(title) {
    words <- unlist(strsplit(title, "\\s+"))
    n <- length(words)
    phrases <- c()
    # 1-word (if long enough)
    phrases <- c(phrases, words[nchar(words) > 4])
    # 2-word
    if (n >= 2) {
      for (i in 1:(n-1)) phrases <- c(phrases, paste(words[i], words[i+1]))
    }
    # 3-word
    if (n >= 3) {
      for (i in 1:(n-2)) phrases <- c(phrases, paste(words[i], words[i+1], words[i+2]))
    }
    unique(phrases)
  }
  # Lowercase, remove generic
  generic_words <- c("science", "sciences", "studies", "study", "with", "and", "the", "of", "in")
  current_phrases <- tolower(extract_phrases(current_course$title))
  current_phrases <- current_phrases[!current_phrases %in% generic_words]
  
  # Flexible phrase/word match (across ALL degrees)
  direct_phrase_matches <- all_courses[
    sapply(all_courses$title, function(other_title) {
      other_title_lower <- tolower(other_title)
      other_phrases <- tolower(extract_phrases(other_title))
      other_phrases <- other_phrases[!other_phrases %in% generic_words]
      # Match if any phrase from current in other, or vice versa
      any(sapply(current_phrases, function(phrase) grepl(phrase, other_title_lower, fixed = TRUE))) ||
        any(sapply(other_phrases, function(phrase) grepl(phrase, tolower(current_course$title), fixed = TRUE)))
    }),
  ]
  direct_phrase_matches <- direct_phrase_matches[!duplicated(direct_phrase_matches$title), ]
  
  # If enough, return
  if (nrow(direct_phrase_matches) >= limit) return(head(direct_phrase_matches, limit))
  
  # 2. Taxonomy category overlap (independent, not stacked)
  taxonomy_groups <- c()
  if(!is.null(course_tags_cache) && length(taxonomy_structure) > 0) {
    # Find the current course's index
    current_course_index <- which(all_courses$title == current_course$title)[1]
    
    if(!is.na(current_course_index) && current_course_index <= length(course_tags_cache)) {
      current_tags <- course_tags_cache[[current_course_index]]
      
      # Extract main categories from current course's tags
      for(tag in current_tags) {
        # Extract main category (everything before the first "::")
        main_cat <- strsplit(tag, "::")[[1]][1]
        if(main_cat %in% names(taxonomy_structure)) {
          taxonomy_groups <- c(taxonomy_groups, main_cat)
        }
      }
      taxonomy_groups <- unique(taxonomy_groups)
    }
  }
  
  # Find courses with similar taxonomy tags
  taxonomy_matches <- all_courses[0, ]
  if(length(taxonomy_groups) > 0 && !is.null(course_tags_cache)) {
    matching_indices <- c()
    
    for(i in 1:length(course_tags_cache)) {
      if(i <= nrow(all_courses)) {
        course_tags <- course_tags_cache[[i]]
        # Check if this course has any tags matching our taxonomy groups
        for(tag in course_tags) {
          main_cat <- strsplit(tag, "::")[[1]][1]
          if(main_cat %in% taxonomy_groups) {
            matching_indices <- c(matching_indices, i)
            break
          }
        }
      }
    }
    
    if(length(matching_indices) > 0) {
      taxonomy_matches <- all_courses[unique(matching_indices), ]
    }
  }
  
  # Remove any already matched
  taxonomy_matches <- taxonomy_matches[!taxonomy_matches$title %in% direct_phrase_matches$title, ]
  
  # Combine, up to limit
  all_matches <- rbind(direct_phrase_matches, taxonomy_matches)
  all_matches <- all_matches[!duplicated(all_matches$title), ]
  if (nrow(all_matches) >= limit) return(head(all_matches, limit))
  
  # 3. Fallback: fill with any remaining courses, preferring shorter titles
  filler <- all_courses[!all_courses$title %in% all_matches$title, ]
  if (nrow(filler) > 0) {
    filler$title_word_count <- sapply(strsplit(filler$title, "\\s+"), length)
    filler <- filler[order(filler$title_word_count, filler$title), ]
    all_matches <- rbind(all_matches, head(filler, limit - nrow(all_matches)))
    filler$title_word_count <- NULL
  }
  
  return(head(all_matches, limit))
}

# Function to determine match type - FIXED LOGIC
get_match_type <- function(student_score, course_score) {
  if(is.na(student_score) || is.na(course_score)) return("No Data")
  
  if(student_score == course_score) return("Exact Match")
  else if(student_score > course_score) {
    difference <- student_score - course_score
    if(difference <= 30) return("Good Match")  # Within ~1 grade difference
    else return("Overmatch")
  }
  else return("Under Match")  # student_score < course_score means underqualified
}
grade_to_score <- function(grade) {
  # A Level grades
  if (grade == "A*") return(56)
  if (grade == "A")  return(48)
  if (grade == "B")  return(40)
  if (grade == "C")  return(32)
  if (grade == "D")  return(24)
  if (grade == "E")  return(16)
  # BTEC grades (Extended Certificate UCAS points)
  if (grade == "D*") return(56)
  if (grade == "D (Distinction)")  return(48)
  if (grade == "M (Merit)")  return(32)
  if (grade == "P (Pass)")  return(16)
  if (grade == "Fail") return(0)
  return(0)
}

# Function to calculate student's best 3 grades score  
calculate_student_score <- function(grades) {
  # Remove empty grades
  valid_grades <- grades[grades != "" & !is.na(grades)]
  
  # Must have at least 3 grades
  if(length(valid_grades) < 3) return(NA)
  
  # Convert to scores and take best 3
  scores <- sapply(valid_grades, grade_to_score, USE.NAMES = FALSE)
  best_3_scores <- sort(scores, decreasing = TRUE)[1:3]
  
  return(sum(best_3_scores))
}

# --- IB score calculation helpers ---
calculate_ib_score <- function(grades, bonus) {
  grades_num <- suppressWarnings(as.numeric(grades))
  grades_num <- grades_num[!is.na(grades_num)]
  if(length(grades_num) < 6) return(NA)
  total <- sum(grades_num[1:6]) + as.numeric(bonus)
  return(total)
}

get_ib_grades <- function(input) {
  grades <- c(input$ib_g1_grade, input$ib_g2_grade, input$ib_g3_grade, input$ib_g4_grade, input$ib_g5_grade, input$ib_g6_grade)
  bonus <- ifelse(is.null(input$ib_core_bonus), 0, as.numeric(input$ib_core_bonus))
  list(grades = grades, bonus = bonus)
}

# Add grade scores to dataset - UPDATED COLUMN NAME
degree_data1$grade_score <- sapply(degree_data1$a_level_grade_req, convert_grade_requirement)

# Sort dataset by grade requirement (highest first)
degree_data1 <- degree_data1[order(-degree_data1$grade_score, na.last = TRUE), ]

# New ggiraph-based functions for enhanced interactivity
find_similar_titles <- function(query, data, k = 10) {
  library(text2vec)
  library(FNN)
  
  # Validate input
  if(nrow(data) == 0 || is.null(query) || query == "") {
    return(data.frame())
  }
  
  tryCatch({
    titles <- tolower(data$title)  # Lowercase for consistency
    
    # 1. Use itoken on titles
    it_titles <- itoken(titles, progressbar = FALSE)
    
    # 2. Build vocabulary and vectorizer
    vocab <- create_vocabulary(it_titles, ngram = c(1L, 2L))  # allow bigrams like "data science"
    
    # Check if vocabulary is empty
    if(nrow(vocab) == 0) {
      cat("Empty vocabulary created, falling back to simple search\n")
      return(data.frame())
    }
    
    vectorizer <- vocab_vectorizer(vocab)
    
    # 3. Build DTM for titles
    dtm_titles <- create_dtm(it_titles, vectorizer)
    
    # Check if DTM is empty
    if(nrow(dtm_titles) == 0 || ncol(dtm_titles) == 0) {
      cat("Empty DTM for titles, falling back\n")
      return(data.frame())
    }
    
    # 4. Create DTM for query (lowercased)
    query_clean <- tolower(query)
    it_query <- itoken(query_clean, progressbar = FALSE)
    dtm_query <- create_dtm(it_query, vectorizer)
    
    # 5. Safeguard: if query yields empty DTM, abort gracefully
    if (nrow(dtm_query) == 0 || sum(dtm_query) == 0) {
      cat("Query term not found in vocabulary:", query, "\n")
      return(data.frame())
    }
    
    # 6. Align dimensions if needed
    if (ncol(dtm_query) != ncol(dtm_titles)) {
      missing_cols <- setdiff(colnames(dtm_titles), colnames(dtm_query))
      if(length(missing_cols) > 0) {
        pad <- Matrix::Matrix(0, nrow = 1, ncol = length(missing_cols), sparse = TRUE)
        colnames(pad) <- missing_cols
        dtm_query <- cbind(dtm_query, pad)
        dtm_query <- dtm_query[, colnames(dtm_titles)]
      }
    }
    
    # 7. Run FNN
    knn_res <- get.knnx(data = dtm_titles, query = dtm_query, k = min(k, nrow(data)))
    
    idx_matches <- knn_res$nn.index[1, ]
    matched <- data[idx_matches, ]
    matched$similarity <- 1 - knn_res$nn.dist[1, ]
    
    return(matched)
    
  }, error = function(e) {
    cat("Error in find_similar_titles:", e$message, "\n")
    return(data.frame())
  })
}

plot_fnnsimilar_degrees <- function(query, data, k = 50, page = 1, per_page = 12) {
  library(ggiraph)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  query_lower <- tolower(query)
  
  # Primary search: exact string matching (most reliable)
  matched <- data %>% 
    mutate(title_lower = tolower(title)) %>%
    filter(str_detect(title_lower, query_lower))
  
  # If no exact matches, try word-by-word matching
  if (nrow(matched) == 0) {
    query_words <- unlist(strsplit(query_lower, "\\s+"))
    matched <- data.frame()
    
    for(word in query_words) {
      if(nchar(word) > 2) {  # Only use words longer than 2 characters
        word_matches <- data %>% 
          mutate(title_lower = tolower(title)) %>%
          filter(str_detect(title_lower, word))
        matched <- rbind(matched, word_matches)
      }
    }
    
    # Remove duplicates
    if(nrow(matched) > 0) {
      matched <- matched[!duplicated(matched$title), ]
    }
  }
  
  # If still no matches, try text similarity search (fallback)
  if (nrow(matched) < 5 && nrow(data) > 0) {
    tryCatch({
      # Only use text similarity if we have some data to work with
      similarity_matches <- find_similar_titles(query, data, k = min(k, nrow(data)))
      if(nrow(similarity_matches) > 0) {
        matched <- rbind(matched, similarity_matches)
        matched <- matched[!duplicated(matched$title), ]
      }
    }, error = function(e) {
      cat("Text similarity search failed:", e$message, "\n")
    })
  }
  
  # Final fallback - show some courses if nothing found
  if (nrow(matched) == 0) {
    matched <- data %>% 
      arrange(desc(ifelse(is.na(median_salary), 0, median_salary))) %>% 
      head(min(10, nrow(data)))
  }
  
  # If still no data, return empty message
  if (nrow(matched) == 0) {
    return(girafe(ggobj = ggplot() + 
                    annotate("text", x = 0.5, y = 0.5, 
                             label = paste("No courses found for:", query), 
                             size = 4, color = "#ffffff") +
                    theme_void() +
                    theme(plot.background = element_rect(fill = "#191414", color = NA)),
                  width_svg = 10, height_svg = 6))
  }
  
  # Separate courses with and without salary data
  courses_with_salary <- matched %>% filter(!is.na(median_salary), median_salary > 0)
  courses_without_salary <- matched %>% filter(is.na(median_salary) | median_salary <= 0)
  
  # If no salary data available, create a simple plot
  if(nrow(courses_with_salary) == 0) {
    # Create simple list plot without salary axis
    # Apply pagination to matched courses
    start_idx <- (page - 1) * per_page + 1
    end_idx <- min(page * per_page, nrow(matched))
    
    if(start_idx <= nrow(matched)) {
      matched_limited <- matched[start_idx:end_idx, ]
    } else {
      matched_limited <- matched[0, ]  # Empty dataframe if page out of range
    }
    
    matched_limited <- matched_limited %>%
      mutate(
        tooltip = paste0(
          "<b>", htmltools::htmlEscape(university_name), "</b><br>",
          "<i>", htmltools::htmlEscape(title), "</i> (", htmltools::htmlEscape(degree_type), ")<br>",
          "<b>A-levels:</b> ", htmltools::htmlEscape(ifelse(is.na(a_level_grade_req) | a_level_grade_req == "", "N/A", a_level_grade_req)), "<br>",
          "<b>IB:</b> ", htmltools::htmlEscape(ifelse(is.na(ib_grade_req) | ib_grade_req == "", "N/A", ib_grade_req)), "<br>",
          "<b>Salary:</b> Not Available<br>",
          "<i>Click to visit course page</i>"
        ),
        onclick = sprintf("window.open(\"%s\")", gsub("\"", "%22", url)),
        y_pos = row_number()
      )
    
    p <- ggplot(matched_limited, aes(x = 1, y = reorder(title, y_pos))) +
      geom_point_interactive(aes(
        tooltip = tooltip,
        data_id = title,
        onclick = onclick,
        color = university_name
      ), size = 4, alpha = 0.8) +
      scale_color_manual(values = c("#ff6b6b", "#4ecdc4", "#45b7d1", "#f9ca24", 
                                    "#f0932b", "#eb4d4b", "#6c5ce7", "#00b894",
                                    "#fdcb6e", "#e17055", "#74b9ff", "#a29bfe")) +
      labs(title = paste("Courses for:", tools::toTitleCase(query)),
           subtitle = paste(nrow(matched_limited), "courses found • Hover for details • Click to visit course page"),
           x = NULL, y = NULL, color = "University") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 5), 
                                  color = "#ffffff", face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15),
                                     color = "#b3b3b3", face = "italic"),
        axis.text.y = element_text(size = 9, color = "#d0d0d0"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, color = "#ffffff", face = "bold"),
        legend.text = element_text(size = 9, color = "#e0e0e0"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        plot.background = element_rect(fill = "#191414", color = NA)
      )
    
  } else {
    # Create salary-based plot for courses with salary data
    # Apply pagination to prevent overcrowding
    courses_with_salary <- courses_with_salary %>%
      arrange(desc(median_salary))
    
    # Calculate pagination
    start_idx <- (page - 1) * per_page + 1
    end_idx <- min(page * per_page, nrow(courses_with_salary))
    
    if(start_idx <= nrow(courses_with_salary)) {
      courses_with_salary <- courses_with_salary[start_idx:end_idx, ]
    } else {
      courses_with_salary <- courses_with_salary[0, ]  # Empty dataframe if page out of range
    }
    
    courses_with_salary <- courses_with_salary %>%
      mutate(
        tooltip = paste0(
          "<b>", htmltools::htmlEscape(university_name), "</b><br>",
          "<i>", htmltools::htmlEscape(title), "</i> (", htmltools::htmlEscape(degree_type), ")<br>",
          "<b>A-levels:</b> ", htmltools::htmlEscape(ifelse(is.na(a_level_grade_req) | a_level_grade_req == "", "N/A", a_level_grade_req)), "<br>",
          "<b>IB:</b> ", htmltools::htmlEscape(ifelse(is.na(ib_grade_req) | ib_grade_req == "", "N/A", ib_grade_req)), "<br>",
          "<b>Salary:</b> £", htmltools::htmlEscape(format(median_salary, big.mark = ",")), "<br>",
          "<i>Click to visit course page</i>"
        ),
        onclick = sprintf("window.open(\"%s\")", gsub("\"", "%22", url))
      )
    
    p <- ggplot(courses_with_salary, aes(x = median_salary, y = reorder(title, median_salary))) +
      geom_point_interactive(aes(
        tooltip = tooltip,
        data_id = title,
        onclick = onclick,
        color = university_name
      ), size = 3.5, alpha = 0.8) +
      scale_x_continuous(labels = scales::comma_format(prefix = "£")) +
      scale_color_manual(values = c("#ff6b6b", "#4ecdc4", "#45b7d1", "#f9ca24", 
                                    "#f0932b", "#eb4d4b", "#6c5ce7", "#00b894",
                                    "#fdcb6e", "#e17055", "#74b9ff", "#a29bfe")) +
      labs(title = paste("Interactive Courses for:", tools::toTitleCase(query)),
           subtitle = paste(nrow(courses_with_salary), "courses with salary data • Hover for details • Click to visit course page"),
           x = "Median Salary", y = NULL, color = "University") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 5), 
                                  color = "#ffffff", face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15),
                                     color = "#b3b3b3", face = "italic"),
        axis.title.x = element_text(size = 13, margin = margin(t = 12), 
                                    color = "#e0e0e0", face = "bold"),
        axis.text.y = element_text(size = 9, color = "#d0d0d0"),
        axis.text.x = element_text(size = 11, color = "#d0d0d0"),
        legend.position = "bottom",
        legend.title = element_text(size = 12, color = "#ffffff", face = "bold"),
        legend.text = element_text(size = 9, color = "#e0e0e0"),
        panel.grid.major.x = element_line(color = "#404040", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "#303030", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        plot.background = element_rect(fill = "#191414", color = NA)
      )
  }
  
  # Return girafe object with interactive options
  return(girafe(
    ggobj = p, 
    width_svg = 12, 
    height_svg = 8,
    options = list(
      opts_hover(css = "stroke:white;stroke-width:3px;opacity:1;"),
      opts_tooltip(
        css = "background-color:rgba(0,0,0,0.9);color:white;padding:12px;border-radius:6px;font-size:13px;box-shadow:0 4px 8px rgba(0,0,0,0.3);z-index:9999;border:1px solid rgba(255,255,255,0.2);",
        opacity = 1,
        use_fill = FALSE,
        delay_mouseover = 200,
        delay_mouseout = 500
      )
    )
  ))
}

# Define the enhanced sim_degrees function with pagination for modal search
sim_degrees_paginated <- function(word, df = degree_data1, page = 1, per_page = 12) {
  # Use the new ggiraph-based function for better interactivity
  tryCatch({
    # Test basic string matching first to ensure we have matches
    basic_matches <- df %>%
      dplyr::filter(str_detect(tolower(title), tolower(word)))
    
    cat("DEBUG: Basic string search for '", word, "' found ", nrow(basic_matches), " matches\n")
    
    # Use the improved ggiraph function with pagination
    girafe_plot <- plot_fnnsimilar_degrees(word, df, k = 50, page = page, per_page = per_page)
    
    # Estimate totals for pagination compatibility (don't filter by salary for counting)
    total_courses <- max(nrow(basic_matches), 1)
    total_pages <- max(1, ceiling(total_courses / per_page))
    
    return(list(
      plot = girafe_plot,
      total_courses = total_courses,
      total_pages = total_pages,
      current_page = page
    ))
  }, error = function(e) {
    cat("ERROR in sim_degrees_paginated:", e$message, "\n")
    # Fallback to simple message if ggiraph fails
    return(list(
      plot = girafe(ggobj = ggplot() + 
                      annotate("text", x = 0.5, y = 0.5, 
                               label = paste("Error loading courses for:", word), 
                               size = 4, color = "#ffffff") +
                      theme_void() +
                      theme(plot.background = element_rect(fill = "#191414", color = NA)),
                    width_svg = 10, height_svg = 6),
      total_courses = 0,
      total_pages = 1,
      current_page = 1
    ))
  })
}

# Keep the original sim_degrees function for backwards compatibility
sim_degrees <- function(word, df = degree_data1) {
  # Filter courses that contain the word - SHOW ALL MATCHES
  matching_courses <- df %>%
    dplyr::filter(str_detect(tolower(title), tolower(word))) %>%
    # Sort by salary descending, with NAs at the bottom (na.last = TRUE)
    arrange(desc(median_salary))
  
  if(nrow(matching_courses) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                      label = paste("No courses found containing:", word), 
                      size = 5, color = "#ffffff") +
             theme_void() +
             theme(plot.background = element_rect(fill = "#191414", color = NA)))
  }
  
  # Separate courses with and without salary data
  courses_with_salary <- matching_courses %>% dplyr::filter(!is.na(median_salary))
  courses_without_salary <- matching_courses %>% dplyr::filter(is.na(median_salary))
  
  # Calculate average salary for the vertical line (only from courses with data)
  avg_salary <- ifelse(nrow(courses_with_salary) > 0, 
                       mean(courses_with_salary$median_salary, na.rm = TRUE), 
                       35000)  # Default if no salary data
  
  # Calculate dynamic scale limits (min - 5k, max + 5k)
  if(nrow(courses_with_salary) > 0) {
    min_salary <- min(courses_with_salary$median_salary, na.rm = TRUE) - 5000
    max_salary <- max(courses_with_salary$median_salary, na.rm = TRUE) + 5000
  } else {
    min_salary <- 15000
    max_salary <- 65000
  }
  
  # Ensure minimum scale starts at reasonable value
  min_salary <- max(min_salary, 15000)
  max_salary <- max(max_salary, min_salary + 10000)
  
  # Fixed plot height for consistency
  plot_height <- min(max(25, nrow(matching_courses)), 50)  # Between 25-50 courses visible
  
  # Create a salary_for_ordering column that treats NAs as minimum value for ALL matching courses
  matching_courses$salary_for_ordering <- ifelse(is.na(matching_courses$median_salary), 
                                                 0,  # Use 0 for NAs to put them at bottom
                                                 matching_courses$median_salary)
  
  # Update the subset dataframes to include the new column
  courses_with_salary <- matching_courses %>% dplyr::filter(!is.na(median_salary))
  courses_without_salary <- matching_courses %>% dplyr::filter(is.na(median_salary))
  
  # Create clean plot sorted by salary descending
  p <- ggplot(matching_courses, aes(x = median_salary, y = reorder(title, salary_for_ordering))) +
    # Add stylish average line with bright cyan for dark theme
    geom_vline(xintercept = avg_salary, 
               color = "#00d4ff", alpha = 0.9, linetype = "solid", linewidth = 0.3) +
    # Points for courses with salary data
    geom_point(data = courses_with_salary, 
               aes(colour = university_name), size = 1.2, alpha = 0.85) +
    # Red crosses for courses without salary data (positioned at average line)
    geom_point(data = courses_without_salary, 
               aes(x = avg_salary), 
               shape = 4, size = 2, color = "#ff4757", stroke = 0.3, alpha = 0.9) +
    scale_x_continuous(
      limits = c(min_salary, max_salary),
      breaks = scales::pretty_breaks(n = 6),
      labels = scales::label_currency(prefix = "£", big.mark = ",", accuracy = 1)
    ) +
    # Use vibrant color palette optimized for dark backgrounds
    scale_color_manual(values = c("#ff6b6b", "#4ecdc4", "#45b7d1", "#f9ca24", 
                                  "#f0932b", "#eb4d4b", "#6c5ce7", "#00b894",
                                  "#fdcb6e", "#e17055", "#74b9ff", "#a29bfe")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5, margin = margin(b = 12), 
                                color = "#ffffff", face = "bold"),
      axis.title.x = element_text(size = 11, margin = margin(t = 10), 
                                  color = "#e0e0e0", face = "bold"),
      axis.text.y = element_text(size = 8, color = "#d0d0d0", margin = margin(r = 8)),
      axis.text.x = element_text(size = 9.5, color = "#d0d0d0"),
      legend.position = "bottom",
      legend.title = element_text(size = 10, color = "#ffffff", face = "bold"),
      legend.text = element_text(size = 8, color = "#e0e0e0"),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = 10),
      panel.grid.major.x = element_line(color = "#404040", linewidth = 0.3),
      panel.grid.major.y = element_line(color = "#303030", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#1a1a1a", color = NA),
      plot.background = element_rect(fill = "#191414", color = NA),
      plot.margin = margin(t = 12, r = 20, b = 20, l = 20)
    )
  
  # Create custom legend that includes both university colors and NA indicator
  if (nrow(courses_without_salary) > 0) {
    # Add both university legend and NA legend
    p <- p + 
      guides(colour = guide_legend(title = "University", nrow = 2, override.aes = list(size = 3))) +
      labs(x = "Median Salary", y = NULL, 
           title = paste0(tools::toTitleCase(word), " Courses (", nrow(matching_courses), " found)"),
           caption = "✕ Data Not Yet Available") +
      theme(
        plot.caption = element_text(color = "#ff4757", size = 8, hjust = 1, 
                                    margin = margin(t = 5), face = "bold")
      )
  } else {
    p <- p + 
      guides(colour = guide_legend(title = "University", nrow = 2, override.aes = list(size = 3))) +
      labs(x = "Median Salary", y = NULL, 
           title = paste0(tools::toTitleCase(word), " Courses (", nrow(matching_courses), " found)"))
  }
  
  # Add modern average line annotation with bright cyan
  p <- p + 
    annotate("text", x = avg_salary + (max_salary - min_salary) * 0.02, 
             y = length(matching_courses$title) * 0.15, 
             label = paste0("Avg: £", format(round(avg_salary), big.mark = ",")), 
             hjust = 0, vjust = 0.5, size = 3.5, alpha = 0.9, 
             color = "#00d4ff", fontface = "bold")
  
  return(p)
}

# Subject categories mapping (simplified for now - you can expand this based on your new dataset)
get_subject_category_courses <- function(category, data) {
  if(category == "Natural Sciences") {
    keywords <- c("Applied Medical Sciences", "Audiology", "Biochemistry", "Biological Sciences", 
                  "Biomedical Sciences", "Bioprocessing", "Business and Health", "Cancer Biomedicine",
                  "Chemistry", "Earth Sciences", "Environmental Geoscience", "Geography and Economics",
                  "Geography", "Geology", "Human Neuroscience", "Human Sciences", "Infection and Immunity",
                  "Mathematics with Mathematical Physics", "Mathematics and Physics", "Neuroscience",
                  "Nutrition and Medical Sciences", "Population Health Sciences", "Psychology",
                  "Science and Engineering", "Sport and Exercise Medical Sciences", "Sustainable Built",
                  "Theoretical Physics", "Biochemical Engineering", "Biomedical Engineering")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Humanities") {
    keywords <- c("Anthropology", "Archaeology", "Experimental Linguistics", "Global Humanitarian",
                  "History and Philosophy", "Philosophy, Politics and Economics", "Politics and International",
                  "Urban Planning", "Urban Studies", "Ancient History", "Classical", "Classics",
                  "Comparative Literature", "Creative Arts and Humanities", "Education, Society",
                  "History", "Philosophy", "Politics, Sociology", "Viking", "Bulgarian", "Czech",
                  "Finnish", "Hungarian", "Polish", "Romanian", "Russian and History", "Ukrainian", "Serbian", "Croatian")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Architecture") {
    keywords <- c("Architectural", "Architecture")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Computational & Mathematical Sciences") {
    keywords <- c("Astrophysics", "Computer Science", "Crime and Security Science", "Data Science",
                  "Geophysics", "Mathematics", "Statistical Science", "Statistics", "Physics",
                  "Electronic and Electrical Engineering", "Mechanical Engineering", "Philosophy and Computer")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Social Sciences") {
    keywords <- c("Social Sciences", "Geography", "Economics", "Politics", "Sociology", "European Social")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Management") {
    keywords <- c("Management", "Business")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Medicine") {
    keywords <- c("Medical", "Medicine", "Biomedical", "Cancer", "Neuroscience", "Pharmacology", "Sport and Exercise")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Sustainability") {
    keywords <- c("Sustainable", "Sustainability")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Engineering") {
    keywords <- c("Engineering", "Computer Science")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Languages") {
    languages <- c("Dutch", "French", "German", "Hebrew", "Hungarian", "Italian", "Norwegian", 
                   "Polish", "Romanian", "Russian Studies", "Scandinavian Studies", 
                   "Spanish and Latin American Studies", "Bulgarian", "Czech", "Danish", 
                   "Finnish", "Serbian", "Croatian", "Swedish", "Ukrainian", "Ancient Languages",
                   "Linguistics", "Psychology and Language Sciences")
    pattern <- paste(languages, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Arts") {
    keywords <- c("Fine Art", "Art", "History of Art", "Media", "Creative Arts", "English", "Literature")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Education") {
    keywords <- c("Education", "Early Childhood")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Technology") {
    keywords <- c("Information Management", "Art and Technology", "Electronic and Electrical")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
    
  } else if(category == "Law") {
    keywords <- c("Law", "Laws")
    pattern <- paste(keywords, collapse = "|")
    return(data[grepl(pattern, data$title, ignore.case = TRUE), ])
  }
  
  return(data)
}

# UI with Spotify-inspired design BUT ALL FUNCTIONALITY + POSTCODE INPUT
ui <- fluidPage(
  useShinyjs(),
  shiny::tags$head(
    shiny::tags$style(HTML("
      /* SPOTIFY-INSPIRED GLOBAL STYLES */
      * {
        box-sizing: border-box;
      }
      
      body {
        background: linear-gradient(135deg, #191414 0%, #1a1a1a 50%, #121212 100%) !important;
        color: #ffffff !important;
        font-family: 'Circular', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
        margin: 0 !important;
        padding: 0 !important;
        overflow-x: hidden !important;
      }
      
      .container-fluid {
        background: linear-gradient(135deg, #191414 0%, #1a1a1a 50%, #121212 100%) !important;
        color: #fff !important;
        min-height: 100vh !important;
        padding: 0 !important;
      }
      
      /* SPOTIFY HEADER STYLING */
      .spotify-header {
        background: linear-gradient(135deg, #1DB954 0%, #1ed760 50%, #1aa34a 100%) !important;
        padding: 60px 50px !important;
        text-align: center !important;
        color: #fff !important;
        position: relative !important;
        overflow: hidden !important;
      }
      
      .spotify-header::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\"><defs><pattern id=\"grain\" width=\"100\" height=\"100\" patternUnits=\"userSpaceOnUse\"><circle cx=\"25\" cy=\"25\" r=\"1\" fill=\"%23ffffff\" opacity=\"0.05\"/><circle cx=\"75\" cy=\"75\" r=\"1\" fill=\"%23ffffff\" opacity=\"0.03\"/><circle cx=\"50\" cy=\"10\" r=\"0.5\" fill=\"%23ffffff\" opacity=\"0.08\"/></pattern></defs><rect width=\"100\" height=\"100\" fill=\"url(%23grain)\"/></svg>') !important;
        pointer-events: none;
      }
      
      .header-content {
        position: relative;
        z-index: 2;
      }
      
      .main-title {
        font-size: 48px !important;
        font-weight: 900 !important;
        margin: 0 0 16px 0 !important;
        letter-spacing: -1px !important;
        text-shadow: 0 2px 4px rgba(0,0,0,0.3) !important;
      }
      
      .main-subtitle {
        font-size: 18px !important;
        font-weight: 400 !important;
        margin: 0 !important;
        opacity: 0.9 !important;
        max-width: 600px !important;
        margin: 0 auto !important;
      }
      
      /* MAIN CONTENT STYLING */
      .main-content {
        padding: 50px !important;
        max-width: 1200px !important;
        margin: 0 auto !important;
      }
      
      /* SECTION HEADERS */
      .section-header {
        font-size: 32px !important;
        font-weight: 700 !important;
        color: #ffffff !important;
        margin: 60px 0 16px 0 !important;
        letter-spacing: -0.5px !important;
      }
      
      .section-subtitle {
        font-size: 16px !important;
        color: #b3b3b3 !important;
        margin: 0 0 32px 0 !important;
        line-height: 1.5 !important;
      }
      
      /* CARD CONTAINERS */
      .card-container {
        background: rgba(255,255,255,0.02) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 16px !important;
        padding: 40px !important;
        margin-bottom: 40px !important;
        backdrop-filter: blur(10px) !important;
        transition: all 0.3s ease !important;
      }
      
      .card-container:hover {
        background: rgba(255,255,255,0.05) !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
        transform: translateY(-2px) !important;
      }
      
      /* FORM STYLING */
      .form-group {
        margin-bottom: 24px !important;
      }
      
      .field-label {
        color: #ffffff !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        margin-bottom: 8px !important;
        text-transform: uppercase !important;
        letter-spacing: 0.5px !important;
      }
      
      /* FORM ELEMENT CONSISTENCY */
      select, input[type=\"text\"], input[type=\"email\"], input[type=\"number\"], textarea {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.15) !important;
        border-radius: 12px !important;
        color: #ffffff !important;
        padding: 14px 16px !important;
        font-size: 14px !important;
        min-height: 50px !important;
        box-sizing: border-box !important;
        transition: all 0.3s ease !important;
      }
      
      input[type=\"text\"]:focus, input[type=\"email\"]:focus, input[type=\"number\"]:focus, select:focus, textarea:focus {
        border-color: #1DB954 !important;
        box-shadow: 0 0 0 3px rgba(29, 185, 84, 0.2) !important;
        outline: none !important;
        background: rgba(255,255,255,0.12) !important;
      }
      
      input::placeholder {
        color: #888888 !important;
      }
      
      /* GRADES GRID */
      .grades-grid {
        display: grid !important;
        grid-template-columns: repeat(4, 1fr) !important;
        gap: 24px !important;
        margin-top: 24px !important;
      }
      
      .grades-grid-btec {
        display: grid !important;
        grid-template-columns: repeat(3, 1fr) !important;
        gap: 16px !important;
        margin-top: 24px !important;
      }
      
      .subject-grade-pair {
        display: flex !important;
        flex-direction: column !important;
        gap: 8px !important;
      }
      
      /* INTERESTS GRID - Consistent Spotify styling */
      .interests-grid {
        display: grid !important;
        grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)) !important;
        gap: 12px !important;
        margin-top: 16px !important;
      }
      
      /* HIERARCHICAL TAXONOMY LAYOUT */
      .interests-grid-container {
        margin-top: 16px !important;
      }
      
      .taxonomy-row {
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 20px !important;
        margin-bottom: 30px !important;
      }
      
      .taxonomy-main-card {
        flex: 1 1 calc(25% - 15px) !important;
        min-width: 280px !important;
        background: rgba(255,255,255,0.03) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 12px !important;
        padding: 16px !important;
      }
      
      .taxonomy-main-header {
        background: rgba(30, 215, 96, 0.1) !important;
        color: #1ed760 !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        padding: 8px 12px !important;
        border-radius: 8px !important;
        margin-bottom: 12px !important;
        cursor: pointer !important;
        transition: all 0.2s ease !important;
        text-align: center !important;
      }
      
      .taxonomy-main-header:hover {
        background: rgba(30, 215, 96, 0.2) !important;
        transform: translateY(-1px) !important;
      }
      
      .taxonomy-main-header.selected {
        background: #1ed760 !important;
        color: #000 !important;
      }
      
      .taxonomy-subcategories {
        display: flex !important;
        flex-direction: column !important;
        gap: 6px !important;
      }
      
      .taxonomy-sub-card {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 6px !important;
        padding: 8px 10px !important;
        font-size: 12px !important;
        color: rgba(255,255,255,0.8) !important;
        cursor: pointer !important;
        transition: all 0.2s ease !important;
      }
      
      .taxonomy-sub-card:hover {
        background: rgba(255,255,255,0.1) !important;
        color: #fff !important;
        border-color: rgba(255,255,255,0.3) !important;
      }
      
      .taxonomy-sub-card.selected {
        background: rgba(30, 215, 96, 0.2) !important;
        border-color: #1ed760 !important;
        color: #1ed760 !important;
      }
      
      .interest-card {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.15) !important;
        border-radius: 12px !important;
        padding: 14px 16px !important;
        text-align: center !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        color: #ffffff !important;
        cursor: pointer !important;
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;
        user-select: none !important;
        position: relative !important;
        overflow: hidden !important;
        min-height: 50px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        box-sizing: border-box !important;
      }
      
      .interest-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(29, 185, 84, 0.1), transparent);
        transition: left 0.5s ease;
      }
      
      .interest-card:hover {
        background: rgba(255,255,255,0.1) !important;
        border-color: rgba(29, 185, 84, 0.6) !important;
        transform: translateY(-3px) scale(1.02) !important;
        box-shadow: 0 8px 25px rgba(29, 185, 84, 0.2) !important;
      }
      
      .interest-card:hover::before {
        left: 100%;
      }
      
      .interest-card.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
        transform: translateY(-2px) !important;
        box-shadow: 0 8px 25px rgba(29, 185, 84, 0.4) !important;
      }
      
      /* DEGREE GRID - Consistent with interests styling */
      .degree-grid {
        display: grid !important;
        grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)) !important;
        gap: 12px !important;
        margin-top: 16px !important;
      }
      
      /* COMPACT DEGREE GRID for three-column layout */
      .degree-grid-compact {
        display: grid !important;
        grid-template-columns: repeat(auto-fill, minmax(80px, 1fr)) !important;
        gap: 8px !important;
        margin-top: 16px !important;
      }
      
      /* FULL-WIDTH DEGREE GRID for two-row layout */
      .degree-grid {
        display: grid !important;
        grid-template-columns: repeat(auto-fill, minmax(100px, 1fr)) !important;
        gap: 12px !important;
        margin-top: 16px !important;
      }
      
      .degree-card {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.15) !important;
        border-radius: 12px !important;
        padding: 18px 20px !important;
        text-align: center !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        color: #ffffff !important;
        cursor: pointer !important;
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;
        user-select: none !important;
        position: relative !important;
        overflow: hidden !important;
        min-height: 60px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        box-sizing: border-box !important;
      }
      
      /* Compact degree cards for three-column layout */
      .degree-grid-compact .degree-card {
        padding: 12px 8px !important;
        font-size: 12px !important;
        min-height: 40px !important;
      }
      
      .degree-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(29, 185, 84, 0.1), transparent);
        transition: left 0.5s ease;
      }
      
      .degree-card:hover {
        background: rgba(255,255,255,0.1) !important;
        border-color: rgba(29, 185, 84, 0.6) !important;
        transform: translateY(-3px) scale(1.02) !important;
        box-shadow: 0 8px 25px rgba(29, 185, 84, 0.2) !important;
      }
      
      .degree-card:hover::before {
        left: 100%;
      }
      
      .degree-card.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
        transform: translateY(-2px) !important;
        box-shadow: 0 8px 25px rgba(29, 185, 84, 0.4) !important;
      }
      
      .degree-card.small-text {
        font-size: 12px !important;
      }
      
      .degree-grid-compact .degree-card.small-text {
        font-size: 10px !important;
      }
      
      /* NEW DEGREES SECTION STYLING */
      .degrees-section {
        background: rgba(255,255,255,0.02) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 12px !important;
        padding: 24px !important;
        transition: all 0.3s ease !important;
        min-height: 280px !important;
        display: flex !important;
        flex-direction: column !important;
      }
      
      .degrees-section:hover {
        background: rgba(255,255,255,0.05) !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      /* TWO-ROW PREFERENCES LAYOUT */
      .preferences-layout {
        display: grid !important;
        grid-template-rows: auto auto !important;
        gap: 30px !important;
        align-items: start !important;
      }
      
      /* Top row with 2 columns: subjects and universities */
      .top-row-grid {
        display: grid !important;
        grid-template-columns: 2fr 1fr !important;
        gap: 30px !important;
        align-items: start !important;
      }
      
      /* Bottom row spans full width for degree types */
      .degrees-section-full {
        grid-column: 1 / -1 !important;
      }
      
      /* COMBINED LAYOUT STYLING */
      .location-combined-grid {
        display: grid !important;
        grid-template-columns: 1fr 2fr !important;
        gap: 40px !important;
        align-items: start !important;
      }
      
      .location-input-section {
        background: rgba(255,255,255,0.02) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 12px !important;
        padding: 24px !important;
        transition: all 0.3s ease !important;
      }
      
      .location-input-section:hover {
        background: rgba(255,255,255,0.05) !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      .distance-options-section {
        background: rgba(255,255,255,0.02) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 12px !important;
        padding: 24px !important;
        transition: all 0.3s ease !important;
      }
      
      .distance-options-section:hover {
        background: rgba(255,255,255,0.05) !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      .distance-compact-grid {
        display: grid !important;
        grid-template-columns: repeat(auto-fit, minmax(110px, 1fr)) !important;
        gap: 8px !important;
        margin-top: 12px !important;
      }
      
      .distance-card.compact {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.15) !important;
        border-radius: 6px !important;
        padding: 10px 8px !important;
        text-align: center !important;
        font-size: 11px !important;
        font-weight: 500 !important;
        color: #ffffff !important;
        cursor: pointer !important;
        transition: all 0.3s ease !important;
        user-select: none !important;
        min-height: 40px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
      }
      
      .distance-card.compact:hover {
        background: rgba(255,255,255,0.1) !important;
        border-color: rgba(29, 185, 84, 0.6) !important;
        transform: translateY(-1px) !important;
      }
      
      .distance-card.compact.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      .distance-card.compact.highlight {
        background: rgba(75, 144, 226, 0.2) !important;
        border-color: rgba(75, 144, 226, 0.5) !important;
        color: #1DB954 !important;
      }
      
      .distance-card.compact.highlight:hover {
        background: rgba(75, 144, 226, 0.3) !important;
        border-color: #1DB954 !important;
      }
      
      .distance-card.compact.highlight.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
      }
      
      /* NEW COMBINED INTERESTS & UNIVERSITIES LAYOUT */
      .interests-universities-grid {
        display: grid !important;
        grid-template-columns: 2fr 1fr !important;
        gap: 40px !important;
        align-items: start !important;
      }
      
      .interests-section {
        background: rgba(255,255,255,0.02) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 12px !important;
        padding: 24px !important;
        transition: all 0.3s ease !important;
      }
      
      .interests-section:hover {
        background: rgba(255,255,255,0.05) !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      .universities-section {
        background: rgba(255,255,255,0.02) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 12px !important;
        padding: 24px !important;
        transition: all 0.3s ease !important;
        min-height: 280px !important;
        display: flex !important;
        flex-direction: column !important;
      }
      
      .universities-section:hover {
        background: rgba(255,255,255,0.05) !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      .section-note {
        font-size: 13px !important;
        color: #888888 !important;
        margin-bottom: 16px !important;
        line-height: 1.4 !important;
        font-style: italic !important;
      }
      
      /* UNIVERSITY SELECTOR COMPACT */
      .university-selector-compact {
        max-width: 100% !important;
        margin: 0 !important;
      }
      
      .university-selector-compact .selectize-control {
        background: rgba(255,255,255,0.05) !important;
        border-radius: 12px !important;
        min-height: 50px !important;
      }
      
      .university-selector-compact .selectize-input {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.15) !important;
        border-radius: 12px !important;
        padding: 14px 16px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        min-height: 50px !important;
        box-sizing: border-box !important;
      }
      
      .university-selector-compact .selectize-input:focus {
        border-color: rgba(29, 185, 84, 0.6) !important;
        box-shadow: 0 0 0 3px rgba(29, 185, 84, 0.2) !important;
      }
      @import url('https://fonts.googleapis.com/css2?family=Circular:wght@300;400;500;600;700;800&display=swap');
      
      * {
        box-sizing: border-box;
        margin: 0;
        padding: 0;
      }
      
      body, html {
        background: linear-gradient(135deg, #191414 0%, #1a1a1a 50%, #121212 100%) !important;
        color: #ffffff !important;
        font-family: 'Circular', -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif !important;
        overflow-x: hidden;
        min-height: 100vh;
      }
      
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        background: transparent !important;
      }
      
      /* SPOTIFY-STYLE HEADER */
      .spotify-header {
        background: linear-gradient(135deg, #1DB954 0%, #1ed760 50%, #1aa34a 100%) !important;
        padding: 40px 60px;
        color: #fff !important;
        position: relative;
        overflow: hidden;
      }
      
      .spotify-header::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\"><circle cx=\"20\" cy=\"20\" r=\"2\" fill=\"%23ffffff\" opacity=\"0.1\"/><circle cx=\"80\" cy=\"40\" r=\"1\" fill=\"%23ffffff\" opacity=\"0.15\"/><circle cx=\"40\" cy=\"80\" r=\"1.5\" fill=\"%23ffffff\" opacity=\"0.1\"/></svg>');
        background-size: 200px 200px;
        animation: float 20s ease-in-out infinite;
      }
      
      @keyframes float {
        0%, 100% { transform: translateY(0px) translateX(0px); }
        50% { transform: translateY(-10px) translateX(5px); }
      }
      
      .header-content {
        position: relative;
        z-index: 2;
      }
      
      .main-title {
        font-size: 48px;
        font-weight: 800;
        margin-bottom: 8px;
        color: #000;
        text-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .badge-alevel {
        background: linear-gradient(90deg, #1DB954 0%, #1ed760 100%) !important;
        color: #fff !important;
        border-radius: 20px;
        padding: 4px 14px;
        font-weight: 700;
        font-size: 13px;
        margin-left: 8px;
      }
      .badge-ib {
        background: linear-gradient(90deg, #1DB954 0%, #1DB954 100%) !important;
        color: #fff !important;
        border-radius: 20px;
        padding: 4px 14px;
        font-weight: 700;
        font-size: 13px;
        margin-left: 8px;
      }
      
      .main-subtitle {
        font-size: 18px;
        font-weight: 400;
        color: rgba(0,0,0,0.7);
        margin-bottom: 0;
      }
      
      /* MAIN CONTENT AREA - FULL WIDTH */
      .main-content {
        padding: 40px 60px;
        max-width: 100% !important;
        width: 100% !important;
        margin: 0 auto;
      }
      
      /* SPOTIFY-STYLE SECTION HEADERS */
      .section-header {
        font-size: 32px;
        font-weight: 700;
        color: #ffffff;
        margin: 60px 0 30px 0;
        letter-spacing: -0.5px;
      }
      
      .section-subtitle {
        font-size: 16px;
        font-weight: 400;
        color: #b3b3b3;
        margin-bottom: 30px;
        line-height: 1.5;
      }
      
      /* CARD-BASED LAYOUT */
      .card-container {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border-radius: 16px;
        padding: 32px;
        margin-bottom: 32px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.3);
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94);
        border: 1px solid rgba(255,255,255,0.05);
      }
      
      .card-container:hover {
        transform: translateY(-4px);
        box-shadow: 0 16px 48px rgba(0,0,0,0.4);
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      /* GRADES & SUBJECTS SECTION - FORCED 4 COLUMNS */
      .grades-grid {
        display: grid !important;
        grid-template-columns: repeat(4, 1fr) !important; /* Default: 4 columns */
        gap: 12px !important;
        margin-bottom: 12px !important;
        width: 100%;
        max-width: auto;
        margin-left: auto;
        margin-right: auto;
      }

      .grades-grid-btec {
        display: grid !important;
        grid-template-columns: repeat(3, 1fr) !important; /* 3 columns for mix */
        gap: 12px !important;
        margin-bottom: 12px !important;
        width: 100%;
        max-width: auto;
        margin-left: auto;
        margin-right: auto;
      }

     /* Shrink the subject-grade-pair boxes */
      .subject-grade-pair {
        background: rgba(255,255,255,0.02);
        border-radius: 8px;
        padding: 8px 6px;
        border: 1px solid rgba(255,255,255,0.04);
        margin-bottom: 0;
      }

      /* Shrink the label text */
      .field-label {
        font-size: 10px;
        font-weight: 600;
        color: #1DB954 !important;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 4px;
      }

      /* Shrink the select inputs */
      .subject-grade-pair select,
      .subject-grade-pair .selectize-input {
        font-size: 12px !important;
        padding: 6px 10px !important;
        border-radius: 6px !important;
        min-height: 28px !important;
      }

      /* Responsive: stack vertically on mobile */
      @media (max-width: 900px) {
        .grades-grid {
            grid-template-columns: 1fr !important;
        }
      }


      
      @media (max-width: 768px) {
        .grades-grid {
          grid-template-columns: repeat(2, 1fr) !important;
        }
      }
      
      @media (max-width: 480px) {
        .grades-grid {
          grid-template-columns: 1fr !important;
        }
      }
      
      
      .subject-grade-pair select {
        width: 100%;
        background: rgba(255,255,255,0.08) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 12px 16px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        transition: all 0.3s ease !important;
        outline: none !important;
      }
      
      .subject-grade-pair select:focus {
        border-color: #1DB954 !important;
        background: rgba(255, 107, 53, 0.1) !important;
        box-shadow: 0 0 0 3px rgba(255, 107, 53, 0.2) !important;
      }
      
      .subject-grade-pair select option {
        background: #2a2a2a !important;
        color: #ffffff !important;
        padding: 8px !important;
      }
      
      /* POSTCODE INPUT STYLING */
      .postcode-container {
        background: rgba(255,255,255,0.03);
        border-radius: 12px;
        padding: 20px;
        transition: all 0.3s ease;
        border: 1px solid rgba(255,255,255,0.05);
        max-width: 300px;
        margin: 0 auto;
      }
      
      .postcode-container:hover {
        background: rgba(255,255,255,0.06);
        border-color: rgba(255, 107, 53, 0.4) !important;
        transform: translateY(-2px);
      }
      
      .postcode-container input {
        width: 100%;
        background: rgba(255,255,255,0.08) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 12px 16px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        transition: all 0.3s ease !important;
        outline: none !important;
        text-align: center;
        text-transform: uppercase;
      }
      
      .postcode-container input:focus {
        border-color: #1DB954 !important;
        background: rgba(255, 107, 53, 0.1) !important;
        box-shadow: 0 0 0 3px rgba(255, 107, 53, 0.2) !important;
      }
      
      .postcode-container input::placeholder {
        color: #888888 !important;
        text-transform: none;
      }
      
      /* INTERESTS SECTION */
      .interests-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(140px, 1fr));
        gap: 8px;
      }
      
      .interest-card {
        background: rgba(255,255,255,0.03);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 8px;
        padding: 12px 8px;
        text-align: center;
        font-size: 11px;
        font-weight: 500;
        color: #ffffff;
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94);
        user-select: none;
        min-height: 50px;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        line-height: 1.2;
      }
      
      .interest-card:hover {
        background: rgba(255,255,255,0.08);
        border-color: rgba(255, 107, 53, 0.5) !important;
        transform: translateY(-2px) scale(1.02);
        box-shadow: 0 4px 15px rgba(0,0,0,0.3);
      }
      
      .interest-card.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
        transform: translateY(-3px) scale(1.02);
        box-shadow: 0 8px 25px rgba(255, 107, 53, 0.4);
      }
      
      /* DEGREE TYPES */
      .degree-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
        gap: 12px;
      }
      
      .degree-card {
        background: rgba(255,255,255,0.03);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 8px;
        padding: 16px 12px;
        text-align: center;
        font-size: 13px;
        font-weight: 500;
        color: #ffffff;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        min-height: 60px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .degree-card:hover {
        background: rgba(255,255,255,0.08);
        border-color: rgba(255, 107, 53, 0.5) !important;
        transform: translateY(-2px);
      }
      
      .degree-card.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      .degree-card.small-text {
        font-size: 11px;
        padding: 12px 8px;
      }
      
      /* DISTANCE FILTER SECTION */
      .distance-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(140px, 1fr));
        gap: 12px;
      }
      
      .distance-card {
        background: rgba(255,255,255,0.03);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 8px;
        padding: 16px 12px;
        text-align: center;
        font-size: 13px;
        font-weight: 500;
        color: #ffffff;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        min-height: 60px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .distance-card:hover {
        background: rgba(255,255,255,0.08);
        border-color: rgba(255, 107, 53, 0.5) !important;
        transform: translateY(-2px);
      }
      
      .distance-card.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      /* NEW COMBINED LOCATION LAYOUT */
      .location-combined-grid {
        display: grid;
        grid-template-columns: 300px 1fr;
        gap: 32px;
        align-items: start;
      }
      
      .location-input-section {
        background: rgba(255,255,255,0.02);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 12px;
        padding: 24px;
        transition: all 0.3s ease;
      }
      
      .location-input-section:hover {
        background: rgba(255,255,255,0.05);
        border-color: rgba(29, 185, 84, 0.3);
      }
      
      .distance-options-section {
        background: rgba(255,255,255,0.02);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 12px;
        padding: 24px;
        transition: all 0.3s ease;
      }
      
      .distance-options-section:hover {
        background: rgba(255,255,255,0.05);
        border-color: rgba(29, 185, 84, 0.3);
      }
      
      .distance-compact-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(110px, 1fr));
        gap: 8px;
        margin-top: 12px;
      }
      
      .distance-card.compact {
        background: rgba(255,255,255,0.05);
        border: 1px solid rgba(255,255,255,0.15);
        border-radius: 6px;
        padding: 10px 8px;
        text-align: center;
        font-size: 11px;
        font-weight: 500;
        color: #ffffff;
        cursor: pointer;
        transition: all 0.3s ease;
        user-select: none;
        min-height: 40px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .distance-card.compact:hover {
        background: rgba(255,255,255,0.1);
        border-color: rgba(255, 107, 53, 0.6);
        transform: translateY(-1px);
      }
      
      .distance-card.compact.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      .distance-card.compact.highlight {
        background: rgba(75, 144, 226, 0.2);
        border-color: rgba(75, 144, 226, 0.5);
        color: #1DB954;
      }
      
      .distance-card.compact.highlight:hover {
        background: rgba(75, 144, 226, 0.3);
        border-color: #1DB954;
      }
      
      .distance-card.compact.highlight.selected {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
      }
      
      /* UNIVERSITY SELECTOR COMPACT */
      .university-selector-compact {
        max-width: 100%;
        margin: 0;
      }
      
      .university-selector-compact .selectize-control {
        background: rgba(255,255,255,0.05) !important;
        border-radius: 12px !important;
      }
      
      .university-selector-compact .selectize-input {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.15) !important;
        border-radius: 12px !important;
        padding: 16px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        min-height: 50px !important;
      }
      
      .university-selector-compact .selectize-input:focus {
        border-color: rgba(255, 107, 53, 0.6) !important;
        box-shadow: 0 0 0 3px rgba(255, 107, 53, 0.2) !important;
      }
      
      /* NEW COMBINED INTERESTS & UNIVERSITIES LAYOUT */
      .interests-universities-grid {
        display: grid;
        grid-template-columns: 2fr 1fr;
        gap: 40px;
        align-items: start;
      }
      
      .interests-section {
        background: rgba(255,255,255,0.02);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 12px;
        padding: 24px;
        transition: all 0.3s ease;
      }
      
      .interests-section:hover {
        background: rgba(255,255,255,0.05);
        border-color: rgba(29, 185, 84, 0.3);
      }
      
      .universities-section {
        background: rgba(255,255,255,0.02);
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 12px;
        padding: 24px;
        transition: all 0.3s ease;
        min-height: 280px;
        display: flex;
        flex-direction: column;
      }
      
      .universities-section:hover {
        background: rgba(255,255,255,0.05);
        border-color: rgba(29, 185, 84, 0.3);
      }
      
      .section-note {
        font-size: 13px;
        color: #888888;
        margin-bottom: 16px;
        line-height: 1.4;
        font-style: italic;
      }
      
      /* SPOTIFY-STYLE BUTTON */
      .spotify-btn {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border: none !important;
        border-radius: 50px !important;
        padding: 16px 48px !important;
        color: #fff !important;
        font-size: 16px !important;
        font-weight: 700 !important;
        text-transform: uppercase !important;
        letter-spacing: 1px !important;
        cursor: pointer !important;
        transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;
        box-shadow: 0 8px 24px rgba(29, 185, 84, 0.3) !important;
        margin: 40px auto !important;
        display: block !important;
        position: relative !important;
        overflow: hidden !important;
      }
      
      .spotify-btn:hover {
        transform: translateY(-3px) scale(1.05) !important;
        box-shadow: 0 12px 36px rgba(255, 107, 53, 0.4) !important;
        background: linear-gradient(135deg, #FF8A50 0%, #F76B47 100%) !important;
      }
      
      .spotify-btn:active {
        transform: translateY(-1px) scale(1.02) !important;
      }
      
      /* FILTER TABS */
      .filter-section {
        margin: 40px 0 !important;
        padding: 0 !important;
      }
      
      .filter-tabs {
        display: flex !important;
        gap: 8px !important;
        margin-bottom: 30px !important;
        flex-wrap: wrap !important;
      }
      
      .filter-tab {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 50px !important;
        padding: 12px 24px !important;
        color: #b3b3b3 !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        cursor: pointer !important;
        transition: all 0.3s ease !important;
        text-decoration: none !important;
      }
      
      .filter-tab:hover {
        background: rgba(255,255,255,0.08) !important;
        color: #ffffff !important;
        border-color: rgba(255,255,255,0.2) !important;
      }
      
      .filter-tab.active {
        background: #ffffff !important;
        color: #000000 !important;
        border-color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      .filter-controls {
        display: flex !important;
        gap: 20px !important;
        align-items: center !important;
        justify-content: flex-end !important;
        flex-wrap: wrap !important;
      }
      
      .filter-controls select {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 8px 12px !important;
        color: #ffffff !important;
        font-size: 14px !important;
        min-width: 120px !important;
      }
      
      .filter-controls select:focus {
        border-color: #1DB954 !important;
        outline: none !important;
      }
      
      .filter-controls label {
        color: #b3b3b3 !important;
        font-size: 14px !important;
        font-weight: 500 !important;
      }
      
      /* COURSE CARDS - FIXED CENTERING FOR NO RESULTS */
      .courses-grid {
        display: grid !important;
        grid-template-columns: repeat(2, minmax(0, 1fr)) !important;
        gap: 30px !important;
        margin-top: 30px !important;
        width: 100% !important;
        padding: 0 10px !important;
        box-sizing: border-box !important;
      }
      
      .courses-grid > * {
        display: contents !important;
      }
      
      /* Fixed centering for no results message */
      .no-results-message {
        grid-column: 1 / -1 !important; /* Span all columns */
        text-align: center !important;
        color: #b3b3b3 !important;
        padding: 60px !important;
        font-size: 18px !important;
        background: rgba(255,255,255,0.02) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 16px !important;
        margin: 20px 0 !important;
      }
      
      .no-results-message h3 {
        color: #ffffff !important;
        margin-bottom: 16px !important;
        font-size: 24px !important;
        font-weight: 700 !important;
      }
      
      .no-results-message p {
        color: #b3b3b3 !important;
        font-size: 16px !important;
        margin: 0 !important;
      }
      
      .course-card {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 12px !important;
        padding: 20px !important;
        border: 1px solid rgba(255,255,255,0.05) !important;
        transition: all 0.3s ease !important;
        cursor: pointer !important;
        position: relative !important;
        overflow: hidden !important;
        min-height: 180px !important;
        width: 100% !important;
        box-sizing: border-box !important;
        margin: 0 !important;
      }
      
      .course-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #1DB954 0%, #1aa34a 100%) !important;
        transform: scaleX(0);
        transition: transform 0.3s ease;
        transform-origin: left;
      }
      
      .course-card:hover {
        transform: translateY(-8px) !important;
        box-shadow: 0 16px 48px rgba(0,0,0,0.4) !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      .course-card:hover::before {
        transform: scaleX(1);
      }
      
      /* Responsive breakpoints */
      @media (min-width: 1200px) {
        .courses-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr)) !important;
          padding: 0 20px !important;
        }
      }
      
      @media (max-width: 768px) {
        .courses-grid {
          grid-template-columns: 1fr !important;
          padding: 0 10px !important;
        }
        
        .location-combined-grid {
          grid-template-columns: 1fr !important;
          gap: 20px !important;
        }
        
        .interests-universities-grid {
          grid-template-columns: 1fr !important;
          gap: 20px !important;
        }
        
        .universities-section {
          min-height: auto !important;
        }
        
        .distance-compact-grid {
          grid-template-columns: repeat(auto-fit, minmax(90px, 1fr)) !important;
          gap: 6px !important;
        }
        
        .distance-card.compact {
          font-size: 10px !important;
          padding: 8px 6px !important;
          min-height: 35px !important;
        }
      }
      
      .match-badge {
        display: inline-block !important;
        padding: 4px 12px !important;
        border-radius: 20px !important;
        font-size: 11px !important;
        font-weight: 600 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.5px !important;
        margin-bottom: 12px !important;
      }
      
      .match-exact { background: #1DB954 !important; color: #000 !important; }
      .match-good { background: #1ed760 !important; color: #fff !important; }
      .match-over { background: #1DB954 !important; color: #000 !important; }
      .match-no { background: #f44336 !important; color: #fff !important; }
      
      .course-title {
        font-size: 20px !important;
        font-weight: 700 !important;
        color: #ffffff !important;
        margin-bottom: 8px !important;
        line-height: 1.3 !important;
        cursor: pointer !important;
        transition: color 0.3s ease !important;
      }
      
      .course-title:hover {
        color: #1DB954 !important;
      }
      
      .course-details {
        font-size: 14px !important;
        color: #b3b3b3 !important;
        margin-bottom: 16px !important;
        line-height: 1.4 !important;
      }
      
      .course-actions {
        display: flex !important;
        gap: 12px !important;
      }
      
      .course-btn {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 20px !important;
        padding: 8px 16px !important;
        color: #ffffff !important;
        font-size: 12px !important;
        font-weight: 500 !important;
        cursor: pointer !important;
        transition: all 0.3s ease !important;
        text-decoration: none !important;
      }
      
      .course-btn:hover {
        background: #1DB954 !important;
        color: #000000 !important;
        border-color: #1DB954 !important;
        transform: translateY(-2px) !important;
      }
      
      .course-btn.primary {
        background: #1DB954 !important;
        color: #000000 !important;
        border-color: #1DB954 !important;
      }
      
      .course-btn.primary:hover {
        background: #FF8A50 !important;
        transform: translateY(-2px) scale(1.05) !important;
      }
      
      /* Distance badge styling */
      .distance-badge {
        display: inline-block !important;
        padding: 4px 8px !important;
        border-radius: 12px !important;
        font-size: 10px !important;
        font-weight: 600 !important;
        background: rgba(255, 107, 53, 0.2) !important;
        color: #1DB954 !important;
        margin-left: 8px !important;
        border: 1px solid rgba(29, 185, 84, 0.3) !important;
      }
      
      /* MODAL STYLING */
      .custom-modal {
        opacity: 0 !important;
        pointer-events: none !important;
        position: fixed !important;
        z-index: 1000 !important;
        left: 0 !important;
        top: 0 !important;
        width: 100% !important;
        height: 100% !important;
        background: rgba(0,0,0,0.9) !important;
        backdrop-filter: blur(10px) !important;
        padding: 20px !important;
        box-sizing: border-box !important;
        transition: opacity 0.3s ease !important;
      }
      
      .custom-modal.active {
        opacity: 1 !important;
        pointer-events: auto !important;
      }
      
      .modal-content-custom {
        background: linear-gradient(135deg, #191414 0%, #1a1a1a 100%) !important;
        border-radius: 16px !important;
        max-width: 1200px !important;
        margin: 0 auto !important;
        padding: 40px !important;
        color: white !important;
        position: relative !important;
        max-height: 90vh !important;
        overflow-y: auto !important;
        box-shadow: 0 25px 50px rgba(0,0,0,0.8) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
      }
      
      .modal-close {
        position: absolute !important;
        top: 20px !important;
        right: 24px !important;
        font-size: 24px !important;
        font-weight: bold !important;
        cursor: pointer !important;
        color: #b3b3b3 !important;
        transition: color 0.3s ease !important;
      }
      
      .modal-close:hover {
        color: #1DB954 !important;
      }
      
      /* TABSET STYLING - Spotify colors */
      .nav-tabs {
        border-bottom: 1px solid rgba(255,255,255,0.1) !important;
        margin-bottom: 24px !important;
      }
      
      .nav-tabs .nav-link {
        background: rgba(255,255,255,0.05) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px 8px 0 0 !important;
        color: #b3b3b3 !important;
        padding: 12px 24px !important;
        margin-right: 4px !important;
        transition: all 0.3s ease !important;
      }
      
      .nav-tabs .nav-link:hover {
        background: rgba(255,255,255,0.1) !important;
        color: #ffffff !important;
        border-color: rgba(29, 185, 84, 0.3) !important;
      }
      
      .nav-tabs .nav-link.active {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-color: #1DB954 !important;
        color: #ffffff !important;
        font-weight: 600 !important;
      }
      
      /* SELECTIZE DROPDOWN STYLING - Enhanced Spotify styling */
      .selectize-dropdown {
        z-index: 9999 !important;
        border-radius: 8px !important;
        background: #2a2a2a !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        box-shadow: 0 6px 18px rgba(0,0,0,0.5) !important;
        color: #ffffff !important;
        font-size: 14px !important;
        padding: 4px 0 !important;
        position: absolute !important;
        top: 100% !important;
        left: 0 !important;
        right: 0 !important;
        transform: none !important;
      }

      .selectize-dropdown-content {
        max-height: 300px !important;
        overflow-y: auto !important;
      }

      .selectize-dropdown .option {
        padding: 10px 16px !important;
        transition: background 0.2s ease !important;
      }

      .selectize-dropdown .option:hover {
        background: rgba(255, 107, 53, 0.2) !important;
        color: #1DB954 !important;
        cursor: pointer !important;
      }

      .selectize-dropdown .active {
        background: rgba(29, 185, 84, 0.3) !important;
        color: #1DB954 !important;
      }

      .selectize-control.single .selectize-input {
        background: rgba(255,255,255,0.08) !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        border-radius: 8px !important;
        padding: 12px 16px !important;
        color: #ffffff !important;
        font-weight: 500 !important;
        position: relative !important;
      }

      .selectize-control {
        position: relative !important;
      }

      .selectize-control .selectize-dropdown {
        position: absolute !important;
        top: calc(100% + 2px) !important;
        left: 0 !important;
        width: 100% !important;
        z-index: 10000 !important;
      }

      body.selectize-dropdown-open {
        pointer-events: auto !important;
      }

      body.selectize-dropdown-open .selectize-dropdown,
      body.selectize-dropdown-open .selectize-input {
        pointer-events: auto !important;
      }
      
      .subject-grade-pair select {
        color: #ffffff !important;
        background-color: rgba(30,30,30,1) !important;
        font-size: 14px !important;
        height: auto !important;
        line-height: 1.5 !important;
      }
      
      .subject-grade-pair select option {
        background: #2a2a2a !important;
        color: #ffffff !important;
      }
      
      .selectize-input,
      .selectize-input input {
        color: #ffffff !important;
        font-size: 14px !important;
      }
      
      /* Additional modal styling elements */
      .go-back-btn {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border: none !important;
        border-radius: 50px !important;
        padding: 12px 24px !important;
        color: #000 !important;
        margin-bottom: 25px !important;
        cursor: pointer !important;
        font-weight: 600 !important;
        transition: all 0.3s ease !important;
        font-size: 14px !important;
      }
      
      .go-back-btn:hover {
        transform: translateY(-2px) scale(1.05) !important;
        box-shadow: 0 4px 15px rgba(75, 144, 226, 0.3) !important;
      }
      
      .course-detail-header {
        display: flex !important;
        gap: 20px !important;
        margin-bottom: 30px !important;
        padding: 20px !important;
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-radius: 15px !important;
        align-items: center !important;
        box-shadow: 0 8px 25px rgba(75, 144, 226, 0.3) !important;
      }
      
      .header-item {
        color: #000 !important;
        font-weight: 600 !important;
        font-size: 16px !important;
        text-align: center !important;
        flex: 1 !important;
      }
      
      .header-title {
        flex: 2 !important;
        font-size: 24px !important;
        font-weight: 700 !important;
        color: #000 !important;
      }
      
      .stats-row {
        display: grid !important;
        grid-template-columns: 1fr 1fr 1fr !important;
        gap: 20px !important;
        margin-bottom: 25px !important;
      }
      
      .stat-card {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 12px !important;
        padding: 20px !important;
        text-align: center !important;
        border: 2px solid rgba(255,255,255,0.1) !important;
        transition: all 0.3s ease !important;
        position: relative !important;
        overflow: hidden !important;
      }
      
      .stat-card::before {
        content: '' !important;
        position: absolute !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        height: 3px !important;
        background: linear-gradient(90deg, #1DB954, #1ed760) !important;
      }
      
      .stat-card:hover {
        transform: translateY(-2px) !important;
        border-color: rgba(30, 215, 96, 0.5) !important;
        box-shadow: 0 8px 24px rgba(30, 215, 96, 0.15) !important;
      }
      
      .stat-label {
        font-size: 12px !important;
        color: #1DB954 !important;
        text-transform: uppercase !important;
        font-weight: 600 !important;
        margin-bottom: 8px !important;
      }
      
      .stat-value {
        font-size: 20px !important;
        font-weight: 700 !important;
        color: white !important;
      }
      
      /* Salary info note styling */
      .salary-info-note {
        text-align: center !important;
        margin-bottom: 24px !important;
        padding: 12px 16px !important;
        background: rgba(244, 67, 54, 0.1) !important;
        border-radius: 8px !important;
        border-left: 4px solid #f44336 !important;
        border-right: 1px solid rgba(244, 67, 54, 0.3) !important;
        border-top: 1px solid rgba(244, 67, 54, 0.3) !important;
        border-bottom: 1px solid rgba(244, 67, 54, 0.3) !important;
      }
      
      /* Enhanced Course Options */
      .options-section {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 16px !important;
        padding: 24px !important;
        margin: 20px 0 !important;
        border: 2px solid rgba(255, 255, 255, 0.1) !important;
        position: relative !important;
        overflow: hidden !important;
      }
      
      .options-section::before {
        content: '' !important;
        position: absolute !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        height: 4px !important;
        background: linear-gradient(90deg, #9c27b0, #e91e63) !important;
      }
      
      .options-header {
        font-size: 18px !important;
        font-weight: 700 !important;
        color: #ffffff !important;
        margin-bottom: 16px !important;
        display: flex !important;
        align-items: center !important;
      }
      
      .options-icon {
        width: 24px !important;
        height: 24px !important;
        background: linear-gradient(135deg, #9c27b0, #e91e63) !important;
        border-radius: 6px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        margin-right: 10px !important;
        font-size: 12px !important;
        color: white !important;
      }
      
      .options-grid {
        display: grid !important;
        grid-template-columns: 1fr 1fr !important;
        gap: 12px !important;
      }
      
      .option-badge {
        background: rgba(156, 39, 176, 0.1) !important;
        border: 1px solid rgba(156, 39, 176, 0.3) !important;
        color: #ffffff !important;
        padding: 12px 16px !important;
        border-radius: 8px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        text-align: center !important;
        transition: all 0.3s ease !important;
      }
      
      .option-badge.available {
        background: rgba(76, 175, 80, 0.2) !important;
        border-color: rgba(76, 175, 80, 0.5) !important;
        color: #4CAF50 !important;
      }
      
      .option-badge:hover {
        transform: translateY(-1px) !important;
        box-shadow: 0 4px 12px rgba(156, 39, 176, 0.2) !important;
      }
      
      /* TEF Ratings styling */
      .ratings-row {
        display: grid !important;
        grid-template-columns: 1fr 1fr 1fr !important;
        gap: 20px !important;
        margin-bottom: 20px !important;
      }
      
      .rating-card {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 12px !important;
        padding: 20px !important;
        text-align: center !important;
        border: 1px solid rgba(255,255,255,0.05) !important;
      }
      
      .rating-label {
        font-size: 12px !important;
        color: #1DB954 !important;
        text-transform: uppercase !important;
        font-weight: 600 !important;
        margin-bottom: 8px !important;
      }
      
      .rating-value {
        font-size: 18px !important;
        font-weight: 700 !important;
        padding: 8px 16px !important;
        border-radius: 8px !important;
        display: inline-block !important;
      }
      
      .rating-value.gold {
        background: linear-gradient(135deg, #FFD700 0%, #FFA500 100%) !important;
        color: #000 !important;
      }
      
      .rating-value.silver {
        background: linear-gradient(135deg, #C0C0C0 0%, #A8A8A8 100%) !important;
        color: #000 !important;
      }
      
      .rating-value.bronze {
        background: linear-gradient(135deg, #CD7F32 0%, #B87333 100%) !important;
        color: white !important;
      }
      
      /* QS Rankings styling - Spotify inspired */
      .rankings-container {
        display: grid !important;
        grid-template-columns: 1fr 1fr !important;
        gap: 20px !important;
        margin: 20px 0 !important;
      }
      
      .ranking-card {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 16px !important;
        padding: 24px !important;
        border: 2px solid rgba(29, 185, 84, 0.3) !important;
        position: relative !important;
        overflow: hidden !important;
        transition: all 0.3s ease !important;
      }
      
      .ranking-card::before {
        content: '' !important;
        position: absolute !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        height: 4px !important;
        background: linear-gradient(90deg, #1DB954, #1ed760) !important;
      }
      
      .ranking-card:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 12px 32px rgba(29, 185, 84, 0.2) !important;
      }
      
      .ranking-header {
        display: flex !important;
        align-items: center !important;
        margin-bottom: 20px !important;
      }
      
      .ranking-icon {
        width: 32px !important;
        height: 32px !important;
        background: linear-gradient(135deg, #1DB954, #1ed760) !important;
        border-radius: 8px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        margin-right: 12px !important;
        font-size: 16px !important;
        font-weight: bold !important;
        color: white !important;
      }
      
      .ranking-title {
        font-size: 18px !important;
        font-weight: 700 !important;
        color: #ffffff !important;
        margin: 0 !important;
      }
      
      .ranking-explanation {
        font-size: 11px !important;
        color: #888 !important;
        margin-top: 4px !important;
        font-style: italic !important;
        line-height: 1.3 !important;
      }
      
      .ranking-main-metric {
        text-align: center !important;
        margin-bottom: 20px !important;
      }
      
      .main-metric-value {
        font-size: 36px !important;
        font-weight: 900 !important;
        color: #1DB954 !important;
        line-height: 1 !important;
        margin-bottom: 4px !important;
      }
      
      .main-metric-label {
        font-size: 12px !important;
        color: #b3b3b3 !important;
        text-transform: uppercase !important;
        font-weight: 600 !important;
        letter-spacing: 1px !important;
      }
      
      .ranking-sub-metrics {
        display: grid !important;
        grid-template-columns: 1fr 1fr !important;
        gap: 12px !important;
      }
      
      .sub-metric {
        background: rgba(255, 255, 255, 0.05) !important;
        border-radius: 8px !important;
        padding: 12px !important;
        text-align: center !important;
      }
      
      .sub-metric-value {
        font-size: 16px !important;
        font-weight: 700 !important;
        color: #ffffff !important;
        margin-bottom: 4px !important;
      }
      
      .sub-metric-label {
        font-size: 10px !important;
        color: #b3b3b3 !important;
        text-transform: uppercase !important;
        font-weight: 600 !important;
      }
      
      /* TEF Rankings styling - Similar but with different accent color */
      .tef-card {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border: 2px solid rgba(255, 215, 0, 0.3) !important;
      }
      
      .tef-card::before {
        background: linear-gradient(90deg, #FFD700, #FFA500) !important;
      }
      
      .tef-card:hover {
        box-shadow: 0 12px 32px rgba(255, 215, 0, 0.2) !important;
      }
      
      .tef-icon {
        background: linear-gradient(135deg, #FFD700, #FFA500) !important;
      }
      
      .tef-main-value {
        color: #FFD700 !important;
      }
      
      .qs-metric-badge {
        background: rgba(29, 185, 84, 0.15) !important;
        border: 1px solid rgba(29, 185, 84, 0.4) !important;
        color: #1DB954 !important;
        padding: 6px 12px !important;
        border-radius: 15px !important;
        font-size: 11px !important;
        font-weight: 600 !important;
      }
      
      /* Course Options styling */
      .options-row {
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 10px !important;
        margin-bottom: 20px !important;
        justify-content: center !important;
      }
      
      .option-badge {
        background: rgba(29, 185, 84, 0.2) !important;
        border: 1px solid #1DB954 !important;
        color: #1DB954 !important;
        padding: 8px 16px !important;
        border-radius: 20px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
      }
      
      .option-badge.available {
        background: #1DB954 !important;
        color: white !important;
      }
      
      .requirements-section {
        margin-bottom: 25px !important;
      }
      
      .requirements-card {
        width: 100% !important;
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 12px !important;
        padding: 20px !important;
        border: 1px solid rgba(255,255,255,0.05) !important;
      }
      
      .requirements-title {
        color: #1DB954 !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        margin-bottom: 10px !important;
        text-transform: uppercase !important;
      }
      
      .requirements-text {
        color: #b3b3b3 !important;
        font-size: 14px !important;
        line-height: 1.5 !important;
        margin-bottom: 15px !important;
      }
      
      .subject-comparison {
        margin-top: 15px !important;
        padding-top: 15px !important;
        border-top: 1px solid rgba(255,255,255,0.1) !important;
      }
      
      .comparison-title {
        color: #1DB954 !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        margin-bottom: 8px !important;
        text-transform: uppercase !important;
      }
      
      .comparison-text {
        color: #b3b3b3 !important;
        font-size: 14px !important;
        font-weight: bold !important;
      }
      
      .enhanced-chart-section {
        margin-top: 20px !important;
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 15px !important;
        padding: 25px !important;
        border: 1px solid rgba(255,255,255,0.05) !important;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3) !important;
      }
      
      .chart-controls-row {
        display: flex !important;
        justify-content: space-between !important;
        align-items: center !important;
        margin-bottom: 20px !important;
        flex-wrap: wrap !important;
        gap: 15px !important;
      }
      
      .search-controls-inline {
        display: flex !important;
        align-items: center !important;
        gap: 10px !important;
        flex-wrap: wrap !important;
      }
      
      .search-controls-inline input {
        background: rgba(255,255,255,0.1) !important;
        border: 1px solid rgba(255,255,255,0.2) !important;
        border-radius: 8px !important;
        color: #ffffff !important;
        padding: 8px 12px !important;
        width: 200px !important;
        font-size: 14px !important;
      }
      
      .search-controls-inline input::placeholder {
        color: #b3b3b3 !important;
      }
      
      .fullscreen-btn {
        background: rgba(75,144,226,0.2) !important;
        border: 1px solid #1DB954 !important;
        color: #1DB954 !important;
        padding: 8px 12px !important;
        font-size: 12px !important;
        border-radius: 6px !important;
      }
      
      .chart-header {
        display: flex !important;
        justify-content: space-between !important;
        align-items: center !important;
        margin-bottom: 15px !important;
        flex-wrap: wrap !important;
        gap: 10px !important;
      }
      
      .chart-title {
        color: #1DB954 !important;
        font-size: 16px !important;
        font-weight: bold !important;
        margin: 0 !important;
      }
      
      .pagination-controls {
        display: flex !important;
        align-items: center !important;
        gap: 10px !important;
      }
      
      .pagination-btn {
        background: rgba(29, 185, 84, 0.2) !important;
        border: 1px solid #1DB954 !important;
        color: #1DB954 !important;
        padding: 6px 12px !important;
        font-size: 12px !important;
        border-radius: 5px !important;
        cursor: pointer !important;
      }
      
      .pagination-btn:hover {
        background: rgba(29, 185, 84, 0.3) !important;
      }
      
      .pagination-btn:disabled {
        background: rgba(255,255,255,0.1) !important;
        border-color: rgba(255,255,255,0.2) !important;
        color: #666666 !important;
        cursor: not-allowed !important;
      }
      
      .page-info {
        color: #b3b3b3 !important;
        font-size: 12px !important;
        margin: 0 5px !important;
      }
      
      .enhanced-chart-container {
        background: #191414 !important;
        border-radius: 10px !important;
        padding: 15px !important;
        border: 1px solid rgba(255,255,255,0.1) !important;
        min-height: 500px !important;
      }
      
      .chart-section {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%) !important;
        border-radius: 15px !important;
        padding: 25px !important;
        text-align: center !important;
        color: #b3b3b3 !important;
        min-height: 250px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        border: 1px solid rgba(255,255,255,0.05) !important;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.3) !important;
      }
      
      .features-section {
        display: flex !important;
        flex-direction: column !important;
        gap: 20px !important;
      }
      
      .search-section {
        display: flex !important;
        flex-direction: column !important;
        gap: 15px !important;
        flex: 1 !important;
      }
      
      .search-controls {
        display: flex !important;
        flex-direction: column !important;
        gap: 10px !important;
      }
      
      .search-results {
        border: 1px solid #e0e0e0 !important;
        border-radius: 8px !important;
        padding: 10px !important;
        background: #f9f9f9 !important;
        min-height: 300px !important;
      }
      
      .features-title {
        color: #1DB954 !important;
        font-size: 18px !important;
        font-weight: 700 !important;
        text-transform: uppercase !important;
        letter-spacing: 1px !important;
      }
      
      .features-toggle {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border-radius: 12px !important;
        padding: 15px 25px !important;
        color: #000 !important;
        text-align: center !important;
        cursor: pointer !important;
        font-weight: 600 !important;
        transition: all 0.3s ease !important;
        box-shadow: 0 4px 15px rgba(75, 144, 226, 0.3) !important;
      }
      
      .features-toggle:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 6px 20px rgba(75, 144, 226, 0.4) !important;
      }
      
      .university-btn {
        background: linear-gradient(135deg, #1DB954 0%, #1aa34a 100%) !important;
        border: none !important;
        border-radius: 12px !important;
        padding: 15px 30px !important;
        color: #000 !important;
        font-weight: 600 !important;
        cursor: pointer !important;
        width: 100% !important;
        transition: all 0.3s ease !important;
        box-shadow: 0 4px 15px rgba(75, 144, 226, 0.3) !important;
      }
      
      .university-btn:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 6px 20px rgba(75, 144, 226, 0.4) !important;
      }
      
      /* RESPONSIVE DESIGN */
      @media (max-width: 768px) {
        .main-content {
          padding: 20px 30px !important;
        }
        
        .spotify-header {
          padding: 30px 30px !important;
        }
        
        .main-title {
          font-size: 36px !important;
        }
        
        .section-header {
          font-size: 24px !important;
        }
        
        .grades-grid {
          grid-template-columns: 1fr !important;
        }
        
        .interests-grid {
          grid-template-columns: repeat(auto-fill, minmax(120px, 1fr)) !important;
          gap: 6px !important;
        }
        
        .filter-controls {
          justify-content: flex-start !important;
        }
        
        .chart-controls-row {
          flex-direction: column !important;
          align-items: stretch !important;
          gap: 10px !important;
        }
        
        .search-controls-inline {
          flex-direction: column !important;
          align-items: stretch !important;
        }
        
        .search-controls-inline input {
          width: 100% !important;
        }
        
        .chart-header {
          flex-direction: column !important;
          align-items: stretch !important;
          text-align: center !important;
        }
        
        /* Two-row layout becomes single column on mobile */
        .top-row-grid {
          display: grid !important;
          grid-template-columns: 1fr !important;
          gap: 20px !important;
        }
        
        .preferences-layout {
          gap: 20px !important;
        }
      }
      
      @media (max-width: 1024px) {
        /* Keep two-column layout on tablets but reduce gap */
        .top-row-grid {
          gap: 20px !important;
        }
        
        .preferences-layout {
          gap: 20px !important;
        }
        
        .degree-grid {
          grid-template-columns: repeat(auto-fill, minmax(90px, 1fr)) !important;
        }
      }
      
      @media (max-width: 480px) {
        .main-content {
          padding: 15px 20px !important;
        }
        
        .spotify-header {
          padding: 20px 20px !important;
        }
        
        .main-title {
          font-size: 28px !important;
        }
        
        .card-container {
          padding: 20px !important;
        }
      }
    "))
  ),
  
  # Header
  div(class = "spotify-header",
      div(class = "header-content",
          h1(class = "main-title", "UK Degree Matchmaker v7"),
          p(class = "main-subtitle", "Match with your perfect university degrees based on grades, interests, and location!")
      )
  ),
  
  # Main Content
  div(class = "main-content",
      
      # Combined Location & Distance Section
      h2(class = "section-header", "Location Preferences"),
      p(class = "section-subtitle", "Set your location and distance preferences (Optional)"),
      
      div(class = "card-container",
          div(class = "location-combined-grid",
              # Postcode input (left side)
              div(class = "location-input-section",
                  div(class = "field-label", "Enter Your UK Postcode"),
                  textInput("user_postcode", 
                            label = NULL,
                            value = "",
                            placeholder = "e.g. SW1A 1AA",
                            width = "100%")
              ),
              # Distance options (right side)
              div(class = "distance-options-section",
                  div(class = "field-label", "Maximum Distance"),
                  div(class = "distance-compact-grid",
                      div(class = "distance-card compact", id = "distance_0_10", 
                          onclick = "toggleInterest('distance_0_10', event)", "0‑10 miles"),
                      div(class = "distance-card compact", id = "distance_10_25", 
                          onclick = "toggleInterest('distance_10_25', event)", "10‑25 miles"),
                      div(class = "distance-card compact", id = "distance_25_50", 
                          onclick = "toggleInterest('distance_25_50', event)", "25‑50 miles"),
                      div(class = "distance-card compact", id = "distance_50_100", 
                          onclick = "toggleInterest('distance_50_100', event)", "50‑100 miles"),
                      div(class = "distance-card compact", id = "distance_100_150", 
                          onclick = "toggleInterest('distance_100_150', event)", "100‑150 miles"),
                      div(class = "distance-card compact", id = "distance_150_plus", 
                          onclick = "toggleInterest('distance_150_plus', event)", "150+ miles"),
                      div(class = "distance-card compact highlight", id = "distance_any", 
                          onclick = "toggleInterest('distance_any', event)", "Any Distance")
                  )
              )
          )
      ),
      
      # Grades & Subjects Section
      h2(class = "section-header", "Your Academic Profile"),
      p(class = "section-subtitle", "Tell us your qualifications and grades to help filter by grade-requirement (Optional)"),
      
      div(class = "card-container",
          tabsetPanel(
            id = "qual_layout",
            tabPanel("A Levels", value = "a_levels"),
            tabPanel("A Levels and BTECs", value = "mix"),
            tabPanel("IB", value = "ib") 
          ),
          uiOutput("dynamic_grades_ui")
      ),
      
      # Interests, Universities & Degree Types Section - Combined Layout
      h2(class = "section-header", "Your Preferences"),
      p(class = "section-subtitle", "Select your interests, preferred universities, and degree types (Optional)"),
      
      div(class = "card-container",
          # Two column layout with degree types spanning below
          div(class = "preferences-layout",
              
              # Top row - Subject Interests and Universities (2 columns)
              div(class = "top-row-grid",
                  # Left column - Subject Interests
                  div(class = "interests-section",
                      div(class = "field-label", "Subject Interests"),
                      p(class = "section-note", "Which subjects interest you most? Select from our comprehensive taxonomy."),
                      div(class = "interests-grid-container", id = "taxonomy-interests-container"
                          # Interest cards will be generated dynamically from taxonomy in hierarchical layout
                      )
                  ),
                  
                  # Right column - Universities
                  div(class = "universities-section",
                      div(class = "field-label", " Preferred Universities"),
                      p(class = "section-note", "Focus your search on specific universities you're interested in."),
                      div(class = "university-selector-compact",
                          selectizeInput(
                            "selected_universities", 
                            label = NULL,
                            choices = universities,
                            multiple = TRUE,
                            options = list(
                              placeholder = "Search and select universities...",
                              maxItems = 10,
                              plugins = list('remove_button'),
                              create = FALSE,
                              hideSelected = TRUE
                            )
                          )
                      )
                  )
              ),
              
              # Bottom row - Degree Types (spanning full width)
              div(class = "degrees-section-full",
                  div(class = "field-label", " Degree Types"),
                  p(class = "section-note", "Filter by specific qualification types."),
                  div(class = "degree-grid",
                      # First row - most common degree types
                      div(class = "degree-card", id = "interest_bsc", onclick = "toggleInterest('bsc', event)", "BSc"),
                      div(class = "degree-card", id = "interest_ba", onclick = "toggleInterest('ba', event)", "BA"),
                      div(class = "degree-card", id = "interest_beng", onclick = "toggleInterest('beng', event)", "BEng"),
                      div(class = "degree-card", id = "interest_meng", onclick = "toggleInterest('meng', event)", "MEng"),
                      div(class = "degree-card", id = "interest_msci", onclick = "toggleInterest('msci', event)", "MSci"),
                      div(class = "degree-card", id = "interest_llb", onclick = "toggleInterest('llb', event)", "LLB"),
                      
                      # Second row - specialized degrees
                      div(class = "degree-card small-text", id = "interest_mbbs", onclick = "toggleInterest('mbbs', event)", "MBBS"),
                      div(class = "degree-card small-text", id = "interest_mbbsbsc", onclick = "toggleInterest('mbbsbsc', event)", "MBBS BSc"),
                      div(class = "degree-card", id = "interest_mpharm", onclick = "toggleInterest('mpharm', event)", "MPharm"),
                      div(class = "degree-card", id = "interest_basc", onclick = "toggleInterest('basc', event)", "BASc"),
                      div(class = "degree-card", id = "interest_bfa", onclick = "toggleInterest('bfa', event)", "BFA"),
                      div(class = "degree-card", id = "interest_mbio", onclick = "toggleInterest('mbio', event)", "MBio"),
                      
                      # Third row - specialized Master's degrees
                      div(class = "degree-card", id = "interest_mchem", onclick = "toggleInterest('mchem', event)", "MChem"),
                      div(class = "degree-card", id = "interest_mphys", onclick = "toggleInterest('mphys', event)", "MPhys"),
                      div(class = "degree-card small-text", id = "interest_mmathphys", onclick = "toggleInterest('mmathphys', event)", "MMathPhys"),
                      div(class = "degree-card", id = "interest_mmath", onclick = "toggleInterest('mmath', event)", "MMath"),
                      div(class = "degree-card small-text", id = "interest_mmathstat", onclick = "toggleInterest('mmathstat', event)", "MMathStat"),
                      div(class = "degree-card", id = "interest_mmorse", onclick = "toggleInterest('mmorse', event)", "MMORSE"),
                      
                      # Fourth row - combined degrees
                      div(class = "degree-card small-text", id = "interest_babsc", onclick = "toggleInterest('babsc', event)", "BA/BSc")
                  )
              )
          )
      ),
      
      # Search Button
      actionButton("submit_filters", "🔍 Find My Perfect Courses", class = "spotify-btn"),
      
      # Filter Section
      div(class = "filter-section",
          div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 20px;",
              # Filter Tabs
              div(class = "filter-tabs",
                  shiny::tags$button("Exact Grade Matches", class = "filter-tab active", id = "tab_exact", 
                                     onclick = "switchTab('exact')"),
                  shiny::tags$button("Overqualified Matches", class = "filter-tab", id = "tab_over", 
                                     onclick = "switchTab('over')"),
                  shiny::tags$button("Underqualified Matches", class = "filter-tab", id = "tab_under", 
                                     onclick = "switchTab('under')"),
                  shiny::tags$button("All Suitable Courses", class = "filter-tab", id = "tab_all", 
                                     onclick = "switchTab('all')")
              ),
              # Filter Controls
              div(class = "filter-controls",
                  div(style = "display: flex; align-items: center; gap: 8px;",
                      shiny::tags$label("Sort by:", `for` = "sort_by"),
                      selectInput("sort_by", NULL,
                                  choices = list("Match Quality" = "match", "Grade Requirement" = "grade", "Distance" = "distance", "Alphabetical" = "alpha"),
                                  selected = "match",
                                  width = "140px")
                  ),
                  div(style = "display: flex; align-items: center; gap: 8px;",
                      shiny::tags$label("Show:", `for` = "num_courses"),
                      selectInput("num_courses", NULL,
                                  choices = list("4" = 4, "16" = 16, "64" = 64, "All" = "all"),
                                  selected = 4,
                                  width = "100px")
                  ),
                  # TEF Filters
                  div(style = "display: flex; align-items: center; gap: 8px;",
                      shiny::tags$label("Overall TEF:", `for` = "tef_overall"),
                      selectInput("tef_overall", NULL,
                                  choices = list("Any" = "any", "Gold" = "Gold", "Silver" = "Silver", "Bronze" = "Bronze"),
                                  selected = "any",
                                  width = "100px")
                  ),
                  div(style = "display: flex; align-items: center; gap: 8px;",
                      shiny::tags$label("Student Exp:", `for` = "tef_experience"),
                      selectInput("tef_experience", NULL,
                                  choices = list("Any" = "any", "Gold" = "Gold", "Silver" = "Silver", "Bronze" = "Bronze"),
                                  selected = "any",
                                  width = "100px")
                  ),
                  div(style = "display: flex; align-items: center; gap: 8px;",
                      shiny::tags$label("Outcomes:", `for` = "tef_outcomes"),
                      selectInput("tef_outcomes", NULL,
                                  choices = list("Any" = "any", "Gold" = "Gold", "Silver" = "Silver", "Bronze" = "Bronze"),
                                  selected = "any",
                                  width = "100px")
                  )
              )
          )
      ),
      
      # Course Results
      h2(class = "section-header", "Your Course Matches"),
      p(class = "section-subtitle", "Courses tailored to your academic profile, interests, and location"),
      
      div(class = "courses-grid",
          uiOutput("course_cards")
      )
  ),
  
  # Full Modal with all functionality (same as before but updated field names)
  div(id = "courseModal", class = "custom-modal",
      div(class = "modal-content-custom",
          span(class = "modal-close", "×"),
          
          # Back button
          shiny::tags$button("← Go Back", class = "go-back-btn", onclick = "closeModal()"),
          
          # Single row header with course info + grade match + subject requirements
          div(class = "course-detail-header",
              div(class = "header-item", id = "modal_degree_type", "BSc"),
              div(class = "header-title", id = "modal_subject_title", "Subject Title"),
              div(class = "header-item", id = "modal_university_name", "University"),
              div(class = "header-item", id = "modal_grade_req", "Grade Req: A"),
              div(style = "flex: 1; display: flex; justify-content: center; align-items: center; gap: 8px;",
                  div(style = "background: #4CAF50; color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600;", 
                      id = "modal_match_badge", "Exact Match"),
                  div(style = "background: #1ed760; color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600; display: none;", 
                      id = "modal_subject_badge", "Subject Requirements Met"),
                  div(style = "background: rgba(46, 134, 171, 0.8); color: white; padding: 8px 16px; border-radius: 8px; font-size: 12px; font-weight: 600;", 
                      id = "modal_distance_badge", "Distance: 25 miles")
              )
          ),
          
          # Enhanced Salary row with LQ, Median, UQ display
          div(class = "stats-row",
              div(class = "stat-card",
                  div(class = "stat-label", "Lower Quartile"),
                  div(class = "stat-value", id = "modal_salary_lq", "£25,000")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Median Salary"),
                  div(class = "stat-value", id = "modal_salary", "£30,000")
              ),
              div(class = "stat-card",
                  div(class = "stat-label", "Upper Quartile"),
                  div(class = "stat-value", id = "modal_salary_uq", "£35,000")
              )
          ),
          
          # Salary info note
          div(class = "salary-info-note",
              span(id = "modal_salary_note", style = "font-size: 11px; color: #f44336; font-style: italic;",
                   "Based on 20 respondents | 15 months after graduation average")
          ),
          
          # Rankings Container - QS and TEF side by side
          div(class = "rankings-container",
              # QS World Rankings Card
              div(class = "ranking-card",
                  div(class = "ranking-header",
                      div(class = "ranking-icon", "🌍"),
                      div(style = "flex: 1;",
                          div(class = "ranking-title", "QS World Ranking"),
                          div(class = "ranking-explanation", "Global university ranking based on academic reputation, employer reputation, and research excellence")
                      )
                  ),
                  div(class = "ranking-main-metric",
                      div(class = "main-metric-value", id = "modal_qs_rank", "#6"),
                      div(class = "main-metric-label", "Global Position")
                  ),
                  div(class = "ranking-sub-metrics",
                      div(class = "sub-metric",
                          div(class = "sub-metric-value", id = "modal_qs_overall_score", "89.5"),
                          div(class = "sub-metric-label", "Overall Score")
                      ),
                      div(class = "sub-metric",
                          div(class = "sub-metric-value", id = "modal_qs_academic_rep", "95.2"),
                          div(class = "sub-metric-label", "Academic Rep")
                      ),
                      div(class = "sub-metric",
                          div(class = "sub-metric-value", id = "modal_qs_employer_rep", "85.3"),
                          div(class = "sub-metric-label", "Employer Rep")
                      ),
                      div(class = "sub-metric",
                          div(class = "sub-metric-value", id = "modal_qs_employment", "88.7"),
                          div(class = "sub-metric-label", "Employment")
                      )
                  )
              ),
              
              # TEF Ratings Card
              div(class = "ranking-card tef-card",
                  div(class = "ranking-header",
                      div(class = "ranking-icon tef-icon", "🏆"),
                      div(style = "flex: 1;",
                          div(class = "ranking-title", "TEF Rating"),
                          div(class = "ranking-explanation", "UK government assessment of teaching quality, student satisfaction, and graduate employment outcomes")
                      )
                  ),
                  div(class = "ranking-main-metric",
                      div(class = "main-metric-value tef-main-value", id = "modal_overall_rating", "Gold"),
                      div(class = "main-metric-label", "Overall Rating")
                  ),
                  div(class = "ranking-sub-metrics",
                      div(class = "sub-metric",
                          div(class = "sub-metric-value", id = "modal_student_experience", "Silver"),
                          div(class = "sub-metric-label", "Student Experience")
                      ),
                      div(class = "sub-metric",
                          div(class = "sub-metric-value", id = "modal_student_outcomes", "Gold"),
                          div(class = "sub-metric-label", "Student Outcomes")
                      )
                  )
              )
          ),
          
          # Enhanced Course Options Section
          div(class = "options-section",
              div(class = "options-header",
                  div(class = "options-icon", "⚙️"),
                  "Study Options Available"
              ),
              div(class = "options-grid",
                  div(class = "option-badge", id = "modal_sandwich_year", "Sandwich Year: No"),
                  div(class = "option-badge", id = "modal_year_abroad", "Year Abroad: No"),
                  div(class = "option-badge", id = "modal_foundation_year", "Foundation Year: No"),
                  div(class = "option-badge", id = "modal_distance_learning", "Distance Learning: No")
              )
          ),
          
          # Requirements textbox with matched subjects indicator
          div(class = "requirements-section",
              div(class = "requirements-card",
                  div(class = "requirements-title", "Requirements & Options"),
                  div(class = "requirements-text", id = "modal_requirements", 
                      "A-Level Subjects: Mathematics, Physics or Chemistry required. Year abroad available. Sandwich placement year optional."),
                  # Subject comparison indicator
                  div(class = "subject-comparison", id = "matched_subjects_indicator",
                      div(class = "comparison-title", "How Your Subjects Compare"),
                      div(class = "comparison-text", id = "matched_subjects_list", "Select subjects to see comparison")
                  )
              )
          ),
          
          # Enhanced single column layout for chart
          div(class = "enhanced-chart-section",
              # University website button and controls row
              div(class = "chart-controls-row",
                  shiny::tags$button("View on University Website", class = "university-btn", 
                                     id = "university_website_btn", onclick = ""),
                  div(class = "search-controls-inline",
                      textInput("plot_search_word", "Search keyword:", value = "", 
                                placeholder = "e.g. math, physics, computer"),
                      actionButton("search_submit", "Search", class = "btn-primary"),
                      actionButton("fullscreen_plot", "⛶ Fullscreen", class = "btn btn-sm fullscreen-btn")
                  )
              ),
              
              # Chart title and pagination controls
              div(class = "chart-header",
                  div(class = "chart-title", id = "chart_title", "Enter a keyword to search for similar courses"),
                  div(class = "pagination-controls", id = "pagination_controls", style = "display: none;",
                      actionButton("prev_page", "← Previous", class = "btn btn-sm pagination-btn"),
                      span(class = "page-info", id = "page_info", "Page 1 of 1"),
                      actionButton("next_page", "Next →", class = "btn btn-sm pagination-btn")
                  )
              ),
              
              # Large chart area
              div(class = "enhanced-chart-container",
                  girafeOutput("modal_chart_plot", height = "500px", width = "100%")
              )
          )
      )
  ),
  
  # Fullscreen Plot Modal
  div(id = "fullscreenModal", class = "custom-modal",
      div(style = "background: linear-gradient(135deg, #191414 0%, #1a1a1a 100%); border-radius: 16px; max-width: 95vw; max-height: 95vh; margin: auto; padding: 20px; color: white; position: relative; overflow: hidden;",
          span(style = "position: absolute; top: 15px; right: 20px; font-size: 24px; font-weight: bold; cursor: pointer; color: #b3b3b3; z-index: 1001;", 
               onclick = "closeFullscreenModal()", "×"),
          div(style = "margin-bottom: 20px;",
              h3(style = "color: #1DB954; margin-bottom: 10px;", "Course Search Results")
          ),
          girafeOutput("fullscreen_chart_plot", height = "70vh", width = "100%")
      )
  ),
  
  # JavaScript for modal and toggle functionality
  shiny::tags$script(HTML("
    // Store scroll position before updates
    var lastScrollPosition = 0;
    
    // Function to save scroll position
    function saveScrollPosition() {
      lastScrollPosition = window.pageYOffset || document.documentElement.scrollTop;
    }
    
    // Function to restore scroll position
    function restoreScrollPosition() {
      window.scrollTo(0, lastScrollPosition);
    }
    
    // Save scroll position before any Shiny updates
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'sort_by' || event.name === 'num_courses') {
        saveScrollPosition();
        // Restore position after a short delay to allow for DOM updates
        setTimeout(restoreScrollPosition, 100);
      }
    });
    
    // Modal functions
    function openModal(courseIndex) {
      document.getElementById('courseModal').classList.add('active');
      Shiny.setInputValue('selected_course_index', courseIndex);
      
      // Reset modal search state when opening new modal
      Shiny.setInputValue('reset_modal_search', Math.random());
      
      // Clear search input
      var searchInput = document.getElementById('modal_search_input');
      if (searchInput) {
        searchInput.value = '';
      }
    }
    
    function closeModal() {
      document.getElementById('courseModal').classList.remove('active');
    }
    
    // Fullscreen modal functions
    function openFullscreenModal() {
      document.getElementById('fullscreenModal').classList.add('active');
    }
    
    function closeFullscreenModal() {
      document.getElementById('fullscreenModal').classList.remove('active');
    }
    
    // Close modal when clicking outside
    window.onclick = function(event) {
      var modal = document.getElementById('courseModal');
      if (event.target == modal) {
        closeModal();
      }
    }
    
    // Close modal when clicking X
    document.querySelector('.modal-close').onclick = function() {
      closeModal();
    }
    
    // Tab switching functionality
    function switchTab(tabName) {
      saveScrollPosition();
      // Update active tab styling
      document.querySelectorAll('.filter-tab').forEach(btn => btn.classList.remove('active'));
      document.getElementById('tab_' + tabName).classList.add('active');
      
      // Send tab change to Shiny
      Shiny.setInputValue('current_tab', tabName);
      
      // Restore scroll position after tab change
      setTimeout(restoreScrollPosition, 100);
    }
    
    // Toggle interest subjects - FIXED to prevent click-through
    function toggleInterest(subject, event) {
      if (event) {
        event.stopPropagation();
        event.preventDefault();
      }
      var element = document.getElementById('interest_' + subject);
      if (!element) {
        element = document.getElementById(subject);
      }
      if (element) {
        element.classList.toggle('selected');
        Shiny.setInputValue('interest_' + subject, element.classList.contains('selected'));
      }
    }
    
    // Update modal content - UPDATED VERSION with new dataset fields
    Shiny.addCustomMessageHandler('updateModal', function(data) {
      document.getElementById('modal_degree_type').innerText = data.degree_type;
      document.getElementById('modal_subject_title').innerText = data.title;
      document.getElementById('modal_university_name').innerText = data.university;
      var gradeReqElem = document.getElementById('modal_grade_req');
      gradeReqElem.innerText = data.grade_req;
      gradeReqElem.classList.remove('badge-alevel', 'badge-ib');
      if (data.grade_type === 'ib') {
        gradeReqElem.classList.add('badge-ib');
      } else {
        gradeReqElem.classList.add('badge-alevel');
      }
      
      // Update salary information - LQ, Median, UQ format
      document.getElementById('modal_salary_lq').innerText = data.salary_lq || 'N/A';
      document.getElementById('modal_salary').innerText = data.salary || 'N/A';
      document.getElementById('modal_salary_uq').innerText = data.salary_uq || 'N/A';
      
      // Update salary note with respondents and timing info
      var salaryNote = document.getElementById('modal_salary_note');
      if (data.salary_respondents && data.salary_respondents !== 'N/A') {
        salaryNote.innerText = 'Based on ' + data.salary_respondents + ' | 15 months after graduation average';
      } else {
        salaryNote.innerText = 'Salary data availability varies | 15 months after graduation average';
      }

      // Update TEF ratings with improved messaging
      var overallRating = document.getElementById('modal_overall_rating');
      var studentExperience = document.getElementById('modal_student_experience'); 
      var studentOutcomes = document.getElementById('modal_student_outcomes');
      
      function updateRatingDisplay(element, rating) {
        // Handle the specific case where university doesn't provide TEF data
        if (!rating || rating === 'No Data' || rating === 'This University does not provide TEF Data.') {
          element.innerText = 'This university does not provide TEF ratings';
          element.classList.remove('gold', 'silver', 'bronze');
          element.style.background = 'rgba(128, 128, 128, 0.3)';
          element.style.color = '#b3b3b3';
        } else {
          element.innerText = rating;
          element.style.background = ''; // Reset to CSS defaults
          element.style.color = ''; // Reset to CSS defaults
          element.classList.remove('gold', 'silver', 'bronze');
          if (rating.toLowerCase() === 'gold') {
            element.classList.add('gold');
          } else if (rating.toLowerCase() === 'silver') {
            element.classList.add('silver');
          } else if (rating.toLowerCase() === 'bronze') {
            element.classList.add('bronze');
          }
        }
      }
      
      updateRatingDisplay(overallRating, data.overall_rating);
      updateRatingDisplay(studentExperience, data.student_experience_rating);
      updateRatingDisplay(studentOutcomes, data.student_outcomes_rating);
      
      // Update QS Rankings
      var qsRank = document.getElementById('modal_qs_rank');
      var qsOverallScore = document.getElementById('modal_qs_overall_score');
      var qsAcademicRep = document.getElementById('modal_qs_academic_rep');
      var qsEmployerRep = document.getElementById('modal_qs_employer_rep');
      var qsEmployment = document.getElementById('modal_qs_employment');
      
      function updateQSDisplay(element, value, prefix = '', suffix = '') {
        if (value && value !== 'N/A' && value !== null && value !== undefined) {
          element.innerText = prefix + value + suffix;
          element.style.color = '#1DB954';
        } else {
          element.innerText = 'Not Available';
          element.style.color = '#b3b3b3';
        }
      }
      
      updateQSDisplay(qsRank, data.qs_rank, '#');
      updateQSDisplay(qsOverallScore, data.qs_overall_score);
      updateQSDisplay(qsAcademicRep, data.qs_academic_rep);
      updateQSDisplay(qsEmployerRep, data.qs_employer_rep);
      updateQSDisplay(qsEmployment, data.qs_employment_outcomes);
      
      // Update course options
      var sandwichYear = document.getElementById('modal_sandwich_year');
      var yearAbroad = document.getElementById('modal_year_abroad');
      var foundationYear = document.getElementById('modal_foundation_year');
      var distanceLearning = document.getElementById('modal_distance_learning');
      
      function updateOptionBadge(element, available, optionName) {
        element.innerText = optionName + ': ' + (available ? 'Available' : 'Not Available');
        element.classList.toggle('available', available);
      }
      
      updateOptionBadge(sandwichYear, data.option_sandwich_year === 'Yes', 'Sandwich Year');
      updateOptionBadge(yearAbroad, data.option_year_abroad === 'Yes', 'Year Abroad');
      updateOptionBadge(foundationYear, data.option_foundation_year === 'Yes', 'Foundation Year');
      updateOptionBadge(distanceLearning, data.option_distance_learning === 'Yes', 'Distance Learning');

      // Update distance badge - only show if distance is available
      var distanceBadge = document.getElementById('modal_distance_badge');
      if(data.distance_text && data.distance_text !== null) {
        distanceBadge.innerText = data.distance_text;
        distanceBadge.style.display = 'block';
      } else {
        distanceBadge.style.display = 'none';
      }

      // Update requirements text with real data
      document.getElementById('modal_requirements').innerText = data.requirements;

      // Update match badge with proper color
      var matchBadge = document.getElementById('modal_match_badge');
      var matchColor = '#666666'; // default
      switch(data.match_type) {
        case 'Exact Match': matchColor = '#4CAF50'; break;
        case 'Good Match': matchColor = '#1ed760'; break;
        case 'Overmatch': matchColor = '#1DB954'; break;
        case 'No Match': matchColor = '#f44336'; break;
      }
      matchBadge.style.backgroundColor = matchColor;
      matchBadge.innerText = data.match_type;

      // Update subject comparison display
      var matchedSubjectsContainer = document.getElementById('matched_subjects_indicator');
      var matchedSubjectsList = document.getElementById('matched_subjects_list');
      
      // Always show the section
      matchedSubjectsContainer.style.display = 'block';
      
      if (!data.has_subject_selection) {
        // User hasn't selected any subjects yet
        matchedSubjectsList.innerText = 'Select subjects to see comparison';
        matchedSubjectsList.style.color = '#b3b3b3'; // Neutral gray
      } else if (data.matched_subjects && data.matched_subjects.length > 0) {
        // User has selected subjects and found matches
        matchedSubjectsList.innerText = data.matched_subjects.join(', ');
        matchedSubjectsList.style.color = '#1DB954'; // Blue for matches
      } else {
        // User has selected subjects but no matches found
        matchedSubjectsList.innerText = 'No matching A-Level subjects found';
        matchedSubjectsList.style.color = '#f44336'; // Red for no matches
      }

      // Update university website button
      document.getElementById('university_website_btn').onclick = function() {
        window.open(data.url, '_blank');
      };
    });
    
    // Handle scroll preservation for filter/sort changes
    Shiny.addCustomMessageHandler('preserveScroll', function(data) {
      setTimeout(function() {
        restoreScrollPosition();
      }, 150);
    });
    
    // Open fullscreen modal
    Shiny.addCustomMessageHandler('openFullscreen', function(data) {
      openFullscreenModal();
    });
    
    // Update just the subject comparison section when subjects change
    Shiny.addCustomMessageHandler('updateSubjectComparison', function(data) {
      console.log('Updating subject comparison with:', data);
      
      var matchedSubjectsContainer = document.getElementById('matched_subjects_indicator');
      var matchedSubjectsList = document.getElementById('matched_subjects_list');
      
      if (!matchedSubjectsContainer || !matchedSubjectsList) {
        console.log('Modal elements not found');
        return;
      }
      
      // Always show the section
      matchedSubjectsContainer.style.display = 'block';
      
      if (!data.has_subject_selection) {
        // User hasn't selected any subjects yet
        matchedSubjectsList.innerText = 'Select subjects to see comparison';
        matchedSubjectsList.style.color = '#b3b3b3'; // Neutral gray
        console.log('No subjects selected');
      } else if (data.matched_subjects && data.matched_subjects.length > 0) {
        // User has selected subjects and found matches
        matchedSubjectsList.innerText = data.matched_subjects.join(', ');
        matchedSubjectsList.style.color = '#1DB954'; // Blue for matches
        console.log('Matches found:', data.matched_subjects);
      } else {
        // User has selected subjects but no matches found
        matchedSubjectsList.innerText = 'No matching A-Level subjects found';
        matchedSubjectsList.style.color = '#f44336'; // Red for no matches
        console.log('No matches found');
      }
    });
    
    // Update chart title
    Shiny.addCustomMessageHandler('updateChartTitle', function(data) {
      var chartTitle = document.getElementById('chart_title');
      if (chartTitle) {
        chartTitle.innerText = data.title;
      }
    });
    
    // Update pagination controls
    Shiny.addCustomMessageHandler('updatePagination', function(data) {
      var paginationControls = document.getElementById('pagination_controls');
      var pageInfo = document.getElementById('page_info');
      var prevBtn = document.getElementById('prev_page');
      var nextBtn = document.getElementById('next_page');
      
      if (data.show) {
        paginationControls.style.display = 'flex';
        pageInfo.innerText = 'Page ' + data.current_page + ' of ' + data.total_pages;
        
        // Update button states
        if (data.prev_disabled) {
          prevBtn.disabled = true;
          prevBtn.classList.add('disabled');
        } else {
          prevBtn.disabled = false;
          prevBtn.classList.remove('disabled');
        }
        
        if (data.next_disabled) {
          nextBtn.disabled = true;
          nextBtn.classList.add('disabled');
        } else {
          nextBtn.disabled = false;
          nextBtn.classList.remove('disabled');
        }
      } else {
        paginationControls.style.display = 'none';
      }
    });
  "))
)

# FULL SERVER FUNCTIONALITY WITH DISTANCE CALCULATION
server <- function(input, output, session) {
  
  # ...inside your server function, after the opening line and before current_selected_subjects...
  
  # ...inside your server function...
  
  output$dynamic_grades_ui <- renderUI({
    layout <- input$qual_layout
    if (is.null(layout) || layout == "a_levels") {
      # 4 A Levels, 4 columns per row
      div(class = "grades-grid",
          lapply(1:4, function(i) {
            label_suffix <- if (i == 4) " (Optional)" else ""
            list(
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("A Level Subject", i, label_suffix)),
                  selectizeInput(paste0("subject", i), label = NULL, choices = c("Select Subject" = "", all_subjects), multiple = FALSE)
              ),
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Grade", i, label_suffix)),
                  selectInput(paste0("grade", i), label = NULL, choices = c("Select Grade" = "", "A*", "A", "B", "C", "D", "E"), selectize = FALSE)
              )
            )
          })
      )
    } else if (layout == "mix") {
      # 4 rows, each with 3 columns (Type, Subject, Grade)
      div(class = "grades-grid-btec",
          lapply(1:4, function(i) {
            label_suffix <- if (i == 4) " (Optional)" else ""
            list(
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Type", i, label_suffix)),
                  selectInput(paste0("type", i), label = NULL, choices = c("A Level", "BTEC"), selected = "A Level", selectize = FALSE)
              ),
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Subject", i, label_suffix)),
                  uiOutput(paste0("mix_subject_ui_", i))
              ),
              div(class = "subject-grade-pair",
                  div(class = "field-label", paste("Grade", i, label_suffix)),
                  uiOutput(paste0("mix_grade_ui_", i))
              )
            )
          })
      )
    } else if (layout == "ib") {
      ib_group1 <- c("Literature HL", "Literature SL", "Language and Literature HL", "Language and Literature SL", "Literature and Performance SL")
      ib_group2 <- c("Language B HL", "Language B SL", "Language ab initio SL", "Classical Languages HL", "Classical Languages SL")
      ib_group3 <- c("Business Management HL", "Business Management SL", "Economics HL", "Economics SL", "Geography HL", "Geography SL", "Global Politics HL", "Global Politics SL", "History HL", "History SL", "ITGS HL", "ITGS SL", "Philosophy HL", "Philosophy SL", "Psychology HL", "Psychology SL", "Social and Cultural Anthropology HL", "Social and Cultural Anthropology SL", "World Religions SL")
      ib_group4 <- c("Biology HL", "Biology SL", "Chemistry HL", "Chemistry SL", "Physics HL", "Physics SL", "Computer Science HL", "Computer Science SL", "Design Technology HL", "Design Technology SL", "Environmental Systems and Societies HL", "Environmental Systems and Societies SL", "Sports, Exercise and Health Science HL", "Sports, Exercise and Health Science SL")
      ib_group5 <- c("Mathematics: Analysis and Approaches HL", "Mathematics: Analysis and Approaches SL", "Mathematics: Applications and Interpretation HL", "Mathematics: Applications and Interpretation SL")
      ib_group6 <- c("Dance HL", "Dance SL", "Film HL", "Film SL", "Music HL", "Music SL", "Theatre HL", "Theatre SL", "Visual Arts HL", "Visual Arts SL", "Any other subject from Groups 1-4")
      ib_grade_choices <- as.character(7:1)
      div(class = "grades-grid",
          # Group 1
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 1: Studies in Language and Literature"),
              selectizeInput("ib_g1_subject", label = NULL, choices = c("Select Subject" = "", ib_group1), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g1_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 2
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 2: Language Acquisition"),
              selectizeInput("ib_g2_subject", label = NULL, choices = c("Select Subject" = "", ib_group2), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g2_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 3
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 3: Individuals and Societies"),
              selectizeInput("ib_g3_subject", label = NULL, choices = c("Select Subject" = "", ib_group3), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g3_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 4
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 4: Sciences"),
              selectizeInput("ib_g4_subject", label = NULL, choices = c("Select Subject" = "", ib_group4), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g4_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 5
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 5: Mathematics"),
              selectizeInput("ib_g5_subject", label = NULL, choices = c("Select Subject" = "", ib_group5), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g5_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Group 6
          div(class = "subject-grade-pair",
              div(class = "field-label", "Group 6: The Arts or Substitute"),
              selectizeInput("ib_g6_subject", label = NULL, choices = c("Select Subject" = "", ib_group6), multiple = FALSE),
              div(class = "field-label", "Grade"),
              selectInput("ib_g6_grade", label = NULL, choices = c("", ib_grade_choices), selectize = FALSE)
          ),
          # Core bonus points (full width)
          div(style = "grid-column: 1 / -1; margin-top: 16px;",
              div(class = "field-label", "Core Bonus Points (TOK/EE)"),
              selectInput("ib_core_bonus", label = NULL, choices = 0:3, selected = 0, selectize = FALSE)
          )
      )
    }
  })
  
  # Render subject and grade inputs for each row in "mix" tab
  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("mix_subject_ui_", idx)]] <- renderUI({
        type <- input[[paste0("type", idx)]]
        if (is.null(type) || type == "A Level") {
          selectizeInput(paste0("subject", idx), label = NULL, choices = c("Select Subject" = "", all_subjects), multiple = FALSE)
        } else {
          selectizeInput(paste0("subject", idx), label = NULL, choices = c("Select Subject" = "", btec_subjects), multiple = FALSE)
        }
      })
      output[[paste0("mix_grade_ui_", idx)]] <- renderUI({
        type <- input[[paste0("type", idx)]]
        if (is.null(type) || type == "A Level") {
          selectInput(paste0("grade", idx), label = NULL, choices = c("Select Grade" = "", "A*", "A", "B", "C", "D", "E"), selectize = FALSE)
        } else {
          selectInput(paste0("grade", idx), label = NULL, choices = c("Select Grade" = "", btec_grades), selectize = FALSE)
        }
      })
    })
  }
  
  # Enhanced modal search state with pagination
  modal_search_state <- reactiveValues(
    plot = NULL,
    search_performed = FALSE,
    search_word = "",
    current_page = 1,
    total_pages = 0,
    total_courses = 0
  )
  
  # Initialize with default plot
  default_plot <- girafe(ggobj = ggplot() + 
                           annotate("text", x = 0.5, y = 0.5, 
                                    label = "Enter a keyword and click Search to find similar courses", 
                                    size = 4, color = "#ffffff") +
                           theme_void() +
                           theme(plot.background = element_rect(fill = "#191414", color = NA)),
                         width_svg = 10, height_svg = 6)
  
  modal_search_state$plot <- default_plot
  
  # Single output assignments that read from reactive values
  output$modal_chart_plot <- renderGirafe({
    modal_search_state$plot
  })
  
  # Fullscreen plot outputs (mirror the modal plot)
  output$fullscreen_chart_plot <- renderGirafe({
    modal_search_state$plot
  })
  
  # Reset modal search when opening new modal
  observeEvent(input$reset_modal_search, {
    modal_search_state$plot <- default_plot
    modal_search_state$search_word <- ""
    modal_search_state$search_performed <- FALSE
    modal_search_state$current_page <- 1
    modal_search_state$total_pages <- 0
    modal_search_state$total_courses <- 0
    
    # Clear the search input on the client side
    updateTextInput(session, "plot_search_word", value = "")
    
    # Reset UI elements
    session$sendCustomMessage("updateChartTitle", list(title = "Enter a keyword to search for similar courses"))
    session$sendCustomMessage("updatePagination", list(show = FALSE))
  })  # Fullscreen button functionality
  observeEvent(input$fullscreen_plot, {
    session$sendCustomMessage("openFullscreen", list())
  })
  
  # Modal search functionality - Update reactive values instead of outputs directly
  observeEvent(input$search_submit, {
    search_word <- input$plot_search_word
    
    if(is.null(search_word) || search_word == "" || trimws(search_word) == "") {
      # Show default message for empty search
      modal_search_state$plot <- girafe(ggobj = ggplot() + 
                                          annotate("text", x = 0.5, y = 0.5, 
                                                   label = "Please enter a keyword to search", 
                                                   size = 4, color = "#ffffff") +
                                          theme_void() +
                                          theme(plot.background = element_rect(fill = "#191414", color = NA)),
                                        width_svg = 10, height_svg = 6)
      modal_search_state$search_performed <- FALSE
      modal_search_state$current_page <- 1
      modal_search_state$total_pages <- 0
      modal_search_state$total_courses <- 0
      
      # Update UI elements
      session$sendCustomMessage("updateChartTitle", list(title = "Enter a keyword to search for similar courses"))
      session$sendCustomMessage("updatePagination", list(show = FALSE))
    } else {
      # Call the enhanced sim_degrees function with pagination
      tryCatch({
        result <- sim_degrees_paginated(search_word, degree_data1, page = 1, per_page = 12)
        modal_search_state$plot <- result$plot
        modal_search_state$search_performed <- TRUE
        modal_search_state$search_word <- search_word
        modal_search_state$current_page <- result$current_page
        modal_search_state$total_pages <- result$total_pages
        modal_search_state$total_courses <- result$total_courses
        
        # Update UI elements
        session$sendCustomMessage("updateChartTitle", list(
          title = paste0(tools::toTitleCase(search_word), " Courses (", result$total_courses, " found)")
        ))
        session$sendCustomMessage("updatePagination", list(
          show = result$total_pages > 1,
          current = result$current_page,
          total = result$total_pages
        ))
      }, error = function(e) {
        modal_search_state$plot <- girafe(ggobj = ggplot() + 
                                            annotate("text", x = 0.5, y = 0.5, 
                                                     label = paste("Error searching for:", search_word), 
                                                     size = 4, color = "#ffffff") +
                                            theme_void() +
                                            theme(plot.background = element_rect(fill = "#191414", color = NA)),
                                          width_svg = 10, height_svg = 6)
        modal_search_state$search_performed <- FALSE
      })
    }
  })
  
  # Pagination event handlers - Re-enabled for proper pagination support
  observeEvent(input$prev_page, {
    if(!is.null(modal_search_state$search_word) && modal_search_state$search_word != "" && modal_search_state$current_page > 1) {
      new_page <- modal_search_state$current_page - 1
      tryCatch({
        result <- sim_degrees_paginated(modal_search_state$search_word, degree_data1, page = new_page, per_page = 12)
        modal_search_state$plot <- result$plot
        modal_search_state$current_page <- result$current_page
        modal_search_state$total_pages <- result$total_pages
        modal_search_state$total_courses <- result$total_courses
        
        # Update pagination UI
        session$sendCustomMessage("updatePagination", list(
          
          
          show = result$total_pages > 1,
          current = result$current_page,
          total = result$total_pages
        ))
      }, error = function(e) {
        cat("Error in prev_page:", e$message, "\n")
      })
    }
  })
  
  observeEvent(input$next_page, {
    if(!is.null(modal_search_state$search_word) && modal_search_state$search_word != "" && modal_search_state$current_page < modal_search_state$total_pages) {
      new_page <- modal_search_state$current_page + 1
      tryCatch({
        result <- sim_degrees_paginated(modal_search_state$search_word, degree_data1, page = new_page, per_page = 12)
        modal_search_state$plot <- result$plot
        modal_search_state$current_page <- result$current_page
        modal_search_state$total_pages <- result$total_pages
        modal_search_state$total_courses <- result$total_courses
        
        # Update pagination UI
        session$sendCustomMessage("updatePagination", list(
          show = result$total_pages > 1,
          current = result$current_page,
          total = result$total_pages
        ))
      }, error = function(e) {
        cat("Error in next_page:", e$message, "\n")
      })
    }
  })
  
  # Reactive function to get current selected subjects (updates dynamically)
  current_selected_subjects <- reactive({
    get_selected_subjects(input)
  })
  
  # FIXED: Proper debounced postcode reactive
  user_postcode_debounced <- reactive({
    input$user_postcode
  }) %>% debounce(1500)  # Increased to 1.5 seconds
  
  # OPTIMIZED: Distance calculation only for unique addresses
  distances_calculated <- reactive({
    user_postcode <- user_postcode_debounced()
    
    if(is.null(user_postcode) || user_postcode == "" || trimws(user_postcode) == "") {
      # No postcode provided, return data without distance calculations
      data_with_distance <- degree_data1
      data_with_distance$distance_miles <- NA
      data_with_distance$distance_range <- NA
      data_with_distance$has_distance <- FALSE
      return(data_with_distance)
    }
    
    # Clean user postcode
    user_postcode <- toupper(trimws(user_postcode))
    
    tryCatch({
      # OPTIMIZATION: Get unique addresses only
      unique_addresses <- unique(degree_data1$provaddress[!is.na(degree_data1$provaddress) & degree_data1$provaddress != ""])
      
      # Show optimization info and progress
      cat("OPTIMIZATION: Processing", length(unique_addresses), "unique addresses instead of", nrow(degree_data1), "courses\n")
      showNotification(paste("Smart optimization: Calculating distances for only", length(unique_addresses), 
                             "unique locations instead of", nrow(degree_data1), "courses!"), 
                       type = "message", duration = 3)
      
      # Calculate distances only for unique addresses
      distance_lookup <- data.frame(
        provaddress = unique_addresses,
        stringsAsFactors = FALSE
      )
      
      # Apply degree_dist function to unique addresses only
      distance_results <- lapply(seq_along(unique_addresses), function(i) {
        address <- unique_addresses[i]
        
        # Show progress every 10 addresses
        if(i %% 10 == 0 || i == length(unique_addresses)) {
          showNotification(paste("Processing location", i, "of", length(unique_addresses)), 
                           type = "message", duration = 1)
        }
        
        result <- degree_dist(user_postcode, address)
        if(nrow(result) > 0) {
          return(list(miles = result$miles[1], distance_range = as.character(result$distance_range[1])))
        } else {
          return(list(miles = NA, distance_range = "Error"))
        }
      })
      
      # Create lookup table
      distance_lookup$distance_miles <- sapply(distance_results, function(x) x$miles)
      distance_lookup$distance_range <- sapply(distance_results, function(x) x$distance_range)
      
      # Map results back to all courses using merge (preserve all columns)
      data_with_distance <- merge(degree_data1, distance_lookup, by = "provaddress", all.x = TRUE, sort = FALSE)
      
      # Handle courses without address matches
      data_with_distance$distance_miles[is.na(data_with_distance$distance_miles)] <- NA
      data_with_distance$distance_range[is.na(data_with_distance$distance_range)] <- "Unknown"
      data_with_distance$has_distance <- TRUE
      
      # Restore original order (by grade score, highest first)
      data_with_distance <- data_with_distance[order(-data_with_distance$grade_score, na.last = TRUE), ]
      
      showNotification("Distances calculated!", type = "message", duration = 1)
      return(data_with_distance)
      
    }, error = function(e) {
      cat("Error in distance calculation:", e$message, "\n")
      data_with_distance <- degree_data1
      data_with_distance$distance_miles <- NA
      data_with_distance$distance_range <- "Error"
      data_with_distance$has_distance <- FALSE
      showNotification("Error calculating distances", type = "error", duration = 3)
      return(data_with_distance)
    })
  })
  
  # Main filtering logic (triggered by submit button) - OPTIMIZED
  filtered_courses <- eventReactive(input$submit_filters, {
    data <- distances_calculated()  # Use reactive function correctly
    
    # Get selected subjects
    student_subjects <- get_selected_subjects(input)
    
    layout <- input$qual_layout
    student_grades <- c()
    student_score <- NA
    ib_score <- NA
    
    if (is.null(layout) || layout == "a_levels") {
      student_grades <- c(input$grade1, input$grade2, input$grade3, input$grade4)
      student_grades <- student_grades[!is.null(student_grades) & student_grades != ""]
      if(length(student_grades) >= 3) {
        student_score <- calculate_student_score(student_grades)
      }
    } else if (layout == "mix") {
      for (i in 1:4) {
        g <- input[[paste0("grade", i)]]
        if (!is.null(g) && g != "") student_grades <- c(student_grades, g)
      }
      student_grades <- student_grades[!is.null(student_grades) & student_grades != ""]
      if(length(student_grades) >= 3) {
        student_score <- calculate_student_score(student_grades)
      }
    } else if (layout == "ib") {
      ib <- get_ib_grades(input)
      if(length(ib$grades) == 6 && !is.na(ib$bonus)) {
        ib_score <- calculate_ib_score(ib$grades, ib$bonus)
      }
    }
    if(layout == "ib" && !is.na(ib_score)) {
      data$ib_score <- ib_score
      data$ib_match_type <- sapply(data$ib_grade_req, function(req) {
        req_num <- suppressWarnings(as.numeric(req))
        if(is.na(req_num)) return("No Data")
        diff <- ib_score - req_num
        if(diff == 0) return("Exact Match")
        else if(diff > 0) return("Overmatch")
        else return("Under Match")
      })
      data$ib_score_diff <- sapply(data$ib_grade_req, function(req) {
        req_num <- suppressWarnings(as.numeric(req))
        if(is.na(req_num)) return(NA)
        ib_score - req_num
      })
    } else if(!is.na(student_score)) {
      data$student_score <- student_score
      data$match_type <- sapply(data$grade_score, function(course_score) {
        get_match_type(student_score, course_score)
      })
      data$score_diff <- sapply(data$grade_score, function(course_score) {
        if(is.na(course_score)) return(NA)
        student_score - course_score
      })
    }
    
    if(layout == "ib" && !is.na(ib_score)) {
      match_order <- c("Exact Match", "Overmatch", "Under Match", "No Data")
      data$match_priority <- match(data$ib_match_type, match_order)
      data <- data[order(data$match_priority, -as.numeric(data$ib_grade_req), na.last = TRUE), ]
    } else if(!is.na(student_score)) {
      match_order <- c("Exact Match", "Good Match", "Overmatch", "No Match", "No Data")
      data$match_priority <- match(data$match_type, match_order)
      data <- data[order(data$match_priority, -data$grade_score, na.last = TRUE), ]
    } else {
      data$match_type <- "No Data"
      data$match_priority <- 5
      student_score <- NA
    }
    
    # Add subject requirements matching - UPDATED COLUMN NAME
    if(length(student_subjects) > 0) {
      data$subject_requirements_met <- sapply(1:nrow(data), function(i) {
        course_requirements <- if(!is.null(data$a_level_subject_reqs[i]) && !is.na(data$a_level_subject_reqs[i])) {
          data$a_level_subject_reqs[i]
        } else {
          ""
        }
        match_subjects_with_requirements(student_subjects, course_requirements)
      })
      
      data$selected_subjects <- list(student_subjects)
    } else {
      data$subject_requirements_met <- FALSE
      data$selected_subjects <- list(character(0))
    }
    
    # Store student score for later use
    data$student_score <- student_score
    
    # Apply distance filters
    selected_distances <- c()
    if(!is.null(input$interest_distance_0_10) && input$interest_distance_0_10) selected_distances <- c(selected_distances, "0‑10 miles")
    if(!is.null(input$interest_distance_10_25) && input$interest_distance_10_25) selected_distances <- c(selected_distances, "10‑25 miles")
    if(!is.null(input$interest_distance_25_50) && input$interest_distance_25_50) selected_distances <- c(selected_distances, "25‑50 miles")
    if(!is.null(input$interest_distance_50_100) && input$interest_distance_50_100) selected_distances <- c(selected_distances, "50‑100 miles")
    if(!is.null(input$interest_distance_100_150) && input$interest_distance_100_150) selected_distances <- c(selected_distances, "100‑150 miles")
    if(!is.null(input$interest_distance_150_plus) && input$interest_distance_150_plus) selected_distances <- c(selected_distances, "150+ miles")
    if(!is.null(input$interest_distance_any) && input$interest_distance_any) selected_distances <- c(selected_distances, "Any Distance")
    
    # Apply distance filtering (only if user has entered a postcode and selected distance filters)
    if(length(selected_distances) > 0 && !"Any Distance" %in% selected_distances && 
       !is.null(input$user_postcode) && input$user_postcode != "" && any(!is.na(data$distance_range))) {
      data <- data[data$distance_range %in% selected_distances | is.na(data$distance_range), ]
    }
    
    # Filter by degree types
    selected_degrees <- c()
    if(!is.null(input$interest_bsc) && input$interest_bsc) selected_degrees <- c(selected_degrees, "BSc")
    if(!is.null(input$interest_ba) && input$interest_ba) selected_degrees <- c(selected_degrees, "BA")
    if(!is.null(input$interest_beng) && input$interest_beng) selected_degrees <- c(selected_degrees, "BEng")
    if(!is.null(input$interest_meng) && input$interest_meng) selected_degrees <- c(selected_degrees, "MEng")
    if(!is.null(input$interest_msci) && input$interest_msci) selected_degrees <- c(selected_degrees, "MSci")
    if(!is.null(input$interest_llb) && input$interest_llb) selected_degrees <- c(selected_degrees, "LLB")
    if(!is.null(input$interest_mbbs) && input$interest_mbbs) selected_degrees <- c(selected_degrees, "MBBS")
    if(!is.null(input$interest_mbbsbsc) && input$interest_mbbsbsc) selected_degrees <- c(selected_degrees, "MBBS BSc")
    if(!is.null(input$interest_mpharm) && input$interest_mpharm) selected_degrees <- c(selected_degrees, "MPharm")
    if(!is.null(input$interest_basc) && input$interest_basc) selected_degrees <- c(selected_degrees, "BASc")
    if(!is.null(input$interest_bfa) && input$interest_bfa) selected_degrees <- c(selected_degrees, "BFA")
    if(!is.null(input$interest_mbio) && input$interest_mbio) selected_degrees <- c(selected_degrees, "MBio")
    if(!is.null(input$interest_mchem) && input$interest_mchem) selected_degrees <- c(selected_degrees, "MChem")
    if(!is.null(input$interest_mphys) && input$interest_mphys) selected_degrees <- c(selected_degrees, "MPhys")
    if(!is.null(input$interest_mmathphys) && input$interest_mmathphys) selected_degrees <- c(selected_degrees, "MMathPhys")
    if(!is.null(input$interest_mmath) && input$interest_mmath) selected_degrees <- c(selected_degrees, "MMath")
    if(!is.null(input$interest_mmathstat) && input$interest_mmathstat) selected_degrees <- c(selected_degrees, "MMathStat")
    if(!is.null(input$interest_mmorse) && input$interest_mmorse) selected_degrees <- c(selected_degrees, "MMORSE")
    if(!is.null(input$interest_babsc) && input$interest_babsc) selected_degrees <- c(selected_degrees, "BA/BSc")
    
    # Filter by hierarchical taxonomy interests
    selected_taxonomy_ids <- input$selected_taxonomy_interests
    
    # Apply taxonomy filter (only if subcategories are selected)
    if(!is.null(selected_taxonomy_ids) && length(selected_taxonomy_ids) > 0 && !is.null(course_tags_cache)) {
      cat("DEBUG SIMPLE: Selected taxonomy IDs:", paste(selected_taxonomy_ids, collapse = ", "), "\n")
      
      # SIMPLE APPROACH: Get all stems from selected subcategories
      all_selected_stems <- c()
      
      for(subcat_id in selected_taxonomy_ids) {
        # Find the corresponding taxonomy terms for this subcategory
        for(main_cat in names(taxonomy_structure)) {
          for(sub_cat in names(taxonomy_structure[[main_cat]]$children)) {
            # Create the same safe ID that was used in UI generation
            safe_sub_id <- gsub("[^a-zA-Z0-9]", "_", tolower(paste(main_cat, sub_cat, sep = "_")))
            
            if(safe_sub_id == subcat_id) {
              # Get all stems for this subcategory
              stems <- taxonomy_structure[[main_cat]]$children[[sub_cat]]$stems
              all_selected_stems <- c(all_selected_stems, stems)
              cat("DEBUG SIMPLE: Found", length(stems), "stems for", main_cat, "::", sub_cat, "\n")
              break
            }
          }
        }
      }
      
      all_selected_stems <- unique(all_selected_stems)
      cat("DEBUG SIMPLE: Total unique stems to search for:", length(all_selected_stems), "\n")
      cat("DEBUG SIMPLE: First 5 stems:", paste(head(all_selected_stems, 5), collapse = ", "), "\n")
      
      # SIMPLE FILTER: Keep courses whose titles contain ANY of the selected stems
      if(length(all_selected_stems) > 0) {
        matching_courses <- c()
        
        for(stem in all_selected_stems) {
          # Find courses whose titles contain this stem (case insensitive)
          stem_lower <- tolower(stem)
          matches <- which(grepl(stem_lower, tolower(data$title), fixed = TRUE))
          matching_courses <- c(matching_courses, matches)
          
          if(length(matches) > 0) {
            cat("DEBUG SIMPLE: Stem '", stem, "' matched", length(matches), "courses\n")
          }
        }
        
        matching_courses <- unique(matching_courses)
        cat("DEBUG SIMPLE: Total matching courses:", length(matching_courses), "\n")
        
        if(length(matching_courses) > 0) {
          data <- data[matching_courses, ]
          cat("DEBUG SIMPLE: After taxonomy filter:", nrow(data), "courses remain\n")
        } else {
          # No courses found
          data <- data[FALSE, ]
          cat("DEBUG SIMPLE: No matching courses found, returning empty dataset\n")
        }
      }
    }
    
    # Apply degree filter (only if degrees are selected)
    if(length(selected_degrees) > 0) {
      data <- data[data$degree_type %in% selected_degrees, ]
    }
    
    # Apply university filter (only if universities are selected)
    if(!is.null(input$selected_universities) && length(input$selected_universities) > 0) {
      data <- data[data$university_name %in% input$selected_universities, ]
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # Store the currently displayed courses for modal access
  displayed_courses <- reactive({
    cat("DEBUG DISPLAYED_COURSES: submit_filters =", input$submit_filters, "\n")
    
    if(input$submit_filters == 0) {
      filtered_data <- distances_calculated()
      filtered_data$match_type <- "No Data"
      filtered_data$subject_requirements_met <- FALSE
      filtered_data$selected_subjects <- list(character(0))
      if(is.null(filtered_data$has_distance)) {
        filtered_data$has_distance <- FALSE
      }
      cat("DEBUG DISPLAYED_COURSES: Using default data with", nrow(filtered_data), "courses\n")
    } else {
      filtered_data <- filtered_courses()
      cat("DEBUG DISPLAYED_COURSES: Using filtered data with", nrow(filtered_data), "courses\n")
    }
    
    current_tab <- if(is.null(input$current_tab)) "exact" else input$current_tab
    layout <- input$qual_layout
    if (is.null(layout)) layout <- "a_levels"
    
    cat("DEBUG DISPLAYED_COURSES: current_tab =", current_tab, ", layout =", layout, "\n")
    cat("DEBUG DISPLAYED_COURSES: Match types in data:", paste(unique(filtered_data$match_type), collapse = ", "), "\n")
    
    if (layout == "ib") {
      if(current_tab == "exact" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type == "Exact Match", ]
      } else if(current_tab == "over" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type == "Overmatch", ]
      } else if(current_tab == "under" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type == "Under Match", ]
      } else if(current_tab == "all" && "ib_match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$ib_match_type %in% c("Exact Match", "Overmatch", "Under Match"), ]
      }
    } else {
      if(current_tab == "exact" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type == "Exact Match", ]
      } else if(current_tab == "over" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type %in% c("Good Match", "Overmatch"), ]
      } else if(current_tab == "under" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type == "Under Match", ]
      } else if(current_tab == "all" && "match_type" %in% names(filtered_data)) {
        filtered_data <- filtered_data[filtered_data$match_type %in% c("Exact Match", "Good Match", "Overmatch", "No Data"), ]
      }
    }
    
    cat("DEBUG DISPLAYED_COURSES: After tab filtering, courses remaining:", nrow(filtered_data), "\n")
    
    # Apply TEF filters
    if(!is.null(input$tef_overall) && input$tef_overall != "any") {
      filtered_data <- filtered_data[!is.na(filtered_data$overall_rating) & 
                                       filtered_data$overall_rating == input$tef_overall & 
                                       filtered_data$overall_rating != "This University does not provide TEF Data.", ]
      cat("DEBUG TEF: After overall TEF filter (", input$tef_overall, "), courses remaining:", nrow(filtered_data), "\n")
    }
    
    if(!is.null(input$tef_experience) && input$tef_experience != "any") {
      filtered_data <- filtered_data[!is.na(filtered_data$student_experience_rating) & 
                                       filtered_data$student_experience_rating == input$tef_experience & 
                                       filtered_data$student_experience_rating != "This University does not provide TEF Data.", ]
      cat("DEBUG TEF: After experience TEF filter (", input$tef_experience, "), courses remaining:", nrow(filtered_data), "\n")
    }
    
    if(!is.null(input$tef_outcomes) && input$tef_outcomes != "any") {
      filtered_data <- filtered_data[!is.na(filtered_data$student_outcomes_rating) & 
                                       filtered_data$student_outcomes_rating == input$tef_outcomes & 
                                       filtered_data$student_outcomes_rating != "This University does not provide TEF Data.", ]
      cat("DEBUG TEF: After outcomes TEF filter (", input$tef_outcomes, "), courses remaining:", nrow(filtered_data), "\n")
    }
    
    # ...existing sorting and limiting code...
    if(!is.null(input$sort_by)) {
      if(input$sort_by == "alpha") {
        filtered_data <- filtered_data[order(filtered_data$title), ]
      } else if(input$sort_by == "grade") {
        filtered_data <- filtered_data[order(-filtered_data$grade_score, na.last = TRUE), ]
      } else if(input$sort_by == "distance") {
        filtered_data <- filtered_data[order(filtered_data$distance_miles, na.last = TRUE), ]
      } else if(input$sort_by == "match") {
        if(any(!is.na(filtered_data$match_priority))) {
          filtered_data <- filtered_data[order(filtered_data$match_priority, -filtered_data$grade_score, na.last = TRUE), ]
        }
      }
    }
    
    num_to_show <- if(input$num_courses == "all") {
      nrow(filtered_data)
    } else {
      min(as.numeric(input$num_courses), nrow(filtered_data))
    }
    
    if(nrow(filtered_data) == 0) {
      return(data.frame())
    }
    
    return(head(filtered_data, num_to_show))
  })
  
  output$course_cards <- renderUI({
    layout <- input$qual_layout
    if (is.null(layout)) layout <- "a_levels"
    courses_to_show <- displayed_courses()
    
    cat("DEBUG COURSE CARDS: submit_filters =", input$submit_filters, ", courses to show =", nrow(courses_to_show), "\n")
    if(nrow(courses_to_show) > 0) {
      cat("DEBUG COURSE CARDS: First 3 course titles:", paste(head(courses_to_show$title, 3), collapse = " | "), "\n")
    }
    
    if(nrow(courses_to_show) == 0) {
      return(div(class = "no-results-message",
                 h3("No courses match your criteria"),
                 p("Try adjusting your filters or selections")))
    }
    
    course_cards <- lapply(1:nrow(courses_to_show), function(i) {
      course <- courses_to_show[i, ]
      # --- MATCH BADGE LOGIC ---
      if (layout == "ib") {
        match_label <- if(!is.null(course$ib_match_type)) course$ib_match_type else ""
        badge_class <- switch(match_label,
                              "Exact Match" = "match-exact",
                              "Overmatch" = "match-over",
                              "Under Match" = "match-no",
                              "")
        diff <- if(!is.null(course$ib_score_diff)) course$ib_score_diff else NA
        match_score <- ""
        if(!is.na(diff) && match_label != "") {
          if(diff > 0) match_score <- paste0(" (", diff, " over)")
          else if(diff < 0) match_score <- paste0(" (", abs(diff), " under)")
          else match_score <- " (exact)"
        }
        match_badge <- if(match_label != "") div(class = paste("match-badge", badge_class), paste0(match_label, match_score)) else NULL
      } else {
        match_label <- if(!is.null(course$match_type)) course$match_type else ""
        badge_class <- switch(match_label,
                              "Exact Match" = "match-exact",
                              "Good Match" = "match-good", 
                              "Overmatch" = "match-over",
                              "Under Match" = "match-no",
                              "No Match" = "match-no",
                              "")
        diff <- if(!is.null(course$score_diff)) course$score_diff else NA
        match_score <- ""
        if(!is.na(diff) && match_label != "") {
          if(diff > 0) match_score <- paste0(" (", round(diff/8, 1), " grades over)")
          else if(diff < 0) match_score <- paste0(" (", abs(round(diff/8, 1)), " grades under)")
          else match_score <- " (exact)"
        }
        match_badge <- if(match_label != "") div(class = paste("match-badge", badge_class), paste0(match_label, match_score)) else NULL
      }
      # --- END MATCH BADGE LOGIC ---
      
      university_name <- if(!is.null(course$university_name) && !is.na(course$university_name) && course$university_name != "") {
        course$university_name
      } else {
        "University"
      }
      distance_action_badge <- if(!is.null(course$has_distance) && course$has_distance == TRUE && 
                                  !is.null(course$distance_miles) && !is.na(course$distance_miles)) {
        span(class = "distance-badge", paste(round(course$distance_miles, 1), "miles"))
      } else {
        NULL
      }
      if (layout == "ib") {
        grade_req_text <- if(!is.null(course$ib_grade_req) && !is.na(course$ib_grade_req) && course$ib_grade_req != "") {
          course$ib_grade_req
        } else {
          "N/A"
        }
      } else {
        grade_req_text <- if(!is.null(course$a_level_grade_req) && !is.na(course$a_level_grade_req) && course$a_level_grade_req != "") {
          course$a_level_grade_req
        } else {
          "N/A"
        }
      }
      course_details <- paste(university_name, "•", course$degree_type, "• Grade Req:", grade_req_text)
      div(class = "course-card",
          match_badge,
          div(class = "course-title", 
              onclick = paste0("openModal(", i, ")"),
              course$title),
          div(class = "course-details", 
              course_details),
          div(class = "course-actions",
              shiny::tags$button("Course URL", class = "course-btn", 
                                 onclick = paste0("window.open('", course$url, "', '_blank')")),
              shiny::tags$button("Learn More", class = "course-btn primary",
                                 onclick = paste0("openModal(", i, ")")),
              distance_action_badge
          )
      )
    })
    course_cards
  })
  
  
  
  observeEvent(input$selected_course_index, {
    
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      # Use the same dataset that was used to display the course cards
      current_data <- displayed_courses()
      
      # Make sure the selected index is within range
      if(input$selected_course_index <= nrow(current_data)) {
        # Get course from the displayed dataset
        course <- current_data[input$selected_course_index, ]
        
        # Format salary with proper handling of NA values
        salary_text <- if(!is.na(course$median_salary) && course$median_salary > 0) {
          paste("£", format(course$median_salary, big.mark = ",", scientific = FALSE))
        } else {
          "N/A"
        }
        
        # Format offer rate - UPDATED COLUMN NAME
        offer_rate_text <- if(!is.null(course$offer_rate) && !is.na(course$offer_rate) && course$offer_rate != "") {
          if(is.numeric(course$offer_rate)) {
            paste0(round(course$offer_rate, 1), "%")
          } else {
            course$offer_rate
          }
        } else {
          "Data Not Available"
        }
        
        # Get university name - UPDATED COLUMN NAME
        university_name <- if(!is.null(course$university_name) && !is.na(course$university_name) && course$university_name != "") {
          course$university_name
        } else {
          "University"
        }
        
        # Distance text for modal - only if user has entered postcode and distance is available
        distance_text <- if(!is.null(course$has_distance) && course$has_distance == TRUE && 
                            !is.null(course$distance_miles) && !is.na(course$distance_miles)) {
          paste("Distance:", round(course$distance_miles, 1), "miles")
        } else {
          NULL
        }
        
        # Get match type and subject requirements
        match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        subject_requirements_met <- if(!is.null(course$subject_requirements_met)) course$subject_requirements_met else FALSE
        
        # Get selected subjects for highlighting
        selected_subjects <- current_selected_subjects()
        
        # Check if user has selected any subjects
        has_subject_selection <- length(selected_subjects) > 0
        
        # Build requirements text from real data - UPDATED COLUMN NAMES with DEBUG
        # Build requirements text and grade req based on selected tab
        layout <- input$qual_layout
        if (is.null(layout)) layout <- "a_levels"
        
        if (layout == "ib") {
          # IB requirements
          grade_req_text <- if(!is.null(course$ib_grade_req) && !is.na(course$ib_grade_req) && course$ib_grade_req != "") {
            course$ib_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$ib_subject_req) && !is.na(course$ib_subject_req) && course$ib_subject_req != "") {
            course$ib_subject_req
          } else {
            "Requirements information not available."
          }
        } else {
          # A Level or Mix requirements
          grade_req_text <- if(!is.null(course$a_level_grade_req) && !is.na(course$a_level_grade_req) && course$a_level_grade_req != "") {
            course$a_level_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$a_level_subject_reqs) && !is.na(course$a_level_subject_reqs) && course$a_level_subject_reqs != "") {
            course$a_level_subject_reqs
          } else {
            "Requirements information not available."
          }
        }
        # Add placement year info if available - UPDATED COLUMN NAME
        placement_text <- ""
        if(!is.null(course$placement_year) && !is.na(course$placement_year)) {
          if(course$placement_year == "Yes" || course$placement_year == TRUE) {
            placement_text <- "Placement year available."
          }
        }
        
        # Add year abroad info if available - UPDATED COLUMN NAME
        abroad_text <- ""
        if(!is.null(course$year_abroad) && !is.na(course$year_abroad)) {
          if(course$year_abroad == "Yes" || course$year_abroad == TRUE) {
            abroad_text <- "Year abroad available."
          }
        }
        
        # Add foundation year info if available
        foundation_text <- ""
        if(!is.null(course$foundation_year_available) && !is.na(course$foundation_year_available)) {
          if(course$foundation_year_available == "Yes" || course$foundation_year_available == TRUE) {
            foundation_text <- "Foundation year available."
          }
        }
        
        # Combine all requirements text
        additional_options <- c(placement_text, abroad_text, foundation_text)
        additional_options <- additional_options[additional_options != ""]
        
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        
        # Fallback if no requirements data
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        
        cat("DEBUG MODAL: Final requirements text:", requirements_text, "\n")
        cat("DEBUG MODAL: Current selected subjects:", paste(selected_subjects, collapse = ", "), "\n")
        
        # Find matched subjects using YOUR working logic with DEBUG
        matched_subjects_list <- if(has_subject_selection && requirements_text != "" && requirements_text != "Requirements information not available.") {
          result <- find_matched_subjects(selected_subjects, requirements_text)
          cat("DEBUG MODAL: find_matched_subjects returned:", paste(result, collapse = ", "), "\n")
          result
        } else {
          cat("DEBUG MODAL: Skipping subject matching - no subjects or requirements\n")
          character(0)
        }
        
        layout <- input$qual_layout
        if (is.null(layout)) layout <- "a_levels"
        if (layout == "ib") {
          match_type <- if(!is.null(course$ib_match_type)) course$ib_match_type else "No Data"
        } else {
          match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        }
        
        # Update modal content with ALL new dataset fields
        session$sendCustomMessage("updateModal", list(
          grade_type = if (input$qual_layout == "ib") "ib" else "alevel",
          degree_type = course$degree_type,
          title = course$title,
          university = university_name,
          grade_req = paste("Grade Req:", grade_req_text),
          requirements = requirements_text,
          
          # Salary information - individual LQ, Median, UQ values
          salary = salary_text,
          salary_lq = if(!is.na(course$salary_lq)) {
            paste("£", format(course$salary_lq, big.mark = ",", scientific = FALSE), sep = "")
          } else { "N/A" },
          salary_uq = if(!is.na(course$salary_uq)) {
            paste("£", format(course$salary_uq, big.mark = ",", scientific = FALSE), sep = "")
          } else { "N/A" },
          salary_respondents = if(!is.na(course$salary_respondents)) {
            paste(course$salary_respondents, "respondents")
          } else { "N/A" },
          
          # TEF Ratings - pass the actual values for better handling
          overall_rating = course$overall_rating,
          student_experience_rating = course$student_experience_rating,
          student_outcomes_rating = course$student_outcomes_rating,
          
          # Course Options
          option_sandwich_year = course$option_sandwich_year,
          option_year_abroad = course$option_year_abroad,
          option_foundation_year = course$option_foundation_year,
          option_distance_learning = course$option_distance_learning,
          
          # QS Rankings data
          qs_rank = if(!is.na(course$qs_rank)) course$qs_rank else "N/A",
          qs_overall_score = if(!is.na(course$qs_overall_score)) course$qs_overall_score else "N/A",
          qs_academic_rep = if(!is.na(course$qs_academic_rep)) course$qs_academic_rep else "N/A",
          qs_employer_rep = if(!is.na(course$qs_employer_rep)) course$qs_employer_rep else "N/A",
          qs_staff_ratio = if(!is.na(course$qs_staff_ratio)) course$qs_staff_ratio else "N/A",
          qs_employment_outcomes = if(!is.na(course$qs_employment_outcomes)) course$qs_employment_outcomes else "N/A",
          
          # Legacy fields
          url = course$url,
          match_type = match_type,
          subject_requirements_met = subject_requirements_met,
          has_subject_selection = has_subject_selection,
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL,
          distance_text = distance_text
        ))
        
      }
    }
  })
  
  observeEvent(input$qual_layout, {
    # If modal is open, re-trigger the modal update for the current course
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      current_data <- displayed_courses()
      if(input$selected_course_index <= nrow(current_data)) {
        course <- current_data[input$selected_course_index, ]
        # Copy the modal update logic from observeEvent(input$selected_course_index, ...) here
        # (You can refactor this into a helper function to avoid duplication)
        # --- BEGIN MODAL UPDATE LOGIC ---
        salary_text <- if(!is.na(course$median_salary) && course$median_salary > 0) {
          paste("£", format(course$median_salary, big.mark = ",", scientific = FALSE))
        } else {
          "N/A"
        }
        offer_rate_text <- if(!is.null(course$offer_rate) && !is.na(course$offer_rate) && course$offer_rate != "") {
          if(is.numeric(course$offer_rate)) paste0(round(course$offer_rate, 1), "%") else course$offer_rate
        } else {
          "Data Not Available"
        }
        university_name <- if(!is.null(course$university_name) && !is.na(course$university_name) && course$university_name != "") {
          course$university_name
        } else {
          "University"
        }
        distance_text <- if(!is.null(course$has_distance) && course$has_distance == TRUE && 
                            !is.null(course$distance_miles) && !is.na(course$distance_miles)) {
          paste("Distance:", round(course$distance_miles, 1), "miles")
        } else {
          NULL
        }
        match_type <- if(!is.null(course$match_type)) course$match_type else "No Data"
        subject_requirements_met <- if(!is.null(course$subject_requirements_met)) course$subject_requirements_met else FALSE
        selected_subjects <- current_selected_subjects()
        has_subject_selection <- length(selected_subjects) > 0
        layout <- input$qual_layout
        if (is.null(layout)) layout <- "a_levels"
        if (layout == "ib") {
          grade_req_text <- if(!is.null(course$ib_grade_req) && !is.na(course$ib_grade_req) && course$ib_grade_req != "") {
            course$ib_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$ib_subject_req) && !is.na(course$ib_subject_req) && course$ib_subject_req != "") {
            course$ib_subject_req
          } else {
            "Requirements information not available."
          }
        } else {
          grade_req_text <- if(!is.null(course$a_level_grade_req) && !is.na(course$a_level_grade_req) && course$a_level_grade_req != "") {
            course$a_level_grade_req
          } else {
            "N/A"
          }
          requirements_text <- if(!is.null(course$a_level_subject_reqs) && !is.na(course$a_level_subject_reqs) && course$a_level_subject_reqs != "") {
            course$a_level_subject_reqs
          } else {
            "Requirements information not available."
          }
        }
        placement_text <- ""
        if(!is.null(course$placement_year) && !is.na(course$placement_year)) {
          if(course$placement_year == "Yes" || course$placement_year == TRUE) {
            placement_text <- "Placement year available."
          }
        }
        abroad_text <- ""
        if(!is.null(course$year_abroad) && !is.na(course$year_abroad)) {
          if(course$year_abroad == "Yes" || course$year_abroad == TRUE) {
            abroad_text <- "Year abroad available."
          }
        }
        foundation_text <- ""
        if(!is.null(course$foundation_year_available) && !is.na(course$foundation_year_available)) {
          if(course$foundation_year_available == "Yes" || course$foundation_year_available == TRUE) {
            foundation_text <- "Foundation year available."
          }
        }
        additional_options <- c(placement_text, abroad_text, foundation_text)
        additional_options <- additional_options[additional_options != ""]
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        matched_subjects_list <- if(has_subject_selection && requirements_text != "" && requirements_text != "Requirements information not available.") {
          result <- find_matched_subjects(selected_subjects, requirements_text)
          result
        } else {
          character(0)
        }
        session$sendCustomMessage("updateModal", list(
          grade_type = if (input$qual_layout == "ib") "ib" else "alevel",   
          degree_type = course$degree_type,
          title = course$title,
          university = university_name,
          grade_req = paste("Grade Req:", grade_req_text),
          requirements = requirements_text,
          
          # Salary information - individual LQ, Median, UQ values
          salary = salary_text,
          salary_lq = if(!is.na(course$salary_lq)) {
            paste("£", format(course$salary_lq, big.mark = ",", scientific = FALSE), sep = "")
          } else { "N/A" },
          salary_uq = if(!is.na(course$salary_uq)) {
            paste("£", format(course$salary_uq, big.mark = ",", scientific = FALSE), sep = "")
          } else { "N/A" },
          salary_respondents = if(!is.na(course$salary_respondents)) {
            paste(course$salary_respondents, "respondents")
          } else { "N/A" },
          
          # TEF Ratings - pass the actual values for better handling
          overall_rating = course$overall_rating,
          student_experience_rating = course$student_experience_rating,
          student_outcomes_rating = course$student_outcomes_rating,
          
          # Course Options
          option_sandwich_year = course$option_sandwich_year,
          option_year_abroad = course$option_year_abroad,
          option_foundation_year = course$option_foundation_year,
          option_distance_learning = course$option_distance_learning,
          
          # QS Rankings data
          qs_rank = if(!is.na(course$qs_rank)) course$qs_rank else "N/A",
          qs_overall_score = if(!is.na(course$qs_overall_score)) course$qs_overall_score else "N/A",
          qs_academic_rep = if(!is.na(course$qs_academic_rep)) course$qs_academic_rep else "N/A",
          qs_employer_rep = if(!is.na(course$qs_employer_rep)) course$qs_employer_rep else "N/A",
          qs_staff_ratio = if(!is.na(course$qs_staff_ratio)) course$qs_staff_ratio else "N/A",
          qs_employment_outcomes = if(!is.na(course$qs_employment_outcomes)) course$qs_employment_outcomes else "N/A",
          
          # Legacy fields
          url = course$url,
          match_type = match_type,
          subject_requirements_met = subject_requirements_met,
          has_subject_selection = has_subject_selection,
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL,
          distance_text = distance_text
        ))
        # --- END MODAL UPDATE LOGIC ---
      }
    }
  }, ignoreInit = TRUE)
  
  # Observe sort/filter changes to preserve scroll position - OPTIMIZED + TEF filters
  observeEvent(c(input$sort_by, input$num_courses, input$current_tab, input$tef_overall, input$tef_experience, input$tef_outcomes), {
    # Only trigger when there are actual courses to display
    if(input$submit_filters > 0 || nrow(degree_data1) > 0) {
      session$sendCustomMessage("preserveScroll", list())
    }
  }, ignoreInit = TRUE)
  observeEvent(current_selected_subjects(), {
    cat("Subjects changed to:", paste(current_selected_subjects(), collapse = ", "), "\n")
    
    # Check if a modal is currently open and re-trigger the modal update
    if(!is.null(input$selected_course_index) && input$selected_course_index > 0) {
      cat("Modal is open, updating subject comparison\n")
      
      # Get current modal course data
      current_data <- displayed_courses()
      if(input$selected_course_index <= nrow(current_data)) {
        # Re-trigger the modal update with new subject matching
        course <- current_data[input$selected_course_index, ]
        
        # Get current selected subjects
        selected_subjects <- current_selected_subjects()
        has_subject_selection <- length(selected_subjects) > 0
        
        cat("Current subjects:", paste(selected_subjects, collapse = ", "), "\n")
        
        # Build requirements text from real data - UPDATED COLUMN NAMES with DEBUG
        requirements_text <- ""
        
        # Add A-level subjects (main requirement)
        if(!is.null(course$a_level_subject_reqs) && !is.na(course$a_level_subject_reqs) && course$a_level_subject_reqs != "") {
          requirements_text <- course$a_level_subject_reqs
          cat("DEBUG UPDATE: Found a_level_subject_reqs:", course$a_level_subject_reqs, "\n")
        }
        
        # Add placement year info if available
        placement_text <- ""
        if(!is.null(course$placement_year) && !is.na(course$placement_year)) {
          if(course$placement_year == "Yes" || course$placement_year == TRUE) {
            placement_text <- "Placement year available."
          }
        }
        
        # Add year abroad info if available  
        abroad_text <- ""
        if(!is.null(course$year_abroad) && !is.na(course$year_abroad)) {
          if(course$year_abroad == "Yes" || course$year_abroad == TRUE) {
            abroad_text <- "Year abroad available."
          }
        }
        
        # Add foundation year info if available
        foundation_text <- ""
        if(!is.null(course$foundation_year_available) && !is.na(course$foundation_year_available)) {
          if(course$foundation_year_available == "Yes" || course$foundation_year_available == TRUE) {
            foundation_text <- "Foundation year available."
          }
        }
        
        # Combine all requirements text
        additional_options <- c(placement_text, abroad_text, foundation_text)
        additional_options <- additional_options[additional_options != ""]
        
        if(length(additional_options) > 0) {
          if(requirements_text != "") {
            requirements_text <- paste(requirements_text, paste(additional_options, collapse = " "), sep = " ")
          } else {
            requirements_text <- paste(additional_options, collapse = " ")
          }
        }
        
        # Fallback if no requirements data
        if(requirements_text == "") {
          requirements_text <- "Requirements information not available."
        }
        
        cat("DEBUG UPDATE: Requirements text:", requirements_text, "\n")
        cat("DEBUG UPDATE: Current subjects:", paste(selected_subjects, collapse = ", "), "\n")
        
        # Find matched subjects using YOUR working logic with DEBUG
        matched_subjects_list <- if(has_subject_selection && requirements_text != "" && requirements_text != "Requirements information not available.") {
          result <- find_matched_subjects(selected_subjects, requirements_text)
          cat("DEBUG UPDATE: find_matched_subjects returned:", paste(result, collapse = ", "), "\n")
          result
        } else {
          cat("DEBUG UPDATE: Skipping subject matching - no subjects or requirements\n")
          character(0)
        }
        
        cat("Matched subjects:", paste(matched_subjects_list, collapse = ", "), "\n")
        
        # Update only the subject comparison part of the modal
        session$sendCustomMessage("updateSubjectComparison", list(
          has_subject_selection = has_subject_selection,
          selected_subjects = selected_subjects,
          matched_subjects = if(length(matched_subjects_list) > 0) matched_subjects_list else NULL
        ))
      }
    }
  }, ignoreInit = TRUE)
  
  # Generate taxonomy interest cards dynamically with hierarchical layout
  output$taxonomy_interests_ui <- renderUI({
    if(length(taxonomy_structure) == 0) {
      return(div("Taxonomy structure not available"))
    }
    
    main_categories <- names(taxonomy_structure)
    
    # Create rows with 4 main categories each
    category_rows <- split(main_categories, ceiling(seq_along(main_categories)/4))
    
    row_elements <- lapply(category_rows, function(row_cats) {
      div(class = "taxonomy-row",
          lapply(row_cats, function(main_cat) {
            # Create safe ID for the main category
            safe_main_id <- gsub("[^a-zA-Z0-9]", "_", tolower(main_cat))
            
            # Get subcategories
            subcategories <- names(taxonomy_structure[[main_cat]]$children)
            
            div(class = "taxonomy-main-card",
                # Main category header
                div(class = "taxonomy-main-header", 
                    id = paste0("main_", safe_main_id),
                    onclick = paste0("toggleMainCategory('", safe_main_id, "', event)"),
                    main_cat),
                
                # Subcategories
                div(class = "taxonomy-subcategories",
                    lapply(subcategories, function(sub_cat) {
                      safe_sub_id <- gsub("[^a-zA-Z0-9]", "_", tolower(paste(main_cat, sub_cat, sep = "_")))
                      
                      div(class = "taxonomy-sub-card",
                          id = paste0("sub_", safe_sub_id),
                          onclick = paste0("toggleSubCategory('", safe_sub_id, "', '", safe_main_id, "', event)"),
                          sub_cat,
                          " (", length(taxonomy_structure[[main_cat]]$children[[sub_cat]]$stems), ")")
                    })
                )
            )
          })
      )
    })
    
    return(div(class = "interests-grid-container", row_elements))
  })
  
  # Insert the taxonomy UI into the container
  observe({
    insertUI(
      selector = "#taxonomy-interests-container",
      where = "afterBegin",
      ui = uiOutput("taxonomy_interests_ui")
    )
  })
  
  # Handle taxonomy interest selections
  selected_taxonomy_categories <- reactiveVal(c())
  
  # Add JavaScript for hierarchical taxonomy interest handling
  observe({
    runjs('
      // Initialize selected interests array
      if (typeof window.selected_taxonomy_interests === "undefined") {
        window.selected_taxonomy_interests = [];
      }
      
      // Function to toggle main category
      window.toggleMainCategory = function(categoryId, event) {
        event.stopPropagation();
        
        var mainCard = document.getElementById("main_" + categoryId);
        var subcatContainer = mainCard.nextElementSibling;
        
        if (subcatContainer && subcatContainer.classList.contains("taxonomy-subcategories")) {
          if (subcatContainer.style.display === "none" || subcatContainer.style.display === "") {
            subcatContainer.style.display = "block";
            mainCard.classList.add("expanded");
          } else {
            subcatContainer.style.display = "none";
            mainCard.classList.remove("expanded");
            
            // Deselect all subcategories when collapsing
            var subCards = subcatContainer.querySelectorAll(".taxonomy-sub-card");
            subCards.forEach(function(card) {
              card.classList.remove("selected");
              var subId = card.id.replace("sub_", "");
              removeFromSelected(subId);
            });
            updateTaxonomyFilter();
          }
        }
      };
      
      // Function to toggle sub-category
      window.toggleSubCategory = function(subCategoryId, mainCategoryId, event) {
        event.stopPropagation();
        
        var element = document.getElementById("sub_" + subCategoryId);
        if (element.classList.contains("selected")) {
          element.classList.remove("selected");
          removeFromSelected(subCategoryId);
        } else {
          element.classList.add("selected");
          addToSelected(subCategoryId);
        }
        
        updateTaxonomyFilter();
      };
      
      // Helper functions
      function addToSelected(categoryId) {
        if (window.selected_taxonomy_interests.indexOf(categoryId) === -1) {
          window.selected_taxonomy_interests.push(categoryId);
        }
      }
      
      function removeFromSelected(categoryId) {
        var index = window.selected_taxonomy_interests.indexOf(categoryId);
        if (index > -1) {
          window.selected_taxonomy_interests.splice(index, 1);
        }
      }
      
      function updateTaxonomyFilter() {
        console.log("Selected taxonomy interests:", window.selected_taxonomy_interests);
        
        if (typeof Shiny !== "undefined") {
          Shiny.setInputValue("selected_taxonomy_interests", window.selected_taxonomy_interests, {priority: "event"});
        }
      }
    ')
  })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(height = 1080))
