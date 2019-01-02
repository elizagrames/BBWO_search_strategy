##### Functions #####
detect_database <- function(df){
  database <- ""
  database_signature <- paste(df[1,], collapse=" ")
  if(stringr::str_detect(database_signature, "ZOOREC:ZOOR")){database <- "ZooRec"}
  if(stringr::str_detect(database_signature, "BCI:BCI")){database <- "BIOSIS"}
  if(stringr::str_detect(database_signature, "www.scopus.com")){database <- "Scopus"}
  if(stringr::str_detect(database_signature, "search.proquest.com")){database <- "ProQuest"}
  if(stringr::str_detect(database_signature, "ebscohost.com")){database <- "EBSCO"}
  
  if(stringr::str_detect(database_signature, "ndltd_scrape")){database <- "NDLTD"}
  if(stringr::str_detect(database_signature, "oatd_scrape")){database <- "OATD"}
  if(stringr::str_detect(database_signature, "openthesis_scrape")){database <- "OpenThesis"}
  
  if(stringr::str_detect(database_signature, "googlescholar_scrape")){database <- "googlescholar_scrape"}
  if(stringr::str_detect(database_signature, "jstor_scrape")){database <- "jstor_scrape"}
  if(stringr::str_detect(database_signature, "scopus_scrape")){database <- "scopus_scrape"}
  if(stringr::str_detect(database_signature, "cabdirect_scrape")){database <- "cabdirect_scrape"}
  if(stringr::str_detect(database_signature, "ingenta_scrape")){database <- "ingenta_scrape"}
  if(stringr::str_detect(database_signature, "pubmed_scrape")){database <- "pubmed_scrape"}
  if(stringr::str_detect(database_signature, "worldcat_scrape")){database <- "worldcat_scrape"}
  if(stringr::str_detect(database_signature, "wos_scrape")){database <- "wos_scrape"}
  
  if (length(database)>0){return(database)}
  
  if (length(database)==0){
    database <- "Unknown"
    print("Database format not recognized.")
  }
  
}
import_results <- function(directory, remove_duplicates = FALSE, duplicate_methods=c("tokens", "quick", "levenshtein"), clean_dataset = TRUE, save_full_dataset = FALSE, verbose = TRUE, save_directory="./"){
  if(save_full_dataset==TRUE){
    if(utils::menu(c("yes", "no"),
                   title="This will save the full dataset to a .csv file in your working directory. Do you want litsearchr to save the full dataset?")==2){
      save_full_dataset <- FALSE
    }
  }
  
  import_files <- paste(directory, list.files(path = directory),
                        sep = "")
  
  for(i in 1:length(import_files)){
    if(i==1){removals <- c()}
    if(stringr::str_detect(import_files[i], ".csv")){}else{
      if(stringr::str_detect(import_files[i], ".txt")){}else{
        if(stringr::str_detect(import_files[i], ".xls")==FALSE){
          print(paste("File format is not recognized. Skipping", import_files[i]))
          removals <- append(removals, i)}}
    }
    if(i==length(import_files)){
      if(length(removals) > 0){
        import_files <- import_files[-removals]
      }
    }
  }
  
  for (i in 1:length(import_files)) {
    df <- c()
    
    if (stringr::str_detect(import_files[i], ".csv") == TRUE) {
      df <- read.csv(import_files[i], header = TRUE, stringsAsFactors = FALSE)
    }
    if (stringr::str_detect(import_files[i], ".txt") == TRUE) {
      df <- read.table(import_files[i], sep = "\t", header = TRUE,
                       comment.char = "#", na.strings = ".", stringsAsFactors = FALSE,
                       quote = "", fill = TRUE)
    }
    if (stringr::str_detect(import_files[i], ".xls") == TRUE) {
      df <- xlsx::read.xlsx(import_files[i], 1)
      df[] <- lapply(df, function(x) if(is.factor(x)) as.character(x) else x)
    }
    
    if (stringr::str_detect(paste(colnames(df), collapse=" "), "\\.\\.")){
      temp_cn <- strsplit(as.character(colnames(df)[1]), "\\.\\.")
      if (length(temp_cn[[1]]) > 1) {
        colnames(df)[1] <- temp_cn[[1]][2]
      }
    }
    if(length(which(colnames(df)=="X"))>0){df <- df[, -which(colnames(df)=="X")]}
    
    if(verbose==TRUE){print(paste("Importing file", import_files[i]))}
    database <- c()
    database <- detect_database(df)
    if(database == "Scopus"){
      df <- as.data.frame(cbind(id = df$EID, title = df$Title,
                                abstract = df$Abstract, keywords = df$Author.Keywords,
                                type = df$Document.Type, authors = df$Authors,
                                affiliation = df$Affiliations, source = df$Source.title,
                                year = df$Year, volume = df$Volume, issue = df$Issue,
                                startpage = df$Page.start, endpage = df$Page.end,
                                doi = df$DOI))
      df$methods <- rep("", length(df$id))
      df$language <- rep("", length(df$id))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "ZooRec"){
      df <- as.data.frame(cbind(id = df$AN, title = df$TI,
                                abstract = df$AB, keywords = df$DE, type = df$DT,
                                authors = df$AU, affiliation = df$C1, source = df$SO,
                                year = df$PY, volume = df$VL, issue = df$IS,
                                startpage = df$PS, doi = df$DI, language = df$LA))
      df$startpage <- as.character(df$startpage)
      df$endpage <- rep("", nrow(df))
      temp <- strsplit(as.character(df$startpage), "-")
      if (length(temp) > 0) {
        for (j in 1:length(temp)) {
          df$startpage[j] <- temp[[j]][1]
          if (length(temp[[j]]) > 1) {
            df$endpage[j] <- temp[[j]][2]
          }
        }
      }
      df$methods <- rep("", length(df$id))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "BIOSIS"){
      df <- as.data.frame(cbind(id = df$UT, title = df$TI,
                                abstract = df$AB, methods = df$MQ, keywords = df$MI,
                                type = df$DT, authors = df$AU, affiliation = df$C1,
                                source = df$SO, year = df$PY, volume = df$VL,
                                issue = df$IS, startpage = df$BP, endpage = df$EP,
                                doi = df$DI, language = df$LA))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "MEDLINE"){
      df <- as.data.frame(cbind(id = df$AN, title = df$TI,
                                abstract = df$AB, keywords = df$ID, type = df$DT,
                                authors = df$AU, affiliation = df$C1, source = df$SO,
                                year = df$Y, volume = df$VL, issue = df$IS, startpage = df$PS,
                                doi = df$DI, language = df$LA))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
      df$methods <- rep("", length(df$id))
      temp <- strsplit(as.character(df$startpage), "-")
      if (length(temp) > 0) {
        for (j in 1:length(temp)) {
          df$startpage[j] <- temp[[j]][1]
          if (length(temp[[j]]) > 1) {
            df$endpage[j] <- temp[[j]][2]
          }
        }
      }
    }
    if(database == "EBSCO"){
      df <- as.data.frame(cbind(id = df$Accession.Number,
                                title = df$Article.Title, abstract = df$Abstract,
                                authors = df$Author, source = df$Journal.Title,
                                year = df$Publication.Date, volume = df$Volume,
                                issue = df$Issue, startpage = df$First.Page,
                                endpage = df$Page.Count, doi = df$DOI, keywords = df$Keywords,
                                type = df$Doctype))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "NDLTD"){
      df <- as.data.frame(cbind(title=df$title, authors=df$author,
                                year=df$date, abstract=df$abstract))
      df$id <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "OATD"){
      df <- as.data.frame(cbind(title=df$title, authors=df$author,
                                abstract=df$abstract))
      df$id <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$year <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "OpenThesis"){
      df <- as.data.frame(cbind(title=df$title, authors=df$author,
                                year=df$date))
      df$id <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database=="ProQuest"){
      df <- as.data.frame(cbind(id=df$StoreId, title=df$Title, abstract=df$Abstract, keywords=df$subjectTerms, type=df$documentType,
                                authors=df$Authors, source=df$pubtitle, year=df$year, volume=df$volume, issue=df$issue,
                                startpage=df$pages, doi=df$digitalObjectIdentifier, language=df$language))
      df$affiliation <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      
      df$startpage <- as.character(df$startpage)
      temp <- strsplit(as.character(df$startpage), "-")
      if (length(temp) > 0) {
        for (j in 1:length(temp)) {
          if(length(temp[[j]])>0){
            df$startpage[j] <- temp[[j]][1]
          }
          if (length(temp[[j]]) > 1) {
            df$endpage[j] <- temp[[j]][2]
          }
        }
      }
      
      df$text <- paste(df$abstract, df$keywords, sep=" ")
    }
    
    if(database == "googlescholar_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$year, source=df$journal))
      df$id <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "jstor_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                source=df$journal, keywords=df$keyword,
                                type=df$type, doi=df$doi, volume=df$pubinfo))
      df$id <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$year <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "scopus_scrape"){
      df <- as.data.frame(cbind(id=df$id,
                                title=df$title,
                                authors=df$authors,
                                year=df$year,
                                source=df$publication,
                                volume=df$volume,
                                issue=df$issue,
                                startpage=df$pages,
                                doi=df$doi))
      df$abstract <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "cabdirect_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                source=df$source))
      df$id <- rep("", nrow(df))
      df$year <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "ingenta_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$date, abstract=df$abstract, doi=df$doi, volume=df$pubinfo))
      df$id <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "pubmed_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$year, abstract=df$abstract,
                                id=df$id, keywords=df$keywords, source=df$source, volume=df$volume, doi=df$doi))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "worldcat_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$date, abstract=df$abstract,
                                type=df$type, language=df$language,
                                source=df$publication, volume=df$pubinfo))
      df$id <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "wos_scrape"){
      df <- as.data.frame(cbind(id = df$id,
                                title = df$title,
                                abstract = df$abstract,
                                keywords = df$keywords,
                                type = df$type,
                                authors = df$authors,
                                affiliation = df$address,
                                source = df$publication,
                                year = df$date,
                                volume = df$volume,
                                issue = df$issue,
                                startpage = df$pgs,
                                doi = df$doi,
                                language = df$language))
      df$endpage <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    
    if (database != "Unknown") {
      df$database <- rep(database, nrow(df))
      df[] <- lapply(df, as.character)
      df <- as.data.frame(cbind(id = df$id,
                                text = df$text,
                                title = df$title,
                                abstract = df$abstract,
                                keywords = df$keywords,
                                methods = df$methods,
                                type = df$type,
                                authors = df$authors,
                                affiliation = df$affiliation,
                                source = df$source,
                                year = df$year,
                                volume = df$volume,
                                issue = df$issue,
                                startpage = df$startpage,
                                endpage = df$endpage,
                                doi = df$doi,
                                language = df$language,
                                database = df$database))
      df[] <- lapply(df, as.character)
      if (i == 1) {
        search_hits <- df
      }
      if (i > 1) {
        search_hits <- rbind(search_hits, df)
      }
    }
    if(database=="Unknown"){
      print(paste("Warning: Unable to recognize format for", import_files[i]))
    }
  }
  
  
  if (save_full_dataset == TRUE) {
    write.csv(search_hits, paste(save_directory, "full_dataset.csv", sep=""))
    print("Complete dataset written to .csv file.")
  }
  if (remove_duplicates == TRUE) {
    print("Removing duplicates.")
    search_hits <- deduplicate(search_hits, method = duplicate_methods)
  }
  if (clean_dataset == TRUE) {
    print("Cleaning dataset.")
    search_hits <- clean_keywords(search_hits)
  }
  return(search_hits)
}
deduplicate <- function(df, use_abstracts=TRUE, use_titles=TRUE, method=c("quick", "levenshtein", "tokens"), doc_sim=.85, title_sim=.95){
  
  remove_abstracts <- c()
  remove_titles <- c()
  
  if (use_abstracts==TRUE){
    
    if(method=="tokens"){
      dfA <- as.data.frame(cbind(id=as.character(df$id), text=as.character(df$text)))
      dfA$text <- as.character(dfA$text)
      full_dfm <- quanteda::dfm(make_corpus(dfA),
                                remove = quanteda::stopwords("english"),
                                remove_numbers=TRUE,
                                remove_punct=TRUE,
                                remove_symbols=TRUE,
                                remove_separators=TRUE,
                                remove_twitter=TRUE,
                                remove_hyphens=TRUE,
                                remove_url=TRUE)
      dfm_similarity <- quanteda::textstat_simil(full_dfm, margin = "documents")
      sim_mat <- as.matrix(dfm_similarity)
      sim_mat[lower.tri(sim_mat, diag=TRUE)] <- NA
      sim_mat <- as.data.frame(sim_mat)
      
      indices <- data.frame(ind = which(sim_mat > doc_sim, arr.ind=TRUE))
      indices$doc1 <- rownames(sim_mat)[indices$ind.row]
      indices$doc2 <- colnames(sim_mat)[indices$ind.col]
      remove_abstracts <- sort(unique(as.numeric(gsub("text", "", indices$doc2))))
    }
    
    if(method=="quick"){
      remove_abstracts <- which(duplicated(tolower(tm::removePunctuation(df$text)))==TRUE)
    }
    
    if(method=="levenshtein"){
      lev_sim <- utils::adist(df$text)
      x <- df$text
      y <- df$text
      
      for(i in 1:length(x)){
        x[i] <- paste(quanteda::tokens_remove(quanteda::tokens(x[i]), custom_stopwords), collapse=" ")
        for(j in 1:length(y)){
          y[j] <- paste(quanteda::tokens_remove(quanteda::tokens(y[j]), custom_stopwords), collapse=" ")
          lev_sim[i,j] <- 1 - utils::adist(x[i], y[j])/max(nchar(x[i]), nchar(y[j]))
        }
      }
      
      lev_sim[lower.tri(lev_sim, diag=TRUE)] <- NA
      
      indices <- data.frame(ind = which(lev_sim > doc_sim, arr.ind=TRUE))
      
      remove_abstracts <- sort(unique(as.numeric(indices$ind.col)))
    }
  }
  
  
  
  if (use_titles==TRUE){
    if (method=="quick"){
      remove_titles <- which(duplicated(tolower(tm::removePunctuation(df$title)))==TRUE)
    }
    
    if (method=="tokens"){
      
      dfT <- as.data.frame(cbind(id=as.character(df$id), text=as.character(df$title)))
      dfT$text <- as.character(dfT$text)
      full_dfm <- quanteda::dfm(make_corpus(dfT),
                                remove = quanteda::stopwords("english"),
                                remove_numbers=TRUE,
                                remove_punct=TRUE,
                                remove_symbols=TRUE,
                                remove_separators=TRUE,
                                remove_twitter=TRUE,
                                remove_hyphens=TRUE,
                                remove_url=TRUE)
      dfm_similarity <- quanteda::textstat_simil(full_dfm, margin = "documents")
      sim_mat <- as.matrix(dfm_similarity)
      sim_mat[lower.tri(sim_mat, diag=TRUE)] <- NA
      sim_mat <- as.data.frame(sim_mat)
      
      indices <- data.frame(ind = which(sim_mat > title_sim, arr.ind=TRUE))
      indices$doc1 <- rownames(sim_mat)[indices$ind.row]
      indices$doc2 <- colnames(sim_mat)[indices$ind.col]
      remove_titles <- sort(unique(as.numeric(gsub("text", "", indices$doc2))))
      
    }
    if(method=="levenshtein"){
      lev_sim <- utils::adist(df$title)
      x <- df$title
      y <- df$title
      
      for(i in 1:length(x)){
        x[i] <- paste(quanteda::tokens_remove(quanteda::tokens(x[i]), custom_stopwords), collapse=" ")
        for(j in 1:length(y)){
          y[j] <- paste(quanteda::tokens_remove(quanteda::tokens(y[j]), custom_stopwords), collapse=" ")
          lev_sim[i,j] <- 1 - utils::adist(x[i], y[j])/max(nchar(x[i]), nchar(y[j]))
        }
      }
      
      lev_sim[lower.tri(lev_sim, diag=TRUE)] <- NA
      
      indices <- data.frame(ind = which(lev_sim > title_sim, arr.ind=TRUE))
      
      remove_titles <- sort(unique(as.numeric(indices$ind.col)))
    }
    
  }
  
  remove_documents <- unique(append(remove_abstracts, remove_titles))
  
  if (length(remove_documents) > 0){new_data <- df[-c(remove_documents),]}
  if (length(remove_documents) == 0){new_data <- df}
  
  return(new_data)
}
clean_keywords <- function(df){
  df$keywords <- tolower(as.character(df$keywords))
  removals <- c("\\(",
                "\\)",
                ":",
                "=",
                "%",
                "\\+",
                "<",
                ">",
                "\\?",
                "\\\\",
                "&",
                "!",
                "\\$",
                "\\*"
  )
  for (i in 1:length(removals)){
    df$keywords <- gsub(removals[i], df$keywords, replacement="")
  }
  
  # replace keyword separators with standardized semicolon
  replacements <- c(", ",
                    ",",
                    "/",
                    ";;",
                    ", ",
                    "\\[",
                    "\\]"
  )
  for (i in 1:length(replacements)){
    df$keywords <- gsub(replacements[i], df$keywords, replacement=";")
  }
  
  df$keywords <- gsub("  ", " ", df$keywords)
  df$keywords <- gsub("; ", ";", df$keywords)
  df$keywords <- gsub(" ;", ";", df$keywords)
  df$keywords <- gsub(";;", ";", df$keywords)
  
  return(df)
}
make_corpus <- function(df){
  search_corpus <- quanteda::corpus(df)
  return(search_corpus)
}
add_stopwords <- function(new_stopwords){
  custom_stopwords <- sort(unique(append(custom_stopwords, new_stopwords)))
  return(custom_stopwords)
}
extract_terms <- function(df, type=c("RAKE", "tagged"), new_stopwords=NULL, min_freq=2, title=TRUE, abstract=TRUE, ngrams=TRUE, n=2){
  if(type=="RAKE"){
    if (title == TRUE){
      if (abstract == TRUE){
        article_subjects <- paste(df$title, df$abstract, collapse=". ")
      }
      if (abstract == FALSE){
        article_subjects <- paste(df$title, collapse=". ")
      }
    }
    if (title == FALSE){
      if (abstract == TRUE){
        article_subjects <- paste(df$abstract, collapse=". ")
      }
      if (abstract == FALSE){print("You aren't selecting any text to pass to RAKE!")}
    }
    
    possible_terms <- rapidraker::rapidrake(tolower(article_subjects),
                                            stop_words = add_stopwords(new_stopwords),
                                            stem=FALSE)
    likely_terms <- possible_terms[[1]]$keyword[which(possible_terms[[1]]$freq >= min_freq)]
    if (ngrams==TRUE){
      likely_terms <- likely_terms[which(sapply(strsplit(as.character(likely_terms), " "), length) >= n)]
    }
    return(likely_terms)
  }
  
  if(type=="tagged"){
    cleaned_keywords <- clean_keywords(df)$keyword
    possible_terms <- paste(df$keywords, collapse=";")
    possible_terms <- strsplit(possible_terms, ";")[[1]]
    possible_terms <- stringr::str_trim(tm::removePunctuation(possible_terms))
    
    term_freq_table <- table(possible_terms)
    
    actual_terms <- tolower(names(term_freq_table)[which(term_freq_table >= min_freq)])
    if(length(which(actual_terms=="")>0)){actual_terms <- actual_terms[-which(actual_terms=="")]}
    if (ngrams==TRUE){
      actual_terms <- actual_terms[which(sapply(strsplit(as.character(actual_terms), " "), length) >= n)]
    }
    
    return(actual_terms)
  }
  
}
make_dictionary <- function(terms=NULL){
  complete_keywords <- terms[[1]]
  for(i in 1:length(terms)){
    complete_keywords <- unique(tolower(append(complete_keywords, terms[[i]])))
  }
  
  my_dic <- as.data.frame(cbind(complete_keywords, complete_keywords))
  colnames(my_dic) <- c("word", "sentiment")
  
  my_dic <- quanteda::as.dictionary(my_dic)
  
  return(my_dic)
}
create_dfm <- function(corpus=make_corpus(df), my_dic=make_dictionary(), custom_stopwords=add_stopwords(NULL)){
  
  search_dfm <- quanteda::dfm(corpus,
                              stem = FALSE,
                              remove=custom_stopwords,
                              remove_numbers=TRUE,
                              remove_punct=TRUE,
                              remove_symbols=TRUE,
                              remove_separators=TRUE,
                              remove_twitter=TRUE,
                              remove_hyphens=TRUE,
                              remove_url=TRUE,
                              dictionary=my_dic,
                              tolower=TRUE)
  
  return(search_dfm)
  
}
create_network <- function(search_dfm, min_studies=3, min_occurrences = 3){
  trimmed_mat <- quanteda::dfm_trim(search_dfm, min_termfreq = min_occurrences, min_docfreq = min_studies)
  search_mat <- quanteda::fcm(trimmed_mat, context = "document", count = "boolean", tri=FALSE)
  search_mat <- as.matrix(search_mat)
  search_graph <- igraph::graph.adjacency(t(search_mat),
                                          weighted=TRUE,
                                          mode="undirected",
                                          diag=FALSE)
  return(search_graph)
}
make_importance <- function(graph, importance_method){
  if (importance_method=="strength") {importance <- sort(igraph::strength(graph))}
  if (importance_method=="eigencentrality"){importance <- sort(igraph::eigen_centrality(graph))}
  if (importance_method=="alpha"){importance <- sort(igraph::alpha_centrality(graph))}
  if (importance_method=="betweenness"){importance <- sort(igraph::betweenness(graph))}
  if (importance_method=="hub"){importance <- sort(igraph::hub_score(graph))}
  if (importance_method=="power"){importance <- sort(igraph::power_centrality(graph))}
  importance_data <- cbind(seq(1, length(importance), 1), as.numeric(importance))
  colnames(importance_data) <- c("rank", "importance")
  importance_data <- as.data.frame(importance_data)
  importance_data$nodename <- names(importance)
  importance_data$rank <- as.numeric(importance_data$rank)
  importance_data$importance <- as.numeric(importance_data$importance)
  return(importance_data)
}
select_ngrams <- function(graph, n=2, importance_method="strength"){
  importance_data <- make_importance(graph, importance_method = importance_method)
  ngrams <- importance_data[which(sapply(strsplit(as.character(importance_data$nodename), " "), length) >= n),]
  return(ngrams)
}
select_unigrams <- function(graph, importance_method="strength"){
  importance_data <- make_importance(graph, importance_method = importance_method)
  unigrams <- importance_data[which(sapply(strsplit(as.character(importance_data$nodename), " "), length) == 1),]
  return(unigrams)
}
find_knots <- function(importance_data, degrees=2, knot_num=1){
  knotselect <- freeknotsplines::freepsgen(importance_data$rank, importance_data$importance,
                                           degree=degrees, numknot=knot_num, seed=5, stream=0)
  knots <- knotselect@optknot
  return(knots)
}
fit_splines <- function(importance_data, degrees=2, knot_num=1, knots){
  spline_b <- splines2::bSpline(as.numeric(importance_data$rank), knots=knots, degree=degrees, numknot=knot_num, intercept=TRUE)
  spline_fit <- lm(as.numeric(importance_data$importance) ~ spline_b)
  return(spline_fit)
}
find_cutoff <- function(graph, method=c("spline", "cumulative"), cum_pct=0.8, degrees=2, knot_num=1, diagnostics=TRUE, importance_method="strength"){
  
  importance_data <- make_importance(graph, importance_method=importance_method)
  
  if (method == "spline") {
    knots <- find_knots(importance_data, degrees=degrees, knot_num=knot_num)
    cut_points <- floor(knots)
    cut_strengths <- (importance_data$importance)[cut_points]
    
    if (diagnostics == TRUE){
      spline_fit <- fit_splines(importance_data, degrees=degrees, knot_num=knot_num, knots=knots)
      plot(importance_data$rank, importance_data$importance,
           main="Spline model fit",
           xlab="Rank", ylab="Node importance (unique)")
      lines(importance_data$rank,spline_fit$fit,col="red",lwd=3)
      abline(v=knots, col="blue", lwd=2)
      
      plot(importance_data$rank, spline_fit$resid, xlab="Rank", ylab="Residual", main="Residuals along the x-axis (rank)")
      abline(h=0, col="red")
      abline(lm(spline_fit$resid ~ importance_data$rank), col="blue", lty=2)
      plot(importance_data$importance, spline_fit$resid, xlab="Importance", ylab="Residual", main="Residuals along the y-axis (importance)")
      abline(lm(spline_fit$resid ~ importance_data$importance), col="blue", lty=2)
      abline(h=0, col="red")
    }
  }
  
  if (method == "cumulative"){
    cum_str <- max(cumsum(sort(importance_data$importance)))
    cut_point <- (which(cumsum(sort(importance_data$importance, decreasing = TRUE))>=cum_str*cum_pct))[1]
    cut_strengths <- as.numeric(sort(as.numeric(importance_data$importance), decreasing = TRUE)[cut_point])
    
    if (diagnostics == TRUE){
      plot(cumsum(sort(importance_data$importance)), type="l", ylab="Cumulative node importance", main="Cumulative sum of ranked node importance")
      abline(v=cut_point, col="blue")
      legend("topleft", legend = c("Cutoff rank"), lwd=2, col="blue")
      
      hist(importance_data$importance, 100,
           main="Histogram of node importance", xlab="Node importance")
      abline(v=cut_strengths, col="blue")
      legend("topright", legend = c("Node importance cutoff"), lwd=2, col="blue")
    }
  }
  return(cut_strengths)
}
get_keywords <- function(reduced_graph, savekeywords=FALSE, makewordle=FALSE, directory="./"){
  if(savekeywords==TRUE){
    if(utils::menu(c("yes", "no"), title="This will write keywords to a plain text file. Do you want to save keywords to a file?")==2){
      savekeywords <- FALSE
    }
  }
  potential_keys <- names(igraph::V(reduced_graph))
  if (savekeywords == TRUE){writeLines(potential_keys, paste(directory, "potential-keywords.txt", sep="")) }
  if (makewordle == TRUE) {make_wordle(reduced_graph)}
  return(potential_keys)
}
reduce_graph <- function(graph, cutoff_strength, importance_method="strength"){
  importance_data <- make_importance(graph, importance_method = importance_method)
  important_nodes <- importance_data$nodename[which(importance_data$importance >= cutoff_strength)]
  reduced_graph <- igraph::induced_subgraph(graph, v=important_nodes)
  return(reduced_graph)
}
make_ngram_graph <- function(graph, min_ngrams=2, unigrams=FALSE){
  if (unigrams==FALSE){ngrams <- select_ngrams(graph, min_ngrams)}
  if (unigrams==TRUE){ngrams <- select_unigrams(graph)}
  
  ngram_graph <- igraph::induced_subgraph(graph, v=ngrams$nodename)
  return(ngram_graph)
}
get_similar_terms <- function(grouped_terms, graph, considered_terms=NULL, ignore_terms=NULL){
  
  all_terms <- names(igraph::V(graph))
  all_strengths <- igraph::strength(graph)
  
  for(h in 1:length(grouped_terms)){
    if(h==1){my_terms <- grouped_terms[[h]]}
    if(h>1){
      my_terms <- append(my_terms, grouped_terms[[h]])
    }
  }
  
  unigrams <- strsplit(paste(my_terms, collapse=" "), " ")[[1]]
  if(length(ignore_terms)>0){
    for(m in 1:length(ignore_terms)){
      if(m==1){removals <- c()}
      detections <- which(unigrams==ignore_terms[m])
      if(length(detections)>0){
        removals <- append(removals,detections)
      }
      if(m==length(ignore_terms)){
        unigrams <- unigrams[-unique(removals)]
      }
    }
  }
  
  for(i in 1:length(unigrams)){
    current_term <- should_stem(unigrams[i])
    current_term <- gsub("\\*", "", current_term)
    
    if(i==1){potential_terms <- c()}
    for(j in 1:length(all_terms)){
      if(stringr::str_detect(all_terms[j], current_term)){
        if(stringr::str_detect(paste(my_terms, considered_terms, collapse="; "), all_terms[j])==FALSE){
          potential_terms <- unique(append(potential_terms, all_terms[j]))
        }
      }
    }
    if(i==length(unigrams)){
      for(k in 1:length(potential_terms)){
        x <- which(all_terms==potential_terms[k])
        if(k==1){
          NS <- all_strengths[x]
        }
        if(k>1){
          NS <- append(NS, all_strengths[x])
        }
      }
    }
  }
  return(NS)
}
should_stem <- function(word){
  splitup <- strsplit(word, " ")[[1]]
  for(i in 1:length(splitup)){
    wordcut <- SnowballC::wordStem(splitup[i], language="en")
    stem_length <- nchar(wordcut)
    
    if(i==1){
      if(stem_length > 3){
        words <- paste(wordcut, "* ", sep="")
      }
      if(stem_length <= 3){
        words <- paste(splitup[i], "* ", sep="")
      }
    }
    if(i > 1){
      if(stem_length > 3){
        words <- paste(words, wordcut, "* ", sep="")
      }
      if(stem_length <= 3){
        words <- paste(words, splitup[i], "* ", sep="")
      }
    }
  }
  
  words <- stringr::str_trim(words)
  return(words)
}
write_search <- function(groupdata, API_key=NULL, languages=NULL, exactphrase=FALSE, directory="./", stemming=TRUE, verbose=TRUE, writesearch=FALSE){
  if(writesearch==TRUE){
    if(utils::menu(c("yes", "no"),
                   title="This is going to write .txt files to your computer containing the search strings. Are you sure you want to write the files?")==2){
      writesearch <- FALSE
    }}
  
  no_groups <- length(groupdata)
  group_holder <- c()
  no_langs <- length(languages)
  
  if(exactphrase==FALSE){
    
    for (i in 1:no_langs){
      current_lang <- languages[i]
      translated_groups <- list()
      length(translated_groups) <- no_groups
      
      for (j in 1:no_groups){
        current_group <- groupdata[j]
        
        if (current_lang!="English"){
          translated_terms <- (translate_search(
            search_terms = current_group[[1]], target_language = current_lang, API_key = API_key))
          each_line <- paste("\\(", "\\(", translated_terms[1], "\\)")
        }
        
        if (current_lang=="English"){
          if(stemming==FALSE){
            translated_terms <- current_group[[1]]
          }
          
          if(stemming==TRUE){
            stemyes <- "stemmed-"
            prestar <- c()
            for (m in 1:length(current_group[[1]])){
              prestar[m] <- should_stem(current_group[[1]][m])
              if(m==length(current_group[[1]])){prestar <- unique(prestar)}
            }
            
            for(n in 1:length(prestar)){
              if(n==1){redundant <- c()}
              if(stringr::str_detect(paste(prestar[-n], collapse=" "), prestar[n])){
                detections <- which(stringr::str_detect(prestar, prestar[n])==TRUE)
                redundant <- append(redundant, detections[-which(detections==n)])
              }
              if(n==length(prestar)){
                redundant <- unique(redundant)
                if(length(redundant > 0)){
                  prestar <- prestar[-redundant]
                }
              }
            }
            
            translated_terms <- unique(paste(prestar, "", sep=""))
            
            
            
          }
          each_line <- paste("\\(", "\\(", translated_terms[1],  "", "\\)")
          
          
        }
        
        for (k in 2:length(translated_terms)){
          each_line <- paste(each_line, " OR \\(", translated_terms[k],"\\)", "",  sep="")
        }
        
        each_line <- paste(each_line, "\\)")
        
        translated_groups[[j]] <- each_line
      }
      
      total_search <- translated_groups[[1]]
      for (l in 2:length(translated_groups)){
        total_search <- paste(total_search, translated_groups[[l]], sep=" AND ")
      }
      total_search <- paste("\\(", total_search, "\\)")
      
      this_one <- which(stringr::str_detect(possible_langs$Language, current_lang)==TRUE)
      trans_encod <- as.character(possible_langs$Encoding[this_one])
      
      converted_search <- iconv(total_search, "UTF-8", trans_encod)
      converted_search <- gsub("\\\\", "\\", converted_search)
      
      if(writesearch==TRUE){
        filename <- paste(directory, "search-in-", current_lang, ".txt", sep="")
        writeLines(converted_search, filename)
        if(verbose==TRUE){
          print(paste(current_lang, "is written"))
        }
      }
      
      if(i==1){
        search_list <- list()
        length(search_list) <- length(i)
      }
      search_list[[i]] <- converted_search
      names(search_list)[[i]] <- current_lang
      
    }
    
    
  }
  
  if(exactphrase==TRUE){
    for (i in 1:no_langs){
      current_lang <- languages[i]
      translated_groups <- list()
      length(translated_groups) <- no_groups
      
      for (j in 1:no_groups){
        current_group <- groupdata[j]
        
        if (current_lang!="English"){
          translated_terms <- (translate_search(
            search_terms = current_group[[1]], target_language = current_lang))
          each_line <- paste("\\(", "\"", translated_terms[1], "\"", sep="")
        }
        
        if (current_lang=="English"){
          if(stemming==FALSE){
            translated_terms <- current_group[[1]]
          }
          if(stemming==TRUE){
            stemyes <- "stemmed-"
            prestar <- c()
            
            for (m in 1:length(current_group[[1]])){
              prestar[m] <- should_stem(current_group[[1]][m])
              if(m==length(current_group[[1]])){prestar <- unique(prestar)}
            }
            
            for(n in 1:length(prestar)){
              if(n==1){redundant <- c()}
              if(stringr::str_detect(paste(prestar[-n], collapse=" "), prestar[n])){
                detections <- which(stringr::str_detect(prestar, prestar[n])==TRUE)
                redundant <- append(redundant, detections[-which(detections==n)])
              }
              if(n==length(prestar)){
                redundant <- unique(redundant)
                if(length(redundant > 0)){
                  prestar <- prestar[-redundant]
                }
              }
            }
            
            translated_terms <- unique(paste(prestar, "", sep=""))
            
          }
          each_line <- paste("\\(", "\"", translated_terms[1], "\"", sep="")
        }
        
        for (k in 2:length(translated_terms)){
          each_line <- paste(each_line, " OR ", "\"", translated_terms[k], "\"", sep="")
        }
        
        each_line <- paste(each_line, "\\)")
        
        translated_groups[[j]] <- each_line
      }
      
      total_search <- translated_groups[[1]]
      for (l in 2:length(translated_groups)){
        total_search <- paste(total_search, translated_groups[[l]], sep=" AND ")
      }
      total_search <- paste("\\(", total_search, "\\)")
      
      this_one <- which(stringr::str_detect(possible_langs$Language, current_lang)==TRUE)
      trans_encod <- as.character(possible_langs$Encoding[this_one])
      
      converted_search <- iconv(total_search, "UTF-8", trans_encod)
      converted_search <- gsub("\\\\", "\\", converted_search)
      
      if(stemming==FALSE){filename <- paste("search-in-", current_lang, ".txt", sep="")}
      if(stemming==TRUE){
        if(current_lang!="English"){filename <- paste("search-in-", current_lang, ".txt", sep="")}
        if(current_lang=="English"){filename <- paste("search-in-stemmed-", current_lang, ".txt", sep="")}
      }
      
      if(writesearch==TRUE){
        writeLines(converted_search, filename)
        if(verbose==TRUE){
          print(paste(current_lang, "is written"))
        }
      }
      
      if(i==1){
        search_list <- list()
        length(search_list) <- length(i)
      }
      search_list[[i]] <- converted_search
      names(search_list)[[i]] <- current_lang
      
      
    }
    
  }
  
  return(search_list)
}


##### Script #####

# data for write_search
possible_langs <- read.csv("./possible_langs.csv", stringsAsFactors = FALSE)

BBWO_import <- import_results("./naive_search/", remove_duplicates = FALSE, clean_dataset = TRUE, save_full_dataset = FALSE)
BBWO_data1 <- deduplicate(BBWO_import, use_abstracts = FALSE, use_titles=TRUE, method = "tokens", title_sim = .8)
BBWO_data <- deduplicate(BBWO_data1, use_abstracts = TRUE, use_titles=FALSE, doc_sim = .8, method="tokens")

custom_stopwords <- read.csv("./custom_stopwords.csv", stringsAsFactors = FALSE)[,2]
raked_keywords <- extract_terms(BBWO_data, type="RAKE", new_stopwords = NULL, min_freq = 2, title = TRUE, abstract = TRUE, ngrams = TRUE, n=2)
real_keywords <- extract_terms(BBWO_data, ngrams = TRUE, n=2, type ="tagged")

BBWO_dict <- make_dictionary(terms=list(raked_keywords, real_keywords))
BBWO_corpus <- make_corpus(BBWO_data)
BBWO_dfm <- create_dfm(BBWO_corpus, my_dic=BBWO_dict)
BBWO_graph <- create_network(BBWO_dfm, min_studies=3, min_occurrences = 3)

hist(igraph::strength(BBWO_graph), 100, main="Histogram of node strengths", xlab="Node strength")
plot(sort(igraph::strength(BBWO_graph)), ylab="Node strength", main="Ranked node strengths", xlab="Rank")
cutoffs_spline <- find_cutoff(BBWO_graph, method = "spline", degrees = 2, knot_num = 3, diagnostics = TRUE, importance_method = "strength")

reduced_graph <- reduce_graph(BBWO_graph, cutoff_strength = cutoffs_spline[1])
search_terms <- get_keywords(reduced_graph, savekeywords = FALSE, makewordle = FALSE)

write.csv(search_terms, "./BBWO_search_terms.csv")

grouped_keywords <- read.csv("./BBWO_search_terms_grouped.csv", stringsAsFactors = FALSE)

process_group <- unique(append(
  c("home range size"),
  grouped_keywords$term[which(stringr::str_detect(grouped_keywords$group, "process"))]))

fire_group <- unique(append(
  c("wildfire"),
  grouped_keywords$term[which(stringr::str_detect(grouped_keywords$group, "fire"))]))

bird_group <- unique(append(
  c("woodpecker", "sapsucker", "Veniliornis", "Picoides", "Dendropicoides", "Melanerpes", "Sphyrapicus"),
  grouped_keywords$term[which(stringr::str_detect(grouped_keywords$group, "bird"))]))

response_group <- unique(append(
  c("occupancy"),
  grouped_keywords$term[which(stringr::str_detect(grouped_keywords$group, "response"))]))

my_search_terms <- list(process_group, fire_group, bird_group, response_group)

similar_terms <- get_similar_terms(my_search_terms, graph = BBWO_graph)

write.csv(similar_terms, "./BBWO_similar_terms.csv")

new_terms <- read.csv("./BBWO_similar_terms_grouped.csv", stringsAsFactors = FALSE)

fire_group <- unique(append(fire_group,
                            new_terms$term[which(stringr::str_detect(new_terms$group, "fire"))]))
bird_group <- unique(append(bird_group,
                            new_terms$term[which(stringr::str_detect(new_terms$group, "bird"))]))
response_group <- unique(append(response_group,
                                new_terms$term[which(stringr::str_detect(new_terms$group, "response"))]))
process_group <- unique(append(process_group,
                               new_terms$term[which(stringr::str_detect(new_terms$group, "process"))]))

my_search_terms <- list(fire_group, bird_group, response_group, process_group)

my_search <- write_search(groupdata = my_search_terms, 
                                      languages = c("English"), stemming = TRUE,
                                      exactphrase = TRUE, writesearch = FALSE, verbose = TRUE)
my_search
