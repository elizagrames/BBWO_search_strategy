BBWO_import <- litsearchr::import_results("./naive_search/", remove_duplicates = FALSE, clean_dataset = TRUE, save_full_dataset = FALSE)
BBWO_data1 <- litsearchr::deduplicate(BBWO_import, use_abstracts = FALSE, use_titles=TRUE, method = "tokens", title_sim = .8)
BBWO_data <- litsearchr::deduplicate(BBWO_data1, use_abstracts = TRUE, use_titles=FALSE, doc_sim = .8, method="tokens")

raked_keywords <- litsearchr::extract_terms(BBWO_data, type="RAKE", new_stopwords = NULL, min_freq = 2, title = TRUE, abstract = TRUE, ngrams = TRUE, n=2)
real_keywords <- litsearchr::extract_terms(BBWO_data, ngrams = TRUE, n=2, type ="tagged")

BBWO_dict <- litsearchr::make_dictionary(terms=list(raked_keywords, real_keywords))
BBWO_corpus <- litsearchr::make_corpus(BBWO_data)
BBWO_dfm <- litsearchr::create_dfm(BBWO_corpus, my_dic=BBWO_dict)
BBWO_graph <- litsearchr::create_network(BBWO_dfm, min_studies=3, min_occurrences = 3)

hist(igraph::strength(BBWO_graph), 100, main="Histogram of node strengths", xlab="Node strength")
plot(sort(igraph::strength(BBWO_graph)), ylab="Node strength", main="Ranked node strengths", xlab="Rank")
cutoffs_spline <- litsearchr::find_cutoff(BBWO_graph, method = "spline", degrees = 2, knot_num = 3, diagnostics = TRUE, importance_method = "strength")

reduced_graph <- litsearchr::reduce_graph(BBWO_graph, cutoff_strength = cutoffs_spline[1])
search_terms <- litsearchr::get_keywords(reduced_graph, savekeywords = FALSE, makewordle = FALSE)

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

similar_terms <- litsearchr::get_similar_terms(my_search_terms, graph = BBWO_graph)

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

my_search <- litsearchr::write_search(groupdata = my_search_terms, 
                                      languages = c("English"), stemming = TRUE,
                                      exactphrase = TRUE, writesearch = FALSE, verbose = TRUE)
my_search
