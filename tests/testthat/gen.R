library(httptest2)

# Start capturing HTTP requests and responses
start_capturing(simplify = TRUE)

# Run the API calls you want to mock
get_drugs(c("Imatinib"))
get_drugs(c("Imatinib", "not-real"))
get_drugs(c("imatinib", "metronidazole"), antineoplastic = TRUE)
get_drugs(c("imatinib", "metronidazole"), antineoplastic = FALSE)
get_drugs(c("not-real"))

get_genes(c("ereg"))
get_genes(c("ereg", "not-real"))
get_genes(c("not-real"))

get_interactions(c("ereg"))
get_interactions(c("ereg", "not-real"))
get_interactions(c("ereg", "braf"))
get_interactions(c("not-real"))

get_interactions(c("sunitinib"), search = "drugs")
get_interactions(c("sunitinib", "not-real"), search = "drugs")
get_interactions(c("sunitinib", "clonazepam"), search = "drugs")
get_interactions(c("not-real"), search = "drugs")

get_categories(c("BRAF"))

get_sources()
get_sources(source_type$GENE)

get_all_genes()

# Stop capturing and save the mocks
stop_capturing()