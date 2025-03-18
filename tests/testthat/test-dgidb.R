httptest::with_mock_dir("fixtures", {
  test_that("Test Get Drugs", {
    results <- get_drugs(c("Imatinib"))
    expect_true(length(results$drug_name) > 0, "FAIL: DataFrame is non-empty")

    results_with_added_fake <- get_drugs(c("Imatinib", "not-real"))
    expect_true(
      length(results_with_added_fake$drug_name) == length(results$drug_name),
      "FAIL: Gracefully ignore non-existent search terms"
    )

    # handling filters
    filtered_results <- get_drugs(
      c("imatinib", "metronidazole"),
      antineoplastic = TRUE
    )
    expect_true(
      length(filtered_results$drug_name) == 1,
      "FAIL: Metronidazole is filtered out"
    )
    expect_true(
      filtered_results$drug_name[[1]] == "IMATINIB",
      "FAIL: Imatinib is retained by the filter"
    )
    expect_true(
      all(unlist(results$drug_is_antineoplastic)),
      "FAIL: All results are antineoplastics"
    )

    filtered_results <- get_drugs(
      c("imatinib", "metronidazole"),
      antineoplastic = FALSE
    )
    expect_true(
      length(filtered_results$drug_name) > 0,
      "FAIL: DataFrame is non-empty"
    )
    expect_true("METRONIDAZOLE" %in% filtered_results$drug_name)

    # empty response
    empty_results <- get_drugs(c("not-real"))
    expect_true(
      length(empty_results$drug_name) == 0,
      "FAIL: Handles empty response"
    )
  })
})

httptest::with_mock_dir("fixtures", {
  test_that("Test Get Genes", {
    results <- get_genes(c("ereg"))
    expect_true(length(results$gene_name) > 0, "FAIL: DataFrame is non-empty")

    results_with_added_fake <- get_genes(c("ereg", "not-real"))
    expect_true(
      length(results_with_added_fake$gene_name) == length(results$gene_name),
      "FAIL: Gracefully ignore non-existent search terms"
    )

    # empty response
    empty_results <- get_genes(c("not-real"))
    expect_true(
      length(empty_results$gene_name) == 0,
      "FAIL: Handles empty response"
    )
  })
})

httptest::with_mock_dir("fixtures", {
  test_that("Test Get Interactions By Genes", {
    results <- get_interactions(c("ereg"))
    expect_true(length(results$gene_name) > 0, "FAIL: Results are non-empty")

    results <- get_interactions(c("ereg", "not-real"))
    expect_true(
      length(results$gene_name) > 0,
      "FAIL: Handles additional not-real terms gracefully"
    )

    # multiple terms
    multiple_gene_results <- get_interactions(c("ereg", "braf"))
    expect_true(
      length(multiple_gene_results$gene_name) > length(results$gene_name),
      "FAIL: Handles multiple genes at once"
    )

    # empty response
    empty_results <- get_interactions(c("not-real"))
    expect_true(
      length(empty_results$gene_name) == 0,
      "FAIL: Handles empty response"
    )
  })
})

httptest::with_mock_dir("fixtures", {
  test_that("Test Get Interactions By Drugs", {
    results <- get_interactions(c("sunitinib"), search = "drugs")
    expect_true(length(results$drug_name) > 0, "FAIL: Results are non-empty")

    results <- get_interactions(c("sunitinib", "not-real"), search = "drugs")
    expect_true(
      length(results$drug_name) > 0,
      "FAIL: Handles additional not-real terms gracefully"
    )

    # multiple terms
    multiple_gene_results <- get_interactions(
      c("sunitinib", "clonazepam"),
      search = "drugs"
    )
    expect_true(
      length(multiple_gene_results$drug_name) > length(results$drug_name),
      "FAIL: Handles multiple drugs at once"
    )

    # empty response
    empty_results <- get_interactions(c("not-real"), search = "drugs")
    expect_true(
      length(empty_results$drug_name) == 0,
      "FAIL: Handles empty response"
    )
  })
})

httptest::with_mock_dir("fixtures", {
  test_that("Test Get Categories", {
    results <- get_categories(c("BRAF"))
    expect_true(length(results$gene_name) > 0, "FAIL: Results are non-empty")
    expect_true("DRUG RESISTANCE" %in% results$gene_category)
    expect_true("DRUGGABLE GENOME" %in% results$gene_category)
    expect_true("CLINICALLY ACTIONABLE" %in% results$gene_category)
  })
})

httptest::with_mock_dir("fixtures", {
  test_that("Test Get Sources", {
    results <- get_sources()
    expect_true(
      length(results$source_name) == 45,
      "Incorrect # of sources: " + str(length(results$source_name))
    )

    results <- get_sources(source_type$GENE)
    sources <- results$source_name
    expect_true(
      length(sources) == 3,
      "Incorrect # of sources: " + str(length(sources))
    )
    # "Check: Contains correct sources"
    expect_setequal(
      sources,
      c("NCBI Gene", "HUGO Gene Nomenclature Committee", "Ensembl")
    )
  })
})

httptest::with_mock_dir("fixtures", {
  test_that("Test Get Gene List", {
    results <- get_all_genes()
    expect_true(length(results$gene_name) == 9)
  })
})
