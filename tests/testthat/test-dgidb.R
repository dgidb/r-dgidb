httptest2::with_mock_api({
    test_that("getDrugs() returns matching drugs", {
        results <- getDrugs("Imatinib")
        expect_gt(length(results$drug_name), 0)

        results_with_added_fake <- getDrugs(c("Imatinib", "not-real"))
        expect_length(
            results_with_added_fake$drug_name,
            length(results$drug_name)
        )

        empty_results <- getDrugs("not-real")
        expect_length(empty_results$drug_name, 0)
    })
})

test_that("getDrugs() applies drug filters", {
    params <- NULL
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            params <<- variables
            list(drugs = list(nodes = list()))
        }
    )

    getDrugs("imatinib", immunotherapy = TRUE, antineoplastic = TRUE)
    expect_true(params$immunotherapy)
    expect_true(params$antiNeoplastic)
})

httptest2::with_mock_api({
    test_that("getGenes() returns matching genes", {
        results <- getGenes("ereg")
        expect_gt(length(results$gene_name), 0)

        results_with_added_fake <- getGenes(c("ereg", "not-real"))
        expect_length(
            results_with_added_fake$gene_name,
            length(results$gene_name)
        )

        empty_results <- getGenes("not-real")
        expect_length(empty_results$gene_name, 0)
    })
})

httptest2::with_mock_api({
    test_that("getInteractions() searches by gene", {
        results <- getInteractions("ereg")
        expect_gt(length(results$gene_name), 0)

        results_with_added_fake <- getInteractions(c("ereg", "not-real"))
        expect_length(
            results_with_added_fake$gene_name,
            length(results$gene_name)
        )

        multiple_results <- getInteractions(c("ereg", "braf"))
        expect_gt(
            length(multiple_results$gene_name),
            length(results$gene_name)
        )

        empty_results <- getInteractions("not-real")
        expect_length(empty_results$gene_name, 0)
    })
})

httptest2::with_mock_api({
    test_that("getInteractions() searches by drug", {
        results <- getInteractions("sunitinib", search = "drugs")
        expect_gt(length(results$drug_name), 0)

        results_with_added_fake <- getInteractions(
            c("sunitinib", "not-real"),
            search = "drugs"
        )
        expect_length(
            results_with_added_fake$drug_name,
            length(results$drug_name)
        )

        multiple_results <- getInteractions(
            c("sunitinib", "clonazepam"),
            search = "drugs"
        )
        expect_gt(
            length(multiple_results$drug_name),
            length(results$drug_name)
        )

        empty_results <- getInteractions("not-real", search = "drugs")
        expect_length(empty_results$drug_name, 0)
    })
})

test_that("getInteractions() applies filters by search type", {
    params <- NULL
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            params <<- variables
            key <- if (grepl("gene", queryFile)) "genes" else "drugs"
            setNames(list(list(nodes = list())), key)
        }
    )

    getInteractions("braf", search = "genes", immunotherapy = TRUE)
    expect_null(params$immunotherapy)

    getInteractions(
        "imatinib",
        search = "drugs",
        immunotherapy = TRUE,
        antineoplastic = TRUE,
        approved = TRUE
    )
    expect_true(params$immunotherapy)
    expect_true(params$antineoplastic)
    expect_true(params$approved)
})

httptest2::with_mock_api({
    test_that("getCategories() returns gene categories", {
        results <- getCategories("BRAF")
        expect_gt(length(results$gene_name), 0)
        expect_true(all(c(
            "DRUG RESISTANCE",
            "DRUGGABLE GENOME",
            "CLINICALLY ACTIONABLE"
        ) %in% results$gene_category))
    })
})

httptest2::with_mock_api({
    test_that("getSources() returns source data", {
        results <- getSources()
        expect_length(results$source_name, 45)

        sources <- getSources(sourceTypes$GENE)$source_name
        expect_length(sources, 3)
        expect_setequal(
            sources,
            c("NCBI Gene", "HUGO Gene Nomenclature Committee", "Ensembl")
        )
    })
})

httptest2::with_mock_api({
    test_that("getAllGenes() returns all genes", {
        results <- getAllGenes()
        expect_gt(length(results$gene_name), 0)
        expect_length(results$gene_concept_id, length(results$gene_name))
    })
})

test_that("getAllDrugs() returns all drugs", {
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            expect_equal(queryFile, "queries/get_all_drugs.graphql")
            expect_null(variables)
            list(drugs = list(nodes = list(list(
                name = "Imatinib",
                conceptId = "chembl:941"
            ))))
        }
    )

    results <- getAllDrugs()
    expect_equal(results$drug_name, "Imatinib")
    expect_equal(results$drug_concept_id, "chembl:941")
})

test_that("getDrugApplications() returns FDA product data", {
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            expect_equal(queryFile, "queries/get_drug_applications.graphql")
            expect_equal(variables, list(names = "Imatinib"))
            list(drugs = list(nodes = list(list(
                name = "Imatinib",
                conceptId = "chembl:941",
                drugApplications = list(list(appNo = "drugsatfda.nda:021588"))
            ))))
        },
        .getFdaProducts = function(application) {
            expect_equal(application, "NDA021588")
            list(list(
                brand_name = "GLEEVEC",
                marketing_status = "Prescription",
                dosage_form = "TABLET",
                active_ingredients = list(list(strength = "100MG"))
            ))
        }
    )

    results <- getDrugApplications("Imatinib")
    expect_equal(results$drug_name, "Imatinib")
    expect_equal(results$drug_concept_id, "chembl:941")
    expect_equal(results$drug_product_application, "NDA021588")
    expect_equal(results$drug_brand_name, "GLEEVEC")
    expect_equal(results$drug_marketing_status, "prescription")
    expect_equal(results$drug_dosage_form, "tablet")
    expect_equal(results$drug_dosage_strength, "100MG")
})
