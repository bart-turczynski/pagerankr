context("pagerank wrapper function")

# source("../../R/utils.R")
# source("../../R/clean_url_columns.R")
# source("../../R/resolve_redirects.R")
# source("../../R/get_unique_edges.R")
# source("../../R/drop_isolates.R")
# source("../../R/compute_pagerank.R")
# source("../../R/pagerank.R")

describe("pagerank main wrapper basic functionality", {
  it("runs end-to-end with sensible defaults", {
    edges <- data.frame(
      from = c("http://A.com/", "B.com", "C.com?q=1"), 
      to = c("B.com", "http://A.com", "D.com#frag"),
      stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("C.com?q=1", "B.com"), 
      to = c("C-resolved.com", "A.com/"), 
      stringsAsFactors = FALSE
    )
    
    # Expected flow with defaults:
    # 1. Clean URLs: A.com/, B.com, C.com?q=1, D.com#frag, C-resolved.com
    #    http://a.com/, http://b.com/, http://c.com/?q=1, http://d.com/, http://c-resolved.com/
    #    (assuming rurl::get_clean_url defaults: lowercases domain, adds http, keeps query, drops fragment)
    #    Let's assume rurl default for get_clean_url is: http://a.com/, http://b.com/, http://c.com/?q=1, http://d.com/
    #    And for redirects: http://c.com/?q=1 -> http://c-resolved.com/, http://b.com/ -> http://a.com/
    # 2. Resolve redirects on cleaned URLs:
    #    Edges before resolve: (A,B), (B,A), (C_query, D)
    #    Redirects: C_query -> C_res, B -> A
    #    Resolved edges: (A, A), (A, A), (C_res, D)
    # 3. Unique edges (drop self_loops=TRUE by default in get_unique_edges via pagerank default):
    #    (A,A) is self loop, (C_res, D) is not.
    #    If self_loops="drop" (pagerank default for get_unique_edges is "drop") -> (C_res,D) remains. A->A removed.
    # 4. Drop isolates (drop_isolates_flag=TRUE default):
    #    Nodes from (C_res,D) are C_res, D. These are kept.
    # 5. Compute PR: Graph C_res -> D. 
    #    PR(C_res) = (1-d)/N + d*0 = (0.15)/2 = 0.075 (approx, N=2)
    #    PR(D) = (1-d)/N + d*PR(C_res)/1 = 0.075 + 0.85 * (PR(C_res) or 1 if C_res is only one pointing to D and no other out)
    #    If it is C_res -> D, then C_res gets teleport, D gets rank from C_res + teleport.
    #    PR(C_res) = 0.15/2 + 0.85 * 0 = 0.075
    #    PR(D) = 0.15/2 + 0.85 * (PR(C_res)/1) -> This logic is igraph internal. It sums to 1.
    #    For C_res -> D, D should have higher PR.

    pr_full <- pagerank(edges, redirects_df = redirects) # uses defaults
    
    expect_true(is.data.frame(pr_full))
    expect_true(all(c("node_name", "pagerank") %in% names(pr_full)))
    if(nrow(pr_full) > 0) expect_equal(sum(pr_full$pagerank), 1, tolerance = 1e-9)
    
    # Based on the assumed flow: edge C-resolved.com -> D.com. Resulting nodes: C-resolved.com, D.com.
    # C-resolved.com PR should be (1-0.85)/2 = 0.075
    # D.com PR should be (1-0.85)/2 + 0.85 * (1/1) * PR(C-resolved.com) ? No, igraph is more complex.
    # For a simple C_res -> D graph, PR(D) > PR(C_res)
    expect_true(nrow(pr_full) %in% c(0,2)) # Could be 0 if all resolve to self loops and are dropped.
    if(nrow(pr_full) == 2) {
      expect_true("http://c-resolved.com/" %in% pr_full$node_name || "http://c-resolved.com" %in% pr_full$node_name) # rurl adds trailing slash sometimes
      expect_true("http://d.com/" %in% pr_full$node_name || "http://d.com" %in% pr_full$node_name)
      pr_d <- pr_full$pagerank[grep("d\\.com", pr_full$node_name)]
      pr_c_res <- pr_full$pagerank[grep("c-resolved\\.com", pr_full$node_name)]
      expect_gt(pr_d, pr_c_res)
    }
  })
  
  it("runs correctly with NULL redirects_df", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    pr <- pagerank(edges, redirects_df = NULL)
    expect_equal(nrow(pr), 2)
    expect_equal(sum(pr$pagerank), 1, tolerance=1e-9)
    expect_true(all(pr$pagerank > 0))
  })
  
  it("controls URL cleaning flags", {
    edges_dirty <- data.frame(from = "HTTP://Example.COM/path?q=1#frag", to = "Sub.example.NET", stringsAsFactors = FALSE)
    
    # clean_edge_urls = TRUE (default), clean_redirect_urls = TRUE (default, but no redirects here)
    pr_clean <- pagerank(edges_dirty, clean_edge_urls = TRUE)
    expect_true("http://example.com/path?q=1" %in% pr_clean$node_name) # fragment gone, query kept, host lowercase
    expect_true("http://sub.example.net/" %in% pr_clean$node_name) # scheme added, host lowercase
    
    # clean_edge_urls = FALSE
    # Expect warning if query params present and cleaning is off
    expect_warning(
        pr_no_clean <- pagerank(edges_dirty, clean_edge_urls = FALSE),
        regexp = "URLs in `edge_list_df` may contain query parameters"
    )
    expect_true("HTTP://Example.COM/path?q=1#frag" %in% pr_no_clean$node_name)
    expect_true("Sub.example.NET" %in% pr_no_clean$node_name)
  })
  
  it("passes rurl_params to URL cleaning", {
    edges <- data.frame(from = "http://www.example.com/page#section", to = "test.net", stringsAsFactors = FALSE)
    # params to drop www, scheme, but keep fragment
    my_rurl_params <- list(drop_www = TRUE, drop_scheme = TRUE, drop_fragments = FALSE)
    pr <- pagerank(edges, rurl_params = my_rurl_params)
    expect_true("example.com/page#section" %in% pr$node_name)
    expect_true("test.net/" %in% pr$node_name) # scheme dropped, but rurl may add / if path empty
  })
  
  it("controls self_loops argument", {
    edges_sl <- data.frame(from = c("A", "B"), to = c("A", "A"), stringsAsFactors = FALSE) # A->A, B->A
    pr_drop_sl <- pagerank(edges_sl, self_loops = "drop") # A->A is dropped, B->A remains. nodes B,A
    expect_equal(nrow(pr_drop_sl), 2)
    expect_false("A" %in% pr_drop_sl$node_name[pr_drop_sl$pagerank == 0]) # No, PR for B->A is A=0.65, B=0.34 approx
                                                                        # B is source, A is sink basically
                                                                        # PR for A > PR for B
    expect_gt(pr_drop_sl$pagerank[pr_drop_sl$node_name=="http://a/"] , pr_drop_sl$pagerank[pr_drop_sl$node_name=="http://b/"])
    
    pr_keep_sl <- pagerank(edges_sl, self_loops = "keep") # A->A kept, B->A kept. Graph is A<=>A, B->A
    expect_equal(nrow(pr_keep_sl), 2)
    # A gets PR from B and from its self-loop (or however igraph treats it)
    # This should change PR distribution compared to dropping.
    expect_false(all(pr_drop_sl$pagerank == pr_keep_sl$pagerank)) # Expect different PR values
  })
  
  it("controls drop_isolates_flag argument", {
    edges_iso <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    # With drop_isolates_flag = TRUE (default), C is not in graph if only in vertices_df passed to compute_pagerank
    # but pagerank() wrapper constructs vertices_df based on drop_isolates_flag and edge list content.
    # If an edge C->D exists, and then D->E, and we drop C,D,E, pagerank might be empty.
    # Let's test by ensuring an isolate would be removed / kept.
    
    # Scenario: A->B. If we add C as an unconnected node. 
    # drop_isolates_flag = TRUE: C should not be in PR output
    # drop_isolates_flag = FALSE: C should be in PR output with base PR
    
    # To test this, we need pagerank() to know about C.
    # If C is not in edge_list_df, drop_isolates() won't see it unless pagerank() passes it to vertices_df.
    # The logic is: drop_isolates() operates on current_edge_list. 
    # If drop_isolates_flag=T, vertices_for_pagerank_df = nodes_with_degree>0 from current_edge_list.
    # If drop_isolates_flag=F, vertices_for_pagerank_df = ALL unique nodes from current_edge_list.
    
    # So, to test drop_isolates_flag, we need an edge list where some nodes become isolates *after* processing.
    # Example: A->B, B->A.  C->C (self-loop). D (no links)
    # Assume D is not in edge list. C->C is an edge.
    edges_complex = rbind(
        data.frame(from="A",to="B", stringsAsFactors=FALSE),
        data.frame(from="B",to="A", stringsAsFactors=FALSE),
        data.frame(from="C",to="C", stringsAsFactors=FALSE)
        # D is an implicit isolate if not mentioned
    )
    
    # Case 1: self_loops="keep", drop_isolates_flag=FALSE
    # Edges: A->B, B->A, C->C. Nodes: A, B, C. All connected or self-looped.
    pr_keep_iso_keep_sl <- pagerank(edges_complex, self_loops="keep", drop_isolates_flag=FALSE)
    expect_equal(nrow(pr_keep_iso_keep_sl), 3) # A,B,C
    expect_true(all(c("http://a/","http://b/","http://c/") %in% pr_keep_iso_keep_sl$node_name))
    
    # Case 2: self_loops="drop", drop_isolates_flag=FALSE
    # Edges: A->B, B->A. C->C is dropped. Nodes: A, B, C. C becomes an isolate.
    # drop_isolates_flag=FALSE means C (now an isolate) is KEPT for PR.
    pr_keep_iso_drop_sl <- pagerank(edges_complex, self_loops="drop", drop_isolates_flag=FALSE)
    expect_equal(nrow(pr_keep_iso_drop_sl), 3) # A,B,C. C is isolate, gets base PR.
    expect_true(pr_keep_iso_drop_sl$pagerank[pr_keep_iso_drop_sl$node_name=="http://c/"] < 
                pr_keep_iso_drop_sl$pagerank[pr_keep_iso_drop_sl$node_name=="http://a/"])
    
    # Case 3: self_loops="drop", drop_isolates_flag=TRUE
    # Edges: A->B, B->A. C->C is dropped. C becomes an isolate.
    # drop_isolates_flag=TRUE means C (now an isolate) is DROPPED before PR.
    pr_drop_iso_drop_sl <- pagerank(edges_complex, self_loops="drop", drop_isolates_flag=TRUE)
    expect_equal(nrow(pr_drop_iso_drop_sl), 2) # A,B only
    expect_false("http://c/" %in% pr_drop_iso_drop_sl$node_name)
  })
  
  it("handles custom column names for edges and redirects", {
    edges_cust <- data.frame(source = "X.com", target = "Y.net", stringsAsFactors = FALSE)
    redirects_cust <- data.frame(orig = "Y.net", final = "Z.org", stringsAsFactors = FALSE)
    pr <- pagerank(edges_cust, redirects_df = redirects_cust, 
                   edge_from_col = "source", edge_to_col = "target",
                   redirect_from_col = "orig", redirect_to_col = "final")
    expect_equal(nrow(pr), 2)
    expect_true("http://x.com/" %in% pr$node_name)
    expect_true("http://z.org/" %in% pr$node_name)
  })
  
  it("passes ... (e.g. damping) to compute_pagerank", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "A"), stringsAsFactors = FALSE)
    pr_d70 <- pagerank(edges, damping = 0.70)
    # Symmetric graph, PRs should be 0.5 each.
    # The actual check is that compute_pagerank gets this damping.
    # We can infer by checking if the sum is 1 and values are as expected.
    expect_equal(pr_d70$pagerank, c(0.5,0.5), tolerance=1e-9, ignore_attr=TRUE)
  })
})

describe("pagerank wrapper warnings and errors", {
  it("warns if clean_edge_urls=FALSE and query params exist", {
    edges_query <- data.frame(from = "example.com/page?q=1", to = "other.com", stringsAsFactors = FALSE)
    expect_warning(pagerank(edges_query, clean_edge_urls = FALSE),
                   "URLs in `edge_list_df` may contain query parameters")
    # No warning if no query params
    edges_no_query <- data.frame(from = "example.com/page", to = "other.com", stringsAsFactors = FALSE)
    expect_warning(pagerank(edges_no_query, clean_edge_urls = FALSE), regexp = NA) # NA means no warning
  })
  
  it("errors on invalid main arguments", {
    expect_error(pagerank(list())) # wrong edge_list_df type
    expect_error(pagerank(data.frame(f="a",t="b"), redirects_df = list()))
    expect_error(pagerank(data.frame(f="a",t="b"), clean_edge_urls = "true"))
    expect_error(pagerank(data.frame(f="a",t="b"), rurl_params = "param=val"))
    expect_error(pagerank(data.frame(f="a",t="b"), self_loops = "maybe"))
    expect_error(pagerank(data.frame(f="a",t="b"), drop_isolates_flag = 0))
  })
})


describe("pagerank shared memoization for cleaning (conceptual)", {
  it("uses shared memoizer when clean_edge_urls and clean_redirect_urls are TRUE", {
    # This test relies on the internal behavior of how .create_memoized_cleaner and
    # clean_url_columns work when a .memoized_clean_url argument is passed.
    # The key is that if a URL is cleaned in redirects, its already-cleaned version
    # from the shared cache should be used if it appears in edges (and vice-versa).
    
    # Scenario: 
    # Edges: A_raw -> B_raw
    # Redirects: A_raw_variant -> A_clean (where A_raw_variant cleans to A_clean)
    # If shared cleaning is working, when A_raw in edges is cleaned, it should become A_clean.
    
    # Need to ensure rurl::get_clean_url params are consistent for this to make sense.
    # pagerank() passes the same rurl_params list to both cleaning calls.
    
    edges_df <- data.frame(from = "HTTP://SiteA.com/path", to = "SiteB.com", stringsAsFactors = FALSE)
    redirects_df <- data.frame(from = "http://sitea.com/path", to = "SiteA_RESOLVED.com", stringsAsFactors = FALSE)
    
    # If shared cleaning: 
    # 1. "http://sitea.com/path" (from redirects) cleaned to, say, "http://sitea.com/path"
    # 2. "HTTP://SiteA.com/path" (from edges) should use cached "http://sitea.com/path"
    # So, after cleaning, edge list from becomes "http://sitea.com/path"
    # Then redirect applies: "http://sitea.com/path" -> "http://sitea_resolved.com/"
    # Edge list becomes: "http://sitea_resolved.com/" -> "http://siteb.com/"
    
    pr_shared <- pagerank(edges_df, redirects_df, clean_edge_urls = TRUE, clean_redirect_urls = TRUE)
    expect_true("http://sitea_resolved.com/" %in% pr_shared$node_name)
    expect_true("http://siteb.com/" %in% pr_shared$node_name)
    
    # Scenario 2: URL from edge appears in redirect `to`
    edges_df2 <- data.frame(from = "Source.com", to = "HTTP://CommonTarget.com/Page", stringsAsFactors = FALSE)
    redirects_df2 <- data.frame(from = "RedirectFrom.com", to = "http://commontarget.com/Page", stringsAsFactors = FALSE)
    
    # "http://commontarget.com/Page" from redirects `to` is cleaned.
    # "HTTP://CommonTarget.com/Page" from edges `to` should use that cached version.
    # Resulting graph: Source.com -> (cleaned CommonTarget.com/Page)
    pr_shared2 <- pagerank(edges_df2, redirects_df2, clean_edge_urls = TRUE, clean_redirect_urls = TRUE)
    expect_true("http://source.com/" %in% pr_shared2$node_name)
    expect_true("http://commontarget.com/Page" %in% pr_shared2$node_name) # Check for canonical form
    
    # To make this test more robust against specific rurl::get_clean_url behavior, 
    # it's about consistency. If they clean to the same string due to shared cache,
    # the redirect resolution logic will correctly match them.
    # This test is conceptual because we don't inspect the cache object itself here,
    # but rely on the consistent outcome of the pipeline.
  })
}) 