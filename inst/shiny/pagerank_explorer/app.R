# pagerankr PageRank Explorer -- Shiny App
#
# Launch with the launch_pagerank_explorer helper from the pagerankr package,
# or point shiny runApp at this app directory inside the installed package.

library(shiny)

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
             Roboto, sans-serif; }
      .sidebar-panel { background: #f8f9fa; border-right: 1px solid #dee2e6; }
      .metric-box { background: #fff; border: 1px solid #dee2e6;
                    border-radius: 8px;
                    padding: 16px; margin: 8px 0; text-align: center; }
      .metric-box h4 { color: #6c757d; font-size: 12px;
                        text-transform: uppercase;
                        letter-spacing: 1px; margin-bottom: 4px; }
      .metric-box .value { font-size: 24px; font-weight: 700; color: #212529; }
      #graph_container { min-height: 500px; }
      .nav-tabs { margin-bottom: 16px; }
    "))
  ),

  titlePanel("PageRank Explorer"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Input"),
      fileInput("edges_file", "Edge List (CSV)", accept = ".csv"),
      fileInput("redirects_file", "Redirects (CSV, optional)", accept = ".csv"),
      fileInput("pr_file", "PageRank Results (CSV, optional)", accept = ".csv"),

      hr(),
      h4("Column Mapping"),
      textInput("from_col", "From column", value = "from"),
      textInput("to_col", "To column", value = "to"),

      hr(),
      h4("PageRank Settings"),
      sliderInput("damping", "Damping Factor", min = 0.5, max = 0.99,
                  value = 0.85, step = 0.01),
      selectInput("self_loops", "Self-loops", choices = c("drop", "keep")),
      checkboxInput("drop_isolates", "Drop Isolates", value = TRUE),
      checkboxInput("clean_urls", "Clean URLs", value = FALSE),

      hr(),
      actionButton("compute_btn", "Compute PageRank",
                   class = "btn-primary btn-block",
                   style = "width: 100%;"),

      hr(),
      h4("Graph Display"),
      sliderInput("min_pr", "Min PR to show", min = 0, max = 0.5,
                  value = 0, step = 0.001),
      sliderInput("max_nodes", "Max nodes", min = 10, max = 500,
                  value = 100, step = 10),
      checkboxInput("show_labels", "Show labels", value = TRUE),
      selectInput("layout_algo", "Layout",
                  choices = c("Force-directed" = "layout_with_fr",
                              "Kamada-Kawai" = "layout_with_kk",
                              "Circle" = "layout_in_circle",
                              "Tree" = "layout_as_tree"))
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        tabPanel(
          "Graph",
          fluidRow(
            column(3, div(
              class = "metric-box",
              h4("Nodes"), div(class = "value", textOutput("n_nodes"))
            )),
            column(3, div(
              class = "metric-box",
              h4("Edges"), div(class = "value", textOutput("n_edges"))
            )),
            column(3, div(
              class = "metric-box",
              h4("Gini"), div(class = "value", textOutput("gini_val"))
            )),
            column(3, div(
              class = "metric-box",
              h4("Max PR"), div(class = "value", textOutput("max_pr_val"))
            ))
          ),
          div(id = "graph_container",
            conditionalPanel(
              condition = "typeof visNetwork !== 'undefined'",
              uiOutput("vis_network_ui")
            ),
            plotOutput("static_graph", height = "500px")
          )
        ),

        tabPanel(
          "PageRank Table",
          DT::dataTableOutput("pr_table")
        ),

        tabPanel(
          "Distribution",
          fluidRow(
            column(6, plotOutput("pr_histogram", height = "400px")),
            column(6, plotOutput("pr_cumulative", height = "400px"))
          )
        ),

        tabPanel(
          "Redirect Audit",
          verbatimTextOutput("audit_report")
        ),

        tabPanel(
          "Export",
          h4("Download Graph"),
          downloadButton("dl_graphml", "GraphML"),
          downloadButton("dl_dot", "DOT (Graphviz)"),
          downloadButton("dl_edgelist", "Edge List CSV"),
          downloadButton("dl_pr_csv", "PageRank CSV")
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  # Reactive: loaded data
  edges_data <- reactiveVal(NULL)
  redirects_data <- reactiveVal(NULL)
  pr_data <- reactiveVal(NULL)
  graph_obj <- reactiveVal(NULL)

  # Load edges CSV
  observeEvent(input$edges_file, {
    df <- utils::read.csv(input$edges_file$datapath, stringsAsFactors = FALSE)
    edges_data(df)
  })

  # Load redirects CSV
  observeEvent(input$redirects_file, {
    df <- utils::read.csv(
      input$redirects_file$datapath,
      stringsAsFactors = FALSE
    )
    redirects_data(df)
  })

  # Load pre-computed PR CSV
  observeEvent(input$pr_file, {
    df <- utils::read.csv(input$pr_file$datapath, stringsAsFactors = FALSE)
    pr_data(df)
  })

  # Compute PageRank
  observeEvent(input$compute_btn, {
    req(edges_data())
    edges <- edges_data()

    pr_args <- list(
      edge_list_df = edges,
      edge_from_col = input$from_col,
      edge_to_col = input$to_col,
      self_loops = input$self_loops,
      drop_isolates_flag = input$drop_isolates,
      damping = input$damping,
      clean_edge_urls = input$clean_urls,
      clean_redirect_urls = input$clean_urls
    )

    redir <- redirects_data()
    if (!is.null(redir) && nrow(redir) > 0) {
      pr_args$redirects_df <- redir
    }

    tryCatch({
      result <- do.call(pagerankr::pagerank, pr_args)
      pr_data(result)

      # Build igraph for visualisation
      resolved_edges <- edges
      if (!is.null(redir) && nrow(redir) > 0) {
        resolved_edges <- pagerankr::resolve_links(
          edges, redir,
          clean_urls = input$clean_urls,
          edge_from_col = input$from_col,
          edge_to_col = input$to_col
        )
      }

      g <- igraph::graph_from_data_frame(
        data.frame(
          from = as.character(resolved_edges[[input$from_col]]),
          to = as.character(resolved_edges[[input$to_col]]),
          stringsAsFactors = FALSE
        ),
        directed = TRUE
      )

      # Attach PR scores as vertex attributes
      pr_map <- stats::setNames(result$pagerank, result$url)
      vn <- igraph::V(g)$name
      igraph::V(g)$pagerank <- unname(pr_map[vn])
      igraph::V(g)$pagerank[is.na(igraph::V(g)$pagerank)] <- 0

      graph_obj(g)

      showNotification("PageRank computed!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # --- Metrics ---
  output$n_nodes <- renderText({
    pr <- pr_data()
    if (is.null(pr)) return("--")
    nrow(pr)
  })

  output$n_edges <- renderText({
    g <- graph_obj()
    if (is.null(g)) {
      edges <- edges_data()
      if (is.null(edges)) return("--")
      return(nrow(edges))
    }
    igraph::ecount(g)
  })

  output$gini_val <- renderText({
    pr <- pr_data()
    if (is.null(pr) || nrow(pr) == 0) return("--")
    round(pagerankr::pr_gini(pr$pagerank), 3)
  })

  output$max_pr_val <- renderText({
    pr <- pr_data()
    if (is.null(pr) || nrow(pr) == 0) return("--")
    round(max(pr$pagerank), 4)
  })

  # --- Graph visualisation ---
  output$vis_network_ui <- renderUI({
    if (requireNamespace("visNetwork", quietly = TRUE)) {
      visNetwork::visNetworkOutput("vis_graph", height = "500px")
    } else {
      NULL
    }
  })

  output$vis_graph <- if (requireNamespace("visNetwork", quietly = TRUE)) {
    visNetwork::renderVisNetwork({
      g <- graph_obj()
      req(g)

      pr_scores <- igraph::V(g)$pagerank
      max_pr <- max(pr_scores, na.rm = TRUE)
      if (max_pr == 0) max_pr <- 1

      # Filter by min PR
      keep <- pr_scores >= input$min_pr
      if (sum(keep) == 0) keep <- rep(TRUE, length(keep))

      # Limit nodes
      if (sum(keep) > input$max_nodes) {
        top_idx <- order(pr_scores, decreasing = TRUE)[1:input$max_nodes]
        keep <- seq_along(pr_scores) %in% top_idx
      }

      vnames <- igraph::V(g)$name[keep]
      sub_g <- igraph::induced_subgraph(g, vnames)

      sub_pr <- igraph::V(sub_g)$pagerank
      sub_max <- max(sub_pr, na.rm = TRUE)
      if (sub_max == 0) sub_max <- 1

      nodes <- data.frame(
        id = igraph::V(sub_g)$name,
        label = if (input$show_labels) igraph::V(sub_g)$name else "",
        value = sub_pr / sub_max * 30 + 5,
        title = paste0(
          "<b>", igraph::V(sub_g)$name, "</b><br>",
          "PageRank: ", round(sub_pr, 6), "<br>",
          "Rank: ", rank(-sub_pr, ties.method = "min")
        ),
        color = grDevices::colorRampPalette(
          c("#4dabf7", "#1971c2", "#862e9c")
        )(100)[pmax(1, pmin(100, ceiling(sub_pr / sub_max * 100)))],
        stringsAsFactors = FALSE
      )

      el <- igraph::as_edgelist(sub_g)
      edges_vis <- data.frame(
        from = el[, 1], to = el[, 2],
        arrows = "to",
        color = "#adb5bd",
        stringsAsFactors = FALSE
      )

      visNetwork::visNetwork(nodes, edges_vis) |>
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1),
          nodesIdSelection = TRUE
        ) |>
        visNetwork::visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(gravitationalConstant = -50)
        ) |>
        visNetwork::visInteraction(
          navigationButtons = TRUE,
          zoomView = TRUE
        )
    })
  } else {
    NULL
  }

  # Static fallback graph (base R plot)
  output$static_graph <- renderPlot({
    if (requireNamespace("visNetwork", quietly = TRUE)) return(NULL)

    g <- graph_obj()
    req(g)

    pr_scores <- igraph::V(g)$pagerank
    max_pr <- max(pr_scores, na.rm = TRUE)
    if (max_pr == 0) max_pr <- 1

    keep <- pr_scores >= input$min_pr
    if (sum(keep) == 0) keep <- rep(TRUE, length(keep))
    if (sum(keep) > input$max_nodes) {
      top_idx <- order(pr_scores, decreasing = TRUE)[1:input$max_nodes]
      keep <- seq_along(pr_scores) %in% top_idx
    }

    vnames <- igraph::V(g)$name[keep]
    sub_g <- igraph::induced_subgraph(g, vnames)
    sub_pr <- igraph::V(sub_g)$pagerank

    layout_fn <- switch(input$layout_algo,
      "layout_with_fr" = igraph::layout_with_fr,
      "layout_with_kk" = igraph::layout_with_kk,
      "layout_in_circle" = igraph::layout_in_circle,
      "layout_as_tree" = igraph::layout_as_tree,
      igraph::layout_with_fr
    )

    lo <- layout_fn(sub_g)
    node_size <- 5 + 20 * (sub_pr / max(sub_pr, na.rm = TRUE))

    igraph::plot.igraph(
      sub_g, layout = lo,
      vertex.size = node_size,
      vertex.label = if (input$show_labels) igraph::V(sub_g)$name else NA,
      vertex.label.cex = 0.7,
      vertex.color = "#4dabf7",
      edge.arrow.size = 0.3,
      edge.color = "#adb5bd",
      main = "PageRank Graph"
    )
  })

  # --- PR Table ---
  output$pr_table <- DT::renderDataTable({
    pr <- pr_data()
    req(pr)
    pr$pagerank <- round(pr$pagerank, 8)
    pr$rank <- rank(-pr$pagerank, ties.method = "min")
    pr <- pr[order(pr$rank), ]
    DT::datatable(
      pr,
      options = list(pageLength = 25, order = list(list(2, "asc")))
    )
  })

  # --- Distribution ---
  output$pr_histogram <- renderPlot({
    pr <- pr_data()
    req(pr)
    hist(pr$pagerank, breaks = 30, col = "#4dabf7", border = "white",
         main = "PageRank Distribution", xlab = "PageRank Score",
         ylab = "Frequency")
  })

  output$pr_cumulative <- renderPlot({
    pr <- pr_data()
    req(pr)
    sorted_pr <- sort(pr$pagerank, decreasing = TRUE)
    cumshare <- cumsum(sorted_pr) / sum(sorted_pr)
    plot(seq_along(cumshare), cumshare, type = "l", lwd = 2, col = "#1971c2",
         main = "Cumulative PageRank Share",
         xlab = "Number of Pages (ranked)", ylab = "Cumulative Share",
         ylim = c(0, 1))
    abline(h = 0.5, lty = 2, col = "#adb5bd")
    abline(h = 0.8, lty = 2, col = "#adb5bd")
  })

  # --- Redirect Audit ---
  output$audit_report <- renderPrint({
    redir <- redirects_data()
    if (is.null(redir) || nrow(redir) == 0) {
      cat(
        "No redirects loaded.",
        "Upload a redirects CSV to see the audit report."
      )
      return(invisible(NULL))
    }

    edges <- edges_data()
    audit <- pagerankr::audit_redirects(
      redir,
      edge_list_df = edges,
      redirect_from_col = input$from_col,
      redirect_to_col = input$to_col
    )
    print(audit)
  })

  # --- Export downloads ---
  output$dl_graphml <- downloadHandler(
    filename = function() "pagerank_graph.graphml",
    content = function(file) {
      pr <- pr_data()
      edges <- edges_data()
      req(pr, edges)
      pagerankr::export_graph(pr, edges, file, format = "graphml",
                              edge_from_col = input$from_col,
                              edge_to_col = input$to_col)
    }
  )

  output$dl_dot <- downloadHandler(
    filename = function() "pagerank_graph.dot",
    content = function(file) {
      pr <- pr_data()
      edges <- edges_data()
      req(pr, edges)
      pagerankr::export_graph(pr, edges, file, format = "dot",
                              edge_from_col = input$from_col,
                              edge_to_col = input$to_col)
    }
  )

  output$dl_edgelist <- downloadHandler(
    filename = function() "pagerank_edges.csv",
    content = function(file) {
      pr <- pr_data()
      edges <- edges_data()
      req(pr, edges)
      pagerankr::export_graph(pr, edges, file, format = "edgelist",
                              edge_from_col = input$from_col,
                              edge_to_col = input$to_col)
    }
  )

  output$dl_pr_csv <- downloadHandler(
    filename = function() "pagerank_results.csv",
    content = function(file) {
      pr <- pr_data()
      req(pr)
      utils::write.csv(pr, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
