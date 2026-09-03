# app.R — PRISM — Pharmacovigilance Real-time Intelligence Signal Monitor
# Tabs:
#   1. Monitor Your Drug  — live FAERS query + signal status + benchmark
#   2. Reference Cohort   — historical 40-drug analysis
#   3. Drug Table         — searchable cohort data
#   4. Methodology        — signal detection math and limitations

# Packages are attached in R/00_utils.R, which Shiny sources before this file.
# They cannot live here: R/50_ui.R builds `ui` at source time and would run
# before these calls.

# ── Startup diagnostics ──────────────────────────────────────────────────────
# Logs whether an openFDA key is in effect. There is no way to inspect env vars
# on a deployed shinyapps.io instance from outside, so this line in the app log
# is the only confirmation that the key actually reached the running app.
# Prints presence and length ONLY — never the key itself (logs are not secret).
local({
  k <- openfda_api_key()
  if (nzchar(k)) {
    message(sprintf("[PRISM] openFDA API key ACTIVE (%d chars) — raised rate limit", nchar(k)))
  } else {
    message("[PRISM] no openFDA API key — running at anonymous rate limits (~1k req/day)")
  }
})

# Everything else lives in R/, which Shiny sources automatically before this file:
#   R/00_utils.R       openFDA client, PRR maths, caching, name resolution
#   R/cohort_data.R    reference cohort load + class remap + lookups
#   R/pt_terms.R       curated MedDRA Preferred Terms
#   R/signal_query.R   live query path, BBW + label coverage, signal status
#   R/timeline.R       regulatory timeline intelligence
#   R/ui.R             UI definition
# This file holds only the server logic and the app entry point.

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Tab 1: Monitor Your Drug ───────────────────────────────────────────────

  live_data <- eventReactive(input$run_check, {
    req(nchar(trimws(input$live_drug)) > 0, nchar(trimws(input$live_ae)) > 0)
    withProgress(message = "Querying FDA FAERS", value = 0,
                 detail  = "0% — starting...", {
      pull_live_signal(
        drug_name   = toupper(trimws(input$live_drug)),
        pt_term     = tolower(trimws(input$live_ae)),
        n_quarters  = 12,
        progress_cb = function(value, detail) {
          setProgress(value = value, detail = detail)
        }
      )
    })
  })

  # Generate a unique session ID for audit trail
  audit_session_id <- paste0("S-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                             sample(1000:9999, 1))

  # Write audit log after each query
  observeEvent(input$run_check, {
    df     <- live_data()
    total_a <- sum(df$count_a, na.rm = TRUE)
    max_a   <- max(df$count_a, na.rm = TRUE)
    sparse  <- total_a < 10 | max_a < 3
    status  <- if (sparse) "INSUFFICIENT DATA" else signal_status(df)
    prr     <- tail(df$PRR[!is.na(df$PRR) & is.finite(df$PRR)], 1)
    prr_lo  <- tail(df$PRR_lo[!is.na(df$PRR_lo) & is.finite(df$PRR_lo)], 1)
    prr_hi  <- tail(df$PRR_hi[!is.na(df$PRR_hi) & is.finite(df$PRR_hi)], 1)
    if (length(prr) == 0) prr <- NA
    if (length(prr_lo) == 0) prr_lo <- NA
    if (length(prr_hi) == 0) prr_hi <- NA
    write_audit_log(
      drug             = trimws(input$live_drug),
      ae               = tolower(trimws(input$live_ae)),
      status           = status,
      current_prr      = prr,
      prr_lo           = prr_lo,
      prr_hi           = prr_hi,
      n_reports        = total_a,
      quarters_queried = nrow(df),
      session_id       = audit_session_id
    )
  })

  # Shared reactive: compute status/stats once, used by all monitor outputs
  monitor_stats <- reactive({
    req(input$run_check > 0)
    df          <- live_data()
    total_a     <- sum(df$count_a, na.rm = TRUE)
    max_a       <- max(df$count_a, na.rm = TRUE)
    sparse      <- total_a < 10 | max_a < 3
    status      <- if (sparse) "INSUFFICIENT DATA" else signal_status(df)
    n_active    <- quarters_active(df)
    current_prr <- tail(df$PRR[!is.na(df$PRR) & is.finite(df$PRR)], 1)
    if (length(current_prr) == 0) current_prr <- NA
    current_prr_lo <- tail(df$PRR_lo[!is.na(df$PRR_lo) & is.finite(df$PRR_lo)], 1)
    current_prr_hi <- tail(df$PRR_hi[!is.na(df$PRR_hi) & is.finite(df$PRR_hi)], 1)
    if (length(current_prr_lo) == 0) current_prr_lo <- NA
    if (length(current_prr_hi) == 0) current_prr_hi <- NA
    prr_above_not_met <- sum(!is.na(df$PRR) & df$PRR >= 2 & !df$signal_met, na.rm = TRUE)
    months_first <- months_since_first_signal(df)
    list(df = df, status = status, n_active = n_active, months_first = months_first,
         current_prr = current_prr,
         current_prr_lo = current_prr_lo, current_prr_hi = current_prr_hi,
         prr_above_not_met = prr_above_not_met, total_a = total_a, sparse = sparse)
  })

  output$monitor_status_row <- renderUI({
    s <- monitor_stats()
    status_theme <- switch(s$status,
      "CONFIRMED"         = "danger",
      "EMERGING"          = "warning",
      "NOT DETECTED"      = "success",
      "INSUFFICIENT DATA" = "secondary")
    status_icon <- switch(s$status,
      "CONFIRMED"         = "triangle-exclamation",
      "EMERGING"          = "circle-exclamation",
      "NOT DETECTED"      = "circle-check",
      "INSUFFICIENT DATA" = "database")
    layout_columns(
      fill = FALSE,
      value_box(title = "Signal Status", value = s$status,
                showcase = icon(status_icon), theme = status_theme,
                if (s$status == "INSUFFICIENT DATA")
                  p(style = "font-size:0.78rem;",
                    "Only ", s$total_a, " report(s) found across 10 quarters.",
                    " Too sparse for reliable PRR — try the generic name or a broader AE term.")
                else if (s$status == "NOT DETECTED" && s$prr_above_not_met > 0)
                  p(style = "font-size:0.78rem;",
                    icon("triangle-exclamation"), " PRR \u2265 2 in ",
                    s$prr_above_not_met, " quarter(s) but report volume too low",
                    " (n\u00a0<\u00a03 or \u03c7\u00b2\u00a0<\u00a04) to confirm signal.")
              ),
      value_box(title = "Current PRR",
                value = if (is.na(s$current_prr)) "N/A" else round(s$current_prr, 2),
                showcase = icon("chart-line"), theme = "primary",
                p(if (is.na(s$current_prr)) "Insufficient reports"
                  else paste0("95% CI: [",
                              if (is.na(s$current_prr_lo)) "—" else round(s$current_prr_lo, 2), ", ",
                              if (is.na(s$current_prr_hi)) "—" else round(s$current_prr_hi, 2), "]")),
                p(if (!is.na(s$current_prr_lo) && s$current_prr_lo > 1)
                    "CI lower bound > 1 — signal credible"
                  else if (!is.na(s$current_prr) && s$current_prr >= 2)
                    "PRR elevated but CI includes 1"
                  else if (!is.na(s$current_prr))
                    "Below detection threshold")),
      value_box(title = "Consecutive Signal Quarters", value = s$n_active,
                showcase = icon("calendar"),
                theme = if (s$n_active >= 2) "warning" else "secondary")
    )
  })

  # ── Resolved names note: show which active ingredient is being searched
  output$resolved_names_note <- renderUI({
    req(input$run_check > 0)
    df <- live_data()
    canonical <- attr(df, "canonical_ingredient")
    input_name <- toupper(trimws(isolate(input$live_drug)))
    if (is.null(canonical)) return(NULL)
    # Only show if we resolved to a different name than what the user typed
    if (canonical == input_name) return(NULL)
    div(
      class = "alert alert-light d-flex align-items-center mb-2 py-2",
      style = "font-size:0.85rem; border-left:4px solid #6c757d;",
      icon("pills", class = "me-2"),
      span(
        tags$strong(input_name), " resolved to active ingredient ",
        tags$strong(canonical),
        " — results include all ", canonical, " products regardless of brand."
      )
    )
  })

  # ── Label change banner: check if queried drug/AE already has a known label change
  output$label_change_banner <- renderUI({
    req(input$run_check > 0)
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))

    req(nchar(drug_upper) > 0, nchar(ae_lower) > 0)
    match <- find_cohort_match(drug_upper, ae_lower)

    if (nrow(match) > 0) {
      m <- match[1, ]
      return(div(
        class = "alert alert-info d-flex align-items-center mb-2",
        style = "font-size:0.9rem; border-left:4px solid #0d6efd;",
        icon("circle-info", class = "me-2", style = "font-size:1.2rem;"),
        div(
          strong("FDA has already acted on this drug/AE. "),
          span(paste0(m$drug_name, " received a \"", m$label_change_type,
                      "\" for ", m$adverse_event, " on ",
                      format(m$label_change_date, "%B %d, %Y"), ".")),
          br(),
          span(class = "text-muted", style = "font-size:0.82rem;",
               "The signal data below reflects current FAERS reporting — useful for monitoring ongoing trends, ",
               "but the primary regulatory action has already occurred.")
        )
      ))
    }
    # If no match in our cohort, check openFDA for a BBW that matches the queried AE
    bbw <- check_boxed_warning(drug_upper)
    if (bbw$has_bbw && !is.null(bbw$bbw_text)) {
      # Check if the queried AE term (or synonyms/roots) appears in the BBW/warnings text
      bbw_lower <- tolower(bbw$bbw_text)
      ae_words <- expand_ae_terms(ae_lower)
      ae_match <- length(ae_words) > 0 && any(sapply(ae_words, function(w) grepl(w, bbw_lower, fixed = TRUE)))
      if (ae_match) {
        return(div(
          class = "alert alert-warning d-flex align-items-start mb-2",
          style = "font-size:0.9rem; border-left:4px solid #ffc107;",
          icon("exclamation-triangle", class = "me-2 mt-1", style = "font-size:1.2rem;"),
          div(
            strong("This drug has a Boxed Warning related to this adverse event. "),
            span("The FDA label for ", drug_upper,
                 " includes a Boxed Warning (the most serious safety alert) ",
                 "that covers risks relevant to your query.")
          )
        ))
      }
    }
  })

  # Reactive: does this drug/AE already have a known label change in our cohort?
  has_existing_action <- reactive({
    req(input$run_check > 0)
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))
    req(nchar(drug_upper) > 0, nchar(ae_lower) > 0)
    nrow(find_cohort_match(drug_upper, ae_lower)) > 0
  })

  output$live_chart_title <- renderUI({
    req(input$run_check > 0)
    paste0(toupper(trimws(isolate(input$live_drug))), " — ",
           tools::toTitleCase(tolower(trimws(isolate(input$live_ae)))))
  })

  output$live_provenance <- renderUI({
    req(input$run_check > 0)
    df <- live_data()
    q_time   <- attr(df, "query_time_utc")   %||% "unknown"
    q_window <- attr(df, "query_window")     %||% "unknown"
    drug_val <- toupper(trimws(isolate(input$live_drug)))
    dailymed_url <- paste0(
      "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=",
      URLencode(drug_val, reserved = TRUE))
    tagList(
      icon("fingerprint", style = "margin-right:4px;"),
      paste0("Queried ", q_time, " UTC | Window: ", q_window, " | "),
      tags$a(href = dailymed_url, target = "_blank", "View FDA label on DailyMed"),
      paste0(" | Quarters: ", nrow(df))
    )
  })

  # Reactive: resolve the queried drug's therapeutic class and filter benchmark
  bench_filtered <- reactive({
    req(input$run_check > 0)
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    queried_class <- drug_class_map[drug_upper]  # NA if not found
    class_match <- !is.na(queried_class) &&
                   queried_class %in% benchmark_drugs$therapeutic_class
    if (class_match) {
      df <- benchmark_drugs |> filter(therapeutic_class == queried_class)
    } else {
      df <- benchmark_drugs
    }
    list(
      df      = df,
      class   = if (class_match) queried_class else NULL,
      median  = median(df$lag_months),
      q25     = quantile(df$lag_months, 0.25),
      q75     = quantile(df$lag_months, 0.75),
      n       = nrow(df)
    )
  })

  output$reg_context <- renderUI({
    s             <- monitor_stats()
    b             <- bench_filtered()
    months_active <- s$months_first
    class_label   <- if (!is.null(b$class)) b$class else "all classes"
    already       <- has_existing_action()

    if (already) {
      # Drug/AE already has a label change — show historical context
      drug_upper <- toupper(trimws(isolate(input$live_drug)))
      ae_lower   <- tolower(trimws(isolate(input$live_ae)))
      m <- find_cohort_match(drug_upper, ae_lower)
      if (nrow(m) > 0) {
        m <- m[1, ]
        lag_text <- if (!is.na(m$lag_months) && m$lag_months > 0)
          paste0("The FAERS signal preceded the label change by ", round(m$lag_months, 1), " months.")
        else if (!is.na(m$lag_months) && m$lag_months <= 0)
          "The FDA acted based on clinical trial data before a clear FAERS signal emerged."
        else
          "No FAERS signal was detected for this drug/AE prior to the label change."
        tagList(
          div(class = "mb-2",
            icon("shield-halved", style = "color:#0d6efd;"),
            strong(" FDA Action on Record")
          ),
          tags$table(class = "table table-sm table-borderless mb-1",
            style = "font-size:0.85rem;",
            tags$tbody(
              tags$tr(tags$td(class="text-muted", "Action"),
                      tags$td(class="fw-semibold", m$label_change_type)),
              tags$tr(tags$td(class="text-muted", "Date"),
                      tags$td(class="fw-semibold", format(m$label_change_date, "%B %d, %Y"))),
              tags$tr(tags$td(class="text-muted", "Adverse event"),
                      tags$td(class="fw-semibold", m$adverse_event)),
              tags$tr(tags$td(class="text-muted", "Class"),
                      tags$td(class="fw-semibold", m$therapeutic_class))
            )
          ),
          p(class = "text-muted mb-0", style = "font-size:0.82rem;", lag_text)
        )
      }
    } else if (s$status == "INSUFFICIENT DATA") {
      p(class = "mb-0 text-muted",
        "Insufficient FAERS reports to compute a reliable signal. ",
        "Try searching by generic name, or check if the drug is reported under a different brand name in FAERS.")
    } else if (s$status == "NOT DETECTED") {
      if (!is.null(b$class)) {
        tagList(
          p(class = "mb-1",
            "No disproportionality signal detected in the queried period."),
          p(class = "mb-0 text-muted", style = "font-size:0.85rem;",
            "Based on ", strong(b$n), " ", class_label,
            " reference drugs, signals typically take a median of ",
            strong(paste0(round(b$median, 0), " months")), " to lead to FDA action.")
        )
      } else {
        p(class = "mb-1",
          "No disproportionality signal detected in the queried period.")
      }
    } else {
      # CONFIRMED or EMERGING
      if (!is.null(b$class)) {
        remaining <- round(b$median - months_active)
        tagList(
          p(class = "mb-1",
            "Comparing against ", strong(b$n), " ", class_label,
            " reference drug(s). Median signal-to-label lag: ",
            strong(paste0(round(b$median, 0), " months")),
            " (IQR: ", round(b$q25, 0), "\u2013", round(b$q75, 0), ")."),
          if (s$status == "CONFIRMED" && remaining > 0)
            p(class = "mb-0 mt-1",
              "Signal active ~", strong(paste0(months_active, " months")),
              ". Precedent suggests ~", remaining, " more months to FDA action.")
          else if (s$status == "CONFIRMED" && remaining <= 0)
            p(class = "mb-0 mt-1 text-danger",
              "Signal active ~", strong(paste0(months_active, " months")),
              " — exceeds the class median. Review label currency urgently.")
          else
            p(class = "mb-0 mt-1 text-muted", style = "font-size:0.85rem;",
              "Signal is early-stage. Most reference drugs required 2\u20134 confirmed quarters before FDA acted.")
        )
      } else {
        # No class match — show signal status without benchmark
        p(class = "mb-1",
          if (s$status == "CONFIRMED")
            "A disproportionality signal has been confirmed for this drug/AE pair."
          else
            "An emerging disproportionality signal has been detected for this drug/AE pair.")
      }
    }
  })

  output$rec_text <- renderUI({
    s <- monitor_stats()
    div(style = "font-size:0.88rem;",
      switch(s$status,
        "CONFIRMED" = tagList(
          strong(class = "text-danger", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Open a formal signal evaluation per ICH E2D/EMA GVP Module IX."),
            tags$li("Assess causality and clinical impact; document in your signal management system."),
            tags$li("Determine whether label update (W&P, Boxed Warning, or AR table) is warranted."),
            tags$li("Include in upcoming PSUR/PBRER with benefit-risk reassessment.")
          )
        ),
        "EMERGING" = tagList(
          strong(class = "text-warning", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Flag in your signal tracking log; set a review trigger for next quarter."),
            tags$li("Pull full case narratives for the reporting quarters to assess clinical plausibility."),
            tags$li("Do not file a label change yet — confirm signal over 2+ quarters before escalating.")
          )
        ),
        "NOT DETECTED" = tagList(
          strong(class = "text-success", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Continue scheduled FAERS surveillance (quarterly or per your SOP)."),
            tags$li("Document 'No signal detected' in your pharmacovigilance system for this period."),
            tags$li("Re-run this check after the next FAERS data refresh (~3 months).")
          )
        ),
        "INSUFFICIENT DATA" = tagList(
          strong(class = "text-secondary", "Recommended next steps:"),
          tags$ol(class = "mb-0 mt-1 ps-3",
            tags$li("Search by generic name (e.g. 'semaglutide' instead of 'OZEMPIC')."),
            tags$li("Check the openFDA FAERS database directly to confirm how this drug is reported."),
            tags$li("Consider whether the AE term is at the correct MedDRA PT level — broader terms have more reports.")
          )
        )
      )
    )
  })

  output$live_chart <- renderPlot({
    req(input$run_check > 0)
    df <- live_data()
    req(nrow(df) > 0)

    # If no reports found at all, show informative blank chart
    if (all(df$count_a == 0, na.rm = TRUE)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0("No FAERS reports found for this drug/AE combination.\n",
                                  "Check the brand name spelling or try the generic name."),
                   size = 5, color = "grey40", hjust = 0.5, vjust = 0.5) +
          theme_void()
      )
    }

    prr_max <- max(df$PRR, na.rm = TRUE)
    if (!is.finite(prr_max) || prr_max == 0) prr_max <- 2.5
    y_max <- max(prr_max * 1.15, 3)

    ggplot(df, aes(x = quarter, y = PRR)) +
      # 95% CI ribbon
      geom_ribbon(aes(ymin = pmax(PRR_lo, 0), ymax = pmin(PRR_hi, y_max)),
                  fill = "#1a3a6b", alpha = 0.12, na.rm = TRUE) +
      # CI lower bound > 1 reference line
      geom_hline(yintercept = 1, linetype = "dotted",
                 color = "grey50", linewidth = 0.6) +
      annotate("label",
        x = min(df$quarter), y = 1,
        label = "CI lower bound = 1", hjust = 0,
        vjust = 1.4,
        color = "grey50", fill = "white", label.size = NA,
        size = 2.8, fontface = "italic"
      ) +
      # Threshold line
      geom_hline(yintercept = 2, linetype = "dashed",
                 color = "darkorange", linewidth = 0.9) +
      annotate("label",
        x = min(df$quarter), y = 2,
        label = "PRR = 2  (detection threshold)", hjust = 0,
        vjust = if (prr_max >= 2) -0.4 else 1.4,
        color = "darkorange", fill = "white", label.size = NA,
        size = 3.3, fontface = "bold"
      ) +
      # PRR line
      geom_line(color = "#1a3a6b", linewidth = 1.4, na.rm = TRUE) +
      # Points coloured by signal status
      geom_point(aes(color = signal_met), size = 3.5, na.rm = TRUE) +
      scale_color_manual(
        values = c("FALSE" = "#6b7280", "TRUE" = "#e05c00"),
        labels = c("FALSE" = "Criteria not met", "TRUE" = "Signal criteria met"),
        na.value = "#6b7280", name = NULL
      ) +
      scale_y_continuous(name = "PRR", limits = c(0, y_max)) +
      scale_x_date(labels = function(d) fmt_quarter(d), date_breaks = "6 months") +
      labs(x = NULL,
           caption = paste0("Shaded band = 95% CI  \u2014  ",
                            "Signal = PRR \u2265 2, CI lower > 1, n \u2265 3, \u03c7\u00b2 \u2265 4")) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position  = "bottom",
        axis.text.x      = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank()
      )
  })

  output$raw_quarterly_table <- DT::renderDT({
    req(input$run_check > 0)
    df <- live_data()
    df |>
      mutate(
        Quarter    = fmt_quarter(quarter),
        `Drug+AE (a)` = count_a,
        `Drug only (b)` = count_b,
        `AE only (c)` = count_c,
        `All other (d)` = count_d,
        PRR        = round(PRR, 2),
        `PRR CI low`  = round(PRR_lo, 2),
        `PRR CI high` = round(PRR_hi, 2),
        `Chi-sq`   = round(chi_sq, 2),
        Signal     = ifelse(signal_met, "YES", "no")
      ) |>
      select(Quarter, `Drug+AE (a)`, `Drug only (b)`, `AE only (c)`, `All other (d)`,
             PRR, `PRR CI low`, `PRR CI high`, `Chi-sq`, Signal) |>
      DT::datatable(options = list(paging = FALSE, searching = FALSE, info = FALSE,
                                   scrollX = TRUE, ordering = FALSE,
                                   autoWidth = FALSE),
                    rownames = FALSE, style = "bootstrap4")
  })
  outputOptions(output, "raw_quarterly_table", suspendWhenHidden = FALSE)

  # ── Regulatory Timeline Intelligence ──────────────────────────────────────

  timeline_data <- reactive({
    req(input$run_check > 0)
    s <- monitor_stats()
    b <- bench_filtered()
    months_active <- s$months_first
    predict_timeline(months_active, b$class, benchmark_drugs)
  })

  # Conditional timeline card — only show when the AE is NOT already on the drug's label
  output$timeline_card <- renderUI({
    req(input$run_check > 0)
    already <- has_existing_action()
    if (already) return(NULL)

    # Check if the AE is already mentioned in the drug's FDA label
    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))
    if (check_label_covers_ae(drug_upper, ae_lower)) return(NULL)

    # Only show historical comparison when the drug maps to a known therapeutic class
    b <- bench_filtered()
    if (is.null(b$class)) return(NULL)

    card(
      card_header(icon("clock"), " Historical Context — How Similar Drugs Played Out"),
      card_body(
        uiOutput("timeline_summary"),
        plotOutput("timeline_plot", height = "180px"),
        div(class = "text-muted mt-1", style = "font-size:0.75rem;",
          "Each dot represents a drug where a FAERS signal eventually led to an FDA label change. ",
          "The yellow band shows the historical IQR. Your drug's current signal duration is marked in red.")
      )
    )
  })

  output$timeline_summary <- renderUI({
    s <- monitor_stats()

    drug_upper <- toupper(trimws(isolate(input$live_drug)))
    ae_lower   <- tolower(trimws(isolate(input$live_ae)))
    already_match <- find_cohort_match(drug_upper, ae_lower)
    already_acted <- nrow(already_match) > 0

    if (s$status %in% c("NOT DETECTED", "INSUFFICIENT DATA")) {
      return(div(class = "text-muted py-3 text-center",
               icon("clock", style = "font-size:1.5rem; color:#adb5bd;"),
               p(class = "mt-2 mb-0",
                 "Historical comparison activates when a signal is confirmed or emerging.",
                 br(), "Run a query that produces a signal to see how it compares to past drugs.")))
    }
    tl <- timeline_data()
    months_active <- tl$months_active

    risk_color <- switch(tl$risk,
      "OVERDUE" = "danger", "EXPECTED WINDOW" = "warning",
      "APPROACHING" = "info", "EARLY" = "secondary"
    )

    # Contextual plain-English explanation
    explainer <- switch(tl$risk,
      "OVERDUE" = paste0(
        "This signal has been active for ", months_active, " months — longer than ",
        tl$pct_at, "% of similar ", tl$ref_class, " drugs at the time FDA acted on them."),
      "EXPECTED WINDOW" = paste0(
        "At ", months_active, " months, this signal falls within the window ",
        "where FDA historically acted on similar ", tl$ref_class, " drugs ",
        "(IQR: ", round(tl$q25), "–", round(tl$q75), " months)."),
      "APPROACHING" = paste0(
        "This signal has been active for ", months_active, " months. ",
        "Historically, FDA action on ", tl$ref_class, " drugs began around ",
        round(tl$q25), " months from signal detection."),
      "EARLY" = paste0(
        "At ", months_active, " months, this signal is relatively early. ",
        "For reference, ", tl$ref_class, " drugs historically had a median of ",
        tl$median_lag, " months between signal detection and label change.")
    )

    # Override explainer if FDA already acted
    if (already_acted) {
      m <- already_match[1, ]
      explainer <- paste0(
        "Note: FDA already issued a \"", m$label_change_type, "\" for ",
        m$drug_name, " / ", m$adverse_event, " on ",
        format(m$label_change_date, "%B %d, %Y"),
        ". The timeline below shows how this compares to other ",
        tl$ref_class, " drugs historically.")
    }

    streak_months <- s$n_active * 3

    tagList(
      layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Signal Duration",
          value = paste0(months_active, " months"),
          showcase = icon("clock"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = risk_color,
          p(style = "font-size:0.78rem; margin:0;",
            "Since first signal detection")
        ),
        value_box(
          title = "Current Streak",
          value = if (streak_months == 0) "None" else paste0(streak_months, " months"),
          showcase = icon("arrow-trend-up"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = if (streak_months == 0) "light"
                  else if (streak_months >= 12) "danger"
                  else if (streak_months >= 6) "dark"
                  else "info",
          p(style = "font-size:0.78rem; margin:0;",
            if (streak_months == 0) "Signal not active in latest quarter"
            else paste0(s$n_active, " consecutive quarter", if (s$n_active != 1) "s"))
        ),
        value_box(
          title = "Historical Median",
          value = paste0(tl$median_lag, " months"),
          showcase = icon("chart-line"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = "primary",
          p(style = "font-size:0.78rem; margin:0;",
            "IQR: ", round(tl$q25), "–", round(tl$q75), " mo for ", tl$ref_class)
        ),
        value_box(
          title = "Historical Precedent",
          value = paste0(tl$pct_at, "%"),
          showcase = icon("chart-bar"),
          showcase_layout = showcase_left_center(width = 0.15),
          theme = "secondary",
          p(style = "font-size:0.78rem; margin:0;",
            "of ", tl$n_ref, " ", tl$ref_class, " drugs had changes by this point")
        )
      ),
      div(class = "mt-2 p-2", style = "background:#f8f9fa; border-radius:6px; font-size:0.85rem;",
        icon("lightbulb", style = "color:#d97706; margin-right:4px;"),
        explainer
      )
    )
  })

  output$timeline_plot <- renderPlot({
    s <- monitor_stats()
    if (s$status %in% c("NOT DETECTED", "INSUFFICIENT DATA")) return(NULL)
    tl <- timeline_data()
    b  <- bench_filtered()
    ref <- b$df
    months_active <- tl$months_active
    x_max <- max(c(ref$lag_months, months_active) * 1.15, 24)

    # Dodge median vs "now" labels when close
    too_close <- abs(tl$median_lag - months_active) < (x_max * 0.12)
    med_hjust <- if (too_close && months_active <= tl$median_lag) 1.1 else -0.1
    now_hjust <- if (too_close && months_active > tl$median_lag) 1.1 else -0.1
    med_y <- if (too_close) 1.15 else 1.05
    now_y <- if (too_close) 0.95 else 1.05

    p <- ggplot(ref, aes(x = lag_months, y = 0.5)) +
      # IQR band
      annotate("rect", xmin = tl$q25, xmax = tl$q75, ymin = 0.15, ymax = 0.85,
               fill = "#fde68a", alpha = 0.5) +
      # Median line
      geom_vline(xintercept = tl$median_lag, color = "#d97706",
                 linetype = "solid", linewidth = 0.9) +
      annotate("text", x = tl$median_lag, y = med_y,
               label = paste0("Median: ", tl$median_lag, " mo"),
               hjust = med_hjust, color = "#d97706", size = 3.2, fontface = "bold") +
      # Reference drug dots
      geom_point(size = 5, color = "#6366f1", alpha = 0.8) +
      ggrepel::geom_text_repel(
        aes(label = drug_name), size = 2.6, color = "grey30",
        nudge_y = 0.25, segment.size = 0.25, segment.color = "grey60",
        max.overlaps = 20, seed = 42
      ) +
      # "You are here" marker
      geom_vline(xintercept = months_active, color = "#dc2626",
                 linetype = "dotted", linewidth = 1.1) +
      annotate("text", x = months_active, y = now_y,
               label = paste0("You: ", months_active, " mo"),
               hjust = now_hjust, color = "#dc2626", size = 3.2, fontface = "bold") +
      scale_x_continuous(
        name = "Months from signal to FDA label action",
        limits = c(0, x_max),
        breaks = seq(0, 200, by = 12),
        labels = function(x) paste0(x, " mo")
      ) +
      scale_y_continuous(limits = c(-0.1, 1.4)) +
      labs(y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()
      )
    p
  })


  # ── Tab 2: Reference Cohort ────────────────────────────────────────────────

  # Update drug dropdown when class/signal filters change
  observe({
    filtered <- combined
    if (nchar(input$class_filter) > 0) {
      filtered <- filtered |> filter(therapeutic_class == input$class_filter)
    }
    if (nchar(input$signal_filter) > 0) {
      if (input$signal_filter == "yes") {
        filtered <- filtered |> filter(!is.na(signal_start_quarter))
      } else {
        filtered <- filtered |> filter(is.na(signal_start_quarter))
      }
    }
    choices <- sort(toupper(filtered$drug_name))
    current <- isolate(input$drug_select)
    selected <- if (current %in% choices) current else choices[1]
    updateSelectInput(session, "drug_select", choices = choices, selected = selected)
  })

  # Reference Cohort filters, as a reactive so the chart and the drug dropdown
  # share one definition. (bench_filtered() is NOT this — that one belongs to the
  # Monitor tab, req()s a live query, and returns a summary list, not rows.)
  cohort_filtered <- reactive({
    d <- combined
    if (nchar(input$class_filter) > 0) {
      d <- d |> filter(therapeutic_class == input$class_filter)
    }
    if (nchar(input$signal_filter) > 0) {
      d <- if (input$signal_filter == "yes") {
        d |> filter(!is.na(signal_start_quarter))
      } else {
        d |> filter(is.na(signal_start_quarter))
      }
    }
    d
  })

  # Cohort lag overview — the Reference Cohort's primary chart. Honours the
  # sidebar filters, so narrowing the class narrows the chart, not just the
  # drug dropdown.
  # Height scales with the number of rows. A fixed height silently crushes the
  # drug labels once the cohort grows — at 42 drugs in 720px each row got ~17px,
  # which is less than the line height of the 12pt axis text, so names collided
  # with their own segments. Sizing per row keeps the spacing constant instead.
  cohort_lag_height <- reactive({
    d  <- cohort_filtered()
    n  <- sum(!is.na(d$lag_months))
    nc <- length(unique(d$therapeutic_class))
    if (n == 0) return(260)
    # 30px per drug row + ~26px per facet strip and its spacing + chrome
    as.integer(min(2600, max(420, 30 * n + 26 * nc + 150)))
  })

  output$cohort_lag <- renderPlot({
    d <- cohort_filtered()
    # Facet only when more than one class is in view; a single-class facet strip
    # is pure noise.
    plot_cohort_lag(d, facet_by_class = length(unique(d$therapeutic_class)) > 1)
  }, height = function() cohort_lag_height())

  # Per-drug drill-down for the Reference Cohort.
  #
  # This replaced a dual-axis chart that drew quarterly report counts as bars and
  # PRR as a line on a secondary axis, tied together by an arbitrary scaling
  # factor (sf <- count_max / prr_max). The apparent relationship between the two
  # series was an artefact of that constant, and five encodings (period-coloured
  # bars, the line, a threshold rule, and two labelled vertical rules) competed in
  # one panel.
  #
  # One y-axis now. PRR is the only quantity on it; report count becomes point
  # SIZE, which is honest — a bigger dot is a better-supported estimate. The y
  # axis is log10 because PRR spans two orders of magnitude across the cohort
  # (Ambien/somnambulism reaches ~58 against a typical 2-5), and a linear axis
  # flattens every ordinary drug to the baseline.
  output$prr_trend <- renderPlot({
    req(input$drug_select)

    drug_signals <- signals |>
      filter(drug == input$drug_select, !is.na(PRR), is.finite(PRR), PRR > 0)

    drug_meta    <- combined |> filter(toupper(drug_name) == input$drug_select)
    label_date   <- drug_meta$label_change_date[1]
    signal_date  <- drug_meta$signal_start_date[1]
    pt_label     <- drug_meta$adverse_event[1]

    # A drug can have NO finite PRR in any quarter — compute_prr() returns NA
    # whenever a marginal or reconstructed cell is degenerate, which is the norm
    # for a rare PT on a low-volume product (CAR-T + "t-cell lymphoma" is the
    # obvious case here). Without this, count_max becomes -Inf and range() on an
    # empty vector makes flip_at NaN, so `if (label_date > flip_at)` throws
    # "missing value where TRUE/FALSE needed" — a hard error in the panel.
    if (nrow(drug_signals) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, size = 5, colour = "grey35",
                   label = paste0(
                     "No quarter for ", input$drug_select,
                     " has enough reports to compute a PRR.\n",
                     "Expected for a rare event on a low-volume product —\n",
                     "absence of a signal is not evidence of absence of risk.")) +
          theme_void()
      )
    }

    drug_signals <- drug_signals |>
      mutate(met = ifelse(signal_met, "Met signal criteria", "Below criteria"))

    date_range <- range(drug_signals$quarter)
    # Annotation boxes are drawn to the RIGHT of their line by default, so a line
    # near the end of the window pushes the box off the panel. 15 of the 42
    # cohort drugs hit this (Ambien at 87%, Yescarta at 81%, the PPIs, statins,
    # Z-drugs, Vioxx, Xeljanz). Flip to the left past 70% of the window.
    flip_at  <- date_range[1] + (date_range[2] - date_range[1]) * 0.70
    prr_top  <- max(drug_signals$PRR, na.rm = TRUE)

    p <- ggplot(drug_signals, aes(x = quarter, y = PRR)) +
      geom_hline(yintercept = 2, linetype = "dashed",
                 colour = "#e05c00", linewidth = 0.8) +
      geom_line(colour = "grey55", linewidth = 0.6) +
      geom_point(aes(size = count_a, fill = met),
                 shape = 21, colour = "white", stroke = 0.7, alpha = 0.95) +
      scale_fill_manual(
        values = c("Met signal criteria" = "#1e1b4b", "Below criteria" = "#b9c2d0"),
        name = NULL) +
      scale_size_area(max_size = 11, name = "Reports in quarter") +
      scale_y_log10() +
      scale_x_date(expand = expansion(mult = c(0.06, 0.06))) +
      labs(
        title    = paste0(input$drug_select, " — ", pt_label),
        subtitle = "Quarterly PRR (log scale). Dot size is the report count; the dashed line is the PRR = 2 signal threshold.",
        x = NULL, y = "PRR"
      ) +
      guides(fill = guide_legend(order = 1, override.aes = list(size = 5)),
             size = guide_legend(order = 2)) +
      theme_minimal(base_size = 14) +
      theme(
        # Top-right, ABOVE the panel rather than floating inside it. An in-panel
        # legend at (0.98, 0.98) would collide with the "Label change" box: for a
        # late label change the box flips to the left of its line and lands in
        # the upper right of the plotting area (Ambien at 87% of its window,
        # Yescarta at 81%). Sitting outside the panel keeps it clear of both
        # annotation boxes at any date.
        legend.position      = "top",
        legend.justification = "right",
        legend.box           = "horizontal",
        legend.box.spacing   = grid::unit(4, "pt"),
        legend.margin        = margin(0, 0, 2, 0),
        panel.border       = element_rect(colour = "grey55", fill = NA, linewidth = 0.7),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.4),
        axis.ticks         = element_line(colour = "grey55", linewidth = 0.4),
        axis.ticks.length  = grid::unit(3, "pt"),
        axis.text          = element_text(size = 12),
        plot.title         = element_text(face = "bold", size = 16),
        plot.subtitle      = element_text(size = 11.5, colour = "grey35"),
        plot.margin        = margin(12, 16, 12, 12)
      )

    if (!is.na(label_date)) {
      p <- p +
        geom_vline(xintercept = as.numeric(label_date),
                   linetype = "solid", colour = "firebrick", linewidth = 1.1) +
        annotate("label",
          x = label_date, y = prr_top, vjust = 1,
          label = paste0("Label change\n", format(label_date, "%b %Y")),
          hjust = if (label_date > flip_at) 1.05 else -0.05,
          colour = "firebrick", fill = "white", label.size = NA,
          size = 4, fontface = "bold")
    }

    if (!is.na(signal_date)) {
      p <- p +
        geom_vline(xintercept = as.numeric(signal_date),
                   linetype = "dotted", colour = "darkgreen", linewidth = 1.1) +
        annotate("label",
          x = signal_date, y = min(drug_signals$PRR, na.rm = TRUE), vjust = 0,
          label = paste0("Signal confirmed\n", format(signal_date, "%b %Y")),
          hjust = if (signal_date > flip_at) 1.05 else -0.05,
          colour = "darkgreen", fill = "white", label.size = NA,
          size = 4, fontface = "bold")
    }

    p
  })

  output$drug_info_box <- renderUI({
    req(input$drug_select)
    meta <- combined |> filter(toupper(drug_name) == input$drug_select)
    lag  <- meta$lag_months[1]
    tc   <- meta$therapeutic_class[1]
    ae   <- meta$adverse_event[1]
    lag_color <- if (is.na(lag)) "text-secondary"
                 else if (lag < 0) "text-danger"
                 else if (lag < 24) "text-success"
                 else "text-warning"

    # Detection limitation flag (reuses pre-computed helpers)
    det_type       <- get_detection_type(tc, ae)
    detection_note <- detection_alert(det_type)

    # Footnote for negative lag (signal came after label change)
    lag_footnote <- if (!is.na(lag) && lag < 0 && is.null(detection_note))
      tags$small(class="text-muted fst-italic",
        "\u2020 FDA acted before FAERS signal emerged")

    div(class = "mt-1",
      div(class = "mb-3",
        div(class = "fw-bold fs-5 lh-sm", meta$drug_name[1]),
        div(class = "text-muted fst-italic", style = "font-size:0.85rem;",
            meta$generic_name[1])
      ),
      tags$table(class = "table table-sm table-borderless mb-1",
        style = "font-size:0.85rem; table-layout:fixed; width:100%;",
        tags$colgroup(tags$col(style="width:42%;"), tags$col(style="width:58%;")),
        tags$tbody(
          tags$tr(tags$td(class="text-muted","Class"),
                  tags$td(class="fw-semibold", meta$therapeutic_class[1])),
          tags$tr(tags$td(class="text-muted","Approved"),
                  tags$td(class="fw-semibold", meta$approval_year[1])),
          tags$tr(
            style = "background-color:#f0f4ff;",
            tags$td(class="text-muted fw-semibold","AE tracked"),
            tags$td(class="fw-bold", style="color:#1a3a6b;",
                    tools::toTitleCase(meta$adverse_event[1]))
          ),
          tags$tr(tags$td(class="text-muted","Label change"),
                  tags$td(class="fw-semibold", format(meta$label_change_date[1], "%b %d, %Y"))),
          tags$tr(tags$td(class="text-muted","Change type"),
                  tags$td(class="fw-semibold", meta$label_change_type[1])),
          tags$tr(tags$td(class="text-muted","Signal lag"),
                  tags$td(class=paste("fw-bold", lag_color),
                    if (is.na(lag)) "No FAERS signal"
                    else if (lag < 0) paste0(abs(lag), " mo early\u2020")
                    else paste0(lag, " months")))
        )
      ),
      lag_footnote,
      detection_note
    )
  })


  # ── Tab 3: Drug Table ──────────────────────────────────────────────────────

  output$drug_table <- DT::renderDT({
    tbl <- combined |>
      mutate(
        Brand = ifelse(nchar(Note) > 0,
          paste0('<span style="display:inline-flex;align-items:center;white-space:nowrap;">',
                 htmltools::htmlEscape(drug_name),
                 '<span data-bs-toggle="tooltip" data-bs-placement="right" title="',
                 htmltools::htmlEscape(Note), '"',
                 ' style="cursor:pointer;color:#d97706;font-size:0.85rem;margin-left:5px;">',
                 '<i class="fa fa-circle-info"></i></span></span>'),
          drug_name)
      ) |>
      select(
        Brand,
        Generic          = generic_name,
        Class            = therapeutic_class,
        `Adverse Event`  = adverse_event,
        `Signal Quarter` = signal_start_quarter,
        `Label Change`   = label_change_date,
        `Change Type`    = label_change_type,
        `Lag (months)`   = lag_months
      )
    DT::datatable(tbl,
      escape = FALSE,
      options = list(
        pageLength = 10, scrollX = TRUE, autoWidth = FALSE,
        columnDefs = list(
          list(width = "90px", targets = c(4, 5)),
          list(width = "80px", targets = 7)
        ),
        initComplete = DT::JS(
          "function(settings, json) {",
          "  var tooltipTriggerList = [].slice.call(",
          "    document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));",
          "  tooltipTriggerList.map(function(el) {",
          "    return new bootstrap.Tooltip(el);",
          "  });",
          "}"
        ),
        drawCallback = DT::JS(
          "function(settings) {",
          "  var tooltipTriggerList = [].slice.call(",
          "    this.api().table().container().querySelectorAll('[data-bs-toggle=\"tooltip\"]'));",
          "  tooltipTriggerList.map(function(el) {",
          "    return new bootstrap.Tooltip(el);",
          "  });",
          "}"
        )
      ),
      rownames = FALSE, filter = "top", style = "bootstrap4")
  })
}

shinyApp(ui, server)
