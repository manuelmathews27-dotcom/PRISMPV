# ui.R — Shiny UI definition
# Layout, theme, and the four nav panels. Server logic lives in app.R.
# Sourced automatically by Shiny before app.R (all files in R/ are).

# ── UI ────────────────────────────────────────────────────────────────────────
prism_logo <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 48 40",
  width = "72", height = "60", style = "vertical-align:middle;",
  tags$defs(
    tags$linearGradient(id = "pg", x1 = "0%", y1 = "0%", x2 = "100%", y2 = "100%",
      tags$stop(offset = "0%",   style = "stop-color:#c4b5fd;stop-opacity:1"),
      tags$stop(offset = "100%", style = "stop-color:#818cf8;stop-opacity:1")
    )
  ),
  # prism triangle body
  tags$polygon(points = "24,2 46,38 2,38",
               fill = "url(#pg)", stroke = "rgba(255,255,255,0.4)",
               `stroke-width` = "1.2"),
  # incoming white beam (left side)
  tags$line(x1 = "0", y1 = "18", x2 = "13", y2 = "24",
            stroke = "white", `stroke-width` = "2", opacity = "0.85"),
  # outgoing rainbow beams (right side)
  tags$line(x1 = "35", y1 = "22", x2 = "48", y2 = "12",
            stroke = "#f87171", `stroke-width` = "1.8", opacity = "0.95"),
  tags$line(x1 = "35", y1 = "25", x2 = "48", y2 = "19",
            stroke = "#fbbf24", `stroke-width` = "1.8", opacity = "0.95"),
  tags$line(x1 = "35", y1 = "28", x2 = "48", y2 = "26",
            stroke = "#34d399", `stroke-width` = "1.8", opacity = "0.95"),
  tags$line(x1 = "35", y1 = "31", x2 = "48", y2 = "33",
            stroke = "#60a5fa", `stroke-width` = "1.8", opacity = "0.95")
)

prism_title <- tags$div(
  style = "display:flex; align-items:center; gap:14px;",
  prism_logo,
  tags$span("PRISM",
    style = paste0(
      "font-weight:800; font-size:1.5rem; letter-spacing:0.14em;",
      "background:linear-gradient(90deg,#e0c3fc,#a5b4fc,#93c5fd);",
      "-webkit-background-clip:text; -webkit-text-fill-color:transparent;",
      "background-clip:text;"
    )
  )
)

ui <- page_navbar(
  title    = prism_title,
  theme    = bs_theme(
    bootswatch = "flatly",
    primary    = "#1e1b4b",
    base_font  = font_google("Inter")
  ),
  fillable = FALSE,
  header   = tags$head(
    tags$style("
    html { overflow-y: scroll !important; height: auto !important; }
    body, .bslib-page-fill, .bslib-flow-mobile, .tab-content, .tab-pane,
    .bslib-sidebar-layout, .bslib-page-navbar, .bslib-page-navbar > .container-fluid,
    .bslib-sidebar-layout > .main, .bslib-sidebar-layout > :not(.sidebar) {
      overflow: visible !important; overflow-y: visible !important;
      height: auto !important; max-height: none !important; min-height: unset !important;
    }
    .sidebar, .bslib-sidebar-layout > .sidebar, .sidebar > .sidebar-content {
      overflow: visible !important; overflow-y: visible !important;
      height: auto !important; max-height: none !important;
      position: sticky !important; top: 60px;
    }
    /* The sticky sidebar above creates a STACKING CONTEXT (sticky elements do,
       regardless of z-index). That trapped the selectize dropdown for the PT
       list inside it, while the main panel -- a later sibling -- painted on top.
       The ADR list rendered but sat underneath the content area: visible, but
       unclickable, with its scrollbar unreachable. Lift the sidebar above the
       main panel and float the dropdown above both. */
    .bslib-sidebar-layout > .sidebar { z-index: 1030 !important; }
    /* Deliberately NO z-index on .selectize-control: selectize already gives it
       position:relative, and adding a z-index turns each control into its own
       STACKING CONTEXT, which traps its dropdown so it cannot paint above the
       NEXT control below it. That is what made the Reference Cohort filters
       overlap. The dropdown's own z-index below is what does the work. */
    .selectize-dropdown { z-index: 3000 !important; }
    /* Guarantee the list keeps its own internal scroll (selectize defaults to
       200px; state it explicitly so the overflow:visible overrides above can
       never flatten it into an unscrollable full-height list). */
    .selectize-dropdown-content {
      max-height: 280px !important; overflow-y: auto !important;
    }
    /* Fix DT filter widgets overflowing */
    .dataTables_wrapper thead th { overflow: visible !important; }
    .dataTables_filter input, thead .form-control { box-sizing: border-box !important; }
    thead td { overflow: visible !important; white-space: nowrap; }
    .noUi-target { margin: 4px 2px !important; }
    thead input[type='search'] { width: 100% !important; box-sizing: border-box !important; }
    .navbar { background-color: #1e1b4b !important; border-bottom: 1px solid rgba(255,255,255,0.08); padding: 0.4rem 1rem !important; }
    .navbar .nav-link { color: rgba(255,255,255,0.75) !important; white-space: nowrap; }
    .navbar .nav-link:hover, .navbar .nav-link.active { color: #fff !important; }
    .navbar > .container-fluid { flex-wrap: nowrap !important; align-items: center !important; position: relative !important; }
    .navbar-brand { flex-shrink: 0 !important; display: flex !important; align-items: center !important; padding-top: 0 !important; padding-bottom: 0 !important; }
    .navbar-nav { position: absolute !important; left: 50% !important; transform: translateX(-50%) !important; flex-wrap: nowrap !important; gap: 0.25rem; align-items: center !important; }
    #loading-overlay {
      display:none; position:fixed; inset:0; z-index:9999;
      background:rgba(255,255,255,0.75);
      justify-content:center; align-items:center; flex-direction:column;
    }
    #loading-overlay.active { display:flex; }
    .spinner { width:48px; height:48px; border:5px solid #e5e7eb; border-top-color:#1e1b4b;
      border-radius:50%; animation:spin 0.8s linear infinite; }
    @keyframes spin { to { transform:rotate(360deg); } }
    #loading-overlay .loading-text { margin-top:16px; font-size:0.95rem; color:#1e1b4b; font-weight:500; }
    "),
    tags$div(id = "loading-overlay",
      tags$div(class = "spinner"),
      tags$div(class = "loading-text", "Querying FDA FAERS database...")
    ),
    tags$script(HTML("
      $(document).on('click', '#run_check', function() {
        $('#loading-overlay').addClass('active');
      });
      $(document).on('shiny:value shiny:error', function(e) {
        if (e.name === 'monitor_status_row') {
          $('#loading-overlay').removeClass('active');
        }
      });
    "))
  ),

  # ── Tab 1: Monitor Your Drug ─────────────────────────────────────────────
  nav_panel(
    title = "Monitor Your Drug",
    icon  = icon("magnifying-glass-chart"),

    layout_sidebar(
      fillable = FALSE,
      sidebar  = sidebar(
        width = 280,
        h6("Enter a drug and adverse event to check its current FAERS signal status.",
           class = "text-muted mb-3"),
        textInput("live_drug", "Drug name (brand or generic)",
                  placeholder = "e.g. HUMIRA, atorvastatin"),
        selectizeInput("live_ae", "Adverse event (MedDRA Preferred Term)",
                       choices  = c("Select a PT term..." = "", pt_terms),
                       selected = "",
                       options  = list(create = FALSE, placeholder = "Type to search PT terms...")),
        actionButton("run_check", "Check Signal",
                     class = "btn-primary w-100 mt-2", icon = icon("play")),
        # Only appears once a query has run — an export of nothing is a broken
        # promise, and a disabled button invites clicking. Server-side via
        # uiOutput so it tracks the same reactive the record is built from.
        uiOutput("download_ui"),
        hr(),
        div(class = "text-muted", style = "font-size:0.78rem;",
          strong("Tips:"), br(),
          "Enter a brand or generic name — PRISM automatically searches all equivalent names (e.g. LIPITOR also searches atorvastatin).", br(), br(),
          "MedDRA PT terms: search the dropdown — only Preferred Terms work in FAERS.", br(), br(),
          "Each check queries 10 quarters of live FDA data — allow ~1 min."
        )
      ),

      # Welcome message (hidden once a check has run)
      conditionalPanel(
        "input.run_check == 0",
        div(class = "d-flex align-items-center justify-content-center py-5",
          div(class = "text-center text-muted",
            tags$i(class = "fa fa-chart-line fa-3x mb-3", style = "color:#adb5bd;"),
            h5("Enter a drug and adverse event, then click Check Signal"),
            p("The tool queries FDA's FAERS database in real time and assesses",
              " whether a disproportionality signal exists for your drug.")
          )
        )
      ),

      # Results (shown after first check) — all output IDs are static in the DOM
      conditionalPanel(
        "input.run_check > 0",
        uiOutput("monitor_status_row"),
        uiOutput("resolved_names_note"),
        uiOutput("label_change_banner"),
        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header(uiOutput("live_chart_title")),
            plotOutput("live_chart", height = "360px"),
            card_footer(class = "text-muted", style = "font-size:0.75rem;",
                        uiOutput("live_provenance"))
          ),
          card(
            card_header("Regulatory Context"),
            card_body(
              uiOutput("reg_context"),
              hr(),
              uiOutput("rec_text")
            )
          )
        ),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span(icon("table-list"), " Raw Quarterly Data"),
            tags$button(class = "btn btn-sm btn-outline-secondary",
                        id = "toggle_quarterly",
                        onclick = "var body = $(this).closest('.card').find('.card-body'); body.toggle(); $(this).text(body.is(':visible') ? 'Hide' : 'Show'); if(body.is(':visible')) { $($.fn.dataTable.tables(true)).DataTable().columns.adjust(); }",
                        "Show")
          ),
          card_body(style = "display:none;", DTOutput("raw_quarterly_table"))
        ),
        # ── Regulatory Timeline Intelligence (hidden when FDA already acted) ──
        uiOutput("timeline_card")
      )
    )
  ),

  # ── Tab 2: Reference Cohort ──────────────────────────────────────────────
  nav_panel(
    title = "Reference Cohort",
    icon  = icon("clock-rotate-left"),

    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        selectInput("class_filter", "Filter by class:",
                    choices = c("All classes" = "", sort(unique(combined$therapeutic_class))),
                    selected = ""),
        selectInput("signal_filter", "Signal detected?",
                    choices = c("All drugs" = "", "Yes — FAERS signal found" = "yes",
                                "No — no FAERS signal" = "no"),
                    selected = ""),
        selectInput("drug_select", "Select drug:",
                    choices = drug_choices, selected = drug_choices[1]),
        hr(),
        uiOutput("drug_info_box")
      ),
      # PRIMARY VIEW: one row per drug, anchored at zero, so lags are directly
      # comparable. This replaced a per-drug quarterly PRR line, which was the
      # wrong encoding for a cross-drug question and unreadably spiky on sparse
      # counts. Height scales with the number of drugs so facets never squash.
      card(
        card_header(
          "Signal-to-label lag across the cohort",
          tags$div(
            style = paste0("font-size:0.78rem;font-weight:400;color:#64748b;",
                           "margin-top:4px;line-height:1.35;"),
            "A signal starts at the first quarter where criteria were met in ",
            tags$strong("2 of any trailing 6 quarters"),
            " \u2014 the same rule that marks a live query CONFIRMED on the Monitor ",
            "tab. Accepting a single isolated crossing instead would nearly double ",
            "these lead times, and the ",
            tags$span(style = "color:#b45309;font-weight:600;", "negative control arm"),
            " on the Methods tab shows why that rule was rejected."
          )
        ),
        # height is set server-side (see cohort_lag_height) so it tracks the
        # number of drugs in view; "auto" lets that value through.
        plotOutput("cohort_lag", height = "auto")
      ),
      # DRILL-DOWN: the quarterly trend still exists, for the one drug selected
      # in the sidebar. Collapsed by default so it does not greet the reader.
      card(
        card_header(
          tags$button(
            class = "btn btn-sm btn-outline-secondary",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#trend-collapse",
            "Show quarterly PRR trend for the selected drug"
          )
        ),
        div(id = "trend-collapse", class = "collapse",
            card_body(plotOutput("prr_trend", height = "450px")))
      ),
      card(
        card_header("How to read this chart"),
        card_body(
          tags$p(tags$strong("Lag chart"), " \u2014 each row is one drug. The bar runs from ",
                 "zero (the FDA label change) to the month the FAERS signal was first ",
                 "detected. Bars to the ", tags$strong("right"), " mean the signal came ",
                 "first; bars to the ", tags$strong("left"),
                 " mean FDA acted before FAERS showed anything \u2014 usually because the ",
                 "risk was found in trials or published case series, not spontaneous ",
                 "reports. The dashed orange line is the cohort median."),
          tags$p(tags$strong("Quarterly PRR trend"), " (collapsed above) \u2014 the evidence ",
                 "behind a single row, for the drug selected in the sidebar:"),
          tags$ul(
            tags$li(tags$strong("Dot size"),
                    ": reports for that drug/event in the quarter. Bigger dots are ",
                    "better-supported estimates; a high PRR on a tiny dot is fragile."),
            tags$li(span(style="color:#1e1b4b;font-weight:600;", "Dark dots"),
                    ": quarters meeting all signal criteria. ",
                    span(style="color:#8b93a3;font-weight:600;", "Pale dots"),
                    ": below criteria."),
            tags$li(span(style="color:#e05c00;font-weight:600;", "Dashed line"),
                    ": the PRR = 2 threshold. The axis is log-scaled because PRR ",
                    "spans two orders of magnitude across the cohort."),
            tags$li(span(style="color:darkgreen;font-weight:600;", "Green dotted line"),
                    ": quarter the signal was first confirmed."),
            tags$li(span(style="color:firebrick;font-weight:600;", "Red line"),
                    ": date of the FDA label update."),
            tags$li(tags$strong("Signal criteria"),
                    ": PRR \u2265 2, 95% CI lower bound > 1, n \u2265 3, ",
                    "\u03c7\u00b2 \u2265 4 (Evans + Rothman CI).")
          )
        )
      )
    )
  ),

  # ── Tab 3: Drug Table ────────────────────────────────────────────────────
  nav_panel(
    title = "Drug Table",
    icon  = icon("table"),
    card(
      card_header("Reference Cohort — Full Data"),
      card_body(DTOutput("drug_table"))
    ),
    card(
      card_header(icon("fingerprint"), " Data Provenance"),
      card_body(
        if (!is.null(provenance)) {
          tags$table(class = "table table-sm table-borderless mb-0",
            style = "font-size:0.85rem;",
            tags$tbody(
              tags$tr(tags$td(class="text-muted", "Pipeline run"),
                      tags$td(class="fw-semibold", provenance$pipeline_run_utc, " UTC")),
              tags$tr(tags$td(class="text-muted", "FAERS date range"),
                      tags$td(class="fw-semibold",
                              paste(provenance$faers_date_range["earliest"], "to",
                                    provenance$faers_date_range["latest"]))),
              tags$tr(tags$td(class="text-muted", "Drugs in cohort"),
                      tags$td(class="fw-semibold",
                              paste(provenance$drugs_queried, collapse = ", "))),
              tags$tr(tags$td(class="text-muted", "Records"),
                      tags$td(class="fw-semibold", provenance$n_records)),
              tags$tr(tags$td(class="text-muted", "API source"),
                      tags$td(class="fw-semibold",
                              tags$a(href="https://open.fda.gov/apis/drug/event/",
                                     target="_blank", "openFDA FAERS API"))),
              tags$tr(tags$td(class="text-muted", "R version"),
                      tags$td(class="fw-semibold", provenance$r_version))
            )
          )
        } else {
          p(class = "text-muted mb-0",
            "Provenance not available. Re-run run_pipeline.R to generate data/provenance.rds.")
        }
      )
    )
  ),

  # ── Tab 4: Methodology ───────────────────────────────────────────────────
  nav_panel(
    title = "Methodology",
    icon  = icon("flask"),

    layout_columns(
      col_widths = c(12),

      card(
        card_header(icon("database"), " Data Source"),
        card_body(
          p("All adverse event data comes from the",
            tags$a(href = "https://open.fda.gov/apis/drug/event/", target = "_blank",
                   "FDA Adverse Event Reporting System (FAERS)"),
            "through the openFDA API. FAERS is a spontaneous reporting system where healthcare professionals,
            consumers, and manufacturers submit reports of suspected adverse drug reactions."),
          p("Drug labeling data (Boxed Warnings, Contraindications) is pulled in real time from the",
            tags$a(href = "https://open.fda.gov/apis/drug/label/", target = "_blank",
                   "openFDA Drug Labeling API"), "."),
          tags$h6(class = "mt-3 fw-semibold", "Limitations of FAERS data"),
          tags$ul(
            tags$li(tags$strong("Underreporting:"), " Most adverse events go unreported. No reports does not mean no risk."),
            tags$li(tags$strong("No causation:"), " A report means a patient took a drug and had an event. It does not prove the drug caused it."),
            tags$li(tags$strong("Reporting bias:"), " Media attention, lawsuits, and FDA safety communications can drive spikes in reporting that have nothing to do with true incidence."),
            tags$li(tags$strong("Duplicate reports:"), " The same case can be submitted by the manufacturer, the doctor, and the patient separately.")
          )
        )
      ),

      card(
        card_header(icon("square-root-variable"), " Signal Detection: PRR Method"),
        card_body(
          p("PRISM uses the", tags$strong("Proportional Reporting Ratio (PRR)"),
            "to measure whether a drug-AE pair is reported more often than expected compared to all other
            drugs in the FAERS database. PRR is the standard disproportionality metric used by the
            EMA and was first described by Evans et al. (2001)."),
          tags$h6(class = "mt-3 fw-semibold", "Count definitions"),
          p("PRISM queries four counts from the openFDA API per drug-AE-quarter combination:"),
          tags$table(class = "table table-bordered table-sm", style = "max-width: 550px; font-size: 0.9rem;",
            tags$thead(
              tags$tr(tags$th("Count"), tags$th("Definition"))
            ),
            tags$tbody(
              tags$tr(tags$td(tags$strong("a")), tags$td("Reports with target drug AND target AE")),
              tags$tr(tags$td(tags$strong("B")), tags$td("All reports with target drug (any AE)")),
              tags$tr(tags$td(tags$strong("C")), tags$td("All reports with target AE (any drug)")),
              tags$tr(tags$td(tags$strong("D")), tags$td("All reports in the quarter"))
            )
          ),
          p(class = "text-muted", style = "font-size: 0.85rem;",
            "B, C, and D are marginal totals (not inner cells of a 2\u00d72 table).
            Each is obtained from a separate openFDA API query."),
          tags$div(style = "background: #f8f9fa; border-radius: 6px; padding: 12px 16px; margin: 12px 0; font-family: monospace; font-size: 0.9rem;",
            tags$div("PRR = (a / B) / (C / D)"),
            tags$div(class = "mt-1", "\u03c7\u00b2 = (a \u2212 E)\u00b2 / E, where E = B \u00d7 C / D"),
            tags$div(class = "mt-1", "95% CI = exp(ln(PRR) \u00b1 1.96 \u00d7 SE)"),
            tags$div(class = "mt-1", "SE = \u221a(1/a \u2212 1/B + 1/C \u2212 1/D)")
          ),
          p("This is equivalent to the standard Evans PRR formula [a/(a+b)] / [c/(c+d)]
            when a is small relative to the marginals, which holds for the vast majority of
            drug-AE pairs in FAERS. The 95% confidence interval uses the log-normal
            approximation for ratio measures (Rothman, 2008)."),
          tags$h6(class = "mt-3 fw-semibold", "Reporting Odds Ratio (ROR)"),
          p("PRR compares", tags$strong("proportions"), "; ROR compares",
            tags$strong("odds"), "of the same 2\u00d72 table:"),
          tags$div(class = "font-monospace small bg-light p-2 rounded",
            tags$div("ROR = (a \u00d7 d) / (b \u00d7 c)"),
            tags$div(class = "mt-1", "SE = \u221a(1/a + 1/b + 1/c + 1/d)")
          ),
          p("Both are reported because regulators differ:", tags$strong("FDA"),
            "screens with PRR (and EBGM internally), while", tags$strong("EMA"),
            "uses ROR in EudraVigilance. For a rare event the two converge, so ROR
            serves as a cross-check rather than a second opinion \u2014 a material
            divergence indicates the event is not rare in the exposed population.
            Signal criteria are applied to PRR only; ROR is displayed, not thresholded."),
          tags$h6(class = "mt-3 fw-semibold", "Why not EBGM or IC?"),
          p("The FDA uses", tags$strong("EBGM (Empirical Bayesian Geometric Mean)"),
            "internally (DuMouchel, 1999). It applies Bayesian shrinkage to reduce false positives
            when report counts are low. The WHO Uppsala Monitoring Centre uses the",
            tags$strong("Information Component (IC)"), "(Bate et al., 1998; Nor\u00e9n et al., 2013)
            for their global VigiBase database."),
          p("Both of these methods need access to the full FAERS database to compute the prior distributions
            that drive the shrinkage. The openFDA API only returns counts for individual queries, not the
            full reporting distribution. PRR is the right frequentist alternative when working through an API,
            and it is still the standard at the EMA and MHRA.")
        )
      ),

      card(
        card_header(icon("check-double"), " Signal Classification Criteria"),
        card_body(
          p("Detection has two layers. A quarter ", tags$strong("crosses"),
            " when all four of the following hold:"),
          tags$table(class = "table table-sm table-bordered", style = "max-width: 500px; font-size: 0.9rem;",
            tags$thead(
              tags$tr(tags$th("Criterion"), tags$th("Threshold"), tags$th("Rationale"))
            ),
            tags$tbody(
              tags$tr(tags$td("Report count (a)"), tags$td("\u2265 3"), tags$td("Minimum sample size")),
              tags$tr(tags$td("PRR"), tags$td("\u2265 2.0"), tags$td("Disproportionality")),
              tags$tr(tags$td("95% CI lower bound"), tags$td("> 1.0"), tags$td("Statistical significance")),
              tags$tr(tags$td("\u03c7\u00b2"), tags$td("\u2265 4.0"), tags$td("Independence test"))
            )
          ),
          p(class = "mt-3",
            "A ", tags$strong("signal"), " requires 2 crossings within any trailing 6 ",
            "quarters. A single isolated crossing is not a signal. Applied to the most ",
            "recent 6 quarters, this gives the status shown on the Monitor tab:"),
          tags$ul(
            tags$li(tags$span(class = "badge bg-danger", "CONFIRMED"), " Signal met in 2+ of the last 6 quarters"),
            tags$li(tags$span(class = "badge bg-warning text-dark", "EMERGING"), " Signal met in exactly 1 of the last 6 quarters"),
            tags$li(tags$span(class = "badge bg-success", "NOT DETECTED"), " Signal not met in any of the last 6 quarters")
          ),
          p(class = "text-muted mt-2",
            "The per-quarter thresholds are from Evans et al. (2001), with a CI lower bound ",
            "> 1 requirement added (per Rothman) to limit false positives in quarters with ",
            "few reports. The persistence requirement was set from the negative control arm ",
            "below, which measured its effect on the false-positive rate."),
          tags$h6(class = "mt-3 fw-semibold", "Signal duration metrics"),
          p("The Monitor tab reports two complementary duration measures:"),
          tags$ul(
            tags$li(tags$strong("Signal Duration"), " \u2014 months since the signal began, under the persistence rule above. The same rule defines the cohort lag, so the two are directly comparable."),
            tags$li(tags$strong("Current Streak"), " \u2014 consecutive quarters, counting back from the latest, in which the criteria were met.")
          ),
          p(class = "text-muted", "A long duration with a short streak may indicate an intermittent signal. A short duration with a long streak suggests a newly emerging but consistent signal.")
        )
      ),

      card(
        card_header(icon("book"), " Reference Cohort"),
        card_body(
          p("The reference cohort includes", tags$strong("42 drugs"), "across",
            tags$strong("12 mechanistic classes"), "where FDA took regulatory action (Boxed Warning, Contraindication,
            Warning, or Withdrawal) after post-market safety signals. These are known, documented cases."),
          p("For each drug, PRISM pulls quarterly FAERS counts for the adverse event that led
            to the labelling action, computes PRR per quarter across a window spanning that
            action, and measures the", tags$strong("signal-to-label lag"),
            ": the interval between signal onset and the date FDA acted."),
          tags$h6(class = "mt-3 fw-semibold", "What the cohort tells us"),
          tags$ul(
            tags$li("For several classes \u2014 fluoroquinolones, proton pump inhibitors,
                    Factor Xa inhibitors \u2014 the FAERS signal preceded FDA action by years."),
            tags$li("For others, FAERS was not the driver. The bisphosphonate osteonecrosis
                    warning followed dental case series, and the atypical antipsychotic
                    mortality warning followed a meta-analysis of 17 placebo-controlled
                    trials that FAERS could not have reproduced."),
            tags$li("Lag ranges from \u221223.8 to 108.9 months. A negative lag indicates the
                    labelling action preceded the reporting increase, consistent with
                    notoriety bias rather than detection.")
          ),
          p(class = "text-muted", "PRISM therefore presents historical context rather than
            predictions.")
        )
      ),

      # Specificity arm. Rendered entirely server-side because
      # data/negative_controls.rds may not exist yet (it is produced by the
      # pipeline, not committed ahead of it) and this file is sourced at app
      # startup -- a hard reference to a missing object here would take the
      # deployed app down, which is exactly how it broke once before.
      card(
        card_header(icon("shield-halved"), " Specificity: Negative Control Arm"),
        card_body(
          p("Every drug in the reference cohort received an FDA labelling action, so ",
            "nearly all of them signal. The cohort therefore measures detection speed ",
            "but not the false-positive rate. A method that flagged every drug-event ",
            "pair would produce the same cohort result."),
          p("The negative control arm addresses this. Each pair combines a drug already ",
            "in the cohort, over that drug's own window, with an adverse event for which ",
            "it has no established mechanism and no label mention. Examples include ",
            tags$em("atorvastatin with somnambulism"), " and ",
            tags$em("rivaroxaban with tuberculosis"), ". The detection rule is applied ",
            "unchanged, so any signal produced is a false positive."),
          p("PRISM applies a single signal definition throughout: ",
            tags$strong("2 crossings within any trailing 6 quarters"),
            ". Results below use that rule. The alternative considered is documented ",
            "under ", tags$em("How this threshold was chosen"), "."),
          uiOutput("negctl_summary"),
          tags$h6(class = "mt-3 fw-semibold", "Pair-level results"),
          DTOutput("negctl_table"),
          p(class = "text-muted mt-2",
            "Pairs with fewer than 3 reports across the window are listed but excluded ",
            "from the rate. The n \u2265 3 criterion means they could not have signalled, ",
            "and including them would overstate specificity. Consistent with Schuemie ",
            "et al., this set is a check on threshold behaviour rather than a formal ",
            "validation study: negative controls can be misclassified, and one pair in ",
            "this set was."),
          p(class = "text-muted",
            tags$strong("Interpreting the failures. "),
            "Two patterns are present. A pair signalling in a single quarter out of ",
            "forty reflects multiple testing. A pair signalling in ten or more does ",
            "not: for esomeprazole and osteonecrosis of jaw, proton pump inhibitors ",
            "and bisphosphonates are prescribed to overlapping elderly and oncology ",
            "populations, so the disproportionality reflects channelling rather than ",
            "pharmacology. Confounding of this type is not removable by threshold ",
            "adjustment and requires clinical assessment."),
          p(class = "text-muted",
            tags$strong("Method: "), "negative control outcomes, following the OHDSI/OMOP ",
            "approach (Ryan et al. 2013; Schuemie et al. 2016). Curation rationale for each ",
            "pair is recorded in ", tags$code("data/negative_controls.csv"), ".")
        )
      ),

      card(
        card_header(icon("scale-balanced"), " Limitations"),
        card_body(
          tags$ul(
            tags$li("PRISM only uses FAERS data. It does not include clinical trial data,
                    published literature, or international databases like EudraVigilance or VigiBase."),
            tags$li("PRR is a screening tool, not a risk assessment. A signal means something is worth
                    investigating, not that the drug caused the event."),
            tags$li("The openFDA API has a 1 to 3 quarter lag, so the most recent quarter may be incomplete."),
            tags$li("PRISM searches three drug name fields (brand name, generic name, and free-text
                    medicinal product) but can still miss reports with misspellings, abbreviations,
                    or non-US trade names."),
            tags$li("This tool is for educational and research purposes only. It is not regulatory advice.")
          )
        )
      ),

      card(
        card_header(icon("quote-left"), " References"),
        card_body(
          tags$ol(style = "font-size: 0.9rem;",
            tags$li("Evans SJW, Waller PC, Davis S. (2001). Use of proportional reporting ratios (PRRs) for signal
                    generation from spontaneous adverse drug reaction reports.", tags$em("Pharmacoepidemiology and
                    Drug Safety"), ", 10(6), 483\u2013486."),
            tags$li("Rothman KJ, Greenland S, Lash TL. (2008).", tags$em("Modern Epidemiology"), ", 3rd ed.
                    Lippincott Williams & Wilkins. [Log-normal CI for ratio measures]"),
            tags$li("DuMouchel W. (1999). Bayesian data mining in large frequency tables, with an application to
                    the FDA spontaneous reporting system.", tags$em("The American Statistician"),
                    ", 53(3), 177\u2013190. [EBGM/MGPS method used by FDA]"),
            tags$li("Bate A, Lindquist M, Edwards IR, et al. (1998). A Bayesian neural network method for
                    adverse drug reaction signal generation.",
                    tags$em("European Journal of Clinical Pharmacology"),
                    ", 54(4), 315\u2013321. [Original IC method used by WHO/UMC]"),
            tags$li("Nor\u00e9n GN, Hopstadius J, Bate A. (2013). Shrinkage observed-to-expected ratios for robust
                    and transparent large-scale pattern discovery.",
                    tags$em("Statistical Methods in Medical Research"),
                    ", 22(1), 57\u201369. [Shrinkage IC refinement]"),
            tags$li("Ryan PB, Schuemie MJ, Welebob E, et al. (2013). Defining a reference set to support
                    methodological research in drug safety.", tags$em("Drug Safety"),
                    ", 36(S1), S33\u2013S47. [OMOP negative control reference set]"),
            tags$li("Schuemie MJ, Ryan PB, Hripcsak G, Madigan D, Suchard MA. (2016). Measuring signal detection
                    performance: can we trust negative controls and do we need them?",
                    tags$em("Drug Safety"), ", 39(11), 1039\u20131042."),
            tags$li("FDA. openFDA: FAERS API documentation.",
                    tags$a(href = "https://open.fda.gov/apis/drug/event/", target = "_blank",
                           "https://open.fda.gov/apis/drug/event/")),
            tags$li("FDA. openFDA: Drug Labeling API documentation.",
                    tags$a(href = "https://open.fda.gov/apis/drug/label/", target = "_blank",
                           "https://open.fda.gov/apis/drug/label/"))
          )
        )
      )
    )
  ),

  footer = tags$footer(
    class = "text-center py-3",
    style = "border-top:1px solid #e2e8f0; margin-top:2rem; background:#f8fafc;",
    tags$div(
      style = "display:inline-flex; align-items:center; gap:6px;",
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 48 40",
        width = "28", height = "23", style = "vertical-align:middle;",
        tags$defs(
          tags$linearGradient(id = "pg-footer", x1 = "0%", y1 = "0%", x2 = "100%", y2 = "100%",
            tags$stop(offset = "0%",   style = "stop-color:#c4b5fd;stop-opacity:1"),
            tags$stop(offset = "100%", style = "stop-color:#818cf8;stop-opacity:1")
          )
        ),
        tags$polygon(points = "24,2 46,38 2,38",
                     fill = "url(#pg-footer)", stroke = "rgba(255,255,255,0.4)",
                     `stroke-width` = "1.2"),
        tags$line(x1 = "0", y1 = "18", x2 = "13", y2 = "24",
                  stroke = "white", `stroke-width` = "2", opacity = "0.85"),
        tags$line(x1 = "35", y1 = "22", x2 = "48", y2 = "12",
                  stroke = "#f87171", `stroke-width` = "1.8", opacity = "0.95"),
        tags$line(x1 = "35", y1 = "25", x2 = "48", y2 = "19",
                  stroke = "#fbbf24", `stroke-width` = "1.8", opacity = "0.95"),
        tags$line(x1 = "35", y1 = "28", x2 = "48", y2 = "26",
                  stroke = "#34d399", `stroke-width` = "1.8", opacity = "0.95"),
        tags$line(x1 = "35", y1 = "31", x2 = "48", y2 = "33",
                  stroke = "#60a5fa", `stroke-width` = "1.8", opacity = "0.95")
      ),
      tags$span(
        style = paste0(
          "font-weight:700; font-size:0.85rem; letter-spacing:0.1em;",
          "background:linear-gradient(90deg,#c4b5fd,#818cf8,#60a5fa);",
          "-webkit-background-clip:text; -webkit-text-fill-color:transparent;",
          "background-clip:text;"
        ),
        "PRISM"
      ),
      tags$span(
        style = "font-size:0.75rem; color:#cbd5e1; margin:0 3px;",
        "|"
      ),
      tags$span(
        style = "font-size:0.75rem; color:#64748b;",
        "Pharmacovigilance Real-time Intelligence Signal Monitor"
      ),
      if (!is.null(provenance)) tags$span(
        style = "font-size:0.72rem; color:#94a3b8; margin-left:12px;",
        paste0("Cohort study period: ",
               provenance$faers_date_range["earliest"], " to ",
               provenance$faers_date_range["latest"],
               " | Last refreshed ",
               format(as.POSIXct(provenance$pipeline_run_utc, tz = "UTC"), "%B %d, %Y"),
               " | Monitor tab queries live data")
      )
    )
  )
)
