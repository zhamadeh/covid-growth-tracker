# app.R — COVID Trend Monitor (safe rolling mean for short series)
library(shiny)
library(bslib)
library(thematic)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)
library(httr)
library(purrr)
library(ragg)

thematic::thematic_shiny()

`%||%` <- function(a,b) if (is.null(a)) b else a

# --- FIXED: safe rolling mean that works for short series too ---
roll_mean <- function(x, k){
  x <- as.numeric(x)
  n <- length(x)
  if (k <= 1 || n == 0) return(x)
  res <- rep(NA_real_, n)
  if (n < k) {
    # partial mean for all points if series is shorter than window
    for (i in seq_len(n)) res[i] <- mean(x[1:i], na.rm = TRUE)
  } else {
    # standard trailing mean for full windows
    res <- as.numeric(stats::filter(x, rep(1/k, k), sides = 1))
    # fill the first k-1 with partial means
    for (i in seq_len(k-1)) res[i] <- mean(x[1:i], na.rm = TRUE)
  }
  res
}

safe_log <- function(x) {
  # Replace non-positive values with a very small positive number
  x <- ifelse(is.na(x) | !is.finite(x) | x <= 0, 1e-12, x)
  log(x)
}

OWID_URL   <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"


fetch_owid <- function(){
  raw <- if (nzchar(OWID_URL) && file.exists(OWID_URL)) {
    message("Loading local OWID data from: ", OWID_URL)
    readr::read_csv(OWID_URL, show_col_types = FALSE, guess_max = 100000)
  } else {
    message("Fetching OWID data from: ", OWID_URL)
    req <- httr::GET(OWID_URL, httr::timeout(60), httr::user_agent("covid-trend-monitor"))
    httr::stop_for_status(req)
    readr::read_csv(httr::content(req, as="raw"), show_col_types = FALSE, guess_max = 100000)
  }
  
  nm <- names(raw)
  result <- tibble(
    country    = as.character(if ("location" %in% nm) raw$location else if ("country" %in% nm) raw$country else NA_character_),
    continent  = as.character(if ("continent" %in% nm) raw$continent else NA_character_),
    code       = as.character(if ("iso_code" %in% nm) raw$iso_code else if ("code" %in% nm) raw$code else NA_character_),
    date       = as.Date(raw$date),
    population = suppressWarnings(as.double(raw$population)),
    total_cases  = suppressWarnings(as.double(raw$total_cases)),
    new_cases    = suppressWarnings(as.double(raw$new_cases)),
    total_deaths = suppressWarnings(as.double(raw$total_deaths)),
    new_deaths   = suppressWarnings(as.double(raw$new_deaths))
  ) %>% filter(!is.na(country), !is.na(date))
  
  message("Loaded ", nrow(result), " rows of data for ", length(unique(result$country)), " countries")
  return(result)
}

metric_label <- function(metric, percap) {
  base <- switch(metric,
                 total_cases="Total cases",
                 new_cases="New cases",
                 total_deaths="Total deaths",
                 new_deaths="New deaths",
                 "Value")
  if (percap) paste0(base, " (per million)") else base
}

make_real_plot <- function(df, ylog=TRUE, metric_lab="Value",
                           global_ref=NULL, ref_label="Global reference",
                           highlight_ranges = NULL,
                           global_backdrop=NULL) {
  gg <- ggplot()
  
  # Add backdrop first (bottom layer)
  if (!is.null(global_backdrop) && nrow(global_backdrop) > 0) {
    gg <- gg +
      geom_line(data=global_backdrop, aes(date, value, group=series),
                inherit.aes=FALSE, linewidth=0.25, alpha=0.10, color="grey35")
  }
  
  # Add highlight rectangles (must be before main data)
  if (!is.null(highlight_ranges) && nrow(highlight_ranges) > 0) {
    message("Adding ", nrow(highlight_ranges), " highlight rectangles to ggplot")
    gg <- gg +
      geom_rect(data = highlight_ranges,
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                inherit.aes = FALSE, fill = "#ff6b6b", alpha = 0.25)
  }
  
  # Add main data line
  gg <- gg + geom_line(data=df, aes(date, value, color=series), linewidth=1.2, na.rm=TRUE)
  
  # Add global reference line if present
  if (!is.null(global_ref) && nrow(global_ref) > 0) {
    gg <- gg +
      geom_line(data=global_ref, aes(date, value),
                inherit.aes=FALSE, color="grey55", linewidth=0.9, alpha=0.85) +
      annotate("text",
               x = max(global_ref$date, na.rm=TRUE),
               y = tail(global_ref$value[!is.na(global_ref$value)], 1),
               label = ref_label, hjust=1, vjust=-0.4, color="grey35", size=3.5)
  }
  
  gg +
    labs(x="Date", y=metric_lab, color=NULL) +
    theme_minimal(base_size=15) +
    theme(
      text = element_text(family = "Inter"),
      legend.position="top", legend.box="horizontal",
      axis.title.x = element_text(margin=margin(t=8)),
      axis.title.y = element_text(margin=margin(r=8)),
      panel.grid.minor = element_blank()
    ) +
    { if (ylog) scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) else NULL }
}

simulate_curve <- function(N0,g,days,intervention_day=NA,g_after=NA){
  t <- 0:days
  if (!is.na(intervention_day) && !is.na(g_after)) {
    pre <- pmax(0, pmin(t, intervention_day)); post <- pmax(0, t - intervention_day)
    N <- N0 * (1+g)^pre * (1+g_after)^post
  } else N <- N0 * (1+g)^t
  tibble(day=t, cases=N)
}

make_sim_plot <- function(df, title, ylog=TRUE){
  ggplot(df, aes(day, cases, color=scenario)) +
    geom_line(linewidth=1.2) +
    labs(x="Day", y="Cases", title=title, color=NULL) +
    theme_minimal(base_size=15) +
    theme(
      text = element_text(family = "Inter"),
      plot.title = element_text(face="bold", hjust=0, margin=margin(b=8)),
      axis.title.x = element_text(margin=margin(t=8)),
      axis.title.y = element_text(margin=margin(r=8)),
      legend.position="top", legend.box="horizontal",
      panel.grid.minor = element_blank()
    ) +
    { if (ylog) scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) else NULL } +
    scale_x_continuous(expand=expansion(mult=c(0,0.02)))
}

app_theme <- bs_theme(
  version=5, primary="#0d6efd",
  base_font=font_google("Inter"), heading_font=font_google("Inter"),
  bg="#ffffff", fg="#111111"
)

resize_css <- HTML("
<style>
.bslib-sidebar-layout {
  --bs-sidebar-width: 360px;
}
.bslib-sidebar-layout > .sidebar {
  min-width: 280px;
  max-width: 600px;
}
/* Make the entire right border draggable */
.bslib-sidebar-layout[data-bslib-sidebar-border='true'] > .sidebar {
  border-right: 3px solid #dee2e6;
  position: relative;
}
.bslib-sidebar-layout > .sidebar:hover {
  border-right-color: #0d6efd;
  cursor: ew-resize;
}
</style>
<script>
$(document).ready(function() {
  // Make sidebar resizable by dragging anywhere on the right edge
  var sidebar = $('.bslib-sidebar-layout > .sidebar');
  var isResizing = false;
  var lastDownX = 0;

  $(document).on('mousedown', '.bslib-sidebar-layout > .sidebar', function(e) {
    var sidebar = $(this);
    var sidebarOffset = sidebar.offset();
    var sidebarWidth = sidebar.outerWidth();
    
    // Check if mouse is near the right edge (within 10px)
    if (e.pageX > sidebarOffset.left + sidebarWidth - 10) {
      isResizing = true;
      lastDownX = e.pageX;
      $('body').css('cursor', 'ew-resize');
      e.preventDefault();
    }
  });

  $(document).on('mousemove', function(e) {
    if (!isResizing) {
      // Show cursor change when hovering near right edge
      var sidebar = $('.bslib-sidebar-layout > .sidebar');
      if (sidebar.length) {
        var sidebarOffset = sidebar.offset();
        var sidebarWidth = sidebar.outerWidth();
        if (e.pageX > sidebarOffset.left + sidebarWidth - 10 && 
            e.pageX < sidebarOffset.left + sidebarWidth + 10) {
          sidebar.css('cursor', 'ew-resize');
        } else {
          sidebar.css('cursor', 'default');
        }
      }
      return;
    }

    var sidebar = $('.bslib-sidebar-layout > .sidebar');
    var newWidth = sidebar.outerWidth() + (e.pageX - lastDownX);
    
    // Enforce min and max width
    newWidth = Math.max(280, Math.min(600, newWidth));
    
    sidebar.css('width', newWidth + 'px');
    $('.bslib-sidebar-layout').css('--bs-sidebar-width', newWidth + 'px');
    lastDownX = e.pageX;
  });

  $(document).on('mouseup', function() {
    if (isResizing) {
      isResizing = false;
      $('body').css('cursor', 'default');
      $('.bslib-sidebar-layout > .sidebar').css('cursor', 'default');
    }
  });
});
</script>
")

ui <- page_navbar(
  theme = app_theme,
  title = "COVID Trend Monitor",
  id = "tabs",
  selected = "Real Data",
  header = tags$style(resize_css),

  footer = tags$div(
    class="container", style="padding:12px 0; font-size:12.5px; color:#666; text-align:center;",
    HTML('Made by <a href=\"https://zeidhamadeh.com\" target=\"_blank\">Zeid Hamadeh</a>. Data: <a href=\"https://ourworldindata.org/coronavirus\" target=\"_blank\">OWID</a>.')
  ),

  nav_panel(
    "Real Data",
    layout_sidebar(
      sidebar = sidebar(
        width = 360, open = TRUE, resizable = TRUE, border = TRUE,
        
        # Main View Settings with darker grey
        tags$div(style="background-color: #e9ecef; padding: 15px; margin: 0 0 12px 0; border-radius: 8px;",
          h5("View Settings", style="margin: 0 0 12px 0; font-weight: 600;"),
          radioButtons("view_mode","View type",
            c("Global"="global","Countries"="countries","Continents"="continents"),
            selected="global"),

          conditionalPanel("input.view_mode == 'countries'",
            selectizeInput("countries","Countries", choices=NULL,
              selected=NULL, multiple=TRUE,
              options=list(maxItems=10, placeholder="Select countries"))
          ),
          conditionalPanel("input.view_mode == 'continents'",
            selectizeInput("continents","Continents", choices=NULL,
              selected=NULL, multiple=TRUE, options=list(maxItems=6))
          ),

          conditionalPanel("input.view_mode == 'global' || input.view_mode == 'countries'",
            checkboxInput("show_backdrop", "Show all countries as backdrop", FALSE)
          ),

          radioButtons("metric","Metric",
            c("Total cases"="total_cases","New cases"="new_cases",
              "Total deaths"="total_deaths","New deaths"="new_deaths"),
            selected="new_deaths"),

          checkboxInput("percap", "Per million (normalize by population)", TRUE),
          checkboxInput("real_logy","Log scale", TRUE),

          conditionalPanel("input.metric == 'new_cases' || input.metric == 'new_deaths'",
            sliderInput("smooth_k","Rolling average (days)", min=1, max=28, value=14, step=1)
          ),

          dateRangeInput("obs_dates","Date range", start="2020-03-01", end=Sys.Date())
        ),
        
        # Steep Rise Highlighting
        tags$div(style="background-color: #fff3cd; padding: 15px; margin: 0 0 12px 0; border-radius: 8px;",
          h5("Steep Rise Highlighting", style="margin: 0 0 12px 0; font-weight: 600;"),
          checkboxInput("hl_enable", "Enable highlighting", FALSE),
          conditionalPanel("input.hl_enable",
            sliderInput("hl_lookback", "Look-back window (days)", 30, 180, value=90, step=10),
            sliderInput("hl_threshold", "Growth threshold (%)", 5, 200, value=40, step=5)
          )
        ),
        
        # R0 Calculator
        tags$div(style="background-color: #d1ecf1; padding: 15px; margin: 0 0 12px 0; border-radius: 8px;",
          h5(HTML("R<sub>0</sub> Calculator"), style="margin: 0 0 12px 0; font-weight: 600;"),
          numericInput("r0_si", "Serial interval (days)", value = 4.8, min = 2, max = 10, step = 0.1),
          sliderInput("r0_window", "Window (days)", min=7, max=60, value=21, step=1),
          tableOutput("r0_table")
        ),

        downloadButton("download_real_csv","Download CSV", style="width: 100%;")
      ),
      fill = TRUE,
      card(
        full_screen = TRUE,
        card_header(tagList(
          h4(textOutput("obs_title"), class="m-0"),
          tags$small(textOutput("obs_subtitle"), class="text-muted")
        )),
        conditionalPanel(
          condition = "!output.obs_plot",
          div(style = "text-align: center; padding: 100px;",
              tags$h4("Loading data...", style = "color: #666;"))
        ),
        plotlyOutput("obs_plot", height = "620px"),
        div(style="margin-top:10px;font-size:13px;color:#666;",
            "Source: Our World in Data (", tags$a(href=OWID_URL, "OWID", target="_blank"), "). ",
            "Note: Many countries reduced or stopped reporting after 2023; ",
            "flat/zero segments may reflect reporting changes.")
      )
    )
  ),

  nav_panel(
    "Simulation",
    layout_sidebar(
      sidebar = sidebar(
        title="Simulation controls", width=340, open=TRUE, resizable = TRUE, border = TRUE,
        checkboxInput("sim_logy","Log scale (recommended)", TRUE),
        h5("Scenario A"),
        sliderInput("A_N0","Initial cases", 1, 1e5, 100, step=1),
        sliderInput("A_g","Daily growth rate (%)", 0, 100, 25, step=1),
        sliderInput("A_days","Days simulated", 14, 200, 60, step=1),
        checkboxInput("A_has_int","Add intervention for A", TRUE),
        conditionalPanel("input.A_has_int",
          sliderInput("A_int_day","Intervention day", 1, 200, 20, step=1),
          sliderInput("A_g_after","Post-intervention growth (%)", -50, 100, 5, step=1)
        ),
        h5("Scenario B"),
        checkboxInput("B_on","Enable Scenario B", TRUE),
        conditionalPanel("input.B_on",
          sliderInput("B_N0","Initial cases (B)", 1, 1e5, 100, step=1),
          sliderInput("B_g","Daily growth (B, %)", 0, 100, 15, step=1),
          sliderInput("B_days","Days (B)", 14, 200, 60, step=1),
          checkboxInput("B_has_int","Add intervention for B", FALSE),
          conditionalPanel("input.B_has_int",
            sliderInput("B_int_day","Intervention day (B)", 1, 200, 25, step=1),
            sliderInput("B_g_after","Post-intervention growth (B, %)", -50, 100, 3, step=1)
          )
        ),
        h5("Animation"),
        sliderInput("anim_day","Time-lapse day", 0, 200, 60, step=1,
                    animate=animationOptions(interval=400, loop=TRUE)),
        h5("Export"),
        radioButtons("export_format","Format", c("PNG","PDF"), inline=TRUE),
        downloadButton("download_plot","Download Figure")
      ),
      fill = TRUE,
      card(full_screen=TRUE,
           card_header("Simulated Exponential Growth"),
           plotlyOutput("sim_plot", height="540px"))
    )
  ),

  nav_panel(
    "About",
    card(
      class="mx-auto",
      style="max-width:900px;",
      card_header("About this project"),
      card_body(
        HTML("
<p><strong>COVID Trend Monitor</strong> lets you explore global COVID-19 time series and simple growth simulations.</p>
<ul>
<li><strong>Real Data:</strong> Global/continent/country series from OWID.</li>
<li><strong>Features:</strong> Per-capita normalization, log scaling, <em>steep-rise highlighting</em>, optional global backdrop, and an <em>R₀</em> estimator.</li>
<li><strong>Simulation:</strong> Two scenarios with optional interventions.</li>
</ul>
<p>Built by <strong>Zeid Hamadeh</strong> (zeidhamadeh.com).</p>
        ")
      )
    )
  )
)

server <- function(input, output, session){

  owid <- reactiveVal(NULL)

  # Load data on startup with higher priority
  observe({
    dat <- tryCatch(fetch_owid(), error=function(e) {
      showNotification("Failed to load OWID data. Please refresh the page.", type = "error")
      return(NULL)
    })
    
    if (!is.null(dat) && nrow(dat) > 0) {
      dat$population <- suppressWarnings(as.double(dat$population))
      owid(dat)
      
      updateDateRangeInput(session, "obs_dates",
        start = max(as.Date("2020-03-01"), min(dat$date, na.rm=TRUE)),
        end   = max(dat$date, na.rm=TRUE)
      )
      
      # FIXED: Update countries dropdown with default selection
      all_countries <- sort(unique(dat$country))
      default_countries <- c("Canada", "United States")
      # Only use defaults that exist in the data
      default_countries <- intersect(default_countries, all_countries)
      
      updateSelectizeInput(session, "countries",
        choices = all_countries,
        selected = default_countries,
        server = TRUE
      )
      updateSelectizeInput(session, "continents",
        choices = sort(unique(na.omit(dat$continent))),
        server = TRUE
      )
    }
  }, priority = 100)

  agg_by_date <- function(df, metric){
    df %>%
      group_by(date) %>%
      summarise(
        value   = sum(.data[[metric]], na.rm=TRUE),
        pop_sum = sum(population, na.rm=TRUE),
        .groups = "drop"
      )
  }
  agg_by_key_date <- function(df, key, metric){
    df %>%
      group_by(.data[[key]], date) %>%
      summarise(
        value   = sum(.data[[metric]], na.rm=TRUE),
        pop_sum = sum(population, na.rm=TRUE),
        .groups = "drop"
      ) %>% rename(series = !!key)
  }

  real_parts <- reactive({
    dat <- owid()
    req(dat, nrow(dat) > 0)
    
    rng <- input$obs_dates %||% c(min(dat$date, na.rm=TRUE), max(dat$date, na.rm=TRUE))
    metric <- input$metric %||% "new_deaths"
    percap <- isTRUE(input$percap %||% TRUE)
    vm <- input$view_mode %||% "global"
    k <- input$smooth_k %||% 14

    df <- dat %>% filter(date >= rng[1], date <= rng[2])
    
    # Ensure we have data after filtering
    req(nrow(df) > 0)

    if (vm == "global") {
      main <- agg_by_date(df, metric) %>% mutate(series="Global")
    } else if (vm == "continents") {
      sel <- input$continents
      if (is.null(sel) || length(sel) == 0) {
        # If no continents selected, show all
        sel <- unique(na.omit(df$continent))
      }
      main <- df %>% filter(continent %in% sel) %>% agg_by_key_date("continent", metric)
    } else {
      # Countries view
      sel <- input$countries
      if (is.null(sel) || length(sel) == 0) {
        # Default to Canada and US if available
        all_countries <- unique(df$country)
        default_countries <- c("Canada", "United States")
        sel <- intersect(default_countries, all_countries)
        # If neither is available, pick the first two countries
        if (length(sel) == 0 && length(all_countries) >= 2) {
          sel <- all_countries[1:2]
        } else if (length(sel) == 0 && length(all_countries) == 1) {
          sel <- all_countries[1]
        }
      }
      main <- df %>% filter(country %in% sel) %>% agg_by_key_date("country", metric)
    }
    
    # Ensure we have data after aggregation
    req(nrow(main) > 0)

    if (percap) {
      main <- main %>% mutate(value = ifelse(pop_sum > 0, value/(pop_sum/1e6), NA_real_))
    }
    main <- main %>% select(date, series, value) %>% arrange(series, date)

    smoothed <- main
    if (metric %in% c("new_cases","new_deaths") && k > 1) {
      main <- main %>% group_by(series) %>% arrange(date, .by_group=TRUE) %>%
        mutate(value = roll_mean(value, k)) %>% ungroup()
      smoothed <- main
    }

    ref <- NULL; backdrop <- NULL
    if (vm %in% c("countries","continents")) {
      ref <- agg_by_date(df, metric)
      if (percap) ref <- ref %>% mutate(value = ifelse(pop_sum>0, value/(pop_sum/1e6), NA_real_))
      ref <- ref %>% select(date, value) %>% arrange(date)
      if (metric %in% c("new_cases","new_deaths") && k > 1) ref <- ref %>% mutate(value = roll_mean(value, k))
    }
    
    # FIXED: Optimized backdrop generation - works for both global and countries view
    if ((vm == "global" || vm == "countries") && isTRUE(input$show_backdrop %||% FALSE)) {
      # Only use countries with significant case/death counts to reduce rendering load
      country_totals <- df %>%
        group_by(country) %>%
        summarise(total_metric = sum(.data[[metric]], na.rm=TRUE), .groups="drop") %>%
        arrange(desc(total_metric)) %>%
        slice_head(n = 50)  # Only top 50 countries
      
      tmp <- df %>%
        filter(country %in% country_totals$country) %>%
        agg_by_key_date("country", metric)
      
      if (percap) tmp <- tmp %>% mutate(value = ifelse(pop_sum>0, value/(pop_sum/1e6), NA_real_))
      backdrop <- tmp %>% select(series, date, value) %>% arrange(series, date)
      if (metric %in% c("new_cases","new_deaths") && k > 1)
        backdrop <- backdrop %>% group_by(series) %>% mutate(value=roll_mean(value, k)) %>% ungroup()
      
      # More aggressive subsampling - every 5th point
      backdrop <- backdrop %>% group_by(series) %>% slice(seq(1, n(), by = 5)) %>% ungroup()
      if (isTRUE(input$real_logy %||% TRUE)) backdrop <- backdrop %>% filter(value > 0)
    }

    # Steep rise detection with debugging
    highlight <- tibble(start=as.Date(character()), end=as.Date(character()))
    
    if (isTRUE(input$hl_enable %||% TRUE) && nrow(smoothed) > 0) {
      tryCatch({
        lookback <- as.numeric(input$hl_lookback %||% 90)
        thresh   <- as.numeric(input$hl_threshold %||% 40) / 100
        
        message("=== Highlighting Debug ===")
        message("Threshold: ", thresh * 100, "%")
        message("Lookback: ", lookback, " days")
        message("Smoothed data rows: ", nrow(smoothed))
        
        # Get the date range for highlighting
        visible_end   <- max(smoothed$date, na.rm=TRUE)
        visible_start <- min(smoothed$date, na.rm=TRUE)
        recent_min    <- max(visible_start, visible_end - lookback)
        
        message("Date range: ", recent_min, " to ", visible_end)
        
        # Work on ALL data in the lookback window, not filtered by series
        recent_data <- smoothed %>%
          filter(date >= recent_min & date <= visible_end) %>%
          group_by(series) %>%
          arrange(date) %>%
          mutate(
            # Get value from 7 days ago
            value_7days_ago = dplyr::lag(value, 7),
            # Calculate growth: needs both values to be positive
            has_both = !is.na(value) & !is.na(value_7days_ago) & 
                      value > 0 & value_7days_ago > 0,
            # Growth rate calculation
            growth_rate = if_else(has_both, (value / value_7days_ago) - 1, NA_real_),
            # Check if exceeds threshold
            exceeds = !is.na(growth_rate) & growth_rate >= thresh
          ) %>%
          ungroup()
        
        message("Recent data points: ", nrow(recent_data))
        message("Points with valid growth: ", sum(!is.na(recent_data$growth_rate)))
        message("Points exceeding threshold: ", sum(recent_data$exceeds, na.rm = TRUE))
        
        # Show some example growth rates
        sample_growth <- recent_data %>% 
          filter(!is.na(growth_rate)) %>% 
          arrange(desc(growth_rate)) %>% 
          head(5)
        if (nrow(sample_growth) > 0) {
          message("Top 5 growth rates:")
          for (i in 1:min(5, nrow(sample_growth))) {
            message("  ", sample_growth$date[i], " (", sample_growth$series[i], "): ", 
                   round(sample_growth$growth_rate[i] * 100, 1), "%")
          }
        }
        
        # Find dates that exceed threshold
        if (any(recent_data$exceeds, na.rm = TRUE)) {
          flagged_data <- recent_data %>%
            filter(exceeds) %>%
            arrange(date)
          
          message("Flagged dates: ", nrow(flagged_data))
          
          if (nrow(flagged_data) > 0) {
            # Group consecutive dates
            flagged_dates <- flagged_data$date
            
            # Calculate gaps between consecutive flagged dates
            if (length(flagged_dates) > 1) {
              date_gaps <- c(999, as.numeric(diff(flagged_dates)))
              # Group if gap is more than 1 day
              groups <- cumsum(date_gaps > 1)
            } else {
              groups <- 1
            }
            
            # Create ranges
            ranges_df <- tibble(date = flagged_dates, group = groups) %>%
              group_by(group) %>%
              summarise(
                start = min(date),
                end = max(date),
                .groups = "drop"
              ) %>%
              select(start, end)
            
            message("Created ", nrow(ranges_df), " highlight ranges")
            if (nrow(ranges_df) > 0) {
              message("Ranges:")
              for (i in 1:nrow(ranges_df)) {
                message("  ", ranges_df$start[i], " to ", ranges_df$end[i])
              }
              highlight <- ranges_df
            }
          }
        } else {
          message("No dates exceed threshold")
        }
      }, error = function(e) {
        message("ERROR in highlighting: ", e$message)
        message("Traceback: ", paste(traceback(), collapse = "\n"))
      })
    }

    # Apply log scale filter AFTER calculating highlights
    # This prevents NaN warnings in log transformation
    if (isTRUE(input$real_logy %||% TRUE)) {
      # Filter out non-positive values to avoid log(0) or log(negative)
      main <- main %>% 
        filter(!is.na(value), is.finite(value), value > 0) %>%
        mutate(value = ifelse(value < 1e-10, 1e-10, value))  # Set minimum threshold
      
      if (!is.null(ref)) {
        ref <- ref %>% 
          filter(!is.na(value), is.finite(value), value > 0) %>%
          mutate(value = ifelse(value < 1e-10, 1e-10, value))
      }
    }

    list(main=main, ref=ref, highlight=highlight, backdrop=backdrop)
  })

  output$obs_title <- renderText({
    metric_label(input$metric %||% "new_deaths", isTRUE(input$percap %||% TRUE))
  })
  output$obs_subtitle <- renderText({
    bits <- c()
    if ((input$metric %||% "new_deaths") %in% c("new_cases","new_deaths"))
      bits <- c(bits, paste0((input$smooth_k %||% 14), "-day average"))
    if (isTRUE(input$hl_enable %||% TRUE)) {
      threshold_pct <- input$hl_threshold %||% 40
      bits <- c(bits, paste0("Highlighting rises in last ", input$hl_lookback %||% 90,
                             " days where 7-day growth ≥ ", threshold_pct, "%"))
    }
    paste(bits, collapse = " • ")
  })

  output$obs_plot <- renderPlotly({
    req(owid())  # Ensure data is loaded
    parts <- real_parts()
    df <- parts$main
    ref <- parts$ref
    hl <- parts$highlight
    bd <- parts$backdrop
    
    message("=== Plot Rendering ===")
    message("Main data rows: ", nrow(df))
    message("Highlight ranges: ", nrow(hl))
    
    validate(need(nrow(df)>0, "No data for the selected filters/dates. Try turning off log scale or widening the date range."))
    
    # Create pure plotly plot instead of ggplotly conversion
    use_log <- isTRUE(input$real_logy %||% TRUE)
    y_label <- metric_label(input$metric %||% "new_deaths", isTRUE(input$percap %||% TRUE))
    
    # Start with empty plot
    p <- plot_ly()
    
    # Add backdrop lines if present
    if (!is.null(bd) && nrow(bd) > 0) {
      for (s in unique(bd$series)) {
        bd_series <- bd %>% filter(series == s)
        p <- p %>% add_trace(
          data = bd_series,
          x = ~date, y = ~value,
          type = "scatter", mode = "lines",
          line = list(color = "rgba(128,128,128,0.1)", width = 0.5),
          showlegend = FALSE,
          hoverinfo = "skip"
        )
      }
    }
    
    # Add highlight rectangles FIRST (so they appear behind data)
    if (nrow(hl) > 0) {
      message("Adding ", nrow(hl), " highlight shapes")
      shapes <- lapply(1:nrow(hl), function(i) {
        list(
          type = "rect",
          xref = "x", yref = "paper",
          x0 = as.character(hl$start[i]),
          x1 = as.character(hl$end[i]),
          y0 = 0, y1 = 1,
          fillcolor = "rgba(255, 107, 107, 0.25)",
          line = list(width = 0),
          layer = "below"
        )
      })
      p <- p %>% layout(shapes = shapes)
    }
    
    # Add global reference line if present
    if (!is.null(ref) && nrow(ref) > 0) {
      p <- p %>% add_trace(
        data = ref,
        x = ~date, y = ~value,
        type = "scatter", mode = "lines",
        line = list(color = "rgba(128,128,128,0.7)", width = 2),
        name = "Global",
        showlegend = TRUE
      )
    }
    
    # Add main data lines
    for (s in unique(df$series)) {
      df_series <- df %>% filter(series == s)
      p <- p %>% add_trace(
        data = df_series,
        x = ~date, y = ~value,
        type = "scatter", mode = "lines",
        line = list(width = 2.5),
        name = s,
        showlegend = TRUE
      )
    }
    
    # Configure layout
    p <- p %>% layout(
      xaxis = list(title = "Date"),
      yaxis = list(
        title = y_label,
        type = if(use_log) "log" else "linear"
      ),
      legend = list(orientation = "h", y = 1.1, x = 0),
      hovermode = "x unified",
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
    
    p
  })

  output$download_real_csv <- downloadHandler(
    filename = function() {
      m <- input$metric %||% "new_deaths"
      paste0("realdata_", m, if (isTRUE(input$percap %||% TRUE)) "_perM" else "", ".csv")
    },
    content  = function(file) {
      parts <- real_parts(); readr::write_csv(parts$main, file)
    }
  )

  output$r0_table <- renderTable({
    parts <- real_parts(); df <- parts$main
    req(nrow(df) > 0)
    win <- input$r0_window %||% 21
    Tg  <- input$r0_si %||% 4.8
    latest <- max(df$date, na.rm=TRUE)
    start  <- latest - win
    dat <- df %>% filter(date > start, !is.na(value), is.finite(value), value > 0)
    by_series <- split(dat, dat$series)
    res <- map_df(by_series, function(d){
      if (nrow(d) < 5) return(tibble(series=unique(d$series), r_per_day=NA_real_))
      tnum <- as.numeric(d$date - min(d$date))
      # Use safe_log which handles edge cases
      fit <- tryCatch(lm(safe_log(d$value) ~ tnum), error=function(e) NULL)
      if (is.null(fit)) return(tibble(series=unique(d$series), r_per_day=NA_real_))
      tibble(series=unique(d$series), r_per_day=coef(fit)[2])
    }) %>%
      mutate(
        R0 = exp(r_per_day * Tg),
        `Doubling/Halving (days)` = dplyr::case_when(
          is.na(r_per_day) ~ NA_real_,
          r_per_day >  0   ~ log(2)/r_per_day,
          r_per_day <  0   ~ -log(2)/r_per_day,
          TRUE             ~ NA_real_
        )
      ) %>%
      transmute(
        Series = series,
        R0 = round(R0, 2),
        `Doubling/Halving (days)` = ifelse(is.finite(`Doubling/Halving (days)`),
                                           round(`Doubling/Halving (days)`, 2), "—")
      )
    res
  }, striped = TRUE, bordered = FALSE, spacing = "s", width = "100%")

  observe({
    A <- input$A_days %||% 60
    B <- if (isTRUE(input$B_on %||% TRUE)) (input$B_days %||% 60) else 0
    mx <- max(A,B,na.rm=TRUE); updateSliderInput(session, "anim_day", max=mx)
    if ((input$anim_day %||% 0) <= 0 || (input$anim_day %||% 0) > mx) updateSliderInput(session,"anim_day",value=mx)
  })

  simulate <- function(N0,g,days,has_int,int_day,g_after){
    simulate_curve(N0, g, days,
      intervention_day = if (isTRUE(has_int)) int_day else NA,
      g_after = if (isTRUE(has_int)) g_after else NA)
  }

  sim_data <- reactive({
    A <- simulate(input$A_N0 %||% 100, (input$A_g %||% 25)/100, input$A_days %||% 60,
                  input$A_has_int %||% TRUE, input$A_int_day %||% 20, (input$A_g_after %||% 5)/100) %>%
      mutate(scenario="A")
    if (isTRUE(input$B_on %||% TRUE)) {
      B <- simulate(input$B_N0 %||% 100, (input$B_g %||% 15)/100, input$B_days %||% 60,
                    input$B_has_int %||% FALSE, input$B_int_day %||% 25, (input$B_g_after %||% 3)/100) %>%
        mutate(scenario="B")
      bind_rows(A,B)
    } else A
  })

  output$sim_plot <- renderPlotly({
    gg <- make_sim_plot(sim_data(), "Simulated Exponential Growth", ylog=isTRUE(input$sim_logy %||% TRUE))
    ggplotly(gg, tooltip=c("x","y","color")) %>% layout(legend=list(orientation="h", y=1.1))
  })

  output$download_plot <- downloadHandler(
    filename = function() paste0("epidemic_curve.", ifelse((input$export_format %||% "PNG")=="PNG","png","pdf")),
    content = function(file){
      gg <- make_sim_plot(sim_data(), "Simulated Exponential Growth", ylog=isTRUE(input$sim_logy %||% TRUE))
      if ((input$export_format %||% "PNG")=="PNG") { ragg::agg_png(file,1400,900,150); print(gg); dev.off() }
      else { grDevices::pdf(file,12,8); print(gg); dev.off() }
    }
  )
}

shinyApp(ui, server)