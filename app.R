library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(purrr)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(tidyr)

tsunami_events <- read.csv("data/processed/tsunami-events.csv")
country_codes <- read.csv("data/processed/country_codes.csv")

years = unique(tsunami_events[['year']])
countries = sort(unique(tsunami_events[['country']]))

app = Dash$new(external_stylesheets = dbcThemes$QUARTZ)

navbar = dbcNavbar(
    dbcContainer(
        list(
            htmlA(
                dbcRow(
                    list(
                        dbcCol(dbcNavbarBrand('Tsunami Events Dashboard'))
                    ),
                    align = 'center',
                    className = 'g-0'
                )
            ),
            dbcNavbarToggler(id = 'navbar-toggler', n_clicks = 0),
            dbcCollapse(
                id = 'navbar-collapse',
                is_open = FALSE,
                navbar = TRUE,
            )
        )
    ),
    color = 'dark',
    dark = TRUE
)

world_plot_card <- dbcCard(
    dbcCardBody(list(
        htmlH6('Total Tsunami Hits by Country with Origin Points'),
        dccGraph(id = 'map_plot')
    )
    )
)

app$layout(dbcContainer(
    list(
        navbar,
        dbcRow(list(
            dbcCol(list(
                htmlH5('Years and Countries Selection', className='form-label'),
                htmlHr(),
                htmlH6('Years of Interest (1802 - 2022)', className='form-label'),
                dccRangeSlider(
                    id = 'year_slider',
                    min=min(tsunami_events$year),
                    max=max(tsunami_events$year),
                    value= list(min(tsunami_events$year), max(tsunami_events$year)),
                    allowCross=FALSE,
                    marks = list(
                        "1802" = "1802",
                        "1850" = "1850",
                        "1900" = "1900",
                        "1950" = "1950",
                        "2000" = "2000",
                        "2022" = "2022"),
                    ),
                htmlBr(),
                htmlBr(),
                htmlH6('Countries of Interest', className='form-label'),
                dccDropdown(
                    id = 'country_select',
                    multi = TRUE,
                    value = list(),
                    options = countries,
                    className = 'text-dark')
            ), width = 4),
            dbcCol(list(
                world_plot_card
                ),
                width = 8
            )
        ))
    )
))


#App callback for world_map_plot
app$callback(
  output('map_plot', 'figure'),
  list(input('year_slider', 'value'),
       input('country_select', 'value')),
  function(years, countries) {
      create_map_plot(years[1], years[2], countries)
  }
)

create_map_plot <- function(year_start, year_end, countries) {
    if (as.integer(year_start) > as.integer(year_end)) {
        stop("Invalid value for year start and/or year end")
    }

    if (typeof(countries) != "list") {
        stop("Invalid value for countries")
    }

    if (length(countries) > 0) {
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries)
    }

    tsunami_events <- tsunami_events %>%
        filter(year >= year_start,
               year <= year_end)

    counts <- tsunami_events %>%
        group_by(country) %>%
        summarise(count = n())
    
    counts <- right_join(
        counts,
        country_codes,
        by = c("country" = "name")
    ) %>%
        mutate_at(vars(count), ~replace_na(., 0))
    
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
    )
    
    fig <- plot_geo(counts)
    
    colorscale <- data.frame(
        z =  c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.4, 0.8, 1.0),
        col = brewer.pal(9, "Blues")
    )
    
    fig <- fig %>%
        add_trace(
            z = ~count, color = ~count, colors = 'Blues',
            text = ~paste("Country:", country, "\nHits:", count), locations = ~alpha.3,
            marker = list(line = l),
            colorscale = colorscale,
            colorbar = list(title = "Tsunami Hits"),
            hoverinfo = "text",
            zmin = 1,
            zmax = max(counts$count)
        )  %>%
        layout(geo = g) %>%
        add_markers(data = tsunami_events, y = ~latitude, x = ~longitude,
                    size = 2, marker = list(size = 2, opacity=0.4, 
                                            color = "red"),
                    text = ~paste("Earthquake Magnitude:",
                                  earthquake_magnitude),
                    hoverinfo = "text"
                   )
    
    fig
}

app$run_server(host = '0.0.0.0')
