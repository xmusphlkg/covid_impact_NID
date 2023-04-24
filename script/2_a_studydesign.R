ggplot() +
     geom_line(
          mapping = aes(x = date, y = value, colour = 'Observed'),
          size = 0.7,
          data = filter(datafile_single, date <= split_date)
     ) +
     geom_line(
          mapping = aes(x = date, y = fit, colour = 'Fitted'),
          size = 0.7,
          data = outcome_plot_1
     ) +
     annotate(
          'text',
          x = median(outcome_plot_1$date),
          y = Inf,
          label = 'Train Database',
          vjust = 1
     ) +
     coord_cartesian(ylim = c(0, NA)) +
     scale_x_date(
          expand = expansion(add = c(0, 31)),
          date_labels = '%Y',
          breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date) + 31, by =
                            "2 years")
     ) +
     scale_y_continuous(
          expand = c(0, 0),
          breaks = pretty(c(min_value, max_value, 0)),
          limits = range(pretty(c(
               min_value, max_value, 0
          )))
     ) +
     scale_color_manual(values = c(
          Fitted = "#00A087B2",
          Forecasted = "#DC0000B2",
          Observed = '#3C5488B2'
     )) +
     theme_set() +
     theme(legend.position = 'none',
           plot.margin = margin(5, 15, 5, 5)) +
     labs(
          x = "Date",
          y = 'Cases',
          color = '',
          title = paste0(LETTERS[6], ': ', "Hybrid")
     )
ggsave(filename = paste0('./fig/test.pdf'),
       width = 7, height = 3.6, family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf)
