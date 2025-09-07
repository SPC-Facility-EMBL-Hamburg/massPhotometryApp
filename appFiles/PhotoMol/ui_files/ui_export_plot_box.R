box(
  title = "Plot options", width = 3, solidHeader = T, status = "primary",

  fluidRow(

    column(
      width=10,
      offset=1,
      p(HTML('<p style="margin-bottom:0px;"></p>'),
        actionButton(
          inputId = "configure_plot_btn",
          label = "Configure",
          icon("palette"),
          style="color: #fff; background-color: #337ab7;
          border-color: #2e6da4")
      )
    )
  )
)
