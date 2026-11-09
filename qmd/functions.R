#' @title Apply Theme to a Flextable
#'
#' @description
#' Applies a consistent visual theme to a \code{flextable} object,
#' including font, colors, borders, and optional captions.
#'
#' @param ft_tbl A \code{flextable} object to which the theme will be applied.
#' @param title Optional. A title to add to the flextable. Default: \code{NULL}.
#' @param subtitle Optional. A subtitle to add to the flextable. Default: \code{NULL}.
#' @param header_color A character string specifying the header background color.
#'   Default: \code{'#2C3E50'}.
#' @param stripe_color A character string specifying the background color for
#'   striped rows. Default: \code{'#F8F9FA'}.
#' @param grouped_column Logical. Whether the first column represents grouped data
#'   and should be formatted differently. Default: \code{FALSE}.
#'
#' @return
#' A \code{flextable} object with the theme applied.
#'
#' @details
#' This function standardizes the appearance of \code{flextable} outputs for
#' reports. It applies consistent font, font size, bolding, color, background,
#' padding, borders, horizontal lines, alignment, and captions. Optional title and
#' subtitle can be added. The first column can be formatted for grouped data if
#' \code{grouped_column = TRUE}.
#'
#' @seealso
#'  \code{\link[flextable]{font}}, \code{\link[flextable]{fontsize}},
#'  \code{\link[flextable]{bold}}, \code{\link[flextable]{color}},
#'  \code{\link[flextable]{bg}}, \code{\link[flextable]{padding}},
#'  \code{\link[flextable]{border_remove}}, \code{\link[flextable]{hline_bottom}},
#'  \code{\link[flextable]{hline}}, \code{\link[flextable]{align}},
#'  \code{\link[flextable]{set_caption}}, \code{\link[flextable]{add_header_lines}},
#'  \code{\link[flextable]{autofit}}, \code{\link[officer]{fp_border}}
#'
#' @rdname ft_add_sofit_theme
#' @keywords internal
#' @importFrom flextable font fontsize bold color bg padding border_remove hline_bottom hline align set_caption add_header_lines autofit
#' @importFrom officer fp_border

ft_add_theme <- function(
  ft_tbl,
  title = NULL,
  subtitle = NULL,
  header_color = "#2C3E50",
  stripe_color = "#F8F9FA",
  grouped_column = FALSE
) {
  # Get column names
  col_names <- colnames(ft_tbl$body$dataset)
  n_cols <- length(col_names)
  if (grouped_column) {
    n_cols <- n_cols - 1
  }

  # Basic table styling
  ft_tbl <- ft_tbl |>
    # Set font
    flextable::font(fontname = "Arial", part = "all") |>
    flextable::fontsize(size = 11, part = "body") |>
    flextable::fontsize(size = 11, part = "header") |>

    # Header styling - bold white text on dark background
    flextable::bold(part = "header") |>
    flextable::color(color = "white", part = "header") |>
    flextable::bg(bg = header_color, part = "header") |>

    # Body text styling
    flextable::color(color = "#2C3E50", part = "body") |>
    flextable::fontsize(size = 11, part = "body") |>

    # Padding
    flextable::padding(padding.top = 8, padding.bottom = 8, part = "all") |>
    flextable::padding(
      padding.top = 10,
      padding.bottom = 10,
      part = "header"
    ) |>

    # Borders
    flextable::border_remove() |>

    # Header bottom border (thick dark line)
    flextable::hline_bottom(
      part = "header",
      border = officer::fp_border(color = "#444444", width = 3)
    ) |>

    # Body horizontal lines (thin gray)
    flextable::hline(
      part = "body",
      border = officer::fp_border(color = "#E5E5E5", width = 1)
    ) |>

    # Bottom border (thick dark line)
    flextable::hline_bottom(
      part = "body",
      border = officer::fp_border(color = "#444444", width = 2)
    )

  # Striped rows (every other row)
  if (nrow(ft_tbl$body$dataset) > 1) {
    ft_tbl <- ft_tbl |>
      flextable::bg(
        i = seq(2, nrow(ft_tbl$body$dataset), 2),
        bg = stripe_color,
        part = "body"
      )
  }

  # Center alignment for all columns except the first
  if (n_cols > 1) {
    ft_tbl <- ft_tbl |>
      flextable::align(j = 2:n_cols, align = "center", part = "all")
  }

  # Left align first column
  ft_tbl <- ft_tbl |>
    flextable::align(j = 1, align = "left", part = "all")

  # Add title and subtitle if provided
  if (!is.null(title)) {
    if (!is.null(subtitle)) {
      # Add both title and subtitle
      ft_tbl <- ft_tbl |>
        flextable::set_caption(
          caption = title,
          style = "Table Caption",
          autonum = NULL
        ) |>
        flextable::add_header_lines(values = subtitle) |>
        flextable::bold(i = 1, part = "header") |>
        flextable::fontsize(i = 1, size = 20, part = "header") |>
        flextable::align(i = 1, align = "center", part = "header")

      # Style subtitle row if it exists
      if (nrow(ft_tbl$header$dataset) > 1) {
        ft_tbl <- ft_tbl |>
          flextable::fontsize(i = 2, size = 14, part = "header") |>
          flextable::align(i = 2, align = "center", part = "header") |>
          flextable::color(i = 2, color = "#666666", part = "header") |>
          flextable::bg(i = 2, bg = "white", part = "header") |>
          flextable::bold(i = 2, bold = FALSE, part = "header")
      }
    } else {
      # Add title only
      ft_tbl <- ft_tbl |>
        flextable::add_header_lines(values = title) |>
        flextable::bold(i = 1, part = "header") |>
        flextable::fontsize(i = 1, size = 20, part = "header") |>
        flextable::align(i = 1, align = "center", part = "header") |>
        flextable::bg(i = 1, bg = "white", part = "header") |>
        flextable::color(i = 1, color = "#2C3E50", part = "header")
    }
  }

  # Auto-fit to optimize column widths
  ft_tbl <- ft_tbl |>
    flextable::autofit()

  return(ft_tbl)
}

#' @title Apply Theme to a gt Table
#'
#' @description
#' Applies a consistent visual theme to a \code{gt} table, including
#' fonts, header styling, row striping, alignment, and optional title/subtitle.
#'
#' @param gt_tbl A \code{gt} table object to which the theme will be applied.
#' @param title Optional. A title to add to the gt table. Default: \code{NULL}.
#' @param subtitle Optional. A subtitle to add to the gt table. Default: \code{NULL}.
#' @param header_color A character string specifying the color of the table header.
#'   Default: \code{'#2C3E50'}.
#' @param stripe_color A character string specifying the background color for
#'   striped rows. Default: \code{'#F8F9FA'}.
#'
#' @return
#' A \code{gt} table with the theme applied.
#'
#' @details
#' This function standardizes the appearance of \code{gt} tables for reports.
#' It sets fonts, header and row styling, column alignment, optional title and
#' subtitle, and alternating row colors. The theme ensures consistency across all
#' tables.
#'
#' @seealso
#'  \code{\link[gt]{opt_table_font}}, \code{\link[gt]{google_font}},
#'  \code{\link[gt]{default_fonts}}, \code{\link[gt]{tab_options}},
#'  \code{\link[gt]{px}}, \code{\link[gt]{tab_style}}, \code{\link[gt]{cell_text}},
#'  \code{\link[gt]{cell_fill}}, \code{\link[gt]{cells_column_labels}},
#'  \code{\link[gt]{everything}}, \code{\link[gt]{cells_body}},
#'  \code{\link[gt]{opt_row_striping}}, \code{\link[gt]{cols_align}},
#'  \code{\link[gt]{tab_header}}, \code{\link[gt]{md}}
#'
#' @rdname gt_add_sofit_theme
#' @importFrom gt opt_table_font google_font default_fonts tab_options px tab_style cell_text cell_fill cells_column_labels everything cells_body opt_row_striping cols_align tab_header md

gt_add_sofit_theme <- function(
  gt_tbl,
  title = NULL,
  subtitle = NULL,
  header_color = "#2C3E50",
  stripe_color = "#F8F9FA"
) {
  gt_tbl <- gt_tbl |>
    # Set table font and size
    gt::opt_table_font(
      font = list(
        gt::google_font("Lato"),
        gt::default_fonts()
      )
    ) |>
    gt::tab_options(
      table.font.size = gt::px(15),
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      data_row.padding = gt::px(8),
      heading.title.font.size = gt::px(20),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(14),
      heading.align = "center",
      column_labels.font.weight = "bold",
      column_labels.font.size = gt::px(15),
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = "#444444",
      column_labels.padding = gt::px(10),
      table_body.hlines.color = "#E5E5E5",
      table_body.hlines.width = gt::px(1),
      table_body.border.bottom.color = "#444444",
      table_body.border.bottom.width = gt::px(2),
      table.align = "left"
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold", color = "white"),
        gt::cell_fill(color = header_color)
      ),
      locations = gt::cells_column_labels(gt::everything())
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "#2C3E50", size = gt::px(14)),
      locations = gt::cells_body()
    ) |>
    gt::opt_row_striping(row_striping = TRUE) |>
    gt::tab_style(
      style = gt::cell_fill(color = stripe_color),
      locations = gt::cells_body(rows = seq(2, nrow(gt_tbl[["_data"]]), 2))
    )

  # Center all columns except the first column
  col_names <- names(gt_tbl[["_data"]])
  if (length(col_names) > 1) {
    gt_tbl <- gt_tbl |>
      gt::cols_align(
        align = "center",
        columns = col_names[-1]
      )
  }
  if (!is.null(title)) {
    gt_tbl <- gt_tbl |>
      gt::tab_header(
        title = gt::md(paste0("**", title, "**")),
        subtitle = subtitle
      )
  }

  return(gt_tbl)
}
