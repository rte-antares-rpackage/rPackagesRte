
#  ------------------------------------------------------------------------
#
# Title : Theme flexdashboard
#    By : Victor
#  Date : 2020-01-27
#
#  ------------------------------------------------------------------------


library(fresh)

create_theme(
  theme = "cosmo",
  bs_vars_color(brand_primary = "#112446"),
  bs_vars_global(body_bg = "#FFF", text_color = "#808488"),
  bs_vars_font(family_sans_serif = "Roboto,sans-serif", size_base = "16px"),
  bs_vars_navbar(
    # height = "80px",
    default_bg = "#FFF",
    default_color = "#112446",
    default_link_color = "#112446",
    default_link_active_color = "#112446",
    default_link_active_bg = "#112446",
    default_link_hover_color = "#112446",
    inverse_bg = "#FFF",
    inverse_color = "#112446",
    inverse_link_color = "#112446",
    inverse_link_active_color = "#112446",
    inverse_link_active_bg = "#112446",
    inverse_link_hover_color = "#112446"
  ),
  output_file = "assets/theme-antares.min.css", 
  include_assets = TRUE
)
