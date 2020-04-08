
# Load fonts when loading the package

.onLoad <- function(libname = find.package("extrafont"), pkgname = "extrafont") {
  extrafont::loadfonts(device = "win")
}
