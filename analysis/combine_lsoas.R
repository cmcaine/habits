# Process LSOAs
library(sf)
library(geojsonio)

lsoas = list()
index = 1
for (LA in c("E08000020", "E08000021", "E08000022", "E08000023", "E08000024")) {
  filename = paste("https://martinjc.github.io/UK-GeoJSON/json/eng/lsoa_by_lad/topo_", LA, ".json", sep = "")
  lsoas[[index]] = st_as_sf(geojson_read(filename, what = "sp"))
  index = index + 1
}

# rbind
combined_lsoas = do.call(rbind, lsoas)

save(combined_lsoas, file = "data/combined_lsoas.RData")

rm(list = ls())
