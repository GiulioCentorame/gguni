####################################################
# Generate hex colour database for `R/sysdata.rda` #
####################################################

# Declare list
colour_data <- list()

# Colours from UQ branding guidelines #
#######################################

# Main colour contains only UQ purple
colour_data[["uq"]][["main"]] <- c(
  "UQ Purple" = "#51247A"
)

# Secondary colours contain all the secondary colours (in order)

colour_data[["uq"]][["secondary"]] <- c(
  "Light purple" = "#962A8B",
  "Red" = "#E62645",
  "Green" = "#2EA836",
  "Gold" = "#BB9D65",
  "Orange" = "#EB602B",
  "Yellow" = "#FBB800",
  "Blue" = "#4085C6",
  "Aqua" = "#00A2C7"
)

# BW contains black and white colours

colour_data[["uq"]][["bw"]] <- c(
  "Black" = "#FFFFFF",
  "White" = "#000000"
)

# Discrete colours are main colour + secondary colours (in order)

colour_data[["uq"]][["discrete"]] <- c(
  "UQ Purple" = "#51247A",
  "Light purple" = "#962A8B",
  "Red" = "#E62645",
  "Green" = "#2EA836",
  "Gold" = "#BB9D65",
  "Orange" = "#EB602B",
  "Yellow" = "#FBB800",
  "Blue" = "#4085C6",
  "Aqua" = "#00A2C7"
)

# Continuous colours use the continuous gradient from UQ Purple
# to Light purple

colour_data[["uq"]][["continuous"]] <- c(
  "UQ Purple" = "#51247A",
  "Light purple" = "#962A8B"
)

# Neutral colours are taken from the guideline's neutral tones

colour_data$"uq"$"neutral" <- c(
  "Neutral" = "#D7D1CC",
  "Dark grey" = "#999490"
)

# Save #
########

save(colour_data, file = "R/sysdata.rda")

