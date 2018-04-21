library(googlesheets)

# Get the list of your google sheets
gsList <- gs_ls()

# Which sheets you'd like to get?
target_number <- 10

# Read sheets by KEY, also can by Title or other args
# First, get the key of the target sheets.

target_key <- gsList$sheet_key[target_number]


target_sheet <- gs_read(gs_key(target_key))