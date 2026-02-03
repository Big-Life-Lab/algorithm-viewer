# If TRUE, then allow users to upload algorithm files.
# This should normally be TRUE. We would set it to FALSE if we always
# want to show a single algorithm without allowing the user to change
# it. When FALSE initial_algorithm_file in server.R should be set
# to the algorithm to display. This is useful if we want to set up
# a website to show the single algorithm so we can share the URL
# so that others can view the algorithm (without having to upload
# anything).
allow_file_uploads <- TRUE
