q(save = "no")

devtools::install()
devtools::build()
devtools::document()
devtools::install(quick = TRUE, upgrade = "never")


devtools::test()

remove.packages("ggdmcPrior") # Remove existing installation
unlink("src/*.o") # Remove object files
unlink("src/*.so") # Remove shared objects
unlink("src/RcppExports.*") # Remove generated Rcpp files
devtools::document()


getNamespaceExports("ggdmcPrior")
getNamespaceExports("ggdmcPhi")

# 1. Use timedatectl (Modern Linux Systems with systemd)
# sudo timedatectl set-ntp on     # Enable automatic time sync
# sudo timedatectl status         # Check sync status
# date  # Should now show correct time

# 3. Manually Update Time (Temporary Fix)
# sudo date -s "$(curl -s --head http://google.com | grep '^Date:' | cut -d' ' -f3-6)"

# 4. Update Timestamps for R Package Files
# Even without fixing system time, you can manually update file timestamps to avoid the R check warning:

# Navigate to your R package directory
# touch DESCRIPTION R/* src/*      # Update timestamps for key files

# 5. Disable Time Check in R (If All Else Fails)
# Add this to your DESCRIPTION file to suppress the warning:

# yaml
# Config/check/use_install_times: false
