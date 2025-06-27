q(save = "no")

tools::showNonASCIIfile("DESCRIPTION")

usethis::use_gpl_license(version = 2) # or use_gpl_license(version = 2)
devtools::check(manual = TRUE, cran = TRUE) # Generates a PDF manual if needed
devtools::check()

devtools::build()
devtools::install()
devtools::document()
devtools::install(quick = TRUE, upgrade = "never")


rhub::rhub_setup()
rhub::rhub_platforms()

rhub::rhub_doctor()
clang20_R_devel <- rhub::rhub_check(
    gh_url = "https://github.com/yxlin/ggdmcPrior",
    platforms = "clang20"
)

windows_R_devel <- rhub::rhub_check(
    gh_url = "https://github.com/yxlin/ggdmcPrior",
    platforms = "windows"
)




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
