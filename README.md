
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rPackagesRte

<!-- badges: start -->
<!-- badges: end -->

The goal of rPackagesRte is to build static site to display informations
about `R packages` manage by [rte-antares-rpackage
oranisation](https://github.com/rte-antares-rpackage)

# build site

``` r
rmarkdown::render_site()
```

# git push site

    # First switch to the gh-pages branch
    git checkout gh-pages

    # Next checkout the specific file you wish to add to the gh-pages branch
    git checkout master -- _site/

    # Perfom the commit
    git commit -m "Updated index.html from master"

    # And push
    git push 
