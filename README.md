mdutils
=======

Collection of various useful functions for R. For function arguments, see `R` documentation.

installation
-----------
```R
# install.packages("devtools")
devtools::install_github("mattdelhey/mdutils")
```

plots
-----------

| function |  what it does |
| -------- | ------------- |
| `ggcolors` | recreate ggplot2 colors for arbitrary number of factors ([stackoverflow](http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette)) |
| `qhist`    | ggplot2 implementation of `hist()`; uses `hist()`'s default binwidth calculation |


file io
-------

| function |  what it does |
| -------- | ------------- |
| `read_query` | read query file ([stackoverflow](http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query)) |
| `unload` | unload a package ([stackoverflow](http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r)) |
| `clear_all` | remove all objects in current session |

utils
------
| function | what it does |
| -------- | ------------ |
| `any_na` | for each col in a df, see if there are any NA's in that col |

author
--------
Matt Delhey: [email](mailto:matt.delhey@rice.edu), [website](mattdelhey.com).
