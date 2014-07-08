mdutils
=======

Collection of various useful functions for R. For function arguments, see the `R` documentation.

Installation
-----------
```R
# install.packages("devtools")
devtools::install_github("mattdelhey/mdutils")
```

Functions
-----------

| catagory  | function     |  what it does |
| --------  | --------     | ------------- |
| plots     | `ggcolors`   | recreate ggplot2 colors for arbitrary number of factors ([stackoverflow](http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette)) |
| plots     | `qhist`      | ggplot2 implementation of `hist()`; uses `hist()`'s default binwidth calculation |
| file i/o  | `read_query` | read query file ([stackoverflow](http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query)) |
| file i/o  | `unload`     | unload a package ([stackoverflow](http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r)) |
| file i/o  | `clear_all`  | remove all objects in current session |
| utils     | `any_na`     | For each col in a df, see if there are any NA's in that col. |
| utils     | `send_email` | Wrapper for sending email using mutt. |
| utils     | `last`       | Get last element of object.  |

Author
--------
Matt Delhey: [email](mailto:matt.delhey@rice.edu), [website](http://mattdelhey.com).
