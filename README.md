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
| plots     | `ggcolor `   | Recreate ggplot2 colors for arbitrary number of factors.  ([stackoverflow](http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette)) |
| plots     | `qhist`      | Implementation of `hist` in ggplot2 (using it's default binwidth calculation). |
| file i/o  | `unload`     | Unload a package using library syntax. ([stackoverflow](http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r)) |
| file i/o  | `clear_all`  | Remove all objects in workspace. |
| utils     | `any_na`     | For each column, see if there are any NA's. |
| utils     | `send_email` | Wrapper for sending email using mutt. |
| utils     | `last`       | Get last element of an object.  |
| sql       | `read_query` | Read sql query from file. Header is useful for setting hiveconf variables. ([stackoverflow](http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query)) |
| sql       | `collapse_vec` | Collapse R vector for use with sql. Default output is string vector. |

Author
--------
Matt Delhey: [email](mailto:matt.delhey@rice.edu), [website](http://mattdelhey.com).
