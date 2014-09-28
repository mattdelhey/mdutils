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

| catagory       | function     |  description  |
| --------       | --------     | ------------- |
| `ggcolor `     | plots        | Recreate ggplot2 colors for arbitrary number of factors.  ([stackoverflow](http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette)) |
| `qhist`        | plots        | Implementation of `hist` in ggplot2 (using it's default binwidth calculation). |
| `unload`       | file i/o     | Unload a package using library syntax. ([stackoverflow](http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r)) |
| `clear_all`    | file i/o     | Remove all objects in workspace. |
| `any_na`       | utils        | For each column, see if there are any NA's. |
| `send_email`   | utils        | Wrapper for sending email using mutt. |
| `last`         | utils        | Get last element of an object.  |
| `read_query`   | sql          | Read sql query from file. Header is useful for setting `hiveconf` variables. ([stackoverflow](http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query)) |
| `collapse_vec` | sql          | Collapse R vector for use with sql. Default output is string vector. |

Author
--------
Matt Delhey: [email](mailto:matt.delhey@rice.edu), [website](http://mattdelhey.com).
