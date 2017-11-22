# Elbank

## Summary

Elbank is a personal finances reporting tool for Emacs.  It uses
[Weboob](https://weboob.org) for scraping data from bank websites.

## Installing

You will need Emacs 25+.

- [Install Weboob](http://weboob.org/install) and configure Boobank to fetch
  your transactions;
  
- Install Elbank (not in Melpa yet);

- Run `M-x elbank-overview`.

## Usage

### Data file

Data is stored as JSON in `elbank-data-file` which defaults to
`$HOME/.emacs.d/elbank-data.json`. 

You might want to customize that variable or make sure to **exclude it from your
`.emacs.d` git repository**.

### Entry point

The command `elbank-overview` is the entry point of Elbank.  You might want to
bind it to a global key like the following:

```emacs-lisp
(global-set-key (kbd "C-c e") #'elbank-overview)
```

Otherwise just do `M-x elbank-overview RET` to get started.

From the overview buffer, press `r` to create a new report.

### Importing data

Before using Elbank, make sure that `weboob` is correctly configured.  You can
make sure of it by evaluating `$ boobank ls` and see if your bank accounts are
correctly outputted.

When opening the overview buffer for the first time, Elbank will scrap data
using weboob.  Press `u` to update data from weboob.

Note: Many bank website only store data for a short period of time (usually just
a few months), so make sure to **import on a regular basis**, otherwise there
will be gaps in the list of transactions.

### Categorizing transactions

Categorizing transactions is important when reporting with Elbank.  It makes it
easy to group or filter transactions based on their categories.

Transactions are automatically categorized when reporting, using
`elbank-categories`, an association list of the form:

```emacs-lisp
'(("category1" . ("regexp1" "regexp2"))
  (("category2" . ("regexp")))
```

Where a category key can be any string.  Category regexps are matched against
transaction labels or raw text.

For convenience when filtering transactions by categories, it is recommended to
create categories with subcategories using ":" as a separator in category keys,
like in the following example:

```emacs-lisp
(setq elbank-categories
      '(("Expenses:Food" . ("^supermarket" 
                            "^restaurant" 
                            "Local store XXX" 
                            "Bakery XXX"))
        ("Expenses:Rent" . ("Real Estate Agency XXX"))
        ("Income:Salary" . ("Bank transfer from Company XXX"))))
```

Note that it is not currently possible to manually add a category to a specific
transaction, you always have to rely on `elbank-categories` to categorize
transactions.

### Customizing reports

Many report options can be customized from within a report buffer.

- `f c`: Filter transactions by category, or a prefix of a category.
- `f p`: Filter transactions by a period (leave empty for no period).
- `f a`: Only show transactions for a specific account.
- `S`: Select the sort column.
- `s`: Reverse the sorting order.
- `G`: Group transactions by a property (leave empty for no grouping).
- `M-n`: Move forward by one period.
- `M-p`: Move backward by one period.

You can customize `elbank-saved-monthly reports` and
`elbank-saved-yearly-reports` to get a quick list of commonly used reports from
the overview buffer.

### Example reports

Here are some example reports:

- All transactions with default columns (date, label, category and amount)
```elisp
(elbank-report)
```

- All transactions with raw text
```elisp
(elbank-report :columns '(date label raw amount))
```
- Monthly income statement by category
```elisp
(elbank-report :sort-by 'amount
               :group-by 'category
               :columns '(date label amount)
               :period `(month ,@(last (elbank-transaction-months))))
```
- Daily expenses of the current month
```elisp
(elbank-report :sort-by 'date
               :group-by 'date
               :columns '(label category amount)
               :period `(month ,@(last (elbank-transaction-months))))
```

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2017 Nicolas Petton.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
