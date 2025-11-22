# IO Documentation

Anything related to IO actions.

## CLUI

Main user interface with parsing and specific command related functions to help in handling user prompts.

### I. printListCmd header typeRenderer list

Prints a `header followed by a rendered list` of items/stocks/transactions.

### II. findWhereCmd filterFunction (header, typeRenderer, source)

Wraps the `built-in filter` function in CLI-friendly function to `filter a list of elements` according to given condition.

How the built-in `filter` function works — checking for `True (boolean) values` returned from comparing every element in the list with given condition. Hence, we needed functions capable of specific filtering according to datatypes and their keys.

>This is where `CustomData.Matchings` come in — as `filterFunction`

The filtered list will then be printed using `printListCmd`

### III. reportCmd targetStockItemName stockList transactionList

Process data for target item. Prints out a summary report.

Steps:

1. Checks stock and transaction list for `stock and transaction for item` named \<targetStockItemName\> (case insensitive).
2. Take and process all data needed by `renderReport from CustomData.Formats`.
3. Print `header and content (values)`.

### IV. addTransactionCmd

> Parameter is quite long, but `TLDR, transaction data + all lists`.

    targetItemName
    qty
    dir
    (dd, mm, yyyy, hh, mn, ss)
    shelves
    itemList
    stockList
    transactionList

Rearranges data from parameter, then calls `newTransHandler`.

### V. load

Hardcoded function used to `load all three datatypes' simulated database`.

`Return all three` already parsed into `lists`.

### VI. save itemList stockList transactionList

Takes the `latest version of all three datatypes' lists`, then `write` into corresponding databases.

### VII. inputHandler itemList stockList transactionList

Main loop for the program. `Handles prompts` through CLI.
>Unfortunately, with the way the program was built (using `Stack environment`), users can't immediately call real functions as if loading a module into *GHCI*. Hence, a `simulated interface` was needed — Custom Interface: Command Line User Interface (`CLUI`) — to manually translate every user-prompt into well-working function calls.
>
> Alternative solution `not yet` implemented.
>
> Idea to try in the future:  
> `(String → Function) Dictionary` similar to how opsInt and opsStr works. Though, some foreseable issues may appear such as:
>
> * general user's misunderstanding of programmed syntax
> * some functions may break
> * etc.
>
> But hopefully doable in order to cut away many heavily hardcoded matching functions.

Commands supported in current version:

* help → displays menu
* list \<item \| stock \| trans\> → prints entire list
* find \<dataType\> where \<key\> \<op\> \<value\> → print filtered list of \<dataType\>
* report \<ItemName\> → prints stock report
* transact \<ItemName\> \<Qty\> \<IN \| OUT\> \<Shelves\> → adds transaction with auto record current date and time
* quit → saves all data and exits

Handles invalid commands gracefully.

## CSVHandler

Functions related to CSV reading, parsing, formatting, and writing

### I. splitCSV line

Helper function: String → List  
`Splits a string (line) by comma (',')`

### II. splitShelves shelfListFromCSV

Helper function: String → List  
`Splits a string (line) by semicolon (';')`

### III. joinShelves shelfList

Helper function: List → String  
`Join elements (String) in a list by semicolon (';')`

### IV. readCSV path

IO Action: File → String  
`Reads a CSV file` located at `./<path>.csv` and returns its entire contents as a `single raw String` (no parsing, no splitting — just plain text loaded from disk).

### V. parse lineParser contentFromCSV

A high-order function that utilizes a given `<xtype>LineParser` to `convert raw CSV text (String)` into a list of typed values.  
It applies the parser to each line (skipping the header), keeping only the valid results, producing a `list of <xtype>`.

### VI. \<xtype\>LineParser lineOfContentFromCSV

Utilizes `splitCSV` to extract the `list of values` expected by the `<xtype> constructor`. Then, builds and `returns an instance` created from said values, or `Nothing` if the line fails to match the required structure.

### VII. parse\<xtype\>CSV contentFromCSV

Convenience functions that call the generic `parse` function tailored to using the corresponding `<xtype>LineParser`, producing a fully parsed `list of <xtype>` values.

### VIII. \<xtype\>Composer \<xtype\>

A function that `combines (join) the fields of an <xtype>` into a single CSV-formatted `String`. Producing the line that will be written into its corresponding CSV database.

### IX. writeCSV composer path header contentToWrite

IO Action: String → File  
A generic function that utilizes the given `<xtype>Composer` and `<xtype>Header format` to process `unlined CSV-formatted content (String)` and write it into a file located at `./<path>.csv`.

It constructs the full CSV output (header + composed lines) and writes it `directly to disk`.

### X. write\<xtype\>CSV contentToWrite

Convenience functions that call the generic `writeCSV` function tailored to using the corresponding `<xtype>Composer` and directed to the corresponding `<xtype>DB path`, producing the final CSV output and writing it to the correct database file.
