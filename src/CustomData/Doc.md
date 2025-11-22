# Custom Data Documentation

Anything related to custom datatype, formatting, and construct rules.

## FileNames

Hardcoded filenames set as FilePath type.

`xDB → "xDB.csv"`

## Formats

Related to how the custom datatypes should be presented.

### I. strLower string

Helper function. Maps `toLower` to operate on a full string instead of just a single char.

### II. padOrCut width string

Take `width`:

* Add `paddings` to fill up empty space `if string is shorter` than width.
* Replace last three char with `... if string is longer`.

> Important note: Made to only take strings with length ≥ 3.  
Otherwise throw error.

### III. wordsWhen partition string

Common function, `separate string` into smaller words (can be single char) separated by `partition` (can be string or char).

### IV. timeNow

Utilizes Data.Time to get `current local time` and return them in the format of `separated Int`
> (dd, mm, yyyy, hh, mn, ss)

### V. formatDateTime dd mm yyyy hh mn ss

Format date and time from `individual Int to String`
> dd-mm-yyyy hh:mn:ss

### VI. Headers

CLUI related. `Hardcoded table headers` for each datatype + report.

### VII. Renderers

CLUI related. `Hardcoded table content column manager` - for each datatype + report values.

## Matchings

Pattern matching for CLUI command input values.
>NOT raw input receiver nor processor,  
but what comes AFTER they're processed.

Each return specific functions based on input.

### I. opsInt | opsStr

`Operator dictionary`, translates from string into actual operator. Made separately between Int operator and Char operator.

### II. matchOpInt opAsString | matchOpStr opAsString

Perform `lookup on operator dictionary`. Returns the actual operator.

### III. dateMatch intOp dateString | timeMatch intOp timeString

Expects user input to be formatted as `dd-mm-yyyy (date)` and `hh:mn:ss (time)`.

Separate using `worsdWhen` with each respective partition

> =='-' | ==':'

Extract each words' value into Int type, arrange biggest to smallest  
>year → month → day | hour → minute → second

then return comparator lambda function.

### IV. shelfMatch stringOp targetShelf

Returns a composite lambda function to take a data type part of the `HasShelves` typeclass and compare each singular shelf with targetShelf
> Important note: only works with 1 input targetShelf in current version

### V. \<xtype\>Match key operator targetValue

> Note: xtype = {item, stock, trans}

Each type tries to match `input key` with their `DB key` and `return specific lambda operators based on it`.

## Types

Custom datatypes and typeclass definitions.

## Updates

Functions to control creations and updates with set rules.

### I. newItemHandler originalItemsList newItemName

`Checks original list` for items with `same name` and return tuples accordingly:

* `Found` → Reject and return tuple of `original list and existing item`.
* `Otherwise` → Create `new item instance`, `append to list` and return tuple of `updated list and new item`.

### II. newStockHandler originalStockList newItemToStock

`Checks original list` for stock with `same item` and return tuples accordingly:

* `Found` → Reject and return tuple of `original list and existing stock related to item`.
* `Otherwise` → Create `new stock instance` with stockQty set to 0 and stockShelfID set to empty list, `append to list` and return tuple of `updated list and new stock`.

### III. stockQtyUpdate targetStock \<TransDirection\> movingQty

Versions based on TransDirection (`IN | OUT`), either `add or subtract` from stock with an exception:

* If TransDirection is `OUT` and `stockQty < movingQty`, then `return Nothing` as a form of error throw. Codes designed to handle errors outside the module.

Otherwise returns updated stock.

### IV. stockShelfUpdate originalStockList targetStock newTargetShelves

Returns a tuple of `updated stock list` and `list of rejected shelves`, where based on duplicate-shelf check:

* `Used by other items` → Invalid. Add to `rejected`.
* `Used by same item` → `Skip` from adding to avoid duplication, but not invalid either.
* `Otherwise` → Valid. `Update stock's shelf list`.

### V. stockUpdate originalStockList transaction

Using previous functions, set update rules and steps:

1. Applies `newStockHandler` to validate/correct stock list and target stock
2. Applies `stockQtyUpdate` to validate quantity update
3. Based on quantity validation return value:
    * `Nothing` → `return Nothing` again as error throw. Codes designed to handle errors outside the module.
    * `Otherwise` → `Continue`
4. Applies `stockShelfUpdate`.
    > In current version, returns only the updated stock list.  
    Ignores the rejected shelf list.

### VI. newTransHandler originalLists transactionData

To keep all three datatype lists in sync:
> originalLists = (originalItems, originalStocks, originalTransactions)

Using previous functions, set creation rules and steps:

1. Applies `newItemHandler` to validate/correct item list and target item
2. Creates `new transaction instance` with `valid item` and the rest of the `data from transactionData`
3. Applies `stockUpdate` to validate the new transaction, where based on the function's return value:
    * `Nothing` → `return Nothing` again as error throw. Codes designed to handle errors outside the module.
    * `Otherwise` → `return updatedLists`
        > updatedLists = (validItems, updatedStocks, updatedTransactions)

        > updatedTransactions = append new transaction to original transaction list.
