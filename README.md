# Inventory Management System: Functional Style

To run by .exe, download latest release.

To run the program using Stack environment:
> stack run

More on first run guide, click here: [Jump to first run guide section](#first-run-guide).

## Documentations

[CustomData](src/CustomData/Doc.md), in alphabetical order, contain:

* FileNames
* Formats
* Matchings
* Types
* Updates

[IO](src/IO/Doc.md), in alphabetical order, contain:

* CLUI (Command Line User Interface)
* CSVHandler

## Functional Programming Concepts

This section talks about important concepts, parts of the *Functional Programming Paradigm*. Listed are the few concepts studied and implemented during the project development.

### Datatype vs Object

In functional programming — *especially in the Haskell language* — data representation is approached using **Algebraic Data Type (ADT)**, which includes `data`, `newtype`, and lastly, `type`. Below are some rules applied to variables that uses ADT, *specifically in the Haskell Language*:

1. An ADT itself only describes the structure of the data. It is not inherently mutable or immutable. However, **because Haskell is a purely functional language, all values are immutable by default**. Therefore, any value constructed from an ADT also becomes immutable once created.
    > data Stock = Stock { stockItem :: Item, stockQty :: Int }  
    >
    > -- case pencil as Item  
    > let stock = Stock pencil 10  
    > let updated = stock { stockQty = 20 }  
    >
    > -- stock = Stock { stockItem :: pencil, stockQty :: 10 }  
    > -- updated = Stock { stockItem :: pencil, stockQty :: 20 }

    The value saved inside the "stock" variable is permanent. **Changes to the stockQty from 10 to 20 became an entirely new and different data** that's then saved inside the "updated" variable. This helps keeping individual data away from unwanted operations.
2. Unlike objects in OOP, which usually bundle data and behavior (methods) into a single entity, **ADTs in Haskell only define structure**. All operations over that data are placed in **separate functions or expressed using typeclasses**.

    This separation makes data:
    * More modular
    * Easier to reason about
    * Easier to pattern match
    * Easier to test and maintain

### Higher-Order Function (HOF), Composition, Layered Abstraction

These three concepts are closely related as functional paradigm, and often used alongside one-another.

1. HOF

    A type of function that's able to take other functions as parameters/arguments and/or return other functions. Some good examples in the Haskell language are `map`, `filter`, `foldr`, etc.
    > Not limited to language-provided function library, HOF can also be made custom.

    HOF may have the characteristics below:
    1. **Recursive functions** replacing loops like for and while.

        Example:  
        “foldr” (fold from right to left). Works by operating on the last element of a given list with an initial value and recursively operate on the rest of the list with the result from previous operations.
    2. **Pattern generalization**. HOFs should be defined in common patterns to be used in many different situations.
        Example:  

        ```filter :: (a -> Bool) -> [a] -> [a]
        filter _ [] = []
        filter p (x:xs)
          | p x       = x : filter p xs
          | otherwise = filter p xs
        ```

        Applying condition "p" to every element of x recursively is considered a common pattern.  "p" on the other hand is a flexible argument, which in this case, any function that creates boolean to be used in filtering.
2. Composition

    In math, there is a concept of passing the result of a function straight to the next function as input. This type of continuous and sequential operation is called a composition and can be written as the formula below:  
    `f (g (h (x))) = (f o g o h) (x)`  
    This technique can be used in the Haskell language. Not only does it significantly lowers the need for extra temporary variables, it also makes reading the code and understanding the flow easier. In Haskell, it is written like the code below:  
    `f . g . h x`  
    This writing style also helps with applying tacit programming or also known as point-free style, which is convenient for passing multiple functions, especially for functions with multi-layered operations.  
    > comp `x` = f . g . h `x`  
    > ↓  
    > `comp = f . g . h`
3. Layered Abstraction

    A useful technique when a function have multi-layered operations, in other words, does so many things and sequentially most of the times. Each of those operations can be made into each helper functions in order to improve  the code quality, readability, and reusability.

    Example: A function that does output formatting followed by printing the formatted result to I/O, but also require those steps to be operated sequentially on every elements of a given list.

    Such program have three different processes (layers): Format, Print, and Loop, which can be made into each of their own abstract helper functions.

Combining the three techniques above, we can have functions like the one below:
> printList :: (a -> String) -> [a] -> IO ()  
> `printList rowFormatter = mapM_ (putStrLn . rowFormatter)`

From the function above we can find:

1. Three abstract functions:
    * `mapM_` that functions as a loop (recursive) for operating on every elements of a given list,
    * `putStrLn` that prints to I/O, and
    * `rowFormatter` that formats every element 'a' into printable String.
2. Two HOFs:
    * `mapM_` that accepts an operator function as the first parameter,
    * `printList` that accepts the formatter function and returns another function that operates on a list.
3. Composition of two functions, which operates in sequence:
    * Apply `rowFormatter` to x (an element of xs),
    * Apply `putStrLn` to the output of rowFormatter.

## First-Run Guide

> Warning: When you first run the program, `it will create 3 CSV type files` as local database `in the same directory as the executable file (*.exe)`. So be sure to run it in the folder you want the DB to be by simply `putting the executable in said folder`.

Before:

<img width="500" alt="image" src="https://github.com/user-attachments/assets/b365e9d0-0f1b-4b30-9d96-33bc54f00001" />

First-Run:

<img width="500" alt="image" src="https://github.com/user-attachments/assets/f57658a0-1572-4589-8460-7c2ca9dd74c3" />

After:

<img width="500" alt="image" src="https://github.com/user-attachments/assets/ccc57163-a355-4861-b4c8-b2de830d9837" />

Commands supported in current version:

* help → displays available commands  
    <img height="400" alt="image" src="https://github.com/user-attachments/assets/b9c77cc5-7506-4301-8897-7a577f0c7e29" />
  
* list \<item \| stock \| trans\> → prints entire list  
    <img height="200" alt="image" src="https://github.com/user-attachments/assets/cb494a3a-2b5f-454f-9e51-72f699ac87e7" />

* transact \<ItemName\> \<Qty\> \<IN \| OUT\> \<Shelves\> → adds transaction with auto record current date and time  
    <img width="650" height="423" alt="image" src="https://github.com/user-attachments/assets/7ce7e655-a210-42cb-803e-e112cce68ece" />

    on next transactions:  
    <img width="650" height="319" alt="image" src="https://github.com/user-attachments/assets/9e93de9a-3bd3-47b2-8fed-9d9f2960fbb9" />
    
* find \<dataType\> where \<key\> \<op\> \<value\> → print filtered list of \<dataType\>
    <img width="650" height="375" alt="image" src="https://github.com/user-attachments/assets/72822464-f7d3-4edb-9445-a589b45d4196" />

* report \<ItemName\> → prints stock report  
    <img width="650" height="171" alt="image" src="https://github.com/user-attachments/assets/e2351446-fcb8-4df9-b895-1f1f4aae4ab0" />

* quit → saves all data in CSV files and exits - You can open the CSV files in any text editor, like Notepad.
