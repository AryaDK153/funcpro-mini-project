# Inventory Management System: Functional Style

To run by .exe, download latest release.

To run the program using Stack environment:
> stack run

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

## First-Run Guide

> Warning: When you first run the program, `it will create 3 CSV type files` as local database `in the same directory as the executable file (*.exe)`. So be sure to run it in the folder you want the DB to be by simply `putting the executable in said folder`.

Before:

<img width="500" alt="image" src="https://github.com/user-attachments/assets/b365e9d0-0f1b-4b30-9d96-33bc54f00001" />

First-Run:

<img width="500" alt="image" src="https://github.com/user-attachments/assets/f57658a0-1572-4589-8460-7c2ca9dd74c3" />

After:

<img width="500" alt="image" src="https://github.com/user-attachments/assets/ccc57163-a355-4861-b4c8-b2de830d9837" />

Commands supported in current version:

* help → displays menu
* list \<item \| stock \| trans\> → prints entire list
* find \<dataType\> where \<key\> \<op\> \<value\> → print filtered list of \<dataType\>
* report \<ItemName\> → prints stock report
* transact \<ItemName\> \<Qty\> \<IN \| OUT\> \<Shelves\> → adds transaction with auto record current date and time
* quit → saves all data and exits
