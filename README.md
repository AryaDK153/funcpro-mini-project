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
