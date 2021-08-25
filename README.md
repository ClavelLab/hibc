# HiBC

Repository contains relevant data for HiBC


# Database

The database has been set up using [DB Browser for SQLite](https://sqlitebrowser.org/).

You can open the [database-project](hiBC.sqbpro) with the program and it will open the actual database [hiBC.db](hiBC.db) as well.


# Visualization

To get a better impression of the database's structure we have visualized it using [sqelton](https://github.com/inukshuk/sqleton).

You either run in your CLI (command line interface):


```
sqelton --layout neato -e -o visualization/<YYYY-MM-DD>-hiBC.png hiBC.db
```
> Replace <YYYY-MM-DD> with the current date.

Or use the [makefile](makefile) in this folder and just go with 

```
make vis 
```
