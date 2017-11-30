# DatabaseTethering

Here are three Client-/Server-Applications as examples of database tethering.
In the three applications, database tables are copied from the server program to the client program.
FireDac and SQLite are used. The server-side change to another database 
(e.g. Oracle, SQL Server, ...) is very easy.

Each of these server program requires the SQLite file "server.s3db". It is in the "bin" directory.
On the client side, the database file "client.s3db" is newly created.
In the environment variable "PATH", the storage location must be entered to the "sqlite3.dll".


## Demo1

The tethering adapter is Network. The stream type is binary.

## Demo2

The tethering adapter is Network. 
In addition, the stream type (binary, JSON, XML) and a compression rate can be selected.

## Demo3

In this Demo, the result of the following SQL query is also transferred as a table to the client.
Here the SQL-Statement with a outer join:
```sql
SELECT p.ProductID as PID, p.ProductName as PName, p.UnitPrice * p.UnitsInStock as TotalPrice,
       c.CategoryID as CID, c.CategoryName as CName
  FROM Products p left join Categories c on p.CategoryID = c.CategoryID
```  
The tethering adapter is selectable, either as a network or as Bluetooth. 
In addition, the stream type (binary, JSON, XML) and a compression rate can be selected.


