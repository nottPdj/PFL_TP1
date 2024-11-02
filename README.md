# PFL_TP1

## travelSales

The implementation of the travelSales function was based on the one illustrated on the book [RL99]. 
For this, we created a new type Table that allows values to be stored and retrieve according to an index. This Table was simply a function that allowed us to have a mutable table that although in the worst case the cost of the selection was linear, most of the times it was close to constant because we selected the last updated values.
Because we couldn't use sets we defined the type Set as an Int, that was operated in binary. A number n (n>0) is in the set if the bit (n+1) (from right to left) is 1 and isn't in the set if the bit is 0.
The RoadMap was also converted to an Adjacency Matrix represantion to allow constant time when selecting the distance between two cities.
Each city was attributed an Int (starting from 1). This helped for the Set and Adjacency Matrix representations
We implemented a dynamic programming solution with a bottom-up approach where we stored in a Table a value *(d, p)* for an index *(i, s)* where *i* is the starting city, *s* is a set with the remaining cities that the path must go through, *p* is the shortest path and *d* the respective distance.

como foi filled

After filling the table the result was the value with index (start, set with all cities except start).




[RL99] Fethi Rabhi and Guy Lapalme. Algorithms: a functional programming approach. Addison-Wesley, 2 edition, 1999.
