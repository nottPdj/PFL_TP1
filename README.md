# PFL_TP1

## Group members and participation

David dos Santos Carvalho Ranito, up202206312  - 33%

Pedro Daniel Freitas João,        up202204962  - 33%

Tiago Freitas Torres,             up202208938  - 33%

## TSP

### Dynamic programming

#### Implementation
The implementation of the travelSales function was based on the one illustrated on the book [RL99]. 

For this, we created a new type Table that allows values to be stored and retrieve according to an index. This Table was simply a function that allowed us to have a mutable table that although in the worst case the cost of the selection was linear, most of the times it was close to constant because we selected the last updated values.

Because we couldn't use sets we defined the type Set as an Int, that was operated in binary. A number n (n>0) is in the set if the bit (n+1) (from right to left) is 1 and isn't in the set if the bit is 0.

The RoadMap was also converted to an Adjacency Matrix represantion to allow constant time when selecting the distance between two cities.

Each city was attributed an Int (starting from 1). This helped for the Set and Adjacency Matrix representations.

We implemented a dynamic programming solution with a bottom-up approach where we stored in a Table a value *(d, p)* for an index *(i, s)* where *i* is the starting city, *s* is a set with the remaining cities that the path must go through, *p* is the shortest path and *d* the respective distance. If the set is empty, then the value is just distance of *i* to the start (in order to end the tour in the starting city).

After filling the table the result was the value with index *(start, set with all cities except start)*.

#### Time Complexity
Since we need 2^n values to represent all sets of up to n citys, the size of the table is in O(n2^n). Each entry requires up to n steps to be computed so the algorithm's efficiency is in O(n²2^n)

### Brute force

#### Implementation
For the brute force approach we calculated the distance for every possible tour starting always in the same city and then returned the shortest.

#### Time Complexity
Since there are (n-1)!/2 possible tours, the time complexity is O((n-1)!) 

### Comparing both algorithms

Comparing the theoretical complexity we can see that O((n-1)!) is worse than O(n²2^n) and that it's very much preferable to use the dynamic programming solution.

#### Differences regarding the test roadmaps

//TODO


## Shortest's Path

#### Implementation

The implementation of the shortestPath function was an adaptation of Dijkstra's algorithm.

For this, we defined the type PriorityQueue*, which was implemented through a min-Heap, since the priority is from the nodes that have the least distance from the source. 

Like in the TSP, the RoadMap was converted to an Adjacency Matrix representation and each city was attributed an Int (starting from 1).
To store, check and update the distances from the source to each node and each path, tables were used, of types SpTable* and PrevTable*. 
 
The Dijkstra's algorithm return the first the shortest path from the source to the target, however if there's more than a shortest path, it returns the first found. Since we were aiming for a different approach, returning all the shortests paths, we needed to adapt the algorithm. After finding a shortest path, we also need to check if we can reach the source with the same total distance as the shortest.

#### Time Complexity

O(n^2) being n the number of cities. To find the shortest path, the algorithm might check the connection for each pair of cities at least once. 

---

[RL99] Fethi Rabhi and Guy Lapalme. Algorithms: a functional programming approach. Addison-Wesley, 2 edition, 1999.
