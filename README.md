# Prolog Chemistry Project
A project that generates all possible branches alkane shapes using Prolog, This project was developed for educational purposes

## Predicate Descriptions

### Main Predicates

#### straight_chain_alkene(N,A)

This predicate calls the predicate method sca(N,1,A).
 
#### branched_alkane(N,BA)

generates a straight chain alkene with length N and passes it in the predicate branchedhelper(BC,BA,SN).

### Helper Predicates

#### sca(N,C,A)

A helper that returns a straight chain alkene, N is it’s length, C is a counter that determines the position of the current carbon, and A is the output, this predicate has 4 cases, output for 1st carbon, output for any middle carbon, output for last carbon and a case where it expects an input of 1 to return carb(h,h,h,h).

#### branch_name(Z,X)

Receives a number that represents the number of carbon atoms and returns its corresponding carbon structure

#### seqA(F,L,O)

returns a sequence from F to L in O in ascending order

#### check_validity(List, I)

Checks the condition stating “The number of carbon atoms in any branch should not lead to any new chains that are longer than the current longest chain. However, it is OK if it leads to a chain of a size less than or equal to the current longest chain“, it accesses every element in the list with nth0/3 predicate and checks the maximum carbon using max_branch and compares it the the length before it and the length after it.

#### split2(L,I,I2,N,L2)

This predicate receives the list L and starting index I and ending index I2, and returns a sublist from list L from index I to I2.

#### total_branch_number2([H | T],Z)

Returns the total number of carbons in the middle branches in the whole list using total_branch_number predicate

#### total_branch_number(carb(A,B,C,D),T)

Returns in T the total number of carbons in the middle (at B and C)

#### branchedhelper(BC,BA,SN)

That's the main predicate, it does the following :
combines every possible combination of middle chains, ie : tries to combine the second and the third element and then eliminates them and put the new merged one instead, then tries to eliminate the third and the fourth, etc...
the number of branches that can be eliminated every time is bounded by the maximum number of carbons that can be formed , ie : if we are forming c3h7 we will remove 3 branches.
At the end we get a new combined list, we filter the outputs using total_branch_number2 to make sure the number of carbons is the exact number needed and check_validity to check the second condition, if this combination is right we call the function again putting as input this new combination to get the rest of the solutions.

#### merge2(carb(A,B,C,D),carb(E,F,G,H),R,CL,T)

Receives two carbon branches and the number of carbons you want to add (CL) and returns the merged branch at R, and also returns T which is the number of the highest carbon in the merged branch

#### max_branch([carb(A,B,C,D)],T)

Receives a carb and returns the length of the number of highest branch out of this carb, this predicate uses branch_number.

#### branch_number(X,Z)

Receives a branch and counts the number of carbons in it.

#### mirror(L1, L2)

Mirrors a List.

#### isomers_helper(N,X,S,Set)

Uses the counter S to access all the list and look for mirrored versions of it,
if found, it removes it from the list and continues to recurse through the whole list.
