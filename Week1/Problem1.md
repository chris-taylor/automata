### Question

How many strings of length N are there in the language consisting of all strings of 0s and 1s that don't have two consecutive 1s?

### Answer

Let's count the strings of lengths 0, 1, 2, 3, 4...

    n=0: eps (empty string)                             [TOTAL = 1]

    n=1: 0, 1                                           [TOTAL = 2]

    n=2: 00, 01, 10                                     [TOTAL = 3]

    n=3: 000, 001, 010, 100, 101                        [TOTAL = 5]

    n=4: 0000, 0001, 0010, 0100, 0101, 1000, 1001, 1010 [TOTAL = 8]

So the sequence is 1, 2, 3, 5, 8, ... which looks like Fibonacci.

Argument for why this is correct:

Call F(N) the number of strings of length N that have no consecutive 1s. Additionally, call A(N) the number of such strings that end in 0, and B(N) the number of such strings that end in 1. Clearly we have

    F(N) = A(N) + B(N)

Now for A(N), we can start with any string from A(N-1) or B(N-1) and append a '0' to it, and clearly this generates all strings in A(N). For B(N), we can't use the strings in B(N-1) (they already have a '1' at the end) so we can only use the strings in A(N-1). Therefore

    A(N) = A(N-1) + B(N-1)
    B(N) = A(N-1)

So

    F(N) = A(N) + B(N)
         = A(N-1) + B(N-1) + A(N-1)
         = F(N-1) + A(N-2) + B(N-2)
         = F(N-1) + F(N-2)

with F(0) = 1 and F(1) = 2, which is the formula defining the Fibonacci series.

Table demonstrating the construction of A(N) and B(N):

            A                                       B

    N=0     e                                       e

    N=1     0                                       1

    N=2     00, 10                                  01

    N=3     000, 010, 100                           001, 101

    N=4     0000, 0010, 0100, 1000, 1010            0001, 0101, 1001
