Compilers
=========

Lab 4 Writeup

This lab proved to be significantly more difficult and time-consuming than
previous labs. There was a sheer mass of front-end work to do - which was 
somewhat exacerbated by our 'test-driven' methology from previous labs - i.e.
doing the work needed to pass the tests. In this lab, we rectified much of this
by doing a lot more thinking before sitting down and writing code. 

Having implemented the front-end, I would identify two locations as particularly
troublesome : parsing, and type-checking. 

Parsing was troublesome mostly due to our still nascent understanding of Parsec. 
We were forced to delve into stackoverflow in order to undertand how to correctly
parse post-ops - we treated '*', '->' and '[expr]' as post-ops, using the Postfix
combinaror. However, while this parsed all valid strings, it parsed them in the wrong
order when building the actual ADT's. The fix to this was a slightly modified 
Postfix parser which flips the ordering. 

Typechecking was also difficult due to our type-checker from previous labs - we had this
strange method of returning Nothing when we observed a type-check error, which doesn't
exactly provide the most 'useful' type-check messages. We fixed this for this lab, until
realizing that for some particularly viscious test-cases, printing out an expr can take
a few seconds and cause you to time-out (this is why the error messages in our submitted
compiler are mostly empty-strings - we promise that you can have good error messages
if you give us more time to compile). 

The general structure of our compiler remained unchanged - we have our front-end, 
which we then translate into an IR-AST (maintaining some structural properties, and 
removing all statements after a return), and then translate this IR-AST into 3-op IR 
form, with temps. On this, we translate it into two-op, run register allocation, which
has more or less stayed the same since lab-1, and then perform instruction selection.

All of the heavy-lifting in terms of dynamic semantics occurs in the IR generation
module. This is where we elaborate alloc/alloc_array into calls to calloc, perform
the kind of nasty code for array indices, and field-select. The cases for field-select
on a struct within an array proved to be particularly difficult. However, once we 
released that the triplet form for memory adressing (B,M,C) could be done with 
an additive constant, as AD(B,M,C), we were able to solve the problem pretty 
quickly. 

Overall, the lab was fun. I feel bad that we didn't tidy up tests2, but the moment 
I saw jpaulson-loop-stress1, I knew we wouldn't get a 10 this lab without quite a 
bit of effort in writing constant prop. The other regressions are mostly some 
l2/l3 edge-cases which might have regressed this lab. They'll be fixed before l5. 

L
