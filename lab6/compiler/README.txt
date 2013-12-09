Create Your Own Adventure : Fun With Javascript

Our l6 project was to compile c0 into Asm.js, a strict
subset of JS that focuses on performance, and the lack of GC
interrupts. The project is organized in the following way:

src/Compile/Asm/ : Main C0 -> JS codebase
src/Compile/IR/GenIRAST : IRAST that we use to translate into JS

It's important that you've installed the spidermonkey shell, and 
aliased it into your bin as 'js'. If this isn't possible, I can 
expose the js binary in my public dir, or send it to you in some way
(scp, mail). 

Once js is exposed, and the 15411.h0 header file is present in the 
main dir, you can test files using the 'tester.py' file. 

The functionality is as follows: 

./tester.py 

-f  : specify input c0 file
-d  : specify input c0 directory
-e  : specify that tests are potentially errorful and should time out 
      after 10 seconds. 
-q  : specify 'quiet'

example:

running all tests in l4:

./tester.py -d js_tests/l4



Test Organization:

Tests are organized in the js_tests directory. They are binned by their type. 

Notably tests which require a large amount of heap are placed in l4heap. In order
for these tests to correctly validate, one must open up the .js file that is generated,
and increase the amount of heap that is allocated. The default value is 5Mb, but 
increasing to 500Mb guarantees that all of these files will succeed. 

We elaborate on this point, as well as the purpose of the different folders in our 
writeup. 
