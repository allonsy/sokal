# sokal
a markov model based random text generator by Alec Snyder

##Build instructions:
Clone the repository
You will need the Network.HTTP and Text.HTML.TagSoup packages so make sure that you build from a cabal sandbox with these packages installed or have these packages globally installed.

run 
* `cabal configure`
* `cabal build`

the executable will default to the dist/build/sokal directory so if you want the executable anywhere else go ahead and move it

Alternatively, you can just run
* `ghc spew.hs`

preferably with -O2 or -O3 to save a few seconds
make sure to have the Suck.hs file in the same directory as spew.hs so it compiles

##Executing instructions
to execute, run:
* `./sokal urlfile numwords(int) [outputfile]`

where urlfile is the location to the url file
numwords is apporximately how many words you want in the final output (it will print until it reaches the specified number of words and then continue until that sentence is finished)
sokal defaults to printing to standard out however, you can pipe the sokal text to an external file with the optional third argument

## Miscellany

* Note that the articles were encoded with unicode which sometimes annoys my largely ascii expecting program
* Additionally, if you run the program all the way through, please allow about a minute to run the program (based on the sheer volume of words: almost a million words in all 89 articles)
If you skip ahead to saving the sokal.model file and reading it in spew (skipping suck), the model will run in a matter of seconds
