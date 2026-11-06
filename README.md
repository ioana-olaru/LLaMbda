# ChatLLaMbda : a natural language interface based on Lambek calculus
## Ioana Olaru & Nina Moncke 
### CSE301 - Functional Programming


## DESCRIPTION OF THE IMPLEMENTED FUNCTIONS

LAMBEK.HS
dec_rinv: Checks if a list of types can form the target type, using the right-inversion rules.
dec_lfoc: Checks whether a focused type can produce the target atom, using the types around it.
typesFor: Finds all grammatical types assigned to a word in the lexicon.
isGrammatical: Checks whether a sentence is grammatical by trying the possible types for each word.
der_rinv: Works like dec_rinv, but builds lambda expressions for all valid derivations instead of just returning True or False.
der_lfoc: Builds the possible arguments for a focused expression, following the left-focus rules.
entriesFor: Gets all the grammatical types and their corresponding meanings for a word.
sentenceDerivations: Finds all possible lambda expressions for a sentence, including different grammatical interpretations.
toPython: Converts a lambda expression into Python code, replacing words with their meanings from the lexicon.
sentenceMeanings: Finds all possible interpretations of a sentence and keeps track of the meaning of each word.

CHATLLAMBDA.HS
stdize: Makes the first letter of a word uppercase and the rest lowercase, so it matches the lexicon.
evaluateMeaning: Converts an interpretation into Python code, runs it, and returns the result.
repl: Reads what the user types and either evaluates the sentence (or handles a Who question)
showInterpretation: Prints the lambda expression and result for one possible interpretation of an ambiguous sentence.
main: Loads the lexicon and world database, starts Python, and launches the chat.
getDB: Asks for the world database file and keeps asking if it can't be opened.
getLex: Loads the lexicon and checks for file or parsing errors.
question: Answers Who questions by finding which names make the sentence true.
checkName: Tries a name in the sentence and checks whether at least one interpretation is true.

In addition to the project instructions, we created two new lexicons, one describing our own friendgroup, and added a function (and adapted repl) so that the user can ask standard Who? questions and get the list of people/animals that answer the given question. 

Example: 
> Who is grey?
Seamus, Tara
> Who is hungry?
Conor, Dara, Ronan
> Who sleeps?
Seamus

## TESTING THE CODE

Compiling: 
ghc ChatLLaMbda.hs
./ChatLLaMbda   

Lexicon file: lexfilename.elx
Facts about the world: worldfilename.py
Starting chat...
> [sentence to test]

Examples: 
> Seamus sleeps
True
> Every grey cat sleeps
True
> Every cat fears a dog
That sentence is ambiguous
Sense [Every_1 Cat_2 (\a. A_4 Dog_5 (\b. Fears_3 a b))]: True
Sense [A_4 Dog_5 (\a. Every_1 Cat_2 (\b. Fears_3 b a))]: False
> Conor sleeps
False
> Conor fears Fiona 
True
> Fiona fears Conor
False
> Every cat sleeps
False
> Every grey cat sleeps
True
> Someone sleeps
True
> Seamus is grey
True
> Seamus sleeps and
That is not a well-formed sentence

## OUR WORKFLOW 

We worked on this project in 3 stages:
1. Checking grammaticality: We implemented Lambek's decision procedure to check whether a sentence is grammatical based on the types assigned to its words.
2. Finding derivations: Instead of just checking whether a sentence is valid, we extended the algorithm to return its possible lambda expressions. This also lets us detect ambiguous sentences.
3. Evaluating sentences: We added meanings to the lexicon and used Python to evaluate the resulting expressions against the animal database. The program can now return True or False, display different interpretations, and answer simple Who questions.

We worked on the project together, initially spliitting up the work and the functions to do, but in the end we thought about most of the logic together and discussed regularly about how to proceed. 


## AI USE FOR THIS PROJECT
We used AI to complete this project mostly to debug the code when there were syntax errors, or for certain list comprehensions and formulations, and to generate extensive examples to test our code. We also researched some Haskell functions to help us with some of the implementations, using the documentations and tutorials linked in the course description and AI. 

