# Language-Interpretor
An interpreter who makes the type synthesis for a simplistic language.


The program takes the input data and parses them into an array of
instructions, then take the instructions and insert them into the program.

These things are achieved with the help of several functions:

- makeInstruction -> fetches the entire string received from the
input file and then returns the array of instructions with the help 
of a few functions such as:

	* takeSequence - takes a line and transforms it into an array of
	terms;
	* takeWord - takes a word from the line (checks each character for 
	a special character, which means the end of the word)
	* checkEndLine - check if the end of the line has been reached
	* removeFirstWord - remove the first word from the line, and also, 
	all special characters that remain until it finds the next word, this is
	done with the help of "removeSpaces" function


- checkValidity -> this function takes the array of instruction returned
by makeInstruction function and checks (in case there is a function that
has to be inserted in the program) if the terms of the expression for a 
function comply with the language specifications => all terms (return type,
parameters type, the class it belongs to) must begin with the capital 
letter, except for the third term representing the name of the function


- interpreter -> performs instructions filtering and then inserts them 
into the program; to validate a data entry in the program it uses several 
auxiliary functions:

	* classExist -> checks if a class exists in the program; in the
	if it does not exist, the variable will be ignored or, if the parent
	class of a class does not exist, it will inherit the Global class
	* checkParam -> checks the validity of the type of parameters of a 
	function
	* createFunctionString -> creates the string corresponding to a
	function statements, which will be introduced in the program
