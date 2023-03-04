# GLADOS
> The goal of the project is to create a simple programming langage in Haskell with a parser part and a compiler part  

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Setup](#setup)
* [Features](#features)

## General info
VSL (Very Simple Langage) is a programming language designed for our project. It is a simple language that focuses on expressing arithmetic expressions and simple control flow.
	
## Technologies
Project is created with:
* Haskell version: 0.0
* Stack version: 0.0
* Docker version: 0.0
	
## Setup
To build this project, just run the script meant for that purpose and run with the created binary by calling the file as a parameter:

```
$ ./start.sh
$ ./glados [file]
```

## Features
**Variables:** Variables are declared using the def keyword, followed by the variable name and the value.  
For example, ```def x = 42" declares a variable x with the value 42```.

**Arithmetic expressions:** It supports basic arithmetic operations such as addition, subtraction, multiplication, and division.  
For example, ```5 + 3 * 4" equals to 17```.

**Functions:** Functions are declared using the def keyword, followed by the function name, the arguments, and the function body.  
For example, ```def square(x) x * x``` defines a function that takes one argument x and returns its square.

**Control flow:** It supports simple control flow constructs such as conditional statements and loops.   
For example, ```if (x < 0) return -x else return x``` returns the absolute value of x,   
and ```for( i = 0; i < 8; i ++){}``` repeatedly increments i until it is greater than 8.
