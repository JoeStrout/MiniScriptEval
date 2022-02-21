// Expression Evaluator
//
// This module provides an `eval` function which, given a string 
// containing a numeric expression, returns the numeric value.
// Example: `eval("4+3*5")` returns 19.
//
// Also supports variable assignment (e.g. "x=42") and use ("3*x").
//
// It also has a `repl` function, which gives a read-eval-print loop.
// As a special feature of this loop, the underscore ("_") is always
// the value of the last expression.  Enter "quit" or "exit" to exit.

// Lexer states (used inside the lex function)
State = {
"START": 0,
"NUMBER": 1,
"OPERATOR": 2,
"IDENTIFIER": 3 }

variables = {}

lex = function(exprStr)
	result = []
	state = State.START
	token = ""
	for c in exprStr
		if c <= " " then
			if token then result.push token
			state = State.START
			token = ""
		else if c >= "0" and c <= "9" then
			if state == State.NUMBER then
				token = token + c
			else
				if token then result.push token
				state = State.NUMBER
				token = c
			end if
		else if "+-*/^()=".indexOf(c) != null then
			if token then result.push token
			state = State.OPERATOR
			token = c
		else
			if state == State.IDENTIFIER then
				token = token + c
			else
				if token then result.push token
				state = State.IDENTIFIER
				token = c
			end if
		end if
	end for
	if token then result.push token
	return result
end function

eval = function(exprStr)
	return evalAssignment(lex(exprStr))
end function

evalAssignment = function(tokens)
	if tokens.len < 3 then return evalAddSub(tokens)
	if tokens[1] != "=" then return evalAddSub(tokens)
	varName = tokens.pull
	if not varName or (varName[0] >= "0" and varName[0] <= "9") then
		print "Error: assignment to numeric value"
		return null
	end if
	tokens.pull // (discard "=")
	rhsVal = evalAddSub(tokens)
	variables[varName] = rhsVal
	return rhsVal
end function

evalAddSub = function(tokens)
	evalNext = @evalMultDiv
	valA = evalNext(tokens)
	if not tokens or
	(tokens[0] != "+" and tokens[0] != "-") then return valA
	op = tokens.pull	
	valB = evalNext(tokens)
	if op == "+" then		
		return valA + valB
	else
		return valA - valB
	end if
end function

evalMultDiv = function(tokens)
	evalNext = @evalPower
	valA = evalNext(tokens)
	if not tokens or
	(tokens[0] != "*" and tokens[0] != "/") then return valA
	op = tokens.pull	
	valB = evalNext(tokens)
	if op == "*" then		
		return valA * valB
	else
		return valA / valB
	end if
end function

evalPower = function(tokens)
	evalNext = @evalParen
	valA = evalNext(tokens)
	if not tokens or tokens[0] != "^" then return valA
	op = tokens.pull	
	valB = evalNext(tokens)
	return valA ^ valB
end function

evalParen = function(tokens)
	evalNext = @evalAtom
	if not tokens then return null
	if tokens[0] == "(" then
		tokens.pull
		result = evalAddSub(tokens)
		if not tokens or tokens[0] != ")" then
			print "Unbalanced parentheses"
			return null
		end if
		tokens.pull
		return result
	else
		return evalAtom(tokens)
	end if	
end function

evalAtom = function(tokens)
	tok = tokens.pull
	if variables.hasIndex(tok) then
		return variables[tok]
	end if
	return val(tok)
end function

repl = function
	while true
		inp = input("eval: ")
		if inp == "quit" or inp == "exit" then return
		result = eval(inp)
		if result != null then print result
		variables["_"] = result
	end while
end function


runUnitTests = function
	
	errCount = 0
	
	checkLex = function(exprStr, expectedList)
		actual = lex(exprStr)
		if actual == expectedList then return
		print "Unit test error on """ + exprStr + """: expected " +
		expectedList + ", but got " + actual
		outer.errCount = outer.errCount + 1
	end function
	
	checkLex "", []
	checkLex "42", ["42"]
	checkLex "3+8", ["3", "+", "8"]
	
	check = function(exprStr, expectedValue)
		actualVal = eval(exprStr)
		if actualVal == expectedValue then return
		print "Unit test error on """ + exprStr + """: expected " +
		expectedValue + ", but got " + actualVal
		outer.errCount = outer.errCount + 1
	end function
	
	check "0", 0
	check "42", 42
	check "3+5", 8	
	check "6*7", 42
	check "50/2", 25
	check "2^3", 8
	check "3^4", 81
	check "2+3*4", 14
	check "(2+3)*4", 20
	check "12+3*(10-2)^2", 204
	check " 3   + 2  ", 5
	eval "x = 42"
	check "x", 42
	check "x+3", 45
	
	if errCount == 0 then
		print "All tests passed.  WOOT!"
	else
		print "Unit tests found " + errCount + " errors."
	end if
end function

if locals == globals then
	runUnitTests
	repl
end if