/*
Experimental:
Store / get named instances for any type in Library. 

Type need not be a class.

This is a simple two-level single-instance identity preserving scheme. 

Problem: How to adjust init behavior. 
Possible solution: Pass behavior as a function.
Yet 2nd problem: How to code the behavior function.

NameSpace(Integer, \one, { 1 } );
NameSpace(Integer, \two, { 2 } );

Library.at(Integer);
*/

NameSpace {

	*new { | type, name, makeFunc |
		var instance;
		makeFunc ?? { makeFunc = { type.asClass.new }};
		// if no name is given, return nameless instance:
		name ?? { ^makeFunc.value };
		instance = Library.at(type, name);
		if (instance.isNil) {
			instance = makeFunc.value;
			Library.put(type, name, instance);
		};
		^instance;
	}

	*doIfFound { | type, name, action |
		var instance;
		instance = Library.at(type, name);
		instance !? { action.(instance) };
	}

	*remove { | type, name |
		Library.global.removeEmptyAt(type, name);
	}
}