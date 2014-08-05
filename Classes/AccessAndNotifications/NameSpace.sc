/*
This version is deprecated. 
Use class Registry instead.

Experimental:
Store / get named instances for any type in Library. 

Path is limited to 2 levels.


Type need not be a class.

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