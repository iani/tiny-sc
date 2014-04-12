
/*

Here all methods that are needed to handle *>

*/

+ Object {
	*> { | chuckee |
		^chuckee.starChuck(this)
	}
}

+ Symbol {
	starChuck { | object |
		^currentEnvironment[value].playPattern(object);
	}
}

+ Ref {
	starChuck { | object |
		^currentEnvironment[value].setPatternDuration(object);
	}
}
