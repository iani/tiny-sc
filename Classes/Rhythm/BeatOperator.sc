
+ Symbol {
	|| { | path | // play a Chuck in BeatPlayer
		var chuck;
		chuck = Chuck (this);
		BeatPlayer (path).add (chuck, { chuck.play });
	}
}

+ Object {
	|| { | path | // set duration as pattern for BeatPlayer at path
		BeatPlayer (path).pattern_ (this).start;
	}
}

+ Function {
	|| { | symbol |
		^Chuck (symbol).makeProcess(this);
	}
}