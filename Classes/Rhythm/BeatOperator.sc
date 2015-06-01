
+ Symbol {
	%% { | path | // play a Chuck in BeatPlayer
		var chuck;
		chuck = Chuck (this);
		BeatPlayer (path).add (chuck, { chuck.play });
	}

	beat { | beatPattern |
		var beat;
		beat = Beat.getFromPath(this);
		if (beatPattern.notNil) {
			beat.pattern = beatPattern;
		};
		^beat;
	}
}

+ Object {
	%% { | path | // set duration as pattern for BeatPlayer at path
		postf("will create beatplayer with pattern: %\n", this);
		^Beat.getFromPath(path).pattern_ (this);//  .start;
	}
}

+ Function {
	%% { | symbol |
		var chuck;
		chuck = Chuck (symbol);
		chuck.makeProcess (this);
		^chuck; // need this for chaining %% operators
	}
}

+ Chuck {
	// { func } %% \chuck %% \beatPattern;
	%% { | path, beatPattern |
		^this.addToBeat(path beat: beatPattern).start;
	}
}

+ Beat {
	// durationpattern %% \beatPatternOrPlayer %% \chuckName;
	%% { | symbol, beatPattern |
		beatPattern !? { this.pattern = beatPattern.asString };
		Chuck(symbol).addToBeat(this).start;
	}
}