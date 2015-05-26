/*
Tue, May 26 2015, 09:37 CEST

Simpler alternative to SynthTree?
*/

Chuck {
	var <name, <process;

	*new { | name |
		^this.newCopyArgs(name).init;
	}

	init { process = ChuckProcess (this) }
 
	play { | template |
		this.makeProcess (template ?? { process.template }).play;
		this.changed (\started);
	}

	makeProcess { | template |
		process.stop;
		process = template asChuckProcess: this;
		^process;
	}

	setArgs { | ... args |
		// set a parameter in the synth of a Chuck type which
		// has a synth or pattern type playing process
		process.setArgs (args);
		this.changed (\args, args);
	}

	setProcessParameter { | parameter, value |
		// Set a parameter in the environment of the process
		process.setProcessParameter (parameter, value);
		this.changed (\parameter, parameter, value);
	}
}

+ Function {
	asChuckProcess { | chuck |
		^FunctionChuck(chuck, this, chuck.process.params);
	}
}