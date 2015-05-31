/*
Tue, May 26 2015, 09:37 CEST

Simpler alternative to SynthTree?
*/

Chuck {
	var <name, <process;

	*new { | name, template, params |
		^Registry(Chuck, name, { this.newCopyArgs(name).init(template, params) });
	}

	init { | template, params |
		process = template.asChuckProcess (this, params)
	}
 
	play { | template |
		this.makeProcess (template ?? { process.template }).play;
		this.changed (\started);
	}

	eval { | func |
		this.play (CfuncTemplate (func))
	}

	makeProcess { | template |
		process.stop;
		process = template.asChuckProcess(this, process.params);
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

	free { process.free }
	release { | dur = 0.1 | process release: dur }
	prepend { | chuck, io = \in_out |
	   thisMethod.notImplemented;
	}
	append { | chuck, io = \in_out |
	   thisMethod.notImplemented;
	}

	notImplemented { | method | postf ("% not implemented in %\n", method, this) }
}

