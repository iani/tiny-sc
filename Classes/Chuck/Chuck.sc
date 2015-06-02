/*
Tue, May 26 2015, 09:37 CEST

Simpler alternative to SynthTree?
*/

Chuck {
	var <name, <process;

	*new { | name, template, args |
		^Registry(Chuck, name, { this.newCopyArgs(name).init(template, args) });
	}

	init { | template, args |
		process = template.asChuckProcess (this, args)
	}
 
	play { | template |
		process.stop;
		this.makeProcess (template ?? { process.template }).play;
		this.changed (\started);
	}

	eval { | func |
		this.play (CfuncTemplate (func))
	}

	makeProcess { | template |
		process = template.asChuckProcess(this, process.args);
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

	fadeTime_ { | dur = 0.1 | process.fadeTime = dur }
	outbus_ { | bus = 0 slot = \out | process.outbus_(bus, slot) }
	dur_ { | dur | process.dur = dur }
	clock_ { | clock | process.clock = clock }

	// Linking audio
	append { | reader, io = \in_out |
	   BusLink.linkAudio(process, reader.process, *io.asString.split($_).collect(_.asSymbol));
	}

	// Rhythm and playing sequences
	addToBeat { | beat |
		beat.add(this, { this.play });
		^beat;
	}
}

