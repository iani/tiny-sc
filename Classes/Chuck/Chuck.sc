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

	eval { | func | this.play (CfuncTemplate (func)) }

	makeProcess { | template |
		^process = template.asChuckProcess(this, process.args);
	}

	setArgs { | ... args |
		// set a parameter in the synth of a Chuck type which
		// has a synth or pattern type playing process
		process.setArgs (args);
		this.changed (\args, args);
	}
	getArg { | name | ^process.args [name] }
	
	free { process.free }
	release { | dur = 0.1 | process release: dur }

	fadeTime_ { | dur = 0.1 | process.fadeTime = dur }
	outbus_ { | bus = 0 slot = \out | process.outbus_(bus, slot) }
	sched { | dur clock | process.sched(dur, clock) }

	synth { ^process.synth }
	readers { ^process.readers }
	writers { ^process.writers }

	// Linking audio
	append { | reader, io = \in_out |
		var in, out;
		#in, out = io.asString.split($_).collect(_.asSymbol);
		if (reader.hasDirectWriter(this, in)) {
			postf ("% is already a writer of %\n", this, reader);
			^this;
		};
		if (reader.readers includes: this) {
			postf("!!!% is a reader of %. Cannot add it as writer!!!\n", this, reader);
			^this;
		};
		BusLink.linkAudio(this, reader, in, out);
	}

	// Rhythm and playing sequences
	addToBeat { | beat |
		beat.add(this, { this.play });
		^beat;
	}

	hasDirectWriter { | chuck, slot = \in |
		var link;
		link = process.args[slot];
		if (link isKindOf: BusLink) {
			^link.writers includes: chuck;
		}{
			^false;
		}
	}

	hasReader { | chuck |
		^process.readers includes: chuck;
	}

	printOn { arg stream;
		stream << "Chuck(" << name << ")";
	}
	storeOn { arg stream;
		stream << "Chuck(" << name << ")";
	}

}

