/*
Tue, May 26 2015, 09:37 CEST

Simpler alternative to SynthTree?
*/

Chuck {
	var <name, <process;
	var <clock, <>durStream, <dur;
	// TODO: // var <template, <args, <argsTemplate, <output;
	// TODO: // classvar >parentArgs;

	*new { | name, template, args |
		^Registry(Chuck, name, { this.newCopyArgs(name).init(template, args) });
	}

	init { | template, args |
		process = template.asChuckProcess (this, args)
	}
 
	play { | template |
		this.changed (\play); // this releases previous synth
		this.makeProcess (template ?? { process.template }).play;
	}

	sched { | argDur argClock pattern |
		clock.stop;
		clock = argClock;
		durStream = argDur.asStream;
		/* Following is a hack to avoid hanging synths when 
			changing back from very short durations to longer ones
			Shortest safe duration at the moment is 0.05 */
		// this.changed(\play);
		this.release;
		// { 0.05.wait; this.release; }.fork; // necessary!
		clock.sched (
			(dur ? 0.1) min: 0.1,  // end of hack
			{
				dur = durStream.next;
				if (dur.isNil) { this.release } { this.play };
				dur;
			}
		);
		this.changed(\sched, dur, argClock);
	}

	eval { | func | this.play (CfuncTemplate (func)) }

	makeProcess { | template |
		^process = template.asChuckProcess(this, process.args);
	}

	setArgs { | ... args |
		process.setArgs (args);
		this.changed (\args, args);
	}
	getArg { | name | ^process.args [name] }
	
	free { process.free; this.changed(\free); }
	release { | dur = 0.1 | process release: dur; this.changed(\release); }

	fadeTime_ { | dur = 0.1 | process.fadeTime = dur }
	outbus_ { | bus = 0 slot = \out | process.outbus_(bus, slot) }
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
		reader addAfter: this;
		BusLink.linkAudio(this, reader, in, out);
		this.changed(\append, reader, in, out);
	}

	addAfter { | writer |
		// TODO: COMPLETE THIS
		//		this.setTarget()
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

	// Rhythm and playing sequences
	addToBeat { | beat |
		beat.add(this, { this.play });
		^beat;
	}
	printOn { arg stream;
		stream << "Chuck(" << name << ")";
	}
	storeOn { arg stream;
		stream << "Chuck(" << name << ")";
	}

}

