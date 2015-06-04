ChuckProcess {
	classvar >parentArgs; // Parent event holding default parameters for args
	var <chuck, <template, <args;
	var <argsTemplate; // TODO: store patterns of the param streams, for cloning

	*new { | chuck, template, args |
		^this.newCopyArgs (
			chuck,
			template,
			args ?? { ().parent_ (this.parentArgs) }
		)
	}

	init {}

	*parentArgs {
		parentArgs ?? {
			parentArgs = (
				out: 0,
				fadeTime: 0.02,
				addAction: \addToHead,
			)
		};
		^parentArgs;
	}

	setArgs { | theArgs |
		var keysValues;
		theArgs keysValuesDo: { | key, value |
			value = value.asStream;
			keysValues = keysValues.add (key).add (value);
			args [key] = value;
		};
		^keysValues;
	}
	
	play {}

	synth { ^nil }

	removeFromBus { | param, role |
		var link;
		link = args [param];
		if (link isKindOf: BusLink) { link.perform (role).remove (chuck) };
	}
	
	readers { | set |
		var directReaders;
		set ?? { set = Set (); }; 
		directReaders = this.directReaders;
		set addAll: directReaders;
		directReaders do: _.readers (set);
		^set;
	}

	directReaders {
		var set;
		set = Set ();
		args do: { | v |
			if (v isKindOf: BusLink and: { v.writers includes: chuck}) {
				set addAll: v.readers;
			}
		};
		^set;
	}

	readersDo { | func |
		var directReaders;
		directReaders = this.directReaders;
		directReaders do: { | r |
			func.(r, this);
			r.readersDo (func);
		}
	}

	writers {
		
	}
	
	out_ { | bus, slot = \out |
		this.setArgs ([slot, bus]);
	}

	fadeTime_ { | dur = 0.1 |
		this.setArgs([\fadeTime, dur]);
	}
	
}


Cnil : ChuckProcess {
	// Just a consistent name for the empty ChuckProcess
}

Csynth : ChuckProcess {
	var <synth;

	init {
		synth = chuck.process.synth;
	}

	play { | argDur |
		this.synth = template.play(
			args [\target].next.asTarget,
			args [\out].next,
			args [\fadeTime].next,
			args [\addAction].next,
			args.getPairs
		);
		synth.addNotifier (chuck, \play, { | n |
			// n.listener.postln;
			if (synth.isPlaying) {
				n.listener.release (args[\fadeTime].next);
			}{
				SystemClock.sched (0.02, {
					n.listener.release (args[\fadeTime].next);
					nil
				});
			};
			n.listener.objectClosed;
		})
	}
		
	synth_ { | argSynth |
		synth = argSynth;
		synth.onEnd (this, { this.changed (\synthStopped)});
	}
	
	stop { this.release }
	release { | dur |
		synth !? {
			if (synth.isPlaying) {
				synth.release(dur ?? { args[\fadeTime].next })
			}{
				synth.onStart (this, {
					synth.release(dur ?? { args[\fadeTime].next })
				})
			}
		}
	}

	free {
		synth !? {
			if (synth.isPlaying) {
				synth.free;
			}{
				synth.onStart (this, { synth.free })
			}
		}
	}

	setArgs { | args |
		synth.set (*super.setArgs (args));
	}

	target_ { | atarget |
		
	}

	/*
	outbus_ { | aoutbus |
		synth.set (\outbus, aoutbus);
	}
	*/
	fadeTime_ { | afadeTime |
		synth.set (\fadeTime, afadeTime);
	}

	addAction_ { | aaddAction |
		
	}

	args_ { | aargs |
		// TODO: Review this!
		synth.set (*aargs)
	}
}

Cfunc : Csynth {
	play {
		var result;
		result = args use: template.func;
		if (result isKindOf: Node) {
			this.synth = result;
		}
	}
}

CfuncTemplate {
	var <func;
	*new { | func | ^this.newCopyArgs(func) }

	chuckProcessClass { ^Cfunc }
}