/*
Tue, May 26 2015, 09:37 CEST

Simpler alternative to SynthTree?
*/

Chuck {
	var <name, <source, <argsTemplate, <args, <output, <count;
	var <clock, <>durStream, <dur;
	classvar >parentArgs;

	parentArgs {
		parentArgs ?? {
			parentArgs = (
				out: 0,
				fadeTime: 0.02,
				addAction: \addToHead,
			)
		};
		^parentArgs;
	}

	
	clone { | childName |
		^this.new(childName, source, argsTemplate)
	}
	
	*new { | name, source, argsTemplate |
		^Registry(Chuck, name, {
			this.newCopyArgs(name, source.asChuckSource, argsTemplate ?? { () }).init;
		})
	}

	init {
		args = ().parent_(this.parentArgs);
		argsTemplate keysValuesDo: { | key value |
			args[key] = value.asStream;
		}
	}
			
	play { | key, argCount, notification |
		#output, count = source.play(output, args, this, notification).asArray;
		this.changed(\play, key, count ? argCount ? 0);
	}

	source_ { | argSource |
		source = argSource.asChuckSource;
	}

	sched { | argDur argClock pattern |
		clock.stop;
		clock = argClock;
		durStream = argDur.asStream;
		/* Following is a hack to avoid hanging synths when 
			changing back from very short durations to longer ones
			Shortest safe duration at the moment is 0.01 */
		this.release;
		clock.sched (
			0.01, //	(dur ? 0.1) min: 0.1,  // end of hack
			{
				dur = durStream.next;
				if (dur.isNil) { this.release } { this.play };
				dur;
			}
		);
		this.changed(\sched, dur, argClock);
	}

	// TODO: use notification to encapsulate current output
	// in order to prevent hanging synths when output is overwritten before release
	release { | argDur |
		if (output isKindOf: Node) {
			if (output.isPlaying) {
				output.release(argDur ?? { args[\fadeTime].next })
			}{
				output.onStart (this, { | notification |
					// postf("look in these contents for the real synth %\n", args);
 					if (notification.listener.isPlaying) {
						notification.listener.release(argDur ?? { args[\fadeTime].next })
					}
				})
			}
		}
	}

	setArgs { | ... newArgs |
		var keysValues;
		newArgs keysValuesDo: { | key, value |
			value = value.asStream;
			keysValues = keysValues.add (key).add (value);
			args [key] = value;
		};
		if (output isKindOf: Node and: { output.isPlaying }) { output.set(*keysValues) };
		this.changed (\args, args);
	}

	getArg { | name | ^args [name] }
	
	free {
		if (output isKindOf: Node ) { output.free } { output.stop };
		this.changed(\free);
	}



	fadeTime_ { | dur = 0.1 | this.setArgs(\fadeTime, dur) }
	outbus_ { | bus = 0 slot = \out | this.setArgs(slot, bus) }
		
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
			if (v isKindOf: BusLink and: { v.writers includes: this}) {
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



	addAfter { | writer |
		// TODO: COMPLETE THIS
		//		this.setTarget()
	}

	hasDirectWriter { | chuck, slot = \in |
		var link;
		link = args[slot];
		if (link isKindOf: BusLink) {
			^link.writers includes: chuck;
		}{
			^false;
		}
	}

	hasReader { | chuck |
		^this.readers includes: chuck;
	}
	
	printOn { arg stream;
		stream << "Chuck(" << name << ")";
	}
	storeOn { arg stream;
		stream << "Chuck(" << name << ")";
	}

}

