/*
Tue, May 26 2015, 09:37 CEST

Simpler alternative to SynthTree?
*/

Chuck {
	var <name, <argsTemplate, <source, <args, <>output, <maps;
§	classvar >parentArgs;

	parentArgs { ^this.class.parentArgs }
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

	clone { | childName | // untested!  - need to see what happens with args, maps!
		^this.new(childName, source, argsTemplate)
	}
	
	*new { | name, source, argsTemplate |
		^Registry(Chuck, name, {
			this.newCopyArgs(name, argsTemplate ?? {
				(out: 0, fadeTime: 0.02)
			}).init(source);
		})
	}

	init { | argSource |
		args = ().parent_(this.parentArgs);
		argsTemplate keysValuesDo: { | key value |
			args[key] = value.asStream;
		};
		source = argSource.asChuckSource(this);
		maps = IdentityDictionary();
	}

	play { | argDur, notification |
		// experimental: remove from task if played without duration:
		argDur ?? { this.removePreviousTask };
		argDur !? { args[\dur] = argDur };
		output = source.play(output, args, this, notification);
		this.changed(\play, argDur);
	}

	source_ { | argSource |
		source = argSource.asChuckSource(this);
	}

	release { | argDur |
		source.release(argDur);
		/*		if (output isKindOf: Node) {
			if (output.isPlaying) {
				output.release(argDur ?? { args[\fadeTime].next });
				output = nil;
			}{
				output.onStart (this, { | notification |
 					if (notification.listener.isPlaying) {
						notification.listener.release(argDur ?? { args[\fadeTime].next });
						output = nil;
					}
				})
			}
		}
		*/
	}

	setArgs { | ... newArgs |
		var keysValues;
		newArgs keysValuesDo: { | key, value |
			value = value.asStream;
			keysValues = keysValues.add (key).add (value);
			args [key] = value;
		};
		// if (output isKindOf: Node and: { output.isPlaying }) { output.set(*keysValues) };
		if (output isKindOf: Node) {
			if (output.isPlaying) {
				// "IS PLAYING ".post; thisMethod.name.postln;
				output.set(*keysValues)
			}{
				// "IS NOT PLAYING ".post; thisMethod.name.postln;
				output.onStart(this, {
					// "setting here".postln;
					// [this, keysValues].postln;
					output.set(*keysValues) })
			}
		};
		this.changed (\args, args);
	}

	getArg { | name | ^args [name] }
	
	free {
		if (output isKindOf: Node ) { output.free } { output.stop };
		this.changed(\free);
	}

	fadeTime_ { | dur = 0.1 | this.setArgs(\fadeTime, dur) }
	outbus_ { | bus = 0 slot = \out | this.setArgs(slot, bus) }

	readersAt { | param = \out |
		param = args[param];
		if (param isKindOf: BusLink) {
			^param.readers
		}{
			^nil;
		}
	}

	writersAt { | param = \in |
		param = args[param];
		if (param isKindOf: BusLink) {
			^param.writers
		}{
			^nil;
		}
	}
	
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
		var targetGroup;
		if (this.isAfter(writer).not) { // do not move to earlier group than currently
			args[\target] = writer.target.getReaderGroup;
			this.readersDo({ | reader writer | reader addAfter: writer });

			targetGroup = this.target.group;

			if (targetGroup.isPlaying.not) {
				targetGroup.onStart(this, {
					if (output isKindOf: Node) { output moveToTail: this.target.group; };
				});
			}{
				if (output isKindOf: Node) { output moveToTail: this.target.group; };
			};
		};
	}
	
	target { ^args[\target] }

	isAfter { | writer |
		^writer.target.readerGroups includes: this.target;
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

	setInput2Null { | inParam = \in |
		this setTarget: GroupLink.nullGroup;
		this.setArgs(inParam, 0);
	}

	setTarget { | grouplink |
		args[\target] = grouplink;
		if (output isKindOf: Node) { output moveToTail: grouplink.group; };
	}

	toRoot { | outParam = \out |  /// INCOMPLETE
		var bus;
		bus = args[outParam];
		if (bus isKindOf: BusLink) {
			bus.readers do: { | reader |
				bus.unlinkAudio(this, reader, reader.getInParam(bus), outParam);
			};
		};
	}

	getInParam { | bus | ^args.findKeyForValue(bus) }
	
	setOutput2Root { | outParam = \out |
		this.setArgs(outParam, 0);
	}

	krOut { | bus |
		// connect to kr bus. See ControlBus
		// different from append, because no Groups involved (? really? - must check !?)
		thisMethod.notImplemented;
		
	}
	
	map { | param bus |
		this.unmap(param); // remove old bus if present!
		args[param] = nil;  // but do we want to keep a copy for restoring maybe?
		maps[param] = bus;
		bus addReader: this;
		if (output isKindOf: Node) { output.map(param, bus.index) };
	}

	unmap { | param |
		bus = maps[param];
		bus !? {
			bus removeReader: this;
			maps[param] = nil;
			// IS this correct? map -1 is unmap?
			if (output isKindOf: Node) { output.map(param, -1) };
		};
		// Here restore previous value of arg maybe?
		
	}

	permanent { | yes = true |
		if (yes) {
			this.addNotifier(GroupLink, \inited, { this.play })
		}{
			this.removeNotifier(GroupLink);
		}
	}
	
	printOn { arg stream;
		stream << "Chuck(" << name << ")";
	}
	storeOn { arg stream;
		stream << "Chuck(" << name << ")";
	}

	remove {
		// TODO! MORE STUFF MUST BE DONE HERE!
		
		this.objectClosed;
	}
}
