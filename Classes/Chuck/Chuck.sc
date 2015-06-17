/*
Tue, May 26 2015, 09:37 CEST

*/

Chuck {
	var <name, <argsTemplate, <source, <args, <>output, <maps;
	classvar >parentArgs;
	classvar inactive;
	var <>level; // checking how to sort graph; for MiniSteno
	
	inactive { ^this.class.inactive }
	*inactive {
		inactive ?? { inactive = List() };
		^inactive;
	}
	*initInactive { inactive = this.all }
	*all { ^Library.at(Chuck).values}

	parentArgs { ^this.class.parentArgs }
	*parentArgs {
		parentArgs ?? {
			parentArgs = (
				out: BusLink.nullBus,
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
				(in: ArBusLink.nullBus, out: ArBusLink.nullBus, fadeTime: 0.02)
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

	play { | task |
		if (task.isNil) { // disengage from task if played directly
			{ this.removePreviousTask } 	
		}{
			task.passArgs(args);
		};
		output = source.play(output, args, this);
		this.changed(\play, task);
	}

	playIfNotPlaying {
		if (output.isPlaying.not) { this.play };
	}
	
	isPlaying {
		^output.isPlaying;
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
	
	append { | reader, in = \in, out = \out |
		if (reader.hasDirectWriter(this, in)) {
			postf ("% is already a writer of %\n", this, reader);
			^this;
		};
		//		[this, thisMethod.name, "starting execution next - wait for crash"].postln;
		if (reader.readers includes: this) {
			postf("!!!% is a reader of %. Cannot add it as writer!!!\n", this, reader);
			^this;
		};
		// "================================================================".postln;
		[this, thisMethod.name, "SURVIVED"].postln;
		reader addAfter: this;
		BusLink.linkAudio(this, reader, in, out);
		this.changed(\append, reader, in, out);
	}

	addAfter { | writer |
			// TODO: first find the *writer with the deepest target group amongst all
			// writers of this chuck*.  Then set the target to the readergroup of that target.
		var latestWriter; // TO TEST
		var targetGroup;
		latestWriter = this.getLatestWriter(writer);
		if (this.isAfter(latestWriter).not) { // do not move to earlier group than currently
			args[\target] = latestWriter.target.getReaderGroup;

			this.readersDo({ | r w | r addAfter: w });

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

	getLatestWriter { | writer |
		// debugging
		// ^writer;
		
		var latest;
		latest = writer;
		this.writers do: { | w | if (w isAfter: latest) { latest = w; } };
		^latest;
		
	}

	writers { | set |
		var directWriters;
		set ?? { set = Set (); }; 
		directWriters = this.directWriters;
		set addAll: directWriters;
		directWriters do: _.writers (set);
		^set;
	}

	directWriters {
		var set;
		set = Set ();
		args do: { | v |
			if (v isKindOf: BusLink and: { v.readers includes: this}) {
				set addAll: v.writers;
			}
		};
		^set;
	}
	target { ^args[\target] }

	isAfter { | writer |
		^writer.target.readerGroups includes: this.target;
	}
	
	readers { | set |
		var directReaders;
		set ?? { set = Set (); }; 
		directReaders = this.directReaders;
		// [thisMethod.name, "current set:", set, "direct readers:", directReaders].postln;
		directReaders do: { | r |
			// postf("checking for cycle. next reader is: %", r);
			if (set includes: r) {
				{" ================ GODDAM CYCLE ================".postln; } ! 10;
				^set;
			}{
				" - was OK".postln;
			}
		};
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
		var bus;
		bus = args[inParam];
		if (bus.isKindOf(BusLink)) { bus.readers.remove(this) };
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
		// args[outParam].unlinkAudio(this, nil);
	}

	getInParam { | bus | ^args.findKeyForValue(bus) }
	
	setOutput2Root { | outParam = \out |
		this.setArgs(outParam, 0);
	}

	removeReader { | reader, in = \in, out = \out |
		var b;
		b = args[out];
		if (b isKindOf: BusLink and: { b.readers includes: reader}) {
			 b.unlinkAudio(this, reader, in, out);
			this.changed(\removeReader, reader, in, out);
		}{
			postf("% does not contain % as reader at %/%. Cannot unlink\n",
				this, reader, in, out);
		};		
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
		var bus;
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
	
	setBussesAndGroups { | inBus, outBus, group, argLevel | // used by MiniStereo
		this setInBus: inBus;
		this setOutBus: outBus;
		this setTarget: group;
		this.inactive remove: this;
		//	level = argLevel;
	}

	setInBus { | bus, param = \in |
		this.setBus(bus, param, \readers);
	}

	setOutBus { | bus, param = \out |
		this.setBus(bus, param, \writers);		
	}

	setBus { | bus param role |
		var oldBus;
		oldBus = args[param];
		if (oldBus isKindOf: BusLink) { oldBus.remove(this, role) };
		bus.add(this, role);
		this.setArgs(param, bus);
	}
	
	inBus { ^args[\in] }
	outBus { ^args[\out] }
	insertSerInPar {}
}
