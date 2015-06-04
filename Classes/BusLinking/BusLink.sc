
BusLink {
	var <rate = \audio, <numChannels = 1, <bus, <writers, <readers;

	*new { | rate = \audio index  numChannels = 1, server |
		^this.newCopyArgs(
			rate, numChannels, // To re-allocate on server reboot
			if (index.isNil) {
				Bus.perform(rate, server.asTarget.server, numChannels)
			}{
				Bus.newCopyArgs(rate, index, numChannels, server.asTarget.server)
			}, Set(), Set()
		)
	}

	*linkAudio { | writer, reader, inParam = \in, outParam = \out |
		var wb, rb, theBus;
		wb = writer.getArg(outParam);
		rb = reader.getArg(inParam);
		case
		{ wb.isKindOf(BusLink).not and: { rb.isKindOf(BusLink).not }} 
		{ BusLink(\audio).add(writer, outParam, \writers).add(reader, inParam, \readers) }
		{ wb.isKindOf(BusLink).not and: { rb.isKindOf(BusLink) } }
		{ rb.add(writer, outParam, \writers); }
		{ wb.isKindOf(BusLink) and: { rb.isKindOf(BusLink).not } }
		{ wb.add(reader, inParam, \readers); }
		{ true; } // all other cases were exhausted above.
			/* negotiate who should change BusLink depending on existing links
			to avoid losing connections. */
		{
			if (writer.readers(outParam).size <= 1) { ^rb.add(writer, outParam, \writers) };
			if (reader.writers(inParam).size <= 1) { ^wb.add(reader, inParam, \readers) };
			postf("cannot link % and % without breaking links\n", writer, reader);
		}
	}

	add { | chuck, param, role |
		// add this BusLink at parameter param of chuck, with role reader or writer.
		chuck.process.removeFromBus(param, role); // remove chuck from previous BusLink 
		this.perform(role) add: chuck;            // add to this BusLink
		chuck.process.setArgs([param, this]);     // Set parameter to this buslink
	}

	readersTree { | set |
		[this, "readers:", readers].postln;
		//		1.wait;
		set ?? { set = Set() };
		
		if (readers.size == 0)
		{ ^set }
		{
			set addAll: readers;
			readers do: { | r |
				r.process.args.select({ | x |
					//			1.wait;
					x.postln;
					x.isKindOf(BusLink) and: { x.readers.includes(r).not };
				}).collect({|y| y.readersTree(set) })
			};
			^set;
		}
	}

	asControlInput { ^bus.index }
}