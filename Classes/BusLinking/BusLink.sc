
BusLink {
	var <rate = \audio, <numChannels = 1, <bus, <writers, <readers;

	*initClass {
		ServerBootCheck addStartup: { this.init }
		 
	}
	*init {
		Library.at(Chuck) do: { | c |
			c.args do: { | a |
				if (a isKindOf: BusLink) { a.remakeBus }
			}
		}
	}

	remakeBus { // re-allocate bus on server boot, to correctly set allocation of server
		bus = Bus.perform(rate, bus.server, numChannels);
	}

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
	
	*unlinkAudio { | writer, reader, inParam = \in, outParam = \out |
		// INCOMPLETE!
		var wb, rb;
		wb = writer.getArg(outParam);
		rb = reader.getArg(inParam);
		if (wb.isKindOf(BusLink) and: { rb.isKindOf(BusLink) } and: { wb == rb }) {
			wb.unlinkAudio(writer, reader, inParam, outParam);
		}{
			postf("% and % are not linked.  Cannot unlink.\n", writer, reader);
		};
	}

	unlinkAudio { | writer, reader, inParam, outParam |
			readers remove: reader;
			writers remove: writer;
			if (writers.size == 0) { reader.setInput2Null(inParam) };
			if (readers.size == 0) { writer.setOutput2Root(outParam) };
			if (readers.size == 0 and: { writers.size == 0 }) { bus.free };
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
		{ true } // all other cases were exhausted above.
			/* negotiate who should change BusLink depending on existing links
			to avoid losing connections. */
		{
			if (writer.readersAt(outParam).size <= 1) { ^rb.add(writer, outParam, \writers) };
			if (reader.writersAt(inParam).size <= 1) { ^wb.add(reader, inParam, \readers) };
			postf("cannot link % and % without breaking links\n", writer, reader);
			this.makeSignalCopyLink(writer, reader, inParam, outParam)
		}
	}

	makeSignalCopyLink { | writer, reader, inParam, outParam |
		thisMethod.notImplemented;
	}

	add { | chuck, param, role |
		// add this BusLink at parameter param of chuck, with role reader or writer.
		// chuck.removeFromBus(param, role); // remove from previous BusLink 
		this.perform(role) add: chuck;            // add to this BusLink
		chuck.setArgs(param, this);     // Set parameter to this buslink
	}

	asControlInput { ^bus.index }

	printOn { arg stream;
		stream << "BusLink(" << bus.index << ")";
	}
	storeOn { arg stream;
		stream << "BusLink(" << bus.index << ")";
	}

}