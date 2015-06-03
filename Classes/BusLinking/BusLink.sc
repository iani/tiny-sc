
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

		postf("%: out: %, in: %", thisMethod.name, outParam, inParam);

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
		chuck.process.removeBus(param);
		this.perform(role) add: chuck;
		chuck.process.setArgs([param, this]);
	}
}