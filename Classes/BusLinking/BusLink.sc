
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

	*linkAudio { | writer, reader, outParam = \out, inParam = \in |
		var wb, rb, theBus;

		postf("%: out: %, in: %", thisMethod.name, outParam, inParam);

		wb = writer.outbus(outParam);
		rb = reader.inbus(inParam);
		case
		{
			wb.isNil and: { rb.isNil }
		} {
			theBus = BusLink(\audio);
			writer.setOutbus(theBus, outParam); 
			reader.setInbus(theBus, inParam);
		}
		{
			wb.isNil and: { rb.notNil }
		} {
			theBus = rb;
			writer.setOutbus(theBus, outParam);
		}
		{
			wb.notNil and: { rb.isNil }
		} {
			theBus = wb;
			reader.setInbus(theBus, inParam);
		}
		{
			wb.notNil and: { rb.notNil }
		} {
			/* negotiate who should change BusLink depending on existing links
				to avoid losing connections. */
			if (writer.readers(outParam).size <= 1) {
				^writer.setOutbus(rb, outParam)
			};
			if (reader.writers(inParam).size <= 1) {
				^reader.setInbus(wb, inParam);
			};
			postf("cannot link % and % without breaking links\n", writer, reader);
		}
	}

	addReader { | chuck | readers add: chuck; }
	addWriter { | chuck | writers add: chuck; }
	removeReader { | chuck | readers remove: chuck; }
	removeWriter { | chuck | writers remove: chuck; }
}