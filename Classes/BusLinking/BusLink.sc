
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

	*new { | rate = \audio index numChannels = 1 server |
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
		// INCOMPLETE!
		/* TODO: Either the writer or the reader have to move to a different bus!
		// Following is a draft showing the decision algorithm:
		// if there are many readers, then:
			  - the writer keeps its output, to feed the rest of the readers.
			  - the reader gets a new bus at this input, so it's disconnected from the writer.
		   if there are many writers, then:
              - the reader keeps its input, so it keeps reading from the rest of the writers
              - the writer gets a new output bus, so it stops writing to this reader
		*/
		postf("% IS STARTING % : writers are: %, readers are: %\n",
			this, thisMethod.name, writers, readers);
		if (readers.size > 1) {
			BusLink(\audio).add(reader, inParam, \readers);
			readers remove: reader;
			postf("This was case 1, remove reader. Removed:% \n", reader);
		}{
			BusLink(\audio).add(writer, outParam, \writers);
			writers remove: writer;
			if (writers.size == 0) {
				readers do: { reader.setInput2Null(inParam) };
				this.free;
				readers = Set();
				writers = Set();
			};
			postf("This was case 2, remove writer. Removed:% \n", writer);
		};
		postf("% JUST FINISHED % : writers are: %, readers are: %\n",
			this, thisMethod.name, writers, readers);
		// The following should also change.  Only remove one of the two, not both:
		/* // the following is old and should all be removed!
		readers remove: reader;
		writers remove: writer;
		if (writers.size == 0) { reader.setInput2Null(inParam) };
		if (readers.size == 0) { writer.setOutput2Root(outParam) };
		if (readers.size == 0 and: { writers.size == 0 }) { bus.free };
		*/ // end of old code
	}

	free { // may do more stuff later
		bus.free;
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
		{  // WRONG!!!!!! MUSt CORRECT!
			if (rb.readers.size <= 1) {
				"There was at most one reader. Therefore adding writers.".postln;
				"Readers are: ".post; rb.readers.postln;
				"Writers are: ".post; rb.writers.postln;
				rb.add(writer, outParam, \writers);
				"I Did it. Now the result is: ".postln;
				postf("readers: %, writers: %\n", rb.readers, rb.writers);
				^rb;
			};
			if (wb.writers.size <= 1) {
				"There was at most one writer. Therefore adding readers.".postln;
				"Readers are: ".post; wb.readers.postln;
				"Writers are: ".post; wb.writers.postln;
				wb.add(reader, inParam, \readers);
				"I Did it. Now the result is: ".postln;
				postf("readers: %, writers: %\n", wb.readers, wb.writers);
				^wb;
			};
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