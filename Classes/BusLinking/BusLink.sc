
BusLink {
	var <bus, <writers, <readers;

	*new { | rate = \audio index  numChannels = 1, server |
		^this.newCopyArgs(
			if (index.isNil) {
				Bus.perform(rate, group.server, numChannels).init(group)
			}{
				Bus.newCopyArgs(rate, index, numChannels, group.server).init(group);	
			}, Set(), Set()
		)
	}

	*linkAudio { | writer, reader, outParam = \out, inParam = \in |
		var wb, rb, theBus, numWritersReaders, numReadersWriters;

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
			theBus = wb; // to tail of target and move reader's readers links after
			reader.setInbus(theBus, inParam);
		}
		{
			wb.notNil and: { rb.notNil }
		} {
			/* negotiate who should change BusLink depending on existing links
				to avoid losing connections. */
			numWritersReaders = writer.readers(outParam).size;
			numReadersWriters = reader.writers(inParam).size;
			if (numWritersReaders <= 1) {
				^writer.setOutbus(rb, outParam)
			}
			if (numReadersWriters <= 1) {
				^reader.setInbus(wb, inParam);
			};
			postf("cannot relink % and % without breaking links\n", writer, reader);
		}
	}

	init { | argGroup |
		group = argGroup;
		writers = Set();
		readers = Set();
	}

	asTarget { ^group }

	addReader { | chuck |
		readers add: chuck;
	}

	addWriter { | chuck |

	}

	removeReader { | chuck |

	}

	removeWriter { | chuck |

	}
}