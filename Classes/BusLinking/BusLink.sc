
BusLink : Bus {
	var <group;
	var <writers, <readers;

	*new { | rate = \audio index  numChannels = 1 group |
		group ?? {
			group = if (rate === \audio) { Group() } { nil.asTarget };
		};
		^if (index.isNil) {
			this.perform(rate, group.server, numChannels).init(group)
		}{
			this.newCopyArgs(rate, index, numChannels, group.server).init(group);	
		}
	}

	*linkAudio { | writer, reader, outParam = \out, inParam = \in, move = \writer |
		var wt, rt, theTarget;
		wt = writer.target;
		rt = reader.target;
		case
		{
			wt.isNil and: { rt.isNil }
		} {
			theTarget = BusLink(\audio);
			writer.setWriterAudioTarget(theTarget, outParam);
			reader.setReaderAudioTarget(theTarget, inParam);
		}
		{
			wt.isNil and: { rt.notNil }
		} {
			theTarget = rt;
			writer.setWriterAudioTarget(theTarget, outParam);
		}
		{
			wt.notNil and: { rt.isNil }
		} {
			theTarget = wt;
			reader.setReaderAudioTarget(theTarget, inParam);
		}
		{
			wt.notNil and: { rt.notNil }
		} {
			if (move === \writer) {
				writer.setWriterAudioTarget(rt, outParam)
			}{
				reader.setReaderAudioTarget(wt, inParam);
			}
		}
	}

	init { | argGroup |
		group = argGroup;
		writers = Set();
		readers = Set();
	}

	addReader { | chuck |
		readers add: chuck;
		//		chuck addInput()
	}

	addWriter { | chuck |

	}

	removeReader { | chuck |

	}

	removeWriter { | chuck |

	}
}