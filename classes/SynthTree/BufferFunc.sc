/*
Provide buffer stored in Library.global under a path [server, name].

If buffer not present, then load it from file using file path select dialog.

While buffer is loading, provide a default empty buffer ("*null-buffer*").
As soon as the buffer has completed loading, notify using object that the buffer is ready.

When the server has finished booting, reload all registered buffers.

IZ Sat, Mar 15 2014, 18:38 EET

*/

BufferFunc {

	var <server, <bufferName, <action;
	var <buffer;

	*initBuffers { | server |
		// Called by ServerTree.initClass at Server boot time.
		var bufferDict, oldBuffer, newBuffer, oldNullBuffer, newNullBuffer;
		server = server ?? { SynthTree.server };
		oldNullBuffer = Library.at(server, '*null-buffer*');
		newNullBuffer = Buffer.alloc(server, server.sampleRate * 0.1, 1);
		Library.put(server, '*null-buffer*', newNullBuffer);
		oldNullBuffer.changed(\buffer, newNullBuffer);
		// making sure that modifying the library while accessing it
		// will not cause any insconsistencies:
		bufferDict = Library.at(server);
		bufferDict.keys.asArray do: { | name |
			oldBuffer = bufferDict[name];
			if (oldBuffer.path.notNil) { this.loadBuffer(server, name, oldBuffer) };
		};
	}

	*postBufNames {
		// fast utility func
		"================ Buffer names are: ================".postln;
		Library.at(SynthTree.server).keys.asArray.sort do: _.postln;
		"===================================================".postln;
	}

	*loadBuffer { | server, name, oldBuffer |
		/* TODO: uniform function to load buffer from path.
			notifies both with buffer.changed and server.changed
			server.changed used by BufferList
		*/
		var newBuffer, path;
		path = oldBuffer.path;
		Library.put(server, name,
			newBuffer = Buffer.read(server, oldBuffer.path, action: {
				oldBuffer.changed(\buffer, newBuffer);
				postf("Loaded: %\n", newBuffer);
			});
		)
	}

	*nullBuffer { | server |
		^Library.at(server ?? { SynthTree.server }, '*null-buffer*');
	}

	*new { | server, bufferName, action |
		^this.newCopyArgs(server, bufferName, action).init;
	}

	//: FIXME!
	init {
		this.setBuffer(this.getBuffer);
	}

	getBuffer {
		var newBuffer;
		newBuffer = Library.at(server, bufferName);
		if (newBuffer.isNil) {
			newBuffer = this.nullBuffer;
			this.loadBufferDialog;
		};
		^newBuffer;
	}

	nullBuffer { ^Library.at(server, '*null-buffer*') }

	loadBufferDialog {
		var newBuffer;
		"Opening buffer load dialog".postln;
		Dialog.openPanel({ | path |
			newBuffer = Buffer.read(server, path, action: {
				Library.put(server, bufferName, newBuffer);
				this.setBuffer(newBuffer);
				postf("Loaded: %s\n", newBuffer);
				this.class.changed(\newBuffer, server);
			})
		})
	}

	setBuffer { | newBuffer |
		if (buffer != newBuffer) {
			this.removeNotifier(buffer, \buffer);
			buffer = newBuffer;
			this.addNotifier(newBuffer, \buffer, { | veryNewBuffer |
				this.setBuffer(veryNewBuffer)
			});
		};
		action.(this);
	}

	bufnum {
		// the notNil no longer needed?   just ^buffer.bufnum ok?
		if (buffer.numFrames.notNil) { ^buffer.bufnum } { ^this.nullBuffer.bufnum };
	}

}
