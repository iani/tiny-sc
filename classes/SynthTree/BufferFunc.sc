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

	// FIXME!
	// TODO: check how and when to create buffers
	// TODO: on making buffers, ensure that they notify when 
	// they are finished reading/allocating and when they are freed
	*initBuffers { | server |
		// Called by ServerTree.initClass at Server boot time.
		var bufferDict, oldBuffer, newBuffer;
		this.addBuffer(server, '*null-buffer*',
			Buffer.alloc(server, server.sampleRate * 0.1, 1)
		);
		// making sure that modifying the library while accessing it
		// will not cause any insconsistencies:
		bufferDict = Library.at(server);
		bufferDict.keys.asArray do: { | name |
			oldBuffer = bufferDict[name];
			if (oldBuffer.path.notNil) {
				Library.put(server, name, 
					newBuffer = Buffer.read(server, oldBuffer.path, action: { 
						oldBuffer.changed(\buffer, newBuffer);
					});
				)
			}
		};
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
		Dialog.openPanel({ | path |
			newBuffer = Buffer.read(server, path, action: {
				this.setBuffer(newBuffer);
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
