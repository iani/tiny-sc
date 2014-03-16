/* 
Provide buffer stored in Library.global under a path [server, name].  

If buffer not present, then load it from file using file path select dialog. 

While buffer is loading, provide a default empty buffer ("*null-buffer*").
As soon as the buffer has completed loading, notify using object that the buffer is ready.

When the server has finished booting, reload all registered buffers.  

IZ Sat, Mar 15 2014, 18:38 EET

*/

BufferFunc {
	
	var <bufferName, <action;
	var <buffer;

	// FIXME!
	// TODO: check how and when to create buffers
	// TODO: on making buffers, ensure that they notify when 
	// they are finished reading/allocating and when they are freed
	*initBuffers { | server |
		// Called by ServerTree.initClass at Server boot time.
		this.changeBuffer(server, '*null-buffer*', 
			Library.at(server,  '*null-buffer*'),
			Buffer.alloc(server, server.sampleRate * 0.1, 1)
		);
		Library.at(server) keysValuesDo: { | name, buffer |
			if (buffer.path.notNil) {
				this.changeBuffer(server, name, buffer, 
					Buffer.read(server, buffer.path)
				);
			}
		};
	}

	// FIXME!
	*changeBuffer { | server, name, oldBuffer, newBuffer |
		Library.put(server, name, newBuffer);
		oldBuffer.changed(\buffer, newBuffer);
	}

	*nullBuffer { | server |
		^Library.at(server ?? { SynthTree.server }, '*null-buffer*');
	}

	*new { | bufferName, action |
		^this.newCopyArgs(bufferName, action).init;
	}

	//: FIXME!
	init {

	}

	bufferChanged { | newBuffer |
		if (buffer != newBuffer) {
			this.removeNotifier(buffer, \buffer);
			buffer = newBuffer;
			this.addNotifier(buffer, \buffer, { | newBuffer |
				this.bufferChanged(newBuffer)
			});
		};
		action.(this);
	}

	bufnum {
		if (this.numFrames > 0) { ^buffer.bufnum } { ^this.nullBuffer.bufnum };
	}

	numFrames { ^buffer.numFrames ? 0 }
	nullBuffer { ^this.class.nullBuffer }
}
