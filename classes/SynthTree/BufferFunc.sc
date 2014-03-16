/* 
Provide buffer stored in Library.global under a path [server, name].  

If buffer not present, then load it from file using file path select dialog. 

While buffer is loading, provide a default empty buffer ("*null-buffer*").
As soon as the buffer has completed loading, notify using object that the buffer is ready.

When the server has finished booting, reload all registered buffers.  

IZ Sat, Mar 15 2014, 18:38 EET
*/

BufferFunc {
	
	var <receiver, <bufferName, onAlloc, onFree;
	var <buffer;

	*initBuffers { | server |
		// Called by ServerTree.initClass at Server boot time.
		this.exchangeBuffer(server, '*null-buffer*', 
			Library.at(server,  '*null-buffer*'),
			Buffer.alloc(server, server.sampleRate * 0.1, 1)
		);
		Library.at(server) keysValuesDo: { | name, buffer |
			if (buffer.path.notNil) {
				this.exchangeBuffer(server, name, buffer, 
					Buffer.read(server, buffer.path)
				);
			}
		};
	}

	*exchangeBuffer { | server, name, oldBuffer, newBuffer |
		Library.put(server, name, newBuffer);
		oldBuffer.changed(\replace, newBuffer);
	}

	*nullBuffer { | server |
		^Library.at(server ?? { SynthTree.server }, '*null-buffer*');
	}

	*new { | receiver, bufferName, onAlloc, onFree |
		^this.newCopyArgs(receiver, bufferName, onAlloc, onFree).init;
	}

	init {
		this.addNotifier(buffer, \replace, { this.changed(\buffer, buffer) });

	}

	bufnum {
		if (buffer.numFrames > 0) { ^buffer.bufnum } { ^this.nullBuffer.bufnum };
	}

	nullBuffer { ^BufferFunc.nullBuffer }


}
