/* Thu, Jun 18 2015, 10:36 EEST

Evaluate a function strictly after a synthdef has been loaded. 

Prevents race situation where the same object asks for a new 
synthdef to be loaded before its own old request has been completed. 

Current implementation is only for the local server as default server.

*/

SynthDefLoader {
	classvar <waiting;  // Array of instances waiting to be processed
	classvar <sent;     // Current instance sent to server, waiting for response
	classvar <server;
	
	var <client, <synthDef, <action;

	*initClass {
		waiting = [];
		server = Server.default;
		StartUp add: {
			OSCFunc(
				{ | ... args |
					sent !? {
						sent.defLoaded(this);
						if (waiting.size > 0) {
							waiting[0].send;
							waiting = waiting[1..];
						}{
							sent = nil;
						};
					};
				},
				"/done",
				argTemplate: [{ | pattern |
					['/d_load', '/d_recv'] includes: pattern
				}]
			).permanent_(true);
		}
	}
	
	*add { | client, synthDef, action |
		// only add client if not already waiting: 
		if (waiting.detect({ | s | s.client === client }).isNil) {
			waiting = waiting add: this.newCopyArgs(client, synthDef, action).trySend;
		};
	}

	trySend { // only send if no other synthdef is already waiting to load
		sent ?? { this.send }
	}

	send {
		sent = this;
		synthDef.doSend(server);
	}

	defLoaded { client.defLoaded(this); }

}
