/*
Use a bus as a check to see if the Server has really booted.  Also wait until Shared Memory is initialized. 
Can be essential in 3 situations: 
1. Waking up after the mac has gone to sleep.
2. Make sure that the shared memory is initialized (normally the 
3. On a fast new generation MacBook (late 2013, 16Gb, 2.3 Ghz i7), The ServerTree does not catch the actual boot of the server.

Note: ServerTree add: { ... } will not work reliably.

This is a prototype that works with the default server.

ServerBootCheck.add({ "test server boot".postln });
 Server.default.quit;
Server.default.boot;

ServerBootCheck.default;

ServerBootCheck.default.makeBuffer;

Server.default.hasShmInterface;
Server.default.quit;

Server.default.isLocal;
*/

ServerBootCheck {

	var server;
	var bus;
	var funcs;

	classvar default;

	*default {
		^default ?? { this.new(Server.default) }
	}

	*new { | server |
		^this.newCopyArgs(server).init;
	}

	init {
		if (server.serverRunning) {
			this.makeBus;
		};
		ServerBoot.add({ this.checkIfReallyBootedAndDoActions }, server);
		ServerTree.add({ "IF THIS WORKS, then all was in vain".postln; });
	}

	makeBus {
		bus = Bus.control(server);
		bus.set(12345);
	}

	checkIfReallyBootedAndDoActions {
		if (bus.isNil) {
			this.serverReallyBooted;
		}{
		bus.get({ | val | 
				if (val == 12345) {
					this.serverDidNotReallyBoot;
				}{
					this.serverReallyBooted;
				 }
			 });
		 };
	 }

	 serverReallyBooted {
		 var time;
		 if (server.isLocal) {
			 {
				 time = Process.elapsedTime;
				 "\n=== Waiting for server shared memory interface ===\n ".postln;
				 "Seconds elapsed: ".post;
				 while { server.hasShmInterface.not }
				 { 
					 postf("% - ", (Process.elapsedTime - time).round(0.5));
					 0.5.wait;
				 };
	
				 this.doActionsAndMakeBus;
			 }.fork;
		 }{
			 this.doActionsAndMakeBus;
		 }
	 }

	doActionsAndMakeBus {
		funcs do: _.value;
		this.makeBus;
	}

	 serverDidNotReallyBoot { "Server did not really boot".postln; }
	 *add { | func | this.default.add(func); }
	 add { | func | funcs = funcs add: func; }
}