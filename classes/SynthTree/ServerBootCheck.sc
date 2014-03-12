/*
Use a bus as a check to 

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
				 "Waiting for server shared memory interface".postln;
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