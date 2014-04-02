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
		 /* Sometimes the Shared Memory Interface of a server can take some
			seconds to initialize.  One should wait until that happens, otherwise
			 synths may not be created in the default group.
			 Here Count 1/2 seconds while waiting, thus notifying user to wait. 
			 Using own ServerBootCheck because ServerTree does not catch
			 false boots on system wake-up after sleep. */
		 if (server.isLocal) {
			 {
				 var time, waitingSecs;
				 time = Process.elapsedTime;
				 "\n=== Waiting for server shared memory interface ===\n ".postln;
				 "Seconds elapsed: ".post;
				 while { server.hasShmInterface.not }
				 {
					 waitingSecs = (Process.elapsedTime - time).round(0.5);
					 postf("% - ", waitingSecs);
					 if (waitingSecs - 4 % 6 == 0) { "".postln; };
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

	 serverDidNotReallyBoot {
		postf("Server % resuming after system wakeup\n", server); 
	 }
	 *add { | func | this.default.add(func); }
	 add { | func | funcs = funcs add: func; }
}