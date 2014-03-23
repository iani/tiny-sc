/*
SynthTreeMixer:  Display list of SynthTree instances with faders for controlling their amps.

IZ Thu, Mar 20 2014, 11:35 GMT
*/

SynthTreeMixer {
	classvar <all;

	var <server, <window;

	*initClass {
		all = IdentityDictionary();
	}

	*new { | server |
		var instance;
		server ?? { server = SynthTree.server };
		server.postln;
		instance = all[server];
		if (instance.isNil) {
			instance = this.newCopyArgs(server).init;
			all[server] = instance;
		}{
			instance.window.toFront;
		};
		^instance;
	}

	init {
		var height;
		height = Window.screenBounds.height;
		window = Window(server.asString, Rect(0, height - 200, 200, height - 200));
		
		window.front;
	}
}