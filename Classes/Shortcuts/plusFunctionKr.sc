/*
Shortcut: 

Make a function play in kr

Sun, Jun 15 2014, 21:57 EEST
*/

+ Function {
	makeControlSynth { | busIndex = 0 server |
		^this.kr(busIndex, server)
	}

	kr { | outBus = 0 outName = \out server |
		^{ Out.kr(outName.kr(0), this.value) }.play(server, args: [outName, outBus]);
	}
}
