/*
Shortcut: 

Make a function play in kr

Sun, Jun 15 2014, 21:57 EEST
*/

+ Function {
	kr { | outBus = 0 outName = \out, server |
		^{ Out.kr(\out.kr(0), this.value) }.play(server);
	}
}