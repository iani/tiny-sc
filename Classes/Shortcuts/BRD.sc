/*
Shortcut for BufRd ugen.  Provides control names for arguments of BufRd ugen.
*/

BRD {
	*new { | numChans = 1, bufnum = 0, rate = 1.0, trigger = 1.0,
		startPos = 0.0, loop = 0.0, doneAction = 0 |
		^BufRd.ar (numChans, bufnum,
			Phasor.ar(0, BufRateScale.kr(bufnum), 0, BufFrames.kr(bufnum)),
			0,
			loop
		)
	}
}