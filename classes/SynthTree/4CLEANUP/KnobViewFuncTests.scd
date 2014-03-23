/* 
Connecting Knobs to ViewFunc
IZ Thu, Mar 13 2014, 06:51 EET
*/

~knob1 = Knobs.knob("test");

~sound = { SinOsc.ar(\freq.kr(440)) }.xplay;

//:
~viewFunc1 = ViewFunc(~knob1,
	{ | val | ~sound.set(\freq, val * 400 + 400) },
	{ | ... args | [\closed, args].postln; }
);
//:

~viewFunc1.inspect;
