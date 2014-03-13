/* 
Connecting Knobs to ViewFunc
IZ Thu, Mar 13 2014, 06:51 EET
*/

~knob1 = Knobs.knob("test");

~knob1.action;

~knob1.action = { "asdfasdfasdf".postln; };

//:
~viewFunc1 = ViewFunc(~knob1,
	{ | ... args | [\action, args].postln; },
	{ | ... args | [\closed, args].postln; }
);
//:

~viewFunc1.inspect;
