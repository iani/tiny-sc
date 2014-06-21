// Testing with addKrFunc

{ SinOsc.ar(\freq.kr(400), 0, 0.1) } => \test;

{ SinOsc.kr(7).range(400, 450) } =>.freq \test;
 
{ LFNoise0.kr(1).range(1000, 1200) } =>.freq \test;


{ Amplitude.kr(Inp.ar(\input, 2)) } =>.amp \test;

{ Pitch.kr(Inp.ar(\input, 2)) } =>.freq \test;

\freq >| \test;  // unmap, seting value to value of bus at time of unmap

{ XLine.kr(1000, 2000, 10) } =>.freq \test;

//:
r = {
	var target = 400, dur;
	loop {
		{ XLine.kr(target, target = 300 exprand: 3000, dur = 0.05 exprand: 0.5) }
		=>.freq \test;
		dur.wait;
		{ LFNoise0.kr(30 * dur min: 20).range(*(target * [0.8, 1.2])) } =>.freq \test;
		(dur * 2).wait;
	}
}.fork

//:

r.stop;

//:
r = {
	var target = 400;
	loop {
		{ XLine.kr(target, target = 300 exprand: 3000, 5) }
		=>.freq \test;
		5.wait;
	}
}.fork

//:

\freq >| \test;

//: Testing interface construction for EventModel 
// see EventModelTries.scd
// Can we get the keys from a Function or Routine?

a = () make: { { ~a = 10 }.fork };
a.keys;  // yes!

a = () make: { ~a = 10 };
a.keys;
 