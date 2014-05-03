

//: SynthTree audio routing (OK)
{ RHPF.ar(Inp.ar(numChannels: 2), \freq.kr(6000, 0.05), \rq.kr(0.2)) * 10 } => \fx3;
//:
{ SinOsc.ar([1, 1.2] * 400 /* , \phase.ar(0 ! 2) * 0.2 */ ) } => \src3;
//:
\fx3 =< \src3;
\fx3.set(\amp, 1);
\src3.set(\amp, 1);
(freq: Pbrown(150, 1200, 150, inf), dur: 0.1) *> \fx3;
//:
\src3.stop;
//:
\src3.start;
//: 

// ================================================================
// Ndef audio routing.  OK 
Ndef(\fx, { RHPF.ar(\in.ar([0, 0]), \freq.kr(6000, 0.05), \rq.kr(0.2)) * 10 });
//: 
Ndef(\src, { WhiteNoise.ar(0.1 ! 2) });

//: NOTE: \phase.ar causes problems when routing *after* starting the source
// Ndef(\src, { SinOsc.ar([1, 1.2] * 400 /* , \phase.ar(0 ! 2) * 0.2 */ ) });

//:
Ndef(\fx).play;
//: The next one is to test for problems: 
Ndef(\src).play;
//:
Ndef(\fx) <<> Ndef(\src);

//:
Ndef(\fx).set(\freq, 1000);
//:
{
	In.ar(0, 2);
  SinOsc.ar([1, 1.2] * 400) 
} => \src3;

//:
{ SinOsc.ar([1, 1.2] * 400, In.ar(0, 2) * 0.2) } => \src3;
//:
