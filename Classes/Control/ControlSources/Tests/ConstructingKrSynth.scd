/*
Developing the code for constructing a kr rate Synth for ControlSynth. 

Sun, Jun 15 2014, 21:36 EEST

The resulting code will be packed into a new method 'kr' in Class Function

*/


Server.killAll;

~func = { SinOsc.kr(7).range(400, 440) };

~csynth = { Out.kr(\out.kr(0), ~func.value) }.play;

~ssynth = { SinOsc.ar(\freq.kr(400), 0, 0.1) }.play;

~ssynth.map(\freq, 0);

//: Tests afer writing the above method: 


~csynth = { SinOsc.kr(7).range(400, 440) }.kr;
~ssynth = { SinOsc.ar(\freq.kr(400), 0, 0.1) }.play;
~ssynth.map(\freq, 0);
~csynth run: 0;
~csynth run: 1;
~csynth.free;

//:

~csynth.poll;


//:

{ SinOsc.ar(\freq.kr(400), 0, 0.1) } => \krtest;

{ SinOsc.kr(4).range(400, 500) } +>.freq \krtest;

{ LFNoise0.kr(40).range(400, 500) } +>.freq \krtest;

{ LFNoise1.kr(12).range(400, 500) } +>.freq \krtest;

(dustrate: [1, 2].pseq, dur: [0.1, 0.2].prand) +> \synthtree;