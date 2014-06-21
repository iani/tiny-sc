/*
First tests with BroadcastingResponder.

Fri, Jun 20 2014, 23:28 EEST
*/

{ SinOsc.ar(\freq.kr(400), 0, 0.1) } => \test_osc;

// .osc_ also creates default OSCFunc
\osc1.osc_ =>.freq \test_osc; // add osc control to \freq

//:
{
	loop {
		NetAddr.localAddr.sendMsg('osc1', 0.5 + 0.2.rand);
		0.1.wait;
	}
}.fork;

//: Remove osc control from freq: 
// .osc accesses existing OSCFunc, but does not make one.
\osc1.osc >|.freq \test_osc;


// Preparing for Leap;
'/left/present'.osc_.spec_([-1.0, 1].asSpec);

NetAddr.localAddr.sendMsg('/left/present', -1.0 rrand: 1.0);

'/left/present'.osc addDependant: { | ... args | args.postln; };

//:
'/left/present'.osc addDependant: { | sender, message, value  |
	(freq: \freq.asSpec.map(value)).play;
};

//:
(dur: 0.1, degree: (0..7).prand) => \pattern;

//:
'/left/present'.osc_
.spec_([-1.0, 1].asSpec)
.addListener(\action1, { | value |
	(dur: 0.1 + value / 3) +> \pattern;
});