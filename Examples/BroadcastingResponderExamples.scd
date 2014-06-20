/*
First tests with BroadcastingResponder.

Fri, Jun 20 2014, 23:28 EEST
*/

{ SinOsc.ar(\freq.kr(400), 0, 0.1) } => \test;

// .osc_ also creates default OSCFunc
\osc1.osc_ =>.freq \test; // add osc control to \freq

//:
{
	loop {
		NetAddr.localAddr.sendMsg('osc1', 0.5 + 0.2.rand);
		0.1.wait;
	}
}.fork;

//: Remove osc control from freq: 
// .osc accesses existing OSCFunc, but does not make one.
\osc1.osc >|.freq \test;