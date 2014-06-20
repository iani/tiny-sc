/*

Wed, Jun 18 2014, 10:36 EEST

*/

a = \test.osc;

a.hash;

a addDependant: { | ... args | args.postln; };

{ | i | NetAddr.localAddr.sendMsg(\test, i); } ! 10;




//:

a = \test.osc;

{ SinOsc.ar(\freq.kr(400), 0, 0.1) } => \test;

b = \test.st.getParam(\freq);

a.addListener(b);

//:
r = { 
	loop {
		NetAddr.localAddr.sendMsg(\test, 0.2.rand + 0.4);
		0.1.wait;
	};
}.fork;

//:
r.stop;