/*

These examples show the concept of connecting a model with views. 
It is only meant as conceptual illustration.

For practical use with SynthTree, see file SliderExamples.scd

*/

//:
{ LFNoise2.ar(\freq.kr(1000)) } => \test;

//:

\test.


//:


/*

Test 1.

Result: EventModel not needed. Attach then new GUI directly to args of SynthTree. 
*/
a = EventModel();

//: 
a.gui({ | w |
	w.view.layout = VLayout(
		NumberBox().setter(\x),
		NumberBox().setter(\x),
		NumberBox().setter(\y),
		Slider().setter(\y),
		Knob().setter(\freq),
		Slider().setter(\freq),
		NumberBox().setter(\freq),
	)
});

//:
{ LFNoise2.ar(\freq.kr(1000)) } => \test;
//:
\test.addNotifier(a, \freq, { | freq |
	\test.set(\freq, freq);
});

//:

a.addNotifier(\test.asSynthTree.args[\freq], \value, { | unscaled, scaled, notification |
	notification.notifier.postln;
	// a.put(\freq, value);
}) 



//:

Server.killAll

//:

\test.asSynthTree;



\test.asSynthTree.args[\freq]  addDependant: { | ... args | args.postln; };


\test.asSynthTree.args.class;
