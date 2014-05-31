
a = EventModel();

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
{ LFNoise2.ar(\freq.kr(1000) } => \test;
//:
\test.addNotifier(a, \freq, { | freq |
	\test.set(\freq, freq)
});
