//: Sun, Jun  8 2014, 11:06 EEST

a = EventModel();
//: 
a.gui({ | w |
	w.right;
	w.view.layout = VLayout(
		StaticText().string_("6 x freq, amp, pan controls:"),
		*(
			({ [
				
				HLayout(
					StaticText().string_("freq:"), a.simpleNumberBox(\freq),
					a.simpleSlider(\freq).orientation_(\horizontal).maxHeight_(23),
				),				
				HLayout(
					StaticText().string_("amp:"), a.simpleNumberBox(\amp), a.simpleKnob(\amp)
				),
				HLayout(StaticText().string_("pan:"), a.simpleNumberBox(\pan), a.simpleKnob(\pan))
			]
			} ! 6).flat
		)
	)
});
//:
b = Synth(\default);

b = { SinOsc.ar(\freq.kr(400), 0, \amp.kr(0.1)) }.play;

a.synthCtl(b, \freq);
a.synthCtl(b, \amp);
//:

// Initialize a synthtree, but do not start playing it.
{ LFNoise2.ar(\freq.kr(400), 0.1) } ==> \test;

c = EventModel();
//:
c.gui({ | w |
	c.stCtl(\test, \freq);
	w.bottom.width_(650);
	w.view.layout = HLayout(
		VLayout(
		StaticText().string_("SynthTree control"),
		StaticText().string_("Press 'space' at the slider to switch synthree on/off"),
		StaticText().string_("Slide the slider to control frequency"),
		),
		c.simpleSlider(\freq).maxWidth_(180).orientation_(\horizontal).st(\test),
		c.simpleNumberBox(\freq).maxWidth_(70).st(\test),
	);
});

//: 



//: