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
					StaticText().string_("freq:"), a.numberBox(\freq),
					a.slider(\freq).orientation_(\horizontal).maxHeight_(23),
				),				
				HLayout(
					StaticText().string_("amp:"), a.numberBox(\amp), a.knob(\amp)
				),
				HLayout(StaticText().string_("pan:"), a.numberBox(\pan), a.knob(\pan))
			]
			} ! 6).flat
		)
	)
});
//:
b = Synth(\default);
a.synthCtl(b, \freq);
a.syntthCtl(b, \amp);
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
		c.slider(\freq).maxWidth_(180).orientation_(\horizontal).st(\test),
		c.numberBox(\freq).maxWidth_(70).st(\test),
	);
});

