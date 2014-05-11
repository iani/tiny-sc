/*
A very simple interface for triggering and controlling objects in ChucK via SC.
For Kostas Koukoudis, Sun, May 11 2014, 18:00 EEST.

More to follow.
*/


UniqueWindow.for(\chuck, \knobs, { | lose | // lose instead of winning.
	var knobs, messages;
	var netAddr;
	netAddr = NetAddr("127.0.0.1", 6449);
	lose.bottom(100);
	knobs = { Knob() } ! 24;
	lose.window.view.layout = HLayout(*
		(knobs collect: { | knob, i | VLayout(
			[knob, s: 4], 
			StaticText().string_(format("%", i + 1)).align_(\center)
		)
		})
	);
	messages = { | i | format("/knob%", i + 1).asSymbol } ! 24;
	knobs do: { | knob, i |
		knob.action = { | me |
			[messages[i], me.value].postln;
			netAddr.sendMsg(messages[i], me.value);
		}
	}
});