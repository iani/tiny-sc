/*
	A very simple interface for triggering and controlling objects in ChucK via SC.
	For Kostas Koukoudis, Sun, May 11 2014, 18:00 EEST.

	Here as class.

	More to follow.
*/

ChucKinator : UniqueWindow {
	classvar <instance;

	var <>knobs, <>labels, <>messages, <>netAddr;

	*new {
		instance = this.for(\chuck, \knobs, { | lose | // lose instead of winning.
			var knobs, labels, messages;
			var netAddr, selected = 0;
			netAddr = NetAddr("127.0.0.1", 6449);
			lose.bottom(100);
			knobs = { Knob() } ! 24;
			messages = { | i | format("/%", (i + 65).asAscii).asSymbol } ! 24;
			labels = { | i | 
				StaticText().string_(messages[i].asString[1].asString).align_(\center)
			} ! 24;
			lose.window.view.layout = VLayout(
				[HLayout(*knobs), s: 4],
				HLayout(*labels)
			);
			knobs do: { | knob, i |
				knob.action = { | me |
					[messages[i], me.value].postln;
					//lose.sendKnobs;
					netAddr.sendMsg(messages[i], me.value);
				};
			};
			lose.window.view.keyDownAction = { | view, char |
				var newSelection, newLabel;
				newSelection = char.ascii - 65;
				newLabel = labels[newSelection];
				newLabel !? {
					labels[selected].background = Color.grey(0.9);
					newLabel.background = Color.red;
					selected = newSelection;
					netAddr.sendMsg("/key", newSelection);
					["/key", newSelection].postln;
				};
			};
			lose.initLoser(knobs, labels, messages, netAddr);
		})
	}

	initLoser { | argKnobs, argLabels, argMessages, argNetaddr |
		knobs = argKnobs;
		labels = argLabels;
		messages = argMessages;
		netAddr = argNetaddr;
	}

	/*
	sendKnobs {
		netAddr.sendMsg('/knobs', *(knobs collect: _.value).postln);
	}
	*/
}
