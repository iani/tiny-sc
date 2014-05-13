/*
	A very simple interface for triggering and controlling objects in ChucK via SC.
	For Kostas Koukoudis, Sun, May 11 2014, 18:00 EEST.

	Here as class.

	More to follow.
*/

ChucKinator : UniqueWindow {
	classvar <instance;

	var <>knobs, <>buttons, <>messages, <>netAddr;
	var <selectedKnob, <selectedButton;
	var netAddr, selected;

	*new {
		instance = this.for(\chuck, \knobs, { | lose | // lose instead of winning.
			lose.initLoser;
		});
	}
	
	initLoser {
		netAddr = NetAddr("127.0.0.1", 6449);
		this.bottom(100);
		knobs = { Knob() } ! 24;
		messages = { | i | format("/%", (i + 65).asAscii).asSymbol } ! 24;
		buttons = { | i | 
			Button().fixedWidth_(40)
			.states_([[messages[i].asString[1].asString]]) // .align_(\center)
		} ! 24;
		window.view.layout = VLayout(
			View().background_(Color.red.alpha_(0.05))
			.layout_(HLayout(*knobs)),
			View().background_(Color.blue.alpha_(0.05))
			.layout_(HLayout(*buttons))
		);
		knobs do: { | knob, i |
			knob.action = { | me |
				[messages[i], me.value].postln;
				netAddr.sendMsg(messages[i], me.value);
			};
			knob.keyDownAction = { | view, char |
				if (char === $ ) { this.selectButton; };
			};
			knob.focusGainedAction = { selectedKnob = knob };
		};
		buttons do: { | b | 
			b.keyDownAction = { | view, char |
				if (char === $ ){
					this.focusKnob;
				}{
					this selectButton: buttons[char.ascii - 97];
				}
			};
		};
		this.focusKnob;
	}
		
	focusKnob {
		selectedKnob ?? { selectedKnob = knobs.first };
		selectedKnob focus: true;
	}

	selectButton { | newButton |
		var buttonNum;
		newButton ?? { newButton = selectedButton ?? { buttons.first} };
		buttonNum = newButton.states.first.first.first.ascii - 65;
		netAddr.sendMsg("/key", buttonNum);
		["/key", buttonNum].postln;
		newButton.background = Color.red;
		selectedButton !? { selectedButton.background = Color.grey(0.9); };
		selectedButton = newButton;
		newButton focus: true;
	}
}
