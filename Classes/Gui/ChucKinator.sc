/*
	A very simple interface for triggering and controlling objects in ChucK from a GUI
	made in SC, over OSC. 

	For Kostas Koukoudis, Sun, May 11 2014, 18:00 EEST.

	Here as class.

	More to follow.

Keyboard commands: 

Anywhere: <space> : switch between last selected knob and button

Switching from knob to last selected button does not send the button's message. 

In button row: letters a - z: select one of the buttons between A and Z, and send. 

Mouse click on button: Selects and send.
 
*/

ChucKinator : UniqueWindow {
	classvar <instance;

	var <>knobs, <>buttons, <>messages, <>netAddr;
	var <selectedKnob, <selectedButton;
	var netAddr, selected;

	*new { | numItems = 26 |
		instance = this.for(\chuck, \knobs, { | lose | // lose instead of winning.
			lose.initLoser(numItems);
		});
	}
	
	initLoser { | numItems = 26 |
		netAddr = NetAddr("127.0.0.1", 6449);
		this.bottom(100);
		knobs = { Knob() } ! numItems;
		messages = { | i | format("/%", (i + 65).asAscii).asSymbol } ! numItems;
		buttons = { | i | 
			Button().fixedWidth_(40)
			.states_([[messages[i].asString[1].asString]]) // .align_(\center)
		} ! numItems;
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
				if (char === $ ) { this.switch2Buttons; };
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
			b.action = { this selectButton: b };
		};
		this.focusKnob;
		selectedButton = buttons[0];
	}
		
	focusKnob {
		selectedKnob ?? { selectedKnob = knobs.first };
		selectedKnob focus: true;
	}

	switch2Buttons { selectedButton focus: true; }

	selectButton { | newButton |
		var buttonNum;
		if (newButton.isNil) { ^this };
		buttonNum = newButton.states.first.first.first.ascii - 65;
		netAddr.sendMsg("/key", buttonNum);
		postf("/key % ('%')\n", buttonNum, newButton.states.first.first);
		newButton.background = Color.red;
		selectedButton !? { selectedButton.background = Color.grey(0.9); };
		selectedButton = newButton;
		newButton focus: true;
	}
}
