/*

Here all methods that are needed to handle =<

*/

+ Object {
	// DEPRECATED!
	=< { | synthTree, inputName = \in |
		// add synthTree to the input synths of the receiver
		this.asSynthTree.addInputSynth(synthTree.asSynthTree, inputName)
	}
}

+ SynthTree {

	addInputSynth{ | synthTree, inputName = \in |
		/*  Add another synthTree as an input to myself. (I am an "fx" synth).
			Add synthTree to my inputs and make it output its signal to my input.
			Add synthTree to your dictionary under its name,
			THEN create the synth, using your group as target,
			addToHead as add method, and setting the output \out
			to one of your inputs, through args at synth creation time.
		*/
		if (inputs.isNil) {
			postf("% has no inputs. Cannot add input.\n", name);
			^this;
		};
		if (this outputsTo: synthTree) {
			postf("% outputs to % and therefore cannot add it as input. Cycle!\n",
			name, synthTree.name);
			^this
		};
		this[synthTree.name] = synthTree;
		synthTree.setOutput(this, inputName);
		if (this.isPlaying) {
			synthTree.start
		}{
			synthTree.addNotifierOneShot(this, \started, { synthTree.start });
		};
	}

	outputsTo { | synthTree |
		if (output.isNil) { ^false };
		if (output === synthTree) { ^true } { ^output outputsTo: synthTree };
	}

	setOutput { | synthTree, inputName = \in |
		var outputBus;
		outputBus = synthTree.getInputBus(inputName);
		if (outputBus.isNil) {
			postf("% has no input named %. Cannot output to it\n",
				synthTree.name, inputName);
			^this;
		};
		output = synthTree;
		outputName = inputName;
		if (synth.isPlaying) {
			this.moveBefore(synthTree.synth);
			synth.set(\out, outputBus.index);
		}
	}

	getInputBus { | inputName = \in |
		^inputs !? { inputs[inputName] };
	}
}