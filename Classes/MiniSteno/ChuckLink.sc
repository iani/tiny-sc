
ChuckLink {
	var <chuck, <inputName;
	// Not yet implemented:
	var <xBus, <xBusLinkSynth; // optional branch out to other bus
	
	*new { | chuckName, inputName = \in |
		^this.newCopyArgs (Chuck (chuckName.asSymbol), inputName.asSymbol);
	}
	chucks { ^[chuck] }

	setBussesAndGroups { | inBus, outBus, group | // used by MiniStereo
		chuck.setInBus (inBus, inputName);
		chuck setOutBus: outBus;
		chuck setTarget: group;
		^group.getReaderGroup;
	}

	target { ^chuck.target }

	inBus { ^chuck inBus: inputName }

	outBus { ^chuck.outBus }

	makeOutBus { ^ArBusLink () }
}