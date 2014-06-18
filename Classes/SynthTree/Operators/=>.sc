/* 
Review of => operator - see Roadmap.
Tue, Jun 17 2014, 17:23 EEST
*/

+ Object {
	=> { | chuckee adverb | chuckee.receiveChuck(this, adverb) }
}

+ SimpleNumber {
	=> { | chuckee paramName = \amp | ^chuckee.receiveNumberChuck(this, paramName) }
}

+ SynthTree {
	=> { | chuckee inputName = \in | ^chuckee.asSynthTree.addInputSynth(this, inputName) }
	receiveChuck { | chucker, inputName | this.addInputSynth(chucker, inputName ? \in) }
}

+ Symbol {
	receiveChuck { | chucker adverb | chucker.chuckInto(this.asSynthTree, adverb) }
	receiveNumberChuck { | number paramName = \amp | ^this.asSynthTree.set(paramName, number) }
}



/*

+ ControlHolder {
	receiveChuck { | chucker, adverb |
	// chucker.chuckInto(this.asSynthTree, adverb);
	}
}

*/