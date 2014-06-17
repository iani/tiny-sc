/* 
Review of => operator - see Roadmap.
Tue, Jun 17 2014, 17:23 EEST
*/

/*
+ SynthTree {

	=> { | st, input = \in |
		[this, thisMethod.name, "under review", input].postln;
	}

}
*/

+ Object {
	=> { | chuckee, adverb |
		chuckee.receiveChuck(this, adverb)
	}
}

+ Symbol {
	receiveChuck { | chucker, adverb |
		chucker.chuckInto(this.asSynthTree, adverb);
	}
}

+ SynthTree {
	=> { | chuckee, inputName = \in |
		^chuckee.asSynthTree.addInputSynth(this, inputName)
	}

	receiveChuck { | chucker, inputName |
		this.addInputSynth(chucker, inputName ? \in);
	}
}

/*

+ ControlHolder {
	receiveChuck { | chucker, adverb |
	// chucker.chuckInto(this.asSynthTree, adverb);
	}
}

*/