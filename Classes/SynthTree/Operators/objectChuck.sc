/* 
Chuck operator and variants.

IZ Sat, Mar  8 2014, 23:40 EET

*/

+ Object {
	=!> { | key |
		currentEnvironment.parent[key] = this;
	}

	=> { | chuckee, param |
		if (param.isNil) {
			^chuckee.receiveChuck(this);
		}{
			this.perform('+>', chuckee, param)
		}
	}

	==> { | synthTree, replaceAction = \fadeOut |
		// as => but do not start the synth now: 
		// synth gets started when the synthTree is added as input with =<
		synthTree = synthTree.asSynthTree;
		if (synthTree.isPlaying) {
			^synthTree.chuck(this, replaceAction);
		}{
			^synthTree.setTemplate(this);
		};
	}

	|> { | synthTree, replaceAction = \fadeOut |
		// Just set the synthTree's template.
		synthTree = synthTree.asSynthTree;
		^synthTree.setTemplate(this);
	}

	// deprecated: 
	=|> { | synthTree, replaceAction = \fadeOut |
		// Just set the synthTree's template.
		synthTree = synthTree.asSynthTree;
		^synthTree.setTemplate(this);
	}

	=@> { | synthTree, param = \amp |
		// map a source to a parameter of the synthtree
		^this doIfSynthTree: { | st | st.map(param, this) }
	}
	
	doIfSynthTree { | action |
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? {
			action.(synthTree, this);
		};
	}

	doIfEdef { | action |
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? {
			action.(synthTree, this);
		}
	}

	%> { | params |  ^SynthPattern (this, params) } // 20140429: obsolete?
	hasInputs { ^false }
	push {}
	globDur { currentEnvironment.parent[\dur] = this.asStream } 
}
