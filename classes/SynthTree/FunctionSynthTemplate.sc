/*

Quick patch to avoid redoing the Function:play method in SynthTree compatible manner. 
Needed because we need the synthdef to make the inputs.

IZ Thu, Mar 27 2014, 17:39 EET

*/

FunctionSynthTemplate {
	var <function, <synthdef;
	*new { | function, synthTree |
		^this.newCopyArgs(function).makeSynthDef(synthTree);
	}

	makeSynthDef { | synthTree |
		synthdef = { function.value.adsrOut(attackTime: synthTree.fadeTime); }
		.asSynthDef; // we don't actually use it on the server. no .add
	}

	inputSpecs { ^synthdef.inputSpecs }

	asSynthTemplate { ^this }

	templateArgs { ^synthdef.allControlNames }

	asSynth { | synthTree, fadeTime |
		^function.asSynth(synthTree, fadeTime);
	}
}