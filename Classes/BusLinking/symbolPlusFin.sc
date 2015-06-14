/* Sun, Jun 14 2015, 10:26 EEST
After looking at /Applications/SuperCollider/SuperCollider.app/Contents/Resources/SCClassLibrary/Common/Audio/SystemSynthDefs.sc

Maybe do away with the group scheme altogether.

Use InFeedback.ar like JITLib.

*/

+ Symbol {
	fin { | numChannels = 1 |
		^InFeedback.ar(this.kr(0), numChannels)
	}

	fadeIn { | numChannels = 1 |
		^InFeedback.ar(this.kr(0), numChannels)
		* EnvGate( doneAction: \doneAction.kr(2), curve:'sin')
		* (this++"vol").asSymbol.kr(1, 0.05); // independent volume control
	}
}