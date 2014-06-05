/*
Utility for setting up Utopia. 

From script by Scott Wilson / BEER. 

Mon, Jun  2 2014, 21:46 EEST
*/

Utopia {

	var <win, <addrBook, <hail, <chatter;
	var <textField, <textView, <sharedData;
	var <codeRelay, <historyGUI;
	*default {
		^NameSpace(this, \default, { this.new });
	}

	*startUp { ^this.default.startUp; }

	*discoverOthers { ^this.default.discoverOthers;	}

	*chat { ^this.default.chat }

	*codeRelay { ^this.default.codeRelay }
}
