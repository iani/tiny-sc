
+ Par {
	insertSerInPar {
		tree = tree collect: { | el |
			// TODO: Test following commented version
			// Wed, Jun 17 2015, 22:01 EEST - still TODO:
			// Not needed to insert link-synthPlayers, if this is already a Ser?
			// But what if the ser only contains one element?
			// So it is easier and safer to always insert the link synthPlayers ...
			// if(el idKindOf: Ser) {
			///	el
			// }{
				Ser(this.makeLinkSynthPlayer, el, this.makeLinkSynthPlayer)
			//}
		};
	}

	/*
	makeLinkSynthPlayer {
		// This method could be useful in other contexts,
		// besides the obsolete insertSerInPar
		var linkSynthPlayer;
		linkSynthPlayer = SynthPlayer(numLinkSynthPlayers.asSymbol);
		linkSynthPlayer.source = { Inp.ar };
		linkSynthPlayer.permanent;
		numLinkSynthPlayers = numLinkSynthPlayers + 1;
		^linkSynthPlayer;
	}
	*/

	/* findContainerOf always traverses the entire tree. 
	   parentOf is a better solution, because it stops traversing 
		as soon as the element is found  */

	findContainerOf { | element |
		var found;
		this.traverseDoing({ | ms | if (ms.tree includes: element) { found = ms } });
		^found;
	}

	traverseDoing { | func |
		func.(this);
		tree do: { | x | if (x isKindOf: MiniSteno) { x.traverseDoing(func) } }
	}

}

+ Ser {
	insertSerInPar { tree do: _.insertSerInPar }
}
