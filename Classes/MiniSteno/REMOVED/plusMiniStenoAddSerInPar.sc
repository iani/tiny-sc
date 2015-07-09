
+ Par {
	insertSerInPar {
		tree = tree collect: { | el |
			// TODO: Test following commented version
			// Wed, Jun 17 2015, 22:01 EEST - still TODO:
			// Not needed to insert link-chucks, if this is already a Ser?
			// But what if the ser only contains one element?
			// So it is easier and safer to always insert the link chucks ...
			// if(el idKindOf: Ser) {
			///	el
			// }{
				Ser(this.makeLinkChuck, el, this.makeLinkChuck)
			//}
		};
	}

	/*
	makeLinkChuck {
		// This method could be useful in other contexts,
		// besides the obsolete insertSerInPar
		var linkChuck;
		linkChuck = Chuck(numLinkChucks.asSymbol);
		linkChuck.source = { Inp.ar };
		linkChuck.permanent;
		numLinkChucks = numLinkChucks + 1;
		^linkChuck;
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
