
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
}

+ Ser {
	insertSerInPar { tree do: _.insertSerInPar }
}
