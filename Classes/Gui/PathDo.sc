/* 
Select a path with dialog box and perform a functino with it. 

IZ Thu, Apr 10 2014, 13:08 EEST

PathDo({ | p | p.postln; });

*/

PathDo {
	*new { | function |
		Dialog.openPanel({ | path |
			function.(path);
		})
	}
}

+ Function {
	doPath {
		Dialog.openPanel({ | path |
			this.(path);
		})
	}
}