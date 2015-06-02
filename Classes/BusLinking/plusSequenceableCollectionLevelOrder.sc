/* Tue, Jun  2 2015, 13:11 EEST

Tests: 

[[1], [5], 3].levelOrder;

[[1, 2, 100], [5, 6, 200, 2000], 3].levelOrder;

*/

+ SequenceableCollection {
	levelOrder {
		if (this.size == 0){
			^this.species.new;
		}{
			^this.collect({ | x | (if (x.size > 0) { x[0] } { x })})
			++ this.select ({| x |  x.size > 0}).collect ({|x| x [1..]})
			.select ({|x| x.size > 0}).levelOrder;
		}
	}
}

// From ddw, See:
// http://new-supercollider-mailing-lists-forums-use-these.2681727.n2.nabble.com/removing-duplicates-inside-an-array-td4913575.html

+ Collection {
    removeDups {    // output a new collection without any duplicate values
        var result;
        result = this.species.new(this.size);
        this.do({ arg item;
            result.includes(item).not.if({ result.add(item) });
        });
        ^result
    }
}

/*
	[1, 2, 3, 2].removeDups
	// for synths, we need to keep the LAST instance
	[1, 2, 3, 2].reverse.removeDups.reverse

*/