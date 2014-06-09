/* create a patterns and its stream in one step from an object 

Wed, Mar  5 2014, 16:20 EET: Since PatternTask takes patterns, 
no longer needed to get the Stream here.
(It is more convenient to work with the pattern, to be able to restart it.)

Maybe add another set of shortcuts with prefix sp : 

array.sprandi becomes: Pub(Prand(array, inf)) etc. 

*/

+ Object {
	pn { | repeats = inf | ^Pn(this, repeats); }
}

+ SequenceableCollection {
    pseq { | repeats = inf | ^Pseq(this, repeats); }
    pseqi { | repeats = inf | ^this.pseq(repeats); } // shortcut/synonym
    pseqn { | repeats = 1 | ^this.pseq(repeats); } // shortcut/synonym
    pseq1 { | repeats = 1 | ^this.pseq(repeats); } // shortcut/synonym

    pser { | repeats = inf | ^Pser(this, repeats); }
    pseri { | repeats = inf | ^this.pser(repeats); }
    psern { | repeats = 1 | ^this.pser(repeats); }
    pser1 { | repeats = 1 | ^this.pser(repeats); }

    prand { | repeats = inf | ^Prand(this, repeats); }
    prandi { | repeats = inf | ^this.prand(repeats); }
    prandn { | repeats = 1 | ^this.prand(repeats); }
    prand1 { | repeats = 1 | ^this.prand(repeats); }
	pwhite { | lo = 0 hi = 7 repeats = inf | ^Pwhite(lo, hi, repeats) }
	pbrown { | lo = 0 hi = 7 step = 1 repeats = inf | ^Pbrown(lo, hi, step, repeats) }
}

+ Function {
    pfunc {
        ^Pfunc(this)
    }
    pfuncn { | repeats = 1 |
        ^Pfuncn(this, repeats)
    }

    pcollect { | pattern |
        ^Pcollect(this, pattern)
    } 
    pselect { | pattern |
        ^Pselect(this, pattern)
    }

    preject { | pattern |
        ^Preject(this, pattern)
    }
}
