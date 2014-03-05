/* create a patterns and its stream in one step from an object 

Wed, Mar  5 2014, 16:20 EET: Since PatternPlayer takes patterns, 
no longer needed to get the Stream here.
(It is more convenient to work with the pattern, to be able to restart it.)

Maybe add another set of shortcuts with prefix sp : 

array.sprandi becomes: Source(Prand(array, inf)) etc. 

*/

+ SequenceableCollection {
    pseq { | repeats = 1 |
        ^Pseq(this, repeats) //.asStream;
    }

    pseqi { | repeats = 1 |
        ^this.pseq(inf);
    }

    pser { | repeats = 1 |
        ^Pser(this, repeats) //.asStream;
    }

    pseri { | repeats = 1 |
        ^this.pser(inf);
    }

    prand { | repeats = 1 |
        ^Prand(this, repeats) //.asStream;
    }

    prandi { | repeats = 1 |
        ^this.prand(inf);
    }
}

+ Function {
    pfunc {
        ^Pfunc(this) //.asStream;
    }
    pfuncn { | repeats = 1 |
        ^Pfuncn(this, repeats) //.asStream;
    }

    pcollect { | pattern |
        ^Pcollect(this, pattern) //.asStream;
    } 
    pselect { | pattern |
        ^Pselect(this, pattern) //.asStream;
    }

    preject { | pattern |
        ^Preject(this, pattern) //.asStream;
    }
}
