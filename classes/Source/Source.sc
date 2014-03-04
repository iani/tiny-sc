/* 
Encapsulate any source that emits values upon to which many different objects can be interested.

Such can be: 
- a routine that gets values from a stream
- an OSCFunc or a MIDIFunc
- A GUI widget. 

To create source functions from objects that can generate streams of values, use 
aSymbol.source(generator, message);

IZ Tue, Mar  4 2014, 01:01 EET
*/

Source {
    classvar registered; // do not necessarily register all sources
    classvar <>pollRate = 0.1;

    var <>message;
    var <source; // routine, responder, view, (other?)
    // No more state. Keep it simple!

    *registered { ^registered ?? { registered = IdentityDictionary() } }

    *new { | source, message = \value |
        ^this.newCopyArgs(message).init(source);
    }

    init { | argSource |
        source = argSource.asSource(this);
    }

    /* Either redefine start, stop, reset for all relevant objects,
        or store the custom messages in a dictionary and translate 
        each time.  The second solution seems unnecessarily complex.
        If start, stop, reset cause conflicts, 
        then use startSource, stopSource, resetSource.
    */
    start {
        source.start;
    }

    stop {
        source.stop;
    }

    reset {
        source.reset;
    }
}

+ Object {
    asSource { | source |
        source = source ?? { Source.newCopyArgs(\value) };
        ^r {
            loop {
                source.changed(\value, this.(source), source);
                source.pollRate.wait;
            };
        }
    }
}

+ AbstractResponderFunc {
    
    asSource { | source |
    source = source ?? { Source.newCopyArgs(\value) };
    this.prFunc = { | ... args |
            source.changed(\value, *(args add: this)) 
        };
    }
}

+ View {
    asSource { | source |
        source = source ?? { Source.newCopyArgs(\value) };
        this.action = {
            source.changed(\value, this.value, source)
        }
    }
}

+ SequenceableCollection {
    asSource { | source |
        var stream;
        stream = Pseq(this, inf).asStream;
        source = source ?? { Source.newCopyArgs(\value) };
        ^r {
            loop {
                source.changed(\value, stream.next, source);
                source.pollRate.wait;
            }
        }
    }
}

/* NEEDS RETHINKING?
Shortcut for creating Source instances.
OSCFunc and MIDIFunc instances are passed an function that broadcasts.
Arrays are treated as Pseqs with infinite repeat and polled at pollRate.
Functions create Pfunc instances polled at pollRate.
Associations create a stream of values and a stream of d-times and polled at d-times. 

The result is always a function that, upon computing or receiving a value,
broadcasts this value with: source.changed(message, value, source);

*/
+ Symbol {
    source { | generator, message = \value |
        
    }
}
