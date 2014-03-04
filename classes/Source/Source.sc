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
    classvar all;
    classvar <>pollRate = 0.1;

    var <name;
    var <>message;
    var <action;

    *new { | name, message = \value, action |
        var instance;
        instance = this.all[name];
        if (instance.isNil) {
            instance = this.newCopyArgs(name, message, action);
            all[name] = instance;
        };
        ^instance;
    }

    *all { ^all ?? { all = IdentityDictionary() } }

    start {
        action.start;
    }

    stop {
        action.stop;
    }

    reset {
        action.reset;
    }

    // Shortcuts for creating different kinds of action funcs
    osc { | responderSpecs |
        
    }

    midi { | midiSpecs |

    }

    routine { | values, durations |
        
    }

    widget { | widget |
        // set action of widget ...
    }
}

/* Shortcut for creating Source instances.
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
