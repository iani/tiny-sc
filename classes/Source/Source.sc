/* 
Encapsulate any source that emits values upon to which many different objects can be interested.

Such can be: 
- A Task that computes values or otherwise gets values from a stream.
    Note: Source is not coded for using Routines safely: Restarting it while
    its previous routine is playing, will add a second copy of the schedule task
    and cause the timing to break.
- An OSCFunc or a MIDIFunc
- A GUI widget. 
- A function, that gets evaluated to return a value each time.
- Any other object, which gets evaluated (object.value(source)) each time.


IZ Tue, Mar  4 2014, 01:01 EET
*/

Source {
    classvar registered; // do not necessarily register all sources
    classvar <>pollRate = 0.1;

    var <source; // routine, responder, view, (other?)
    var <template; // object that created the source. Used for reset.
    
    *registered { ^registered ?? { registered = IdentityDictionary() } }

    *new { | source |
        ^super.new.source_(source);
    }

    source_ { | argSource |
        var isPlaying = false;
        if (source.isPlaying) {
            source.stop;
            isPlaying = true;
        };
        template = argSource;
        source = argSource.makeSourceAction(this);
        if (isPlaying) { this.start };
    }

    asSource { /* just return yourself... */ }

    /* Either redefine start, stop, reset for all relevant objects,
        or store the custom messages in a dictionary and translate 
        each time.  The second solution seems unnecessarily complex.
        If start, stop, reset cause conflicts, 
        then use startSource, stopSource, resetSource.
    */
    start {
        source.start(this);
    }

    stop {
        source.stop(this);
    }

    reset {
        this.source = template;
    }
    isPlaying { ^source.isPlaying }
    pollRate { ^pollRate }
}

+ Task {
    start {
        if (this.streamHasEnded) { this.reset };
        this.play;
    }
}


+ Object {
    setSource { | source, action |
        // simple version, for debugging.
        this.addNotifier(source, \value, action ?? {{ | ... args | args.postln }}); 
    }

    src { | source, mapper |
        // convert source to Source instance and connect it to self
        /* mapper becomes an object that responds to .value by taking
            the arguments passed from the source, maps them or otherwise
            processes them, and finally sends a message to the listener */
        source = source.asSource;
        this.addNotifier(source, \value, mapper.asMapper(source, this));
		^source;
    }

    asSource { ^Source(this); }

    makeSourceAction { | source |
        ^PatternPlayer(this).makeSourceAction(source);
    }
    /*
    makeSourceAction { | source |
        ^Task {
            loop {
                source.changed(\value, this.(source));
                source.pollRate.wait;
            };
        }
    }
    */
}

/*
+ SequenceableCollection {
    makeSourceAction { | source |
        var stream;
        stream = Pseq(this, inf).asStream;
        ^Task {
            loop {
                source.changed(\value, stream.next);
                source.pollRate.wait;
            }
        }
    }
}
*/

+ AbstractResponderFunc {
    makeSourceAction { | source |
        this.prFunc = { | ... args | source.changed(\value, *args) }; 
        /* return self */
    }

    start { this.enable }
    stop { this.disable }
    isPlaying { ^this.enabled }
}
