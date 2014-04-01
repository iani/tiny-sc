/* Shortcuts for creating mappers from various objects.

The methods below work for:

- any symbol, such as 'freq', that returns a spec with symbol.asSpec
- any array that is a valid spec specification, such as [0, 10, \lin, 0, 1] or the like.
- a ControlSpec


IZ Tue, Mar  4 2014, 15:20 EET
*/

+ Object {

	asMapper { | source, listener |
		^MapSet(listener.sourceMsg, this.asSpec).asMapper(source, listener);
	}

	sourceMsg { ^\set }

    mapSet { | parameter |
        ^MapSet(parameter, this.asSpec)
    }

    unmapSet { | parameter |
        ^UnmapSet(parameter, this.asSpec)
    }

    bimapSet { | parameter, unmapSpec |
        ^BimapSet(parameter, this.asSpec, unmapSpec.asSpec);
    }

    selectFunc {
        // only send value to listener when a condition succeeds

    }

    rejectFunc {
        // only send value to listener when a condition fails

    }
}

+ Symbol {
    mapSet { | spec |
        ^MapSet(this, (spec ? this).asSpec)
    }

    asMapper { | source, listener |
        ^this.mapSet.asMapper(source, listener);
        //  { "still debugging".postln; }
    }
}