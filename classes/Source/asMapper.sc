/* Shortcuts for creating mappers from various objects.

The methods below work for:

- any symbol, such as 'freq', that returns a spec with symbol.asSpec
- any array that is a valid spec specification, such as [0, 10, \lin, 0, 1] or the like.
- a ControlSpec


IZ Tue, Mar  4 2014, 15:20 EET
*/

+ Object {

    mapSet { | parameter |
        ^MapSet(this.asSpec, parameter)
    }

    unmapSet { | parameter |
        ^UnmapSet(this.asSpec, parameter)
    }

    bimapSet { | parameter, unmapSpec |
        ^BimapSet(parameter, this.asSpec, unmapSpec.asSpec);
    }

    selectFunc {

    }

    rejectFunc {

    }
}

