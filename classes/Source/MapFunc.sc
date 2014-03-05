/*
An algorithm, optionally with some state, which responds to the value message by 
processing the arguments sent together with that argument and optionally sending 
the results to a listener objects as a message array (=listener.perform(*message)=). 

This is used in place of a function as a Notification's action by Source.  
It serves for example to map the value sent by a Source to the range 
appropriate for the listener. 

IZ Tue, Mar  4 2014, 14:09 EET
*/

MapFunc {

    var <>source;
    var <>listener;
    var <>mapper;

    *new { | source, listener, mapper |
        ^this.newCopyArgs(source, listener, mapper);
    }

    valueArray { | ... args |
        listener.perform(*mapper.(*args));
    }
    /* Note: Although practically all of the functionality of a mapper can be 
        encapsulated in the func, it may be simpler and more efficient
        to define subclasses that specialize in behaviours such as setting 
        the parameter of a Node: 
        value ... source.set(message, mapper.map(args[0]))
        See examples started below.
    */

    asMapFunc { | argSource, argListener |
        /* Store source and listener.
            Called by Object:src.  See Source class.
        */
        source = argSource;
        listener = argListener;
    }

    asMapper { | argSource, argListener |
        // sent by Object.src, to create mapper and install source and listener 
        source = argSource;
        listener = argListener;
    }
}

MapSet : MapFunc {
    var parameter;

    *new { | parameter, spec |
        ^this.newCopyArgs(nil, nil, spec.asSpec, parameter);
    }

    valueArray { | val |
        listener.set(parameter, mapper.map(*val));
    }
}

UnmapSet : MapSet {

    valueArray { | val |
        listener.set(parameter, mapper.unmap(*val));
    }
}

BimapSet : MapSet {
    var unmapper;
    /* the unmapper takes the input value and unmaps it to range 0-1, 
        then the mapper maps it to the desired range. 
        For example, if the input range is 10-300 and you want to map
        it to an output range 600-700, then use 
        [10, 300].asSpec for the unmapper
        and [600, 700].asSpec for the mapper
    */


    *new { | parameter, mapSpec, unmapSpec |
        ^this.newCopyArgs(nil, nil, mapSpec.asSpec, parameter, unmapSpec.asSpec);
    }

    valueArray { | val |
        listener.set(parameter, mapper.map(unmapper.unmap(*val)));
    }
}

Args2Message : MapFunc {
    
    valueArray { | ... args |
        listener.perform(*mapper.collect(_.(*args)))
    }
}

SelectFunc : MapFunc {

}

RejectFunc : MapFunc {

}