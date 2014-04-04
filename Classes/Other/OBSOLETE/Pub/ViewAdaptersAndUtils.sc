/* 
Combine a View with a ControlSpec

+ Utilities for testing views quickly

IZ Wed, Mar  5 2014, 07:30 EET

*/

QSli {
    *new { | name = "slider" |
        var w, s;
        w = Window(name, Rect(0, 0, 150, 30)).front;
        w.view.layout = HLayout(s = Slider().orientation_(\horizontal));
        ^s;
    }
}

MappingView {
    var <view;
    var <>spec;

    *new { | view, spec |
        ^this.newCopyArgs(view, spec.asSpec);
    }

    makePubAction { | pub |
        view.action = {
            pub.changed(\value, spec map: view.value);
        };
		view.onClose = {
			// this notifies "objectClosed" and then disconnects:
			view.objectClosed;
		}
        /* return self */
    }
    start { | pub |
        this.makePubAction(pub);
    }
    stop { view.action = nil }
    isPlaying { ^view.action.notNil }
    set { | param, val |
        // make views work as listeners of a pub
        { view.value = val; }.defer;
    }
}

/* Note: Cannot define this in View, because it is a redirect class, and returns
platform specific classes instead.  Therefore, doing it for QT.  Other GUI
    classes can be added in a similar way */

+ QView {
    makePubAction { | pub |
        ^MappingView(this).makePubAction(pub);
    }

    map { | spec |
        ^MappingView(this, spec.asSpec); 
    }

    set { | param, val |
        // make views work as listeners of a pub
        { this.value = val; }.defer;
    }
    /*
    makePubAction { | pub | 
        this.action = { pub.changed(\value, this.value) };
        /* return self */
    }
    start { | pub |
        this.makePubAction(pub);
    }
    stop { this.action = nil }
    isPlaying { ^this.action.notNil }
    */
}