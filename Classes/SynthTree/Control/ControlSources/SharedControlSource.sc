/* 

================ SharedControlSource ================

Fri, Jun 13 2014, 10:11 EEST
*/


SharedControlSource {

}

/*
** TODO Control Sources (+MultiControl: Single control source)
:PROPERTIES:
:DATE:     <2014-06-13 Fri 09:24>
:END:

Simplify the way in which diverse controls are added to a MultiControl instance.
Possibly make MultiControl a base Class - not an IdentityDictionary.

*** two types of control sources:

**** =ControlSource= unnamed, not-shared control sources

- added to a synth's parameter by object:

#+BEGIN_EXAMPLE
{ function } +>.paramname \synthtree;

number +>.paramname \synthtree;

buffer +>.paramname \synthtree;

`\bufname +>.paramname \synthtree;

MIDIfunc +>.paramname \synthtree;

OSCfunc +>.paramname \synthtree;

Event +>.paramname \synthree;

#+END_EXAMPLE

- when a new control source is added, the previous one is freed.

- starting and stopping or freeing of the control source is independent of the starting and stopping of the controlled parameter's synthtree.   However, there are explicit messages / operators for starting, stopping or freeing or removing of a control of a parameter.

- implementations for starting, stopping, freeing and for initializing (reconnecting) when controlled synth restarts are coded by subclassing.

**** =SharedControlSource= named, registered, shared control sources

- added to a synthtree's parameter by name:

: \controlsource +>.paramname \synthtree;

- created and registered in global register, using Registry

- Connected to listeners through =Notification=, broadcast their changes through the =changed= message mechanism.


*** instance var control:

may contain one of:
- MIDIfunc
- EventStream
- OSCfunc
- Bus
- A protean kind of broadcasting control source of yet undefined class (not yet implemented).

when setting control:

1. Disconnect previous control.
2.


*/
