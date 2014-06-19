/*

Binding any sliders to parameters. 

One slider may control many synthrees.
Many sliders may control the same parameter on the same synthtree.

Thu, Jun 19 2014, 18:55 EEST
*/

{ WhiteNoise.ar(0.1) } => \sound1;

//:

\window1.slider(\amp) => \sound1; // default control parameter is \amp

//: Order of creation of sliders and synths is free

\window1.slider(\amp) => \sound2; // bind same slider to different synthtree

//:

{ SinOsc.ar(\freq.kr(400), 0, 0.1) } => \sound2;

//: Bind new slider to freq of second synthtree

\window1.slider(\freq) =>.freq \sound2; // bind same slider to different synthtree

//: Create second slider for freq of sound2 on different window

\window2.slider(\just_another_freq) =>.freq \sound2;

//: Create independent control for amp freq of sound2

\window2.slider(\sinamp) => \sound2;

//: And another amp control, with slider of same name, on different window

\aThirdWindow.slider(\amp) => \sound2;

/* 
Typing 'space' on a slider toggles the synthtree controlled by that slider on-off. 

Example: 

- Type 'space' on amp slider of window1:gui to switch both sound1 and sound2 on and off.
- Type 'space' on sinamp slider of window2Lgui to switch only sound2 on and off.

Note: Sliders whose controls are shared will be updated when any one of their
controlled parameters is set.

*/