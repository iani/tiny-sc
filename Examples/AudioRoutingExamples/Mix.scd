/*
From a session with Konstantinos Vasilakos. 

Comments: Later, guys.  Sorry. 

Sun, Jun  1 2014, 17:22 EEST
*/

{ LPF.ar(Inp.ar, \freq.kr(500)) } => \lpf;

{ WhiteNoise.ar(0.1) } => \whitenoise;
{ GrayNoise.ar(0.1) } => \greynoise;

[\whitenoise, \greynoise] do: { | s | \lpf =< s };

{ Inp.ar } => \mix;

[\whitenoise, \greynoise] do: { | s | \mix =< s };


