
//: 1. Play a new EventPattern (Pbind-like) in a SynthTree
(dur: 0.1) => \epat1;
//: 2. The => operator *replaces* the source's pattern entirely:
(degree: 1) => \epat1;
//: 3. To *add* or *replace* patterns of single parameters, use +> 
(dur: 0.1) +> \epat1;
//: Replace patterns for dur and degree.
(dur: [0.1, 0.2].pseq, degree: (0..7).pseq) +> \epat1;
//: Replace pattern for dur, add pattern for amp
(dur: [0.1, 0.1, 0.2].pseq, amp: { 0.01 exprand: 0.1 }.pfunc) +> \epat1;

