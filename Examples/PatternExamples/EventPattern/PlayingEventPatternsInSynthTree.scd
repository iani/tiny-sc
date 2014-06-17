
//: 1. Play a new EventPattern (Pbind-like) in a SynthTree
(dur: 0.1) => \epat1;

//: 2. The => operator *replaces* the source's pattern entirely:
(degree: 1) => \epat1;

//: 3. To *add* or *replace* patterns of single parameters, use +> 
//: 3.1. Add pattern for dur
(dur: 0.1) +> \epat1;

//: 3.2. Replace patterns for dur and degree.
(dur: [0.1, 0.2].pseq, degree: (0..7).pseq) +> \epat1;

//: 3.3. Replace pattern for dur, add pattern for amp
(dur: [0.1, 0.1, 0.2].pseq, amp: { 0.01 exprand: 0.1 }.pfunc) +> \epat1;

//: 4. Add filter modifying the existing pattern for degree
(degree:  { ~degree + [0, 2, 5, 7, 8] }) %> \epat1;


// More examples

//: Substitute basic pattern (not filter) of for degree
(degree:  [0, 2, 3, 5].prand) +> \epat1;

//: Set patterns for dur and amp keys.
(dur: 0.12, amp: [[0.1, 0.05] pn: 5, [0.05, 0.2]].pseq) +> \epat1;