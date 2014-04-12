/* Sat, Apr 12 2014, 16:28 EEST */

SynthTree.clearEnvir;

500 *> \freq;

{ 100 rrand: 400 }.pfunc *> \freq;

\bassfoundation *> \instr;

\werkitd *> \instr;

{ 1000 rrand: 4000 }.pfunc *> \freq;

{ 0.1 exprand: 3 }.pfunc *> \leg;

0.1 *> \dur;

[0.5 pn: 5, { 2.0 exprand: 4.5 }.pfuncn(1)].prand *> \leg;

Prand([500, 1000, 1500, 2000, 5000], inf) *> \freq;

Pbrown(30, 90, 2, inf).midicps *> \freq;

0.03 *> \dur;

~st.fadeOut(20);
