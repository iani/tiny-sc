
a = "b[(abc)[ax]]".miniSteno;
a.pp;
a.inspect;

b = "(abc)[x]de(fgh[ij])".miniSteno;
b.findContainerOf(\a.chuck);

b.pp;
 b.inspect;

"a".miniSteno;
"[a]".miniSteno;

"(ab)".arlink;
"a".arlink;
Chuck.initInactive;
Chuck.inactive;



