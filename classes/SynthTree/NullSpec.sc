// Could define these in Nil instead. But no: 
// We want to see errors when trying to map nil (unset variable) instead of a spec!
NullSpec {
    *map { | val | ^val }
    *unmap { | val | ^val }
    *asSpec { ^this }
	*default { ^0 }
}
