package cs.system.io;

@:native("System.IO.MatchType")
extern enum abstract MatchType(Int) {
	var Simple = 0;
	var Win32 = 1;
}
