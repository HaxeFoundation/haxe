package cs.system.io;

@:native("System.IO.MatchCasing")
extern enum abstract MatchCasing(Int) {
	var CaseInsensitive = 2;
	var CaseSensitive = 1;
	var PlatformDefault = 0;
}
