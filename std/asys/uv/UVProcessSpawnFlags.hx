package asys.uv;

enum abstract UVProcessSpawnFlags(Int) {
	var None = 0;
	var SetUid = 1 << 0;
	var SetGid = 1 << 1;
	var WindowsVerbatimArguments = 1 << 2;
	var Detached = 1 << 3;
	var WindowsHide = 1 << 4;

	function new(raw:Int)
		this = raw;

	inline function get_raw():Int return this;

	@:op(A | B)
	inline function join(other:UVProcessSpawnFlags) return new UVProcessSpawnFlags(this | other.get_raw());
}
