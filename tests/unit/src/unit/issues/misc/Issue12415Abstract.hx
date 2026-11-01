package unit.issues.misc;

import haxe.Int64;

@:fromNull
abstract Issue12415Abstract(Int64) from Int64 to Int64 {
	public inline function isNull() {
		if (abstract == null) return true;
		return haxe.Int64.isZero(this);
	}
}
