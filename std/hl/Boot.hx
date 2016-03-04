package hl;

import hl.types.ArrayDyn;
import hl.types.BaseType;

extern class Boot {
	@:extern public inline static function dump( v : Dynamic ) : Void {
		untyped $dump(v);
	}
}
