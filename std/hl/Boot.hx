package hl;

import hl.types.ArrayObj;
import hl.types.ArrayI32;
import hl.types.ArrayF64;
import hl.types.Class;

extern class Boot {
	@:extern public inline static function dump( v : Dynamic ) : Void {
		untyped $dump(v);
	}
}
