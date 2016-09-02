enum ToString<T> {
	ToString<S>(s:S, f:S->String);
}

class Main1 {
    static function main () {
        var d1 = ToString(5, function (i:Int) return "" + i);
        var d2 = ToString("bar", function (i:String) return i);
        toString(d1);
        toString(d2);
	}

	public static function toString<T>(x:ToString<T>) {
		switch (x) {
			case ToString(a,f):
				// at this point, we don't know the type of a and the parameter type of f, but we know that they are the same type and cannot be used with other types.
				trace(f([1,2])); // shouldn't work here
		}
	}
}