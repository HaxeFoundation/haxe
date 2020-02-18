enum abstract A(String) {
	var A1;
	var A2;

	function f() {
		var {-1-}a:A = cast this; // hovering `a` will print Dynamic
		switch (a) {
			// no exhaustiveness check
		}
	}
}
