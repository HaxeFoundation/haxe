package unit.issues;

typedef Constraint = { }

typedef T1<T:Constraint> = { }

typedef T2<T:Constraint> = {
	x:T1<T>
}

class Issue3545 extends Test {
	// if it compiles it works
	function test() { }
}