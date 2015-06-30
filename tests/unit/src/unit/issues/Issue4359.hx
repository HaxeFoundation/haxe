package unit.issues;

private extern class E {
	@:require(false) static public inline var x = 1;
}


class Issue4359 extends Test { }