package unit.issues;

class Issue5705 extends Test {
	static function foo(baseClass:BaseClass_):Int {
		return cast(baseClass, IInterface).z;
	}
}

class BaseClass_ {}

interface IInterface {
	public var z:Int;
}

class ImplementingClass extends BaseClass_ implements IInterface {
	public var z:Int;
}