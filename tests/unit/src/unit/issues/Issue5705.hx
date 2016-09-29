package unit.issues;

class Main extends Test {
	static function foo(baseClass:BaseClass):Int {
		return cast(baseClass, IInterface).z;
	}
}

class BaseClass {}

interface IInterface {
	public var z:Int;
}

class ImplementingClass extends BaseClass implements IInterface {
	public var z:Int;
}