import haxe.coro.Coroutine;

interface MyUnrestrictedInterface {}

class MyUnrestrictedClass implements MyUnrestrictedInterface {
	public function new() {}
}

@:coroutine.scope(restrictedSuspension)
typedef MyRestrictedTypedef = MyUnrestrictedClass;

@:coroutine.scope(restrictedSuspension)
class MyRestrictedChildClass extends MyUnrestrictedClass {}

@:coroutine function passToUnrestrictedClass(_:MyUnrestrictedClass) { }
@:coroutine function passToUnrestrictedInterface(_:MyUnrestrictedInterface) { }
@:coroutine function passToDynamic(_:Dynamic) { }

// failures here

@:coroutine function typedefToClass(scope:MyRestrictedTypedef) {
	passToUnrestrictedClass(scope);
}

@:coroutine function classToClass(scope:MyRestrictedChildClass) {
	passToUnrestrictedClass(scope);
}

@:coroutine function classToInterface(scope:MyRestrictedChildClass) {
	passToUnrestrictedInterface(scope);
}

@:coroutine function classToDynamic(scope:MyRestrictedChildClass) {
	passToDynamic(scope);
}

function main() {

}