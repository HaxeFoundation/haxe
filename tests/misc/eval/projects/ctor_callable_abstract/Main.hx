@:callable abstract ContextProvider(Void -> Void) {}

@:build(Macro.build())
class Base {}

class Child extends Base {}

function main() {
	new Child();
}
