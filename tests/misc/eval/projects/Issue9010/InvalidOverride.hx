class InvalidOverride extends Parent {
	static function main() {}

	override var field:String;

	override function some():String {
		return null;
	}
}

class Parent {}