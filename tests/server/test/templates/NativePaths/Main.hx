class Main {
	static function main() {
		trace(Type.getEnumConstructs(MyEnum));
	}
}

@:native('TheirEnum')
private enum MyEnum {
	@:native('Two')
	One;
}
