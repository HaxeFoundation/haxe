function main() {}

typedef Foo = {
	function get<TField>():Null<TField>;
}

abstract HaxeMemento(Foo) {
	public function getInner<TAbstract>():Null<TAbstract> {
		return this.get();
	}
}
