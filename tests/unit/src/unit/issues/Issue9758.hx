package unit.issues;

private abstract Resolver(String) {
	public function new(value:String) {
		this = value;
	}

	@:op(a.b) function resolveRead(name:String) {
		return new Resolver('$this.$name');
	}

	@:op(a.b) function resolveWrite(name:String, value:Resolver) {
		return new Resolver('$this.$name = $value');
	}

	@:op(a * b) function mul(rhs:Resolver) {
		return new Resolver('$this * ${rhs.getString()}');
	}

	@:op(a -= b) function subAssign(rhs:Resolver) {
		return new Resolver('$this -= ${rhs.getString()}');
	}

	public function getString() {
		return this;
	}

	@:from static function fromString(s:String) {
		return new Resolver(s);
	}
}

class Issue9758 extends unit.Test {
	function test() {
		var resolver = new Resolver("resolver");
		resolver = resolver.readme;
		eq("resolver.readme", resolver.getString());

		var resolver = new Resolver("resolver");
		resolver = resolver.writeMe = "writeMeValue";
		eq("resolver.writeMe = writeMeValue", resolver.getString());

		var resolver = new Resolver("resolver");
		resolver = (resolver.readAndWriteMe = resolver.readAndWriteMe * "readAndWriteMeValue");
		eq("resolver.readAndWriteMe = resolver.readAndWriteMe * readAndWriteMeValue", resolver.getString());

		var resolver = new Resolver("resolver");
		resolver = (resolver.readAndWriteMe *= "readAndWriteMeValue");
		eq("resolver.readAndWriteMe = resolver.readAndWriteMe * readAndWriteMeValue", resolver.getString());

		var resolver = new Resolver("resolver");
		resolver = (resolver.readAndWriteMe -= "readAndWriteMeValue");
		eq("resolver.readAndWriteMe -= readAndWriteMeValue", resolver.getString());
	}
}