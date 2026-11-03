@:nativeGen
interface ITest {
	public function foo():Void;
}

@:nativeGen
class TestImpl implements ITest {
	public function foo() {}
}

function main() {}
