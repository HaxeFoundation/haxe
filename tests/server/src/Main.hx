import utest.Assert.*;
import TestStrings.*;
import AsyncMacro.async;

class NoModification extends HaxeServerTestCase {
	public function test() {
		async({
			vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
			haxe("-main", "HelloWorld.hx", "--no-output", "-js", "no.js");
			haxe("-main", "HelloWorld.hx", "--no-output", "-js", "no.js");
			isTrue(context.hasMessage(TestStrings.reusing("HelloWorld")));
			resetMessages();
			haxe("-main", "HelloWorld.hx", "--no-output", "-js", "no.js");
			isTrue(context.hasMessage(TestStrings.reusing("HelloWorld")));
		});
	}
}

class Modification extends HaxeServerTestCase {
	public function test() {
		async({
			vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
			haxe("-main", "HelloWorld.hx", "--no-output", "-js", "no.js");
			vfs.touchFile("HelloWorld.hx");
			haxe("-main", "HelloWorld.hx", "--no-output", "-js", "no.js");
			isTrue(hasMessage(skipping("HelloWorld")));
			isTrue(hasMessage(notCachedModified("HelloWorld")));
		});
	}
}

class Dependency extends HaxeServerTestCase {
	public function test() {
		async({
			vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
			vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
			haxe("-main", "WithDependency.hx", "--no-output", "-js", "no.js");
			vfs.touchFile("Dependency.hx");
			haxe("-main", "WithDependency.hx", "--no-output", "-js", "no.js");
			isTrue(hasMessage(skipping("WithDependency")));
			isTrue(hasMessage(skippingDep("WithDependency", "Dependency")));
			isTrue(hasMessage(notCachedModified("Dependency")));
		});
	}
}

class Main {
	static public function main() {
		Vfs.removeDir("test/cases");
		var runner = new utest.Runner();
		runner.addCase(new NoModification());
		runner.addCase(new Modification());
		runner.addCase(new Dependency());
		utest.ui.Report.create(runner);
		runner.run();
	}
}