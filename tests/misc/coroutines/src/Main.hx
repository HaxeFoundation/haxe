function main() {
	utest.UTest.run([
		new TestBasic(),
		new TestTricky(),
		new TestControlFlow(),
		new TestGenerator(),
		#if js
		new TestJsPromise(),
		#end
	]);
}