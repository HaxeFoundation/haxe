function main() {
	utest.UTest.run([
		new TestStaticFields(),
		#if js
		new TestJsPromise(),
		#end
	]);
}