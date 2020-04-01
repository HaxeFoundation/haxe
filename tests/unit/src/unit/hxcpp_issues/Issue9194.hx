package unit.hxcpp_issues;


class Issue9194 extends Test {

	@:analyzer(no_optimize)
	function test() {
		#if (cpp && !cppia)
		// will fail during C++ compile
		var buffer: cpp.RawPointer<cpp.Void> = null;
		var floatBuffer: cpp.RawPointer<cpp.Float32> = cast buffer;
		// generates incorrect: float* floatBuffer = buffer
		// the lack of native casting means the compiler throws an error here

		var buffer: cpp.Star<cpp.Void> = null;
		var floatBuffer: cpp.Star<cpp.Float32> = cast buffer;
		// generates correct: float* floatBuffer = ( (float*) buffer ) 
		#end

		// empty test to keep the test-runner happy
		t(true);
	}

}
