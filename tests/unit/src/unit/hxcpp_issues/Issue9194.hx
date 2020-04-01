package unit.hxcpp_issues;

import cpp.*;

class Issue9194 extends Test {

	@:analyzer(no_optimize)
	function test() {
		var buffer: RawPointer<cpp.Void> = null;
		var floatBuffer: RawPointer<Float32> = cast buffer;
		// generates incorrect: float* floatBuffer = buffer
		// the lack of native casting means the compiler throws an error here

		var buffer: Star<cpp.Void> = null;
		var floatBuffer: Star<Float32> = cast buffer;
		// generates correct: float* floatBuffer = ( (float*) buffer ) 
	}

}
