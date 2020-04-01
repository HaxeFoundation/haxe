package unit.hxcpp_issues;

#if (cpp && !cppia)
import cpp.*;
#end

class Issue9194 extends Test {

	#if (cpp && !cppia)
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
	#end

}
