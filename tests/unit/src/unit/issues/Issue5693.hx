package unit.issues;
import haxe.io.*;
class Issue5693 extends Test{
	#if !lua
	function test(){
		var original:Float = 0.012755102040816;
		var btOutput:BytesOutput = new BytesOutput();
		btOutput.writeDouble(original);

		var byteArray:Bytes = btOutput.getBytes();

		var btInput:BytesInput = new BytesInput(byteArray);
		var copyVal:Float = btInput.readDouble();

		eq(original, copyVal);
	}
	#end
}
