package flash.sampler;

extern class ClassFactory {
	function new() : Void;
	static var DeleteObjectSampleClass(default,never) : Class<Dynamic>;
	static var NewObjectSampleClass(default,never) : Class<Dynamic>;
	static var SampleClass(default,never) : Class<Dynamic>;
	static var StackFrameClass(default,never) : Class<Dynamic>;
}
