package flash.sampler;

extern class Api {

	public inline static function clearSamples() : Void {
		untyped __global__["flash.sampler.clearSamples"]();
	}

	public inline static function getGetterInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getGetterInvocationCount"](obj,qname);
	}

	public inline static function getSetterInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getSetterInvocationCount"](obj,qname);
	}

	public inline static function getInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getInvocationCount"](obj,qname);
	}

	public inline static function getMemberNames( obj : Dynamic, instanceNames : Bool = false ) : Dynamic<flash.utils.QName> {
		return untyped __global__["flash.sampler.getMemberNames"](obj,instanceNames);
	}

	public inline static function getSampleCount() : Float {
		return untyped __global__["flash.sampler.getSampleCount"]();
	}

	public inline static function getSamples() : Array<flash.sampler.Sample> {
		return untyped __foreach__(__global__["flash.sampler.getSamples"]());
	}

	public inline static function getSize( obj : Dynamic ) : Float {
		return untyped __global__["flash.sampler.getSize"](obj);
	}

	public inline static function isGetterSetter( obj : Dynamic, qname : flash.utils.QName ) : Bool {
		return untyped __global__["flash.sampler.isGetterSetter"](obj,qname);
	}

	public inline static function pauseSampling() : Void {
		untyped __global__["flash.sampler.pauseSampling"]();
	}

	public inline static function startSampling() : Void {
		untyped __global__["flash.sampler.startSampling"]();
	}

	public inline static function stopSampling() : Void {
		untyped __global__["flash.sampler.stopSampling"]();
	}

	public inline static function getLexicalScopes( fun : Dynamic ) : Array<Dynamic> {
		return untyped __global__["flash.sampler.getLexicalScopes"](fun);
	}

	public inline static function getMasterString( s : String ) : String {
		return untyped __global__["flash.sampler.getMasterString"](s);
	}

	public inline static function getSavedThis( fun : Dynamic ) : Dynamic {
		return untyped __global__["flash.sampler.getSavedThis"](fun);
	}

	public inline static function sampleInternalAllocs( b : Bool ) : Void {
		untyped __global__["flash.sampler.sampleInternalAllocs"](b);
	}

	public inline static function setSamplerCallback( callb : Dynamic ) : Void {
		untyped __global__["flash.sampler.setSamplerCallback"](callb);
	}

}