package flash.sampler;

class Api {

	public static function clearSamples() {
		untyped __global__["flash.sampler.clearSamples"]();
	}

	public static function getGetterInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getGetterInvocationCount"](obj,qname);
	}

	public static function getSetterInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getSetterInvocationCount"](obj,qname);
	}

	public static function getInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getInvocationCount"](obj,qname);
	}

	public static function getMemberNames( obj : Dynamic, ?instanceNames : Bool ) : Dynamic<flash.utils.QName> {
		return untyped __global__["flash.sampler.getMemberNames"](obj,instanceNames);
	}

	public static function getSampleCount() : Float {
		return untyped __global__["flash.sampler.getSampleCount"]();
	}

	public static function getSamples() : Dynamic<flash.sampler.Sample> {
		return untyped __global__["flash.sampler.getSamples"]();
	}

	public static function getSize( obj : Dynamic ) : Float {
		return untyped __global__["flash.sampler.getSize"](obj);
	}

	public static function isGetterSetter( obj : Dynamic, qname : flash.utils.QName ) : Bool {
		return untyped __global__["flash.sampler.isGetterSetter"](obj,qname);
	}

	public static function pauseSampling() {
		untyped __global__["flash.sampler.pauseSampling"]();
	}

	public static function startSampling() {
		untyped __global__["flash.sampler.startSampling"]();
	}

	public static function stopSampling() {
		untyped __global__["flash.sampler.stopSampling"]();
	}

}