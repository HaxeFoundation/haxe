package flash.sampler;

class Api {

	public function clearSamples() {
		untyped __global__["flash.sampler.clearSamples"]();
	}

	public function getGetterInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getGetterInvocationCount"](obj,qname);
	}

	public function getSetterInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getSetterInvocationCount"](obj,qname);
	}

	public function getInvocationCount( obj : Dynamic, qname : flash.utils.QName ) : Float {
		return untyped __global__["flash.sampler.getInvocationCount"](obj,qname);
	}

	public function getMemberNames( obj : Dynamic, ?instanceNames : Bool ) : Dynamic<flash.utils.QName> {
		return untyped __global__["flash.sampler.getMemberNames"](obj,instanceNames);
	}

	public function getSampleCount() : Float {
		return untyped __global__["flash.sampler.getSampleCount"]();
	}

	public function getSamples() : Dynamic<flash.sampler.Sample> {
		return untyped __global__["flash.sampler.getSamples"]();
	}

	public function getSize( obj : Dynamic ) : Float {
		return untyped __global__["flash.sampler.getSize"](obj);
	}

	public function isGetterSetter( obj : Dynamic, qname : flash.utils.QName ) : Bool {
		return untyped __global__["flash.sampler.isGetterSetter"](obj,qname);
	}

	public function pauseSampling() {
		untyped __global__["flash.sampler.pauseSampling"]();
	}

	public function startSampling() {
		untyped __global__["flash.sampler.startSampling"]();
	}

	public function stopSampling() {
		untyped __global__["flash.sampler.stopSampling"]();
	}

}