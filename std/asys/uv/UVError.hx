package asys.uv;

import haxe.PosInfos;
import asys.uv.UVErrorType;

/**
	LibUV errors wrapper.
**/
class UVError extends haxe.Error {

	public final type:UVErrorType;

	public function new(type:UVErrorType, ?p:PosInfos) {
		super(type, p);
		this.type = type;
	}
}
