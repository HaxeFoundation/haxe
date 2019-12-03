package asys.uv;

import asys.uv.UVErrorType;

/**
	LibUV errors wrapper.
**/
class UVError extends haxe.Error<UVErrorType> {
	override function get_message():String {
		return data;
	}
}
