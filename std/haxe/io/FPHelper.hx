package haxe.io;

/**
	Helper that converts between floating point and binary representation.
	Always works in low-endian encoding.
**/
class FPHelper {

	#if !(java || cs)
	static var i64tmp = Int64.ofInt(0);
	#end
	
	#if neko
		#if neko_v21
		static var helper = neko.NativeArray.alloc(2);
		#else
		static var helperf = neko.NativeString.alloc(4);
		static var helperd = neko.NativeString.alloc(8);
		static var _float_of_bytes = neko.Lib.load("std","float_of_bytes",2);
		static var _double_of_bytes = neko.Lib.load("std","double_of_bytes",2);
		static var _float_bytes = neko.Lib.load("std","float_bytes",2);
		static var _double_bytes = neko.Lib.load("std","double_bytes",2);
		#end
	#elseif cpp
		static var helper = haxe.io.Bytes.alloc(8);
		static var _float_of_bytes = cpp.Lib.load("std","float_of_bytes",2);
		static var _double_of_bytes = cpp.Lib.load("std","double_of_bytes",2);
		static var _float_bytes = cpp.Lib.load("std","float_bytes",2);
		static var _double_bytes = cpp.Lib.load("std","double_bytes",2);
	#elseif cs
		static var helper = new cs.NativeArray<cs.types.UInt8>(8);
	#elseif java
		static var helper = {
			var h = java.nio.ByteBuffer.allocateDirect(8);
			h.order(java.nio.ByteOrder.LITTLE_ENDIAN);
			h;
		}
	#elseif flash
		static var helper = {
			var b = new flash.utils.ByteArray();
			b.endian = flash.utils.Endian.LITTLE_ENDIAN;
			b;
		}
	#elseif php
		static var isLittleEndian : Bool = untyped __call__('unpack','S','\x01\x00')[1] == 1;
	#else
		static inline var LN2 = 0.6931471805599453; // Math.log(2)
	#end

	#if neko_v21 inline #end
	public static function i32ToFloat( i : Int ) : Float {
		#if neko
			#if neko_v21
			return untyped $itof(i);
			#else
			var helper = helperf;
			untyped $sset(helper,0,i&0xFF);
			untyped $sset(helper,1,(i>>8)&0xFF);
			untyped $sset(helper,2,(i>>16)&0xFF);
			untyped $sset(helper,3,i>>>24);
			return _float_of_bytes(helper,false);
			#end
		#elseif cpp
			// TODO : more direct version
			var helper = helper;
			helper.set(0,i);
			helper.set(1,i>>8);
			helper.set(2,i>>16);
			helper.set(3,i>>>24);
			return _float_of_bytes(helper.getData(),false);
		#elseif cs
			if( cs.system.BitConverter.IsLittleEndian )
			{
				helper[0] = i;
				helper[1] = i>>8;
				helper[2] = i>>16;
				helper[3] = i>>>24;
			} else {
				helper[0] = i>>>24;
				helper[1] = i>>16;
				helper[2] = i>>8;
				helper[3] = i;
			}
			return cs.system.BitConverter.ToSingle(helper, 0);	
		#elseif java
			var helper = helper;
			helper.putInt(0, i);
			return helper.getFloat(0);
		#elseif php
			return untyped  __call__('unpack', 'f', __call__('pack', 'l', i))[1];
		#elseif flash
			var helper = helper;
			helper.position = 0;
			helper.writeUnsignedInt(i);
			helper.position = 0;
			return helper.readFloat();
		#else
			var sign = 1 - ((i >>> 31) << 1);
			var exp = (i >>> 23) & 0xFF;
			var sig = i & 0x7FFFFF;
			if( sig == 0 && exp == 0 )
				return 0.0;
			return sign*(1 + Math.pow(2, -23)*sig) * Math.pow(2, exp-127);
		#end
	}
	
	#if neko_v21 inline #end
	public static function floatToI32( f : Float ) : Int {
		#if neko
			#if neko_v21
			return untyped $ftoi(f);
			#else
			var r = _float_bytes(f,false);
			return untyped $sget(r,0) | ($sget(r,1)<<8) | ($sget(r,2)<<16) | ($sget(r,3)<<24);
			#end
		#elseif cpp
			// TODO : no allocation version
			var r = haxe.io.Bytes.ofData(_float_bytes(f,false));
			return r.getI32(0);
		#elseif cs
			// TODO : is this bytes allocation eliminated by JIT ? If not can we do otherwise ?
			var bytes = cs.system.BitConverter.GetBytes(cast(f, Single));
			return bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24);
		#elseif java
			var helper = helper;
			helper.putFloat(0, f);
			return helper.getInt(0);
		#elseif flash
			var helper = helper;
			helper.position = 0;
			helper.writeFloat(f);
			helper.position = 0;
			return helper.readUnsignedInt();
		#elseif php
			return untyped __call__('unpack','l',__call__('pack', 'f', f))[1];
		#else
			if( f == 0 ) return 0;
			var af = f < 0 ? -f : f;
			var exp = Math.round(Math.log(af) / LN2);
			if( exp < -127 ) exp = -127 else if( exp > 128 ) exp = 128;
			var sig = Math.round(af / Math.pow(2, exp) * 0x800000) & 0x7FFFFF;
			return (f < 0 ? 0x80000000 : 0) | ((exp + 127) << 23) | sig;
		#end
	}
	
	#if neko_v21 inline #end
	public static function i64ToDouble( low : Int, high : Int ) : Float {
		#if neko
			#if neko_v21
			return untyped $itod(low,high);
			#else
			var helper = helperd;
			untyped $sset(helper,0,low&0xFF);
			untyped $sset(helper,1,(low>>8)&0xFF);
			untyped $sset(helper,2,(low>>16)&0xFF);
			untyped $sset(helper,3,low>>>24);
			untyped $sset(helper,4,high&0xFF);
			untyped $sset(helper,5,(high>>8)&0xFF);
			untyped $sset(helper,6,(high>>16)&0xFF);
			untyped $sset(helper,7,high>>>24);
			return _double_of_bytes(helper,false);
			#end
		#elseif cpp
			// TODO : more direct version
			var helper = helper;
			helper.set(0,low);
			helper.set(1,low>>8);
			helper.set(2,low>>16);
			helper.set(3,low>>>24);
			helper.set(4,high);
			helper.set(5,high>>8);
			helper.set(6,high>>16);
			helper.set(7,high>>>24);
			return _double_of_bytes(helper.getData(),false);		
		#elseif java
			var helper = helper;
			helper.putInt(0, low);
			helper.putInt(4, high);
			return helper.getDouble(0);
		#elseif flash
			var helper = helper;
			helper.position = 0;
			helper.writeUnsignedInt(low);
			helper.writeUnsignedInt(high);
			helper.position = 0;
			return helper.readDouble();
		#elseif php
			return untyped  __call__('unpack', 'd', __call__('pack', 'ii', isLittleEndian ? low : high, isLittleEndian ? high : low))[1];
		#else
			var sign = 1 - ((high >>> 31) << 1);
			var exp = ((high >> 20) & 0x7FF) - 1023;
			var sig = (high&0xFFFFF) * 4294967296. + (low>>>31) * 2147483648. + (low&0x7FFFFFFF);
			if( sig == 0 && exp == -1023 )
				return 0.0;
			return sign*(1.0 + Math.pow(2, -52)*sig) * Math.pow(2, exp);
		#end
	}
	
	/**
		Returns an Int64 representing the bytes representation of the double precision IEEE float value.
		WARNING : for performance reason, the same Int64 value might be reused every time. Copy its low/high values before calling again.
	**/
	#if neko_v21 inline #end
	public static function doubleToI64( v : Float ) : Int64 {
		#if neko
			#if neko_v21
			var helper = helper, i64 = i64tmp;
			untyped $dtoi(v,helper);
			@:privateAccess {
				i64.low = helper[0];
				i64.high = helper[1];
			}
			return i64;
			#else
			var r = _double_bytes(v,false), i64 = i64tmp;
			@:privateAccess {
				i64.low = untyped $sget(r,0) | ($sget(r,1)<<8) | ($sget(r,2)<<16) | ($sget(r,3)<<24);
				i64.high =  untyped $sget(r,4) | ($sget(r,5)<<8) | ($sget(r,6)<<16) | ($sget(r,7)<<24);
			}
			return i64;
			#end
		#elseif cpp
			// TODO : no allocation version
			var r = haxe.io.Bytes.ofData(_double_bytes(v,false)), i64 = i64tmp;
			@:privateAccess {
				i64.low = r.getI32(0);
				i64.high = r.getI32(4);
			}
			return i64;
		#elseif java
			var helper = helper;
			helper.putDouble(0, v);
			return helper.getLong(0);
		#elseif cs
			// TODO : is this bytes allocation eliminated by JIT ? If not can we do otherwise ?
			var bytes = cs.system.BitConverter.GetBytes(v);
			return haxe.Int64.make(bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24), bytes[4] | (bytes[5] << 8) | (bytes[6] << 16) | (bytes[7] << 24));	
		#elseif flash
			var helper = helper;
			helper.position = 0;
			helper.writeDouble(v);
			helper.position = 0;
			var i64 = i64tmp;
			@:privateAccess {
				i64.low = helper.readUnsignedInt();
				i64.high = helper.readUnsignedInt();
			}
			return i64;
		#elseif php	
			var a = untyped __call__('unpack',isLittleEndian ? 'VV' : 'NN',__call__('pack', 'd', v));
			var i64 = i64tmp;
			@:privateAccess {
				i64.low = a[isLittleEndian ? 1 : 2];
				i64.high = a[isLittleEndian ? 2 : 1];
			}
			return i64;
		#else
			var i64 = i64tmp;
			if( v == 0 ) {
				@:privateAccess {
					i64.low = 0;
					i64.high = 0;
				}
			} else {
				var av = v < 0 ? -v : v;
				var exp = Math.round(Math.log(av) / LN2);
				var sig = Math.fround((av / Math.pow(2, exp)) * 4503599627370496.); // 2^52
				var sig_l = Std.int(sig);
				var sig_h = Std.int(sig / 4294967296.0);
				@:privateAccess {
					i64.low = sig_l;
					i64.high = (v < 0 ? 0x80000000 : 0) | ((exp + 1023) << 20) | sig_h; 
				}
			}
			return i64;
		#end
	}

}