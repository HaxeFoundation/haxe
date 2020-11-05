package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import haxe.exceptions.NotSupportedException;
import java.nio.file.attribute.PosixFilePermission;
import java.util.EnumSet;
import java.util.Set;

private typedef NativePermissions = Set<PosixFilePermission>;

@:coreApi
abstract FilePermissions(NativePermissions) to NativePermissions {
	static public inline function ignoresSpecialBit():Bool {
		return true;
	}

	static inline function empty():NativePermissions {
		return new FilePermissions(EnumSet.noneOf((cast PosixFilePermission:java.lang.Class<PosixFilePermission>)));
	}

	static public function octal(s:Int, u:Int, g:Int, o:Int):FilePermissions {
		var set = empty();
		switch u {
			case 0:
			case 1: set.add(OWNER_EXECUTE);
			case 2: set.add(OWNER_WRITE);
			case 3: set.add(OWNER_EXECUTE); set.add(OWNER_WRITE);
			case 4: set.add(OWNER_READ);
			case 5: set.add(OWNER_EXECUTE); set.add(OWNER_READ);
			case 6: set.add(OWNER_WRITE); set.add(OWNER_READ);
			case 7: set.add(OWNER_EXECUTE); set.add(OWNER_WRITE); set.add(OWNER_READ);
			case _: throw new ArgumentException('u');
		}
		switch g {
			case 0:
			case 1: set.add(GROUP_EXECUTE);
			case 2: set.add(GROUP_WRITE);
			case 3: set.add(GROUP_EXECUTE); set.add(GROUP_WRITE);
			case 4: set.add(GROUP_READ);
			case 5: set.add(GROUP_EXECUTE); set.add(GROUP_READ);
			case 6: set.add(GROUP_WRITE); set.add(GROUP_READ);
			case 7: set.add(GROUP_EXECUTE); set.add(GROUP_WRITE); set.add(GROUP_READ);
			case _: throw new ArgumentException('g');
		}
		switch o {
			case 0:
			case 1: set.add(OTHERS_EXECUTE);
			case 2: set.add(OTHERS_WRITE);
			case 3: set.add(OTHERS_EXECUTE); set.add(OTHERS_WRITE);
			case 4: set.add(OTHERS_READ);
			case 5: set.add(OTHERS_EXECUTE); set.add(OTHERS_READ);
			case 6: set.add(OTHERS_WRITE); set.add(OTHERS_READ);
			case 7: set.add(OTHERS_EXECUTE); set.add(OTHERS_WRITE); set.add(OTHERS_READ);
			case _: throw new ArgumentException('g');
		}
		return new FilePermissions(set);
	}

	@:from static inline function fromOctal(mode:Array<Int>):FilePermissions {
		if(mode.length != 4) {
			throw new ArgumentException('mode', '"mode" array should contain exactly four items');
		}
		return octal(mode[0], mode[1], mode[2], mode[3]);
	}

	@:from static function fromDecimal(dec:Int):FilePermissions {
		var set = empty();
		if(dec & (1 << 0) != 0) set.add(OTHERS_EXECUTE);
		if(dec & (1 << 1) != 0) set.add(OTHERS_WRITE);
		if(dec & (1 << 2) != 0) set.add(OTHERS_READ);
		if(dec & (1 << 3) != 0) set.add(GROUP_EXECUTE);
		if(dec & (1 << 4) != 0) set.add(GROUP_WRITE);
		if(dec & (1 << 5) != 0) set.add(GROUP_READ);
		if(dec & (1 << 6) != 0) set.add(OWNER_EXECUTE);
		if(dec & (1 << 7) != 0) set.add(OWNER_WRITE);
		if(dec & (1 << 8) != 0) set.add(OWNER_READ);
		return new FilePermissions(set);
	}

	@:to function toDecimal():Int {
		var result = 0;
		for(v in this) {
			switch v {
				case OTHERS_EXECUTE: result = result | (1 << 0);
				case OTHERS_WRITE: result = result | (1 << 1);
				case OTHERS_READ: result = result | (1 << 2);
				case GROUP_EXECUTE: result = result | (1 << 3);
				case GROUP_WRITE: result = result | (1 << 4);
				case GROUP_READ: result = result | (1 << 5);
				case OWNER_EXECUTE: result = result | (1 << 6);
				case OWNER_WRITE: result = result | (1 << 7);
				case OWNER_READ: result = result | (1 << 8);
			}
		}
		return result;
	}

	@:op(A & B) static function intersect(perm1:FilePermissions, perm2:FilePermissions):FilePermissions {
		var set1:NativePermissions = perm1;
		var set2:NativePermissions = perm2;
		var result = empty();
		var values = java.NativeArray.make(
			OTHERS_EXECUTE, OTHERS_WRITE, OTHERS_READ,
			GROUP_EXECUTE, GROUP_WRITE, GROUP_READ,
			OWNER_EXECUTE, OWNER_WRITE, OWNER_READ
		);
		for(i in 0...values.length) {
			if(set1.contains(values[i]) && set2.contains(values[i])) {
				result.add(values[i]);
			}
		}
		return new FilePermissions(result);
	}

	@:op(A | B) static function merge(perm1:FilePermissions, perm2:FilePermissions):FilePermissions {
		var result = EnumSet.copyOf(perm1);
		result.addAll(perm2);
		return new FilePermissions(result);
	}

	@:op(A == B) static function equals(perm1:Null<FilePermissions>, perm2:Null<FilePermissions>):Bool {
		var p1:Null<NativePermissions> = perm1;
		var p2:Null<NativePermissions> = perm2;
		if(p1 == p2) {
			return true;
		} else if(p1 == null || p2 == null) {
			return false;
		} else {
			return p1.equals(p2);
		}
	}

	@:op(A == B) @:commutative static inline function equalsDecimal(perm1:Null<FilePermissions>, dec:Int):Bool {
		return equals(perm1, fromDecimal(dec));
	}

	inline function new(perm:NativePermissions) {
		this = perm;
	}

	public inline function toString():String {
		return '${toDecimal()}';
	}
}