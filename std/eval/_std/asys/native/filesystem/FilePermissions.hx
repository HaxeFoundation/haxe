package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import eval.luv.File;

private typedef NativePermissions = Array<FileMode>;

@:coreApi
abstract FilePermissions(NativePermissions) to NativePermissions {
	static public inline function ignoresSpecialBit():Bool {
		return false;
	}

	static public function octal(s:Int, u:Int, g:Int, o:Int):FilePermissions {
		var set = switch u {
			case 0: [];
			case 1: [IXUSR];
			case 2: [IWUSR];
			case 3: [IXUSR, IWUSR];
			case 4: [IRUSR];
			case 5: [IXUSR, IRUSR];
			case 6: [IWUSR, IRUSR];
			case 7: [IRWXU];
			case _: throw new ArgumentException('u');
		}
		switch g {
			case 0:
			case 1: set.push(IXGRP);
			case 2: set.push(IWGRP);
			case 3: set.push(IXGRP); set.push(IWGRP);
			case 4: set.push(IRGRP);
			case 5: set.push(IXGRP); set.push(IRGRP);
			case 6: set.push(IWGRP); set.push(IRGRP);
			case 7: set.push(IRWXG);
			case _: throw new ArgumentException('g');
		}
		switch o {
			case 0:
			case 1: set.push(IXOTH);
			case 2: set.push(IWOTH);
			case 3: set.push(IXOTH); set.push(IWOTH);
			case 4: set.push(IROTH);
			case 5: set.push(IXOTH); set.push(IROTH);
			case 6: set.push(IWOTH); set.push(IROTH);
			case 7: set.push(IRWXO);
			case _: throw new ArgumentException('g');
		}
		switch s {
			case 0:
			case 1: set.push(ISVTX);
			case 2: set.push(ISGID);
			case 3: set.push(ISVTX); set.push(ISGID);
			case 4: set.push(ISUID);
			case 5: set.push(ISVTX); set.push(ISUID);
			case 6: set.push(ISGID); set.push(ISUID);
			case 7: set.push(ISVTX); set.push(ISGID); set.push(ISUID);
			case _: throw new ArgumentException('s');
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
		var set = [];
		if(dec & (1 << 0) != 0) set.push(IXOTH);
		if(dec & (1 << 1) != 0) set.push(IWOTH);
		if(dec & (1 << 2) != 0) set.push(IROTH);
		if(dec & (1 << 3) != 0) set.push(IXGRP);
		if(dec & (1 << 4) != 0) set.push(IWGRP);
		if(dec & (1 << 5) != 0) set.push(IRGRP);
		if(dec & (1 << 6) != 0) set.push(IXUSR);
		if(dec & (1 << 7) != 0) set.push(IWUSR);
		if(dec & (1 << 8) != 0) set.push(IRUSR);
		if(dec & (1 << 9) != 0) set.push(ISVTX);
		if(dec & (1 << 10) != 0) set.push(ISGID);
		if(dec & (1 << 11) != 0) set.push(ISUID);
		return new FilePermissions(set);
	}

	@:to function toDecimal():Int {
		var result = 0;
		for(v in this) {
			switch v {
				case IXOTH: result = result | 1;
				case IWOTH: result = result | (1 << 1);
				case IROTH: result = result | (1 << 2);
				case IRWXO: result = result | 1 | (1 << 1) | (1 << 2);
				case IXGRP: result = result | (1 << 3);
				case IWGRP: result = result | (1 << 4);
				case IRGRP: result = result | (1 << 5);
				case IRWXG: result = result | (1 << 3) | (1 << 4) | (1 << 5);
				case IXUSR: result = result | (1 << 6);
				case IWUSR: result = result | (1 << 7);
				case IRUSR: result = result | (1 << 8);
				case IRWXU: result = result | (1 << 6) | (1 << 7) | (1 << 8);
				case ISVTX: result = result | (1 << 9);
				case ISGID: result = result | (1 << 10);
				case ISUID: result = result | (1 << 11);
				case _:
			}
		}
		return result;
	}

	@:op(A & B) static function intersect(perm1:FilePermissions, perm2:FilePermissions):FilePermissions {
		return fromDecimal((perm1:Int) & (perm2:Int));
	}

	@:op(A | B) static function merge(perm1:FilePermissions, perm2:FilePermissions):FilePermissions {
		return fromDecimal((perm1:Int) & (perm2:Int));
	}

	@:op(A == B) static function equals(perm1:Null<FilePermissions>, perm2:Null<FilePermissions>):Bool {
		var p1:Array<FileMode> = perm1;
		var p2:Array<FileMode> = perm2;
		if(p1 == p2) {
			return true;
		} else if(p1 == null || p2 == null) {
			return false;
		} else {
			return (perm1:Int) == (perm2:Int);
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