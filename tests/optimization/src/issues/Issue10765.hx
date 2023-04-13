package issues;

@:forward
private abstract ComplexArray(js.lib.DataView) {
	public inline function new(length: Int) {
		final buffer = new js.lib.ArrayBuffer(length * 2 * 4);
		this = new js.lib.DataView(buffer, 0, buffer.byteLength);
	}

	@:arrayAccess
	public static inline function get(impl: ComplexArray, index: Int): Complex {
		return new Complex(impl.getFloat32(index * 2 * 4), impl.getFloat32((index * 2 + 1) * 4));
	}

	@:arrayAccess
	public static inline function set(impl: ComplexArray, index: Int, value: Complex): Complex {
		impl.setFloat32(index * 2 * 4, value.real);
		impl.setFloat32((index * 2 + 1) * 4, value.imag);
		return value;
	}
}

private class Complex {
	public var real: Float;
	public var imag: Float;

	public inline function new(real: Float, imag: Float) {
		this.real = real;
		this.imag = imag;
	}

	public inline function add(other: Complex): Complex {
		return new Complex(this.real + other.real, this.imag + other.imag);
	}
}

class Issue10765 {
	@:js('
		var buffer = new ArrayBuffer(80);
		var array = new DataView(buffer,0,buffer.byteLength);
		var real = array.getFloat32(0);
		var imag = array.getFloat32(4);
		array.setFloat32(0,real + real);
		array.setFloat32(4,imag + imag);
	')
	static function test() {
		final array = new ComplexArray(10);

		final tmp = array[0];
		array[0] = tmp.add(tmp);
	}
}