
package python.lib.io;

@:import("io", "BytesIO")
extern class BytesIO extends python.lib.io.BufferedIOBase {

	public function new (base:python.lib.io.IOBase);

}