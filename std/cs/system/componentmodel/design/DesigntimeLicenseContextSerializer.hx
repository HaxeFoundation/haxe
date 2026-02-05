package cs.system.componentmodel.design;

/** Provides support for design-time license context serialization. */
@:native("System.ComponentModel.Design.DesigntimeLicenseContextSerializer")
extern class DesigntimeLicenseContextSerializer {
	/**
	 * Serializes the licenses within the specified design-time license context using
	 * the specified key and output stream.
	 * @param o The stream to output to.
	 * @param cryptoKey The key to use for encryption.
	 * @param context A  indicating the license context.
	 */
	static function Serialize(o:cs.system.io.Stream, cryptoKey:String, context:cs.system.componentmodel.design.DesigntimeLicenseContext):Void;
}
