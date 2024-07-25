class Main {
	static function main() {
		var collectionType = Type.resolveClass("data.WeaponCollection");
		if (collectionType != null) trace("data.WeaponCollection has been generated");
	}
}
