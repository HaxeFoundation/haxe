mod HxObject;
pub struct Std;
pub impl Std {
	pub fn string(s: @HxObject) -> Option<@str> {
		return s.to_str();

		}
}
impl HxObject for Std {
}
