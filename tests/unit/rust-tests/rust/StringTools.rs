use rust::Lib;
mod HxObject;
pub struct StringTools;
pub impl StringTools {
	pub fn startsWith(s: Option<@str>, start: Option<@str>) -> bool {
		return s.len() >= start.len() && s.substr(0i32,start.len()) == start;
	}
}
impl HxObject for StringTools {
}
