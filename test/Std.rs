mod HxObject;
mod HxEnum;
pub struct Std ;
pub impl Std {
	pub fn string(s: Option<@HxObject>) -> Option<@str> {
		return None;
	}
}
impl HxObject for Option<@Std> {
	pub fn __get_field(&self, &field:str) {
		return None;
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		}
}
