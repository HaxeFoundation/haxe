mod HxObject;
mod HxEnum;
pub struct TestInterface {
	, , 
}
pub trait TestInterface {fn isAwesome() : i32;
}
impl HxObject for Option<@TestInterface> {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"obj" => Some(self.obj),
			"awesomeness" => Some(self.awesomeness),
			"objs" => Some(self.objs),
			_ => None
		}
	}
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"obj" => self.obj = value,
			"awesomeness" => self.awesomeness = value,
			"objs" => self.objs = value,
			_ => None
		}
	}
}
