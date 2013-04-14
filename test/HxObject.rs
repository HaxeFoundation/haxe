pub trait HxObject {
	pub fn __get_field(&self, field:@str) -> Option<HxObject>;
	pub fn __set_field(&mut self, field:@str, value:Option<@HxObject>) -> Option<@HxObject>;
	pub fn toString(&self) -> @str;
}
impl ToStr for HxObject {
	pub fn to_str(&self) -> ~str {
		return self.toString();
	}
}
impl HxObject for Option<@BaseIter> {
	pub fn __get_field(&self, &field:str)->Option<@HxObject> {
		return None;
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
	pub fn __fields(&mut self) -> Option<@[@str]> {
		return __instance_fields();
	}
	pub fn __instance_fields() -> Option<@[@str]> {
		return Some(@[]);
	}
	pub fn __name() -> Option<@str> {
		return Some(@BaseIter);
	}
}
impl HxObject for Option<@vec> {
	pub fn __get_field(&self, &field:str)->Option<@HxObject> {
		return match(field) {
			"length" => Some(self.length),
			"size_hint" => self.size_hint,
			"each" => self.each,
			_ => None
		}
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"length" => self.length = value,
			_ => None
		}
	}
	pub fn __fields(&mut self) -> Option<@[@str]> {
		return __instance_fields();
	}
	pub fn __instance_fields() -> Option<@[@str]> {
		return Some(@[@"length"]);
	}
	pub fn __name() -> Option<@str> {
		return Some(@vec);
	}
}
impl HxObject for Option<@ArrayAccess> {
	pub fn __get_field(&self, &field:str)->Option<@HxObject> {
		return None;
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
	pub fn __fields(&mut self) -> Option<@[@str]> {
		return __instance_fields();
	}
	pub fn __instance_fields() -> Option<@[@str]> {
		return Some(@[]);
	}
	pub fn __name() -> Option<@str> {
		return Some(@ArrayAccess);
	}
}
impl HxObject for Option<@str> {
	pub fn __get_field(&self, &field:str)->Option<@HxObject> {
		return match(field) {
			"length" => Some(self.length),
			"toString" => self.toString,
			"substring" => self.substring,
			"substr" => self.substr,
			"split" => self.split,
			"lastIndexOf" => self.lastIndexOf,
			"indexOf" => self.indexOf,
			"charCodeAt" => self.charCodeAt,
			"charAt" => self.charAt,
			"toLowerCase" => self.toLowerCase,
			"toUpperCase" => self.toUpperCase,
			_ => None
		}
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"length" => self.length = value,
			_ => None
		}
	}
	pub fn __fields(&mut self) -> Option<@[@str]> {
		return __instance_fields();
	}
	pub fn __instance_fields() -> Option<@[@str]> {
		return Some(@[@"length"]);
	}
	pub fn __name() -> Option<@str> {
		return Some(@String);
	}
}
impl HxObject for Option<@Type> {
	pub fn __get_field(&self, &field:str)->Option<@HxObject> {
		return None;
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
	pub fn __fields(&mut self) -> Option<@[@str]> {
		return __instance_fields();
	}
	pub fn __instance_fields() -> Option<@[@str]> {
		return Some(@[]);
	}
	pub fn __name() -> Option<@str> {
		return Some(@Type);
	}
}
impl HxObject for Option<@haxe::EnumTools> {
	pub fn __get_field(&self, &field:str)->Option<@HxObject> {
		return None;
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
	pub fn __fields(&mut self) -> Option<@[@str]> {
		return __instance_fields();
	}
	pub fn __instance_fields() -> Option<@[@str]> {
		return Some(@[]);
	}
	pub fn __name() -> Option<@str> {
		return Some(@haxe.EnumTools);
	}
}
impl HxObject for Option<@haxe::EnumValueTools> {
	pub fn __get_field(&self, &field:str)->Option<@HxObject> {
		return None;
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		}
	pub fn __fields(&mut self) -> Option<@[@str]> {
		return __instance_fields();
	}
	pub fn __instance_fields() -> Option<@[@str]> {
		return Some(@[]);
	}
	pub fn __name() -> Option<@str> {
		return Some(@haxe.EnumValueTools);
	}
}
