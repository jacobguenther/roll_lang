// macros.rs

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct MacroData {
	pub in_bar: bool,
	pub source: String,
}
impl MacroData {
	pub fn new(in_bar: bool, source: &str) -> MacroData {
		MacroData {
			in_bar: in_bar,
			source: source.to_string(),
		}
	}
}

pub type Macros = HashMap<String, MacroData>;



#[cfg(feature = "web")]
use crate::web;


#[cfg(feature = "web")]
pub trait MacrosWebT {
	fn init() -> Macros;
	fn handle_macro_update_create(&mut self);
	fn handle_macro_change_in_bar(&mut self, name: &str);
	fn handle_macro_delete(&mut self, name: &str);

	fn handle_macro_select(&self, name: &str);

	fn source(&self, name: &str) -> Option<String>;
}



#[cfg(feature = "web")]
impl MacrosWebT for Macros {
	fn init() -> Macros {
		let macros = web::macros_from_cookies();
		for (name, data) in &macros {
			web::add_macro_to_table(name, data.in_bar);
			if data.in_bar {
				web::add_macro_to_bar(name);
			}
		}
		macros
	}
	fn handle_macro_update_create(&mut self) {
		let name = web::get_value::macros::create::name();
		let in_bar = web::get_value::macros::create::checkbox();
		let source = web::get_value::macros::create::source();

		if self.contains_key(&name) {
			// FIX ME replace with update macros row
			web::remove_macro_from_table(&name);

			let currently_in_bar = self.get(&name).unwrap().in_bar;
			if in_bar && !currently_in_bar {
				web::add_macro_to_bar(&name);
			} else if !in_bar && currently_in_bar {
				web::remove_macro_from_bar(&name);
			}
		} else {
			if in_bar {
				web::add_macro_to_bar(&name);
			}
		}
		web::add_macro_to_table(&name, in_bar);

		let data = MacroData::new(in_bar, &source);
		web::cookies::add_macro(&name, &data);
		self.insert(name, data);
	}
	fn handle_macro_change_in_bar(&mut self, name: &str) {
		if self.contains_key(name) {
			let in_bar = web::get_value::macros::create::checkbox();
			let mut data = self.get(name).unwrap().clone();
			data.in_bar = in_bar;
			self.insert(name.to_string(), data);
			if in_bar {
				web::add_macro_to_bar(name);
			} else {
				web::remove_macro_from_bar(name);
			}
		}
	}
	fn handle_macro_delete(&mut self, name: &str) {
		if self.contains_key(name) {
			web::cookies::remove_macro(name);
			web::remove_macro_from_table(name);
			if self.get(name).unwrap().in_bar {
				web::remove_macro_from_bar(name);
			}
			self.remove(name);
		}
	}

	fn handle_macro_select(&self, name: &str) {
		use web::id;
		use wasm_bindgen::JsCast;
		web::element::get_element(&id::macros::create_mod::name())
			.dyn_ref::<web_sys::HtmlInputElement>().unwrap()
			.set_value(name);

		let data = self.get(name).unwrap();
		web::element::get_element(&id::macros::create_mod::source())
			.dyn_ref::<web_sys::HtmlTextAreaElement>().unwrap()
			.set_value(&data.source);

		web::element::get_element(&id::macros::create_mod::shortcut_checkbox())
			.dyn_ref::<web_sys::HtmlInputElement>().unwrap()
			.set_checked(data.in_bar);	
	}
	fn source(&self, name: &str) -> Option<String> {
		let data = self.get(name)?;
		Some(data.source.clone())
	}
}