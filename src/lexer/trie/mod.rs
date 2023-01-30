// // File: src/lexer/trie/mod.rs

use super::keywords::KEYWORDS;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TrieMatch {
	Prefix(usize),
	Exact(usize),
	ExactLongest,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Node {
	// data index in data_arena, child indices in node_arena
	Inner(usize, Vec<usize>),
	InnerEndWord(usize, Vec<usize>),
	LeafEndWord(usize),
}
impl Node {
	fn data_index(&self) -> usize {
		match self {
			Node::Inner(i, _) | Node::InnerEndWord(i, _) | Node::LeafEndWord(i) => *i,
		}
	}
	fn children_indices(&self) -> Option<&[usize]> {
		match self {
			Node::Inner(_i, children_i) | Node::InnerEndWord(_i, children_i) => Some(children_i),
			Node::LeafEndWord(_i) => None,
		}
	}
	fn add_child(&mut self, i: usize) {
		match self {
			Node::Inner(_i, children_i) | Node::InnerEndWord(_i, children_i) => children_i.push(i),
			Node::LeafEndWord(_i) => (),
		}
	}
	fn is_leaf(&self) -> bool {
		matches!(self, Node::LeafEndWord(_))
	}
}

#[derive(Clone, Debug)]
pub struct Trie<const ALPHEBET_SIZE: usize> {
	root_children_ids: Vec<usize>,
	node_arena: Vec<Node>,
	alphebet: [char; ALPHEBET_SIZE],
}

pub const ALPABET_SIZE: usize = 28;
impl Default for Trie<ALPABET_SIZE> {
	fn default() -> Self {
		let alphebet = [
			'F', '_', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
			'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		];
		let mut trie = Trie {
			root_children_ids: vec![],
			node_arena: vec![],
			alphebet,
		};

		for keyword in KEYWORDS.iter() {
			trie.insert_word(keyword);
		}

		trie
	}
}
impl<const ALPHEBET_SIZE: usize> Trie<ALPHEBET_SIZE> {
	#[inline]
	pub fn is_match(&self, s: &str) -> Option<TrieMatch> {
		let mut char_iter = s.chars();
		let mut current_node_id = self.root_child_id(&char_iter.next().unwrap())?;
		for c in char_iter {
			current_node_id = self.next_node_id_from_id(&c, current_node_id)?;
		}
		match self.node(current_node_id).unwrap() {
			Node::Inner(_, _) => Some(TrieMatch::Prefix(current_node_id)),
			Node::InnerEndWord(_, _) => Some(TrieMatch::Exact(current_node_id)),
			Node::LeafEndWord(_) => Some(TrieMatch::ExactLongest),
		}
	}
	pub fn is_match_from(&self, s: &str, id: Option<usize>) -> Option<TrieMatch> {
		let mut current_node_id = match id {
			Some(id) => id,
			None => return self.is_match(s),
		};
		for c in s.chars() {
			current_node_id = self.next_node_id_from_id(&c, current_node_id)?;
		}
		match self.node(current_node_id).unwrap() {
			Node::Inner(_, _) => Some(TrieMatch::Prefix(current_node_id)),
			Node::InnerEndWord(_, _) => Some(TrieMatch::Exact(current_node_id)),
			Node::LeafEndWord(_) => Some(TrieMatch::ExactLongest),
		}
	}
	pub fn is_prefix(&self, s: &str) -> bool {
		matches!(self.is_match(s), Some(_))
	}

	#[allow(dead_code)]
	fn root_child_node(&self, c: &char) -> Option<&Node> {
		let id = self.root_child_id(c)?;
		self.node(id)
	}
	fn root_child_id(&self, c: &char) -> Option<usize> {
		let res = self
			.root_children_ids
			.as_slice()
			.binary_search_by(|probe| self.value_from_node_id(*probe).unwrap().cmp(c))
			.ok()?;
		Some(self.root_children_ids[res])
	}

	#[allow(dead_code)]
	fn next_node(&self, c: &char, parent: &Node) -> Option<&Node> {
		let id = self.next_node_id(c, parent)?;
		self.node(id)
	}
	fn next_node_id(&self, c: &char, parent: &Node) -> Option<usize> {
		let children = parent.children_indices()?;
		let res = children
			.binary_search_by(|probe| self.value_from_node_id(*probe).unwrap().cmp(c))
			.ok()?;
		Some(children[res])
	}
	fn next_node_id_from_id(&self, c: &char, parent_id: usize) -> Option<usize> {
		let parent = self.node(parent_id)?;
		self.next_node_id(c, parent)
	}

	fn value_from_node_id(&self, node_id: usize) -> Option<&char> {
		let node = self.node(node_id)?;
		self.value_of(node)
	}
	fn node(&self, id: usize) -> Option<&Node> {
		self.node_arena.get(id)
	}

	fn value_of(&self, node: &Node) -> Option<&char> {
		let i = node.data_index();
		self.value_at(i)
	}
	fn value_at(&self, i: usize) -> Option<&char> {
		self.alphebet.get(i)
	}

	pub fn insert_word(&mut self, word: &str) {
		let word_len = word.chars().count();

		let mut char_iter = word.chars().enumerate();
		let (mut i, mut c) = char_iter.next().unwrap();

		match self.root_child_id(&c) {
			// root child exists
			Some(root_child_id) => {
				// if root is leaf
				if self.node_arena[root_child_id].is_leaf() {
					self.replace_leaf(root_child_id);
					self.insert_new_segment_after(root_child_id, word_len, &mut char_iter);
					return;
				}

				(i, c) = char_iter.next().unwrap();
				let mut current_id = root_child_id;
				while i < word_len {
					if let Some(next_id) = self.next_node_id_from_id(&c, current_id) {
						if self.node_arena[next_id].is_leaf() {
							self.replace_leaf(next_id);
							self.insert_new_segment_after(next_id, word_len, &mut char_iter);
							break;
						} else {
							current_id = next_id;
							(i, c) = char_iter.next().unwrap();
						}
					} else {
						let next_id = self.node_arena.len();
						self.node_arena[current_id].add_child(next_id);
						current_id = next_id;

						let data_index = self.alphebet_index(&c).unwrap();
						if i == word_len - 1 {
							self.node_arena.push(Node::LeafEndWord(data_index));
						} else {
							self.node_arena.push(Node::Inner(data_index, vec![]));
							self.insert_new_segment_after(current_id, word_len, &mut char_iter);
						}
						break;
					}
				}
			}
			// no root child exists
			None => {
				let root_child_id = self.node_arena.len();
				self.root_children_ids.push(root_child_id);

				let data_index = self.alphebet_index(&c).unwrap();
				if i == word_len - 1 {
					self.node_arena.push(Node::LeafEndWord(data_index));
				} else {
					self.node_arena.push(Node::Inner(data_index, vec![]));
					self.insert_new_segment_after(root_child_id, word_len, &mut char_iter);
				}
			}
		}
	}
	fn insert_new_segment_after(
		&mut self,
		id: usize,
		word_len: usize,
		char_iter: &mut std::iter::Enumerate<std::str::Chars>,
	) {
		let (i, c) = char_iter.next().unwrap();
		let data_index = self.alphebet_index(&c).unwrap();
		let current_id = self.node_arena.len();
		self.node_arena.get_mut(id).unwrap().add_child(current_id);

		if i == word_len - 1 {
			self.node_arena.push(Node::LeafEndWord(data_index));
		} else {
			self.node_arena.push(Node::Inner(data_index, vec![]));
			self.insert_new_segment_after(current_id, word_len, char_iter);
		}
	}
	fn replace_leaf(&mut self, id: usize) {
		let data_index = self.node_arena[id].data_index();
		self.node_arena[id] = Node::InnerEndWord(data_index, vec![]);
	}
	fn alphebet_index(&self, c: &char) -> Option<usize> {
		self.alphebet.binary_search(c).ok()
	}
}
