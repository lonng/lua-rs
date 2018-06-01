extern crate lua;

use lua::state::State;

fn main() {
    let mut state = State::new();
    state.load_string("hello world").unwrap();
}
