extern crate lua;

use lua::state::State;

fn main() {
    let mut state = State::new();
    let result = state.load_string(r"
        if hello and hello1 or hello2 and hello3 then
        elseif world then
        else
        end
    ");
    //let result = state.load_file("lua-tests/api.lua");
    match result {
        Ok(_) => {}
        Err(e) => println!("Error=>{:?}", e)
    };
}
