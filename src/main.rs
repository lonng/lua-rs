extern crate lua;

use lua::state::State;

fn main() {
    let mut state = State::new();
    let result = state.load_string(r"
        --[[if hello and hello1 or hello2 and hello3 then
            if {
                a = b,
                c = d;
                [e] = f,
                ['x'] = 'x',
            } then
            end
        elseif world then
        elseif world2 then
        elseif world3 then
        else
            if {
                a = b,
                c = d;
                [e] = f,
                ['x'] = x,
            } then
            end
        end

        do
            if hello2 then
            end
        end

        if function(a, b, c, ...) if a then  end end then
        end
        if a.b.c.d.e.f then end
        if a[b+c*x+d+c] then end
        if a:x(y,b, z(cc)) then end
        while x.y.z:w(a,b,c) do end
        --]]

        for a = 1, a < 10, 1 do end
        for a, b in xya do end
    ");
    //let result = state.load_file("lua-tests/api.lua");
    match result {
        Ok(_) => {}
        Err(e) => println!("Error=>{:?}", e)
    };
}
