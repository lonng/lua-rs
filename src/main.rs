extern crate lua;

use lua::state::State;

fn main() {
    let mut state = State::new();
    /*
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

        for a = 1, a < 10, 1 do end
        for a, b in xya do end
        repeat until x.b.c > 1000

        function a.b.c:d(x,y,z)
            local function hello(x,b,c,d) end
            local x,c,d,d = 1+2,3+4,5+6,7+8
            return x,c.d,c.a
        end
        --]]
        a = 100
        b = a + 100
    ");
    */
    let result = state.load_file("_lua5.1-tests/verybig.lua");
    match result {
        Ok(_) => {}
        Err(e) => println!("Error=>{:?}", e)
    };
}
