
let test () = begin
    assert (Lib.Main.sieve_mod 1000 == 168);
    assert (Lib.Main.sieve_mod 100 == 25);
    assert (Lib.Main.sieve_heap 1000 == 168);
    assert (Lib.Main.sieve_heap 100 == 25)
  end

let _ = test ()
