#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
    Pkg.describe "Z3x" @@ fun c ->
    Ok [
        Pkg.mllib ~api:["Z3x"] "src/Z3x.mllib";
        Pkg.test "test/test";
    ]
