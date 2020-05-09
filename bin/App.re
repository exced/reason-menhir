let () = {
    let filename = Sys.argv[1];
    Lib.Driver.print_ast(filename);
};