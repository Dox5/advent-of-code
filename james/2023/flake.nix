{
    inputs = {
        nixpkgs.url = "nixpkgs/23.11";
        flake-parts = {
            url = "github:hercules-ci/flake-parts";
            inputs.nixpkgs-lib.follows = "nixpkgs";
        };
    };

    outputs = inputs@{ flake-parts, ... }: flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [
            "x86_64-linux"
        ];

        perSystem = { pkgs, lib, self', ... }: 
        let
            fs = lib.fileset;
        in {
            packages.util = pkgs.clangStdenv.mkDerivation {
                pname = "util";
                version = "0.0.0";

                buildInputs = [
                    pkgs.cmake
                    pkgs.libargs
                ];

                src = fs.toSource {
                    root = ./util;
                    fileset = fs.unions [
                        ./util/CMakeLists.txt
                        (fs.fileFilter
                          (file: file.hasExt "h" || file.hasExt "cpp")
                         ./util
                        )
                    ];
                };
            };

            packages.day01 = pkgs.clangStdenv.mkDerivation {
                pname = "day01";
                version = "0.0.0";

                buildInputs = [
                    pkgs.cmake
                    self'.packages.util
                ];

                src = fs.toSource {
                    root = ./day01;
                    fileset = fs.unions [
                        ./day01/CMakeLists.txt
                        (fs.fileFilter
                          (file: file.hasExt "h" || file.hasExt "cpp")
                         ./day01
                        )
                    ];
                };
            };
        };
    };
}
