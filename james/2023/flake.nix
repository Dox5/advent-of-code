{
    inputs = {
        nixpkgs.url = "nixpkgs/23.11";
        flake-parts.url = "github:hercules-ci/flake-parts";
    };

    outputs = inputs@{ flake-parts, ... }: flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [
            "x86_64-linux"
        ];

        perSystem = { pkgs, ... }: {
            packages.day01 = pkgs.clangStdenv.mkDerivation {
                pname = "day01";
                version = "0.0.0";

                buildInputs = [
                    pkgs.cmake
                ];

                src = ./day01;
            };
        };
    };
}
