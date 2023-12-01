#include "framework.h"

#include <args.hxx>

#include <fstream>
#include <functional>
#include <iostream>
#include <string_view>
#include <string>

auto aoc::make_runner(std::string_view name, part_runner part_a_runner, part_runner part_b_runner) -> std::function<main_signature> {
    return [name, part_a_runner, part_b_runner](int argc, char **argv) {
        auto parser = args::ArgumentParser{std::string{name}};
        auto help = args::HelpFlag{parser, "help", "display help", {'h', "help"}};

	    auto mode = args::Group{parser, "Which puzzle part to run (exclusive)", args::Group::Validators::Xor};
        auto part_a = args::Flag{mode, "part-a", "Run the part-a solution", {"part-a"}};
        auto part_b = args::Flag{mode, "part-b", "Run the part-b solution", {"part-b"}};

        auto input = args::Positional<std::string>{parser, "puzzle input", "Input to the puzzle (or - for stdin)", args::Options::Required };

        try {
            parser.ParseCLI(argc, argv);
        } catch (args::Help const &) {
            std::cerr << parser;
            return 0;
        } catch (args::ParseError const& e) {
            std::cerr << e.what() << std::endl;
            std::cerr << parser;
            return 1;
        } catch (args::ValidationError const &e) {
	    	std::cerr << e.what() << std::endl;
	    	std::cerr << parser;
            return 1;
	    }

        auto process_part = [&part_a, &part_b, &part_a_runner, &part_b_runner](std::istream &input) {
            if(part_a) {
                return part_a_runner(input, {});
            } else if (part_b) {
                return part_b_runner(input, {});
            } else {
                std::cerr << "part selection error!\n";
                return 1;
            }
        };

        auto const input_file = args::get(input);

        if(input_file == "-") {
            return process_part(std::cin);
        } else {
            auto file_stream = std::ifstream{input_file};
            if(!file_stream) {
                std::cerr << "Failed to open file " << input_file << "\n";
                return 2;
            }
            return process_part(file_stream);
        }
    };
}
