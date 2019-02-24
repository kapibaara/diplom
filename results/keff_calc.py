import os


def read_file(file):
    lines = []
    with open(file, 'r') as f:
        lines = f.readlines()
    return lines


def write_file(file, lines):
    with open(file, 'w') as f:
        f.writelines(lines)


def change_concentrations(lines, nH, nD, nO):
    h_identifier = "@ *H*   @"
    d_identifier = "@  D    @"
    o_identifier = "@ *O*   @"

    new_file_lines = []

    for line in lines:
        if h_identifier in line:
            splited = line.split(" ")
            splited[10] = nH + ",\n"
            new_file_lines.append(' '.join(splited))
        elif d_identifier in line:
            splited = line.split(" ")
            splited[12] = nD + ",\n"
            new_file_lines.append(' '.join(splited))
        elif o_identifier in line:
            splited = line.split(" ")
            splited[10] = nO + ",\n"
            new_file_lines.append(' '.join(splited))
        else:
            new_file_lines.append(line)
    return new_file_lines


def get_useful_data(lines):
    keff_indicator = "***k(inf)="
    flux_indicator = "*grp*flux 1/cm2c"

    for i, line in enumerate(lines):
        if keff_indicator in line:
            splited = line.split(keff_indicator)
            if splited[1].startswith(" "):
                keff = splited[1].split(" ")[1]
            else:
                keff = splited[1].split(" ")[0]
        elif flux_indicator in line:
            numbers = lines[i + 1].split(" ")
            flux_fast = numbers[5]

            numbers = lines[i + 2].split(" ")
            flux_heat = numbers[5]

    return "{} {} {}\n".format(keff, flux_fast, flux_heat)


def exec_getera():
    os.chdir("../getera")
    os.system("getera.exe")
    os.chdir("../results")


GETERA_INPUT_FILE = "../getera/praktice_1.dat"
GETERA_OUTPUT_FILE = "../getera/praktice_1.out"
INPUT_FILE = "input.txt"
OUTPUT_FILE = "output.txt"

if __name__ == "__main__":
    input = read_file("input.txt")
    getera_input = read_file(GETERA_INPUT_FILE)
    output = []
    for line in input:
        nH, nD, nO = [str.rstrip() for str in line.split("\t")]
        getera_input = change_concentrations(getera_input, nH, nD, nO)
        write_file(GETERA_INPUT_FILE, getera_input)
        exec_getera()
        getera_output = read_file(GETERA_OUTPUT_FILE)
        output.append(get_useful_data(getera_output))
    write_file(OUTPUT_FILE, output)
